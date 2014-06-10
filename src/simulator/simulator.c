/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 199309L

#include "config.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/controller.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/logger.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/timerfd.h>
#include <time.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define ROTATION_SPEED 512u
#define DEAD_ZONE (LINTED_UPDATER_INT_MAX / 8)

enum {
    ON_READ_TIMER,
    ON_RECEIVE_CONTROLLER_EVENT,
    ON_SENT_UPDATER_EVENT,
    MAX_TASKS
};

struct action_state
{
    linted_updater_int x_tilt;
    linted_updater_int y_tilt;

    int x : 2;
    int z : 2;

    bool jumping : 1;
};

struct simulator_state
{
    linted_updater_int x_position;
    linted_updater_int y_position;
    linted_updater_int z_position;

    linted_updater_int x_velocity;
    linted_updater_int y_velocity;
    linted_updater_int z_velocity;

    linted_updater_angle x_rotation;
    linted_updater_angle y_rotation;

    bool update_pending : 1;
    bool write_in_progress : 1;
};

struct sim_updater_task;

struct sim_tick_task
{
    struct linted_asynch_task_read parent;
    uint64_t timer_ticks;
    struct linted_asynch_pool *pool;
    struct sim_updater_task *updater_task;
    struct action_state const *action_state;
    struct simulator_state *simulator_state;
    linted_ko updater;
};

struct sim_controller_task
{
    struct linted_controller_task_receive parent;
    struct linted_asynch_pool *pool;
    struct action_state *action_state;
};

struct sim_updater_task
{
    struct linted_updater_task_send parent;
    struct simulator_state *simulator_state;
    struct linted_asynch_pool *pool;
    linted_ko updater;
};

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_read_timer(struct linted_asynch_task *completed_task);
static linted_error on_controller_receive(struct linted_asynch_task *task);
static linted_error on_sent_update(struct linted_asynch_task *completed_task);

static void simulate_forces(linted_updater_int *position,
                            linted_updater_int *velocity,
                            linted_updater_int thrust);
static void simulate_rotation(linted_updater_angle *rotation,
                              linted_updater_int tilt);
static void simulate_clamped_rotation(linted_updater_angle *rotation,
                                      linted_updater_int tilt);
static linted_updater_uint absolute(linted_updater_int x);
static linted_updater_int min_int(linted_updater_int x, linted_updater_int y);
static linted_updater_int sign(linted_updater_int x);

static linted_error simulator_help(linted_ko ko, char const *program_name,
                                   struct linted_str package_name,
                                   struct linted_str package_url,
                                   struct linted_str package_bugreport);

static linted_ko kos[3u + 3u];

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-simulator",
    .open_current_working_directory = false,
    .kos_size = LINTED_ARRAY_SIZE(kos),
    .kos = kos
};

uint_fast8_t linted_start(int cwd, char const *const program_name, size_t argc,
                          char const *const argv[const])
{
    linted_ko stdout = kos[1u];
    linted_ko stderr = kos[2u];

    linted_logger logger = kos[3u];
    linted_controller controller = kos[4u];
    linted_updater updater = kos[5u];

    bool need_help = false;
    bool need_version = false;
    char const *bad_option = NULL;

    for (size_t ii = 1u; ii < argc; ++ii) {
        char const *argument = argv[ii];

        if (0 == strcmp(HELP_OPTION, argument)) {
            need_help = true;
        } else if (0 == strcmp(VERSION_OPTION, argument)) {
            need_version = true;
        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        simulator_help(stdout, program_name, LINTED_STR(PACKAGE_NAME),
                       LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(stderr, program_name, bad_option);
        linted_locale_try_for_more_help(stderr, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(stdout, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    linted_error errnum;

    if ((errnum = linted_util_sanitize_environment()) != 0) {
        linted_io_write_format(stderr, NULL, "\
%s: can not sanitize the environment: %s",
                               program_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    {
        static char const message[] = "starting simulator";
        linted_logger_log(logger, message, sizeof message - 1);
    }

    struct action_state action_state = { .x = 0, .z = 0, .jumping = false };

    struct simulator_state simulator_state = {
        .update_pending = true, /* Initialize the gui at start */
        .write_in_progress = false,
        .x_position = 0,
        .y_position = 0,
        .z_position = 3 * 1024,
        .x_velocity = 0,
        .y_velocity = 0,
        .z_velocity = 0,
        .x_rotation = LINTED_UPDATER_ANGLE(1u, 2u),
        .y_rotation = LINTED_UPDATER_ANGLE(0u, 1u)
    };

    linted_ko timer =
        timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC);
    if (-1 == timer) {
        errnum = errno;
        assert(errnum != 0);
        goto exit;
    }

    {
        struct itimerspec itimer_spec;
        memset(&itimer_spec, 0, sizeof itimer_spec);

        long const second = 1000000000;

        /* Strangely, it_value has to be nozero */
        itimer_spec.it_value.tv_nsec = 1;

        itimer_spec.it_interval.tv_sec = 0;
        itimer_spec.it_interval.tv_nsec = second / 60;

        if (-1 == timerfd_settime(timer, 0, &itimer_spec, NULL)) {
            errnum = errno;
            assert(errnum != 0);
            goto close_timer;
        }
    }

    struct linted_asynch_pool *pool;
    {
        struct linted_asynch_pool *xx;
        if ((errnum = linted_asynch_pool_create(&xx, MAX_TASKS)) != 0) {
            goto close_timer;
        }
        pool = xx;
    }

    struct sim_tick_task timer_task;
    struct sim_controller_task controller_task;
    struct sim_updater_task updater_task;

    linted_asynch_read(LINTED_UPCAST(&timer_task), ON_READ_TIMER, timer,
                       (char *)&timer_task.timer_ticks,
                       sizeof timer_task.timer_ticks);
    timer_task.pool = pool;
    timer_task.updater_task = &updater_task;
    timer_task.action_state = &action_state;
    timer_task.simulator_state = &simulator_state;
    timer_task.updater = updater;

    linted_controller_receive(LINTED_UPCAST(&controller_task),
                              ON_RECEIVE_CONTROLLER_EVENT, controller);
    controller_task.pool = pool;
    controller_task.action_state = &action_state;

    linted_asynch_pool_submit(pool, LINTED_UPCAST(LINTED_UPCAST(&timer_task)));
    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&controller_task))));

    /* TODO: Detect SIGTERM and exit normally */
    for (;;) {
        struct linted_asynch_task *completed_tasks[20u];
        size_t task_count;
        linted_asynch_pool_wait(pool, completed_tasks,
                                LINTED_ARRAY_SIZE(completed_tasks),
                                &task_count);

        for (size_t ii = 0u; ii < task_count; ++ii) {
            if ((errnum = dispatch(completed_tasks[ii])) != 0) {
                goto destroy_pool;
            }
        }
    }

destroy_pool : {
    linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
    if (0 == errnum) {
        errnum = destroy_errnum;
    }
}

close_timer : {
    linted_error close_errnum = linted_ko_close(timer);
    assert(close_errnum != EBADF);

    if (0 == errnum) {
        errnum = close_errnum;
    }
}

exit:
    return errnum;
}

static linted_error dispatch(struct linted_asynch_task *completed_task)
{
    switch (completed_task->task_action) {
    default:
        assert(false);

    case ON_READ_TIMER:
        return on_read_timer(completed_task);

    case ON_RECEIVE_CONTROLLER_EVENT:
        return on_controller_receive(completed_task);

    case ON_SENT_UPDATER_EVENT:
        return on_sent_update(completed_task);
    }
}

static linted_error on_read_timer(struct linted_asynch_task *completed_task)
{
    linted_error errnum;

    if ((errnum = completed_task->errnum) != 0) {
        return errnum;
    }

    struct sim_tick_task *timer_task =
        LINTED_DOWNCAST(struct sim_tick_task, completed_task);

    struct linted_asynch_pool *pool = timer_task->pool;
    linted_ko updater = timer_task->updater;
    struct sim_updater_task *updater_task = timer_task->updater_task;
    struct action_state const *action_state = timer_task->action_state;
    struct simulator_state *simulator_state = timer_task->simulator_state;

    uint64_t timer_ticks;
    memcpy(&timer_ticks, &timer_task->timer_ticks, sizeof timer_ticks);

    linted_asynch_pool_submit(pool, completed_task);

    for (size_t ii = 0u; ii < timer_ticks; ++ii) {
        simulate_forces(&simulator_state->x_position,
                        &simulator_state->x_velocity,
                        8 * (linted_updater_int)action_state->x);

        simulate_forces(&simulator_state->z_position,
                        &simulator_state->z_velocity,
                        8 * (linted_updater_int)action_state->z);

        simulate_forces(&simulator_state->y_position,
                        &simulator_state->y_velocity,
                        -8 * (linted_updater_int)action_state->jumping);

        simulate_rotation(&simulator_state->x_rotation, action_state->x_tilt);
        simulate_clamped_rotation(&simulator_state->y_rotation,
                                  action_state->y_tilt);

        simulator_state->update_pending = true;
    }

    if (!simulator_state->update_pending ||
        simulator_state->write_in_progress) {
        return 0;
    }

    struct linted_updater_update update = {
        .x_position = simulator_state->x_position,
        .y_position = simulator_state->y_position,
        .z_position = simulator_state->z_position,
        .x_rotation = simulator_state->x_rotation,
        .y_rotation = simulator_state->y_rotation
    };

    linted_updater_send(LINTED_UPCAST(updater_task), ON_SENT_UPDATER_EVENT,
                        updater, &update);
    updater_task->simulator_state = simulator_state;
    updater_task->pool = pool;
    updater_task->updater = updater;

    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(updater_task))));

    simulator_state->update_pending = false;
    simulator_state->write_in_progress = true;

    return 0;
}

static linted_error on_controller_receive(struct linted_asynch_task *task)
{
    linted_error errnum;

    if ((errnum = task->errnum) != 0) {
        return errnum;
    }

    struct sim_controller_task *controller_task =
        LINTED_DOWNCAST(struct sim_controller_task, task);

    struct linted_asynch_pool *pool = controller_task->pool;
    struct action_state *action_state = controller_task->action_state;

    struct linted_controller_message message;
    if ((errnum = linted_controller_decode(LINTED_UPCAST(controller_task),
                                           &message)) != 0) {
        return errnum;
    }

    linted_asynch_pool_submit(pool, task);

    action_state->x = message.right - message.left;
    action_state->z = message.back - message.forward;

    action_state->x_tilt = -message.x_tilt;
    action_state->y_tilt = -message.y_tilt;

    action_state->jumping = message.jumping;

    return 0;
}

static linted_error on_sent_update(struct linted_asynch_task *completed_task)
{
    linted_error errnum;

    if ((errnum = completed_task->errnum) != 0) {
        return errnum;
    }

    struct sim_updater_task *updater_task =
        LINTED_DOWNCAST(struct sim_updater_task, completed_task);

    struct linted_asynch_pool *pool = updater_task->pool;
    linted_ko updater = updater_task->updater;
    struct simulator_state *simulator_state = updater_task->simulator_state;

    simulator_state->write_in_progress = false;

    if (!simulator_state->update_pending) {
        return 0;
    }

    struct linted_updater_update update = {
        .x_position = simulator_state->x_position,
        .y_position = simulator_state->y_position,
        .z_position = simulator_state->z_position,
        .x_rotation = simulator_state->x_rotation,
        .y_rotation = simulator_state->y_rotation
    };

    linted_updater_send(LINTED_UPCAST(updater_task), ON_SENT_UPDATER_EVENT,
                        updater, &update);
    updater_task->simulator_state = simulator_state;
    updater_task->pool = pool;
    updater_task->updater = updater;

    linted_asynch_pool_submit(pool, completed_task);

    simulator_state->update_pending = false;
    simulator_state->write_in_progress = true;

    return 0;
}

static void simulate_forces(linted_updater_int *position,
                            linted_updater_int *velocity,
                            linted_updater_int thrust)
{
    linted_updater_int old_position = *position;
    linted_updater_int old_velocity = *velocity;

    linted_updater_int guess_velocity =
        linted_updater_isatadd(thrust, old_velocity);

    linted_updater_int friction =
        min_int(absolute(guess_velocity), 3 /* = μ Fₙ */) *
        -sign(guess_velocity);

    linted_updater_int new_velocity =
        linted_updater_isatadd(guess_velocity, friction);

    linted_updater_int new_position =
        linted_updater_isatadd(old_position, new_velocity);

    *position = new_position;
    *velocity = new_velocity;
}

static void simulate_rotation(linted_updater_angle *rotation,
                              linted_updater_int tilt)
{

    linted_updater_angle increment = LINTED_UPDATER_ANGLE(1, ROTATION_SPEED);

    *rotation = linted_updater_angle_add(
        (absolute(tilt) > DEAD_ZONE) * sign(tilt), *rotation,
        increment);
}

static void simulate_clamped_rotation(linted_updater_angle *rotation,
                                      linted_updater_int tilt)
{
    linted_updater_int tilt_sign = sign(tilt);

    linted_updater_angle new_rotation;

    if (absolute(tilt) <= DEAD_ZONE) {
        new_rotation = *rotation;
    } else {
        linted_updater_angle minimum = LINTED_UPDATER_ANGLE(15, 16);
        linted_updater_angle maximum = LINTED_UPDATER_ANGLE(3, 16);
        linted_updater_angle increment = LINTED_UPDATER_ANGLE(1, ROTATION_SPEED);

        new_rotation = linted_updater_angle_add_clamped(tilt_sign, minimum,
                                                        maximum, *rotation,
                                                        increment);
    }

    *rotation = new_rotation;
}

static linted_updater_int min_int(linted_updater_int x, linted_updater_int y)
{
    return x < y ? x : y;
}

static linted_updater_int sign(linted_updater_int x)
{
    return x > 0 ? 1 : 0 == x ? 0 : -1;
}

static linted_updater_uint absolute(linted_updater_int x)
{
    /* The implicit cast to unsigned is okay, obviously */
    return LINTED_UPDATER_INT_MIN == x ? -(int_fast64_t)LINTED_UPDATER_INT_MIN
                                       : imaxabs(x);
}

static linted_error simulator_help(linted_ko ko, char const *program_name,
                                   struct linted_str package_name,
                                   struct linted_str package_url,
                                   struct linted_str package_bugreport)
{
    linted_error errnum;

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(ko, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(" [OPTIONS]\n"))) !=
        0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Run the simulator.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --logger            the logger file descriptor\n\
  --controller        the controller file descriptor\n\
  --updater           the updater file descriptor\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}
