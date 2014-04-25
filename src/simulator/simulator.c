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
#include "config.h"

#include "linted/controller.h"
#include "linted/logger.h"
#include "linted/io.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define LOGGER_OPTION "--logger"
#define CONTROLLER_OPTION "--controller"
#define SHUTDOWNER_OPTION "--shutdowner"
#define UPDATER_OPTION "--updater"

#define ROTATION_SPEED (UINT32_MAX / 512)
#define DEAD_ZONE (INT32_MAX / 8)

struct action_state {
    int x:2;
    int y:2;

    int32_t x_tilt;
    int32_t y_tilt;

    bool jumping:1;
};

struct simulator_state {
    int_fast32_t x_position;
    int_fast32_t y_position;
    int_fast32_t z_position;

    int_fast32_t x_velocity;
    int_fast32_t y_velocity;
    int_fast32_t z_velocity;

    uint_fast32_t x_rotation;
    uint_fast32_t y_rotation;

    bool update_pending:1;
};

static errno_t on_timer_readable(int timer,
                                 struct action_state const *action_state,
                                 struct simulator_state *simulator_state);

static errno_t on_updater_writeable(linted_updater updater,
                                    struct simulator_state *simulator_state);

static errno_t on_shutdowner_readable(linted_shutdowner shutdowner,
                                      bool * should_exit);

static errno_t on_controller_readable(linted_controller controller,
                                      struct action_state *action_state);

static void simulate_forces(int_fast32_t * position,
                            int_fast32_t * velocity,
                            int_fast32_t thrust);
static void simulate_rotation(uint_fast32_t * rotation, int_fast32_t tilt);
static uint_fast32_t absolute(int_fast32_t x);
static int_fast32_t saturate(int_fast64_t x);
static int_fast32_t min(int_fast32_t x, int_fast32_t y);
static int_fast32_t sign(int_fast32_t x);

static errno_t missing_process_name(int fildes, struct linted_str package_name);
static errno_t simulator_help(int fildes, char const *program_name,
                              struct linted_str package_name,
                              struct linted_str package_url,
                              struct linted_str package_bugreport);
static errno_t on_bad_option(int fildes, char const *program_name,
                             char const *bad_option);
static errno_t try_for_more_help(int fildes, char const *program_name,
                                 struct linted_str help_option);
static errno_t version_text(int fildes, struct linted_str package_string,
                            struct linted_str copyright_year);
static errno_t missing_option(int fildes, char const *program_name,
                              struct linted_str help_option);

int main(int argc, char *argv[])
{
    if (argc < 1) {
        missing_process_name(STDERR_FILENO,
                             LINTED_STR(PACKAGE_TARNAME "-simulator"));
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;
    char const *bad_option = NULL;

    char const *logger_name = NULL;
    char const *controller_name = NULL;
    char const *shutdowner_name = NULL;
    char const *updater_name = NULL;
    for (unsigned ii = 1; ii < (unsigned)argc; ++ii) {
        char *argument = argv[ii];

        if (0 == strcmp(HELP_OPTION, argument)) {
            need_help = true;
        } else if (0 == strcmp(VERSION_OPTION, argument)) {
            need_version = true;
        } else if (0 == strncmp(argument, LOGGER_OPTION "=",
                                strlen(LOGGER_OPTION "="))) {
            logger_name = argument + strlen(LOGGER_OPTION "=");
        } else if (0 == strncmp(argument, CONTROLLER_OPTION "=",
                                strlen(CONTROLLER_OPTION "="))) {
            controller_name = argument + strlen(CONTROLLER_OPTION "=");
        } else if (0 == strncmp(argument, SHUTDOWNER_OPTION "=",
                                strlen(SHUTDOWNER_OPTION "="))) {
            shutdowner_name = argument + strlen(SHUTDOWNER_OPTION "=");
        } else if (0 == strncmp(argument, UPDATER_OPTION "=",
                                strlen(UPDATER_OPTION "="))) {
            updater_name = argument + strlen(UPDATER_OPTION "=");
        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        simulator_help(STDOUT_FILENO,
                       program_name,
                       LINTED_STR(PACKAGE_NAME),
                       LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        on_bad_option(STDERR_FILENO, program_name, bad_option);
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        version_text(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                     LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    if (NULL == controller_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(CONTROLLER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == shutdowner_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(SHUTDOWNER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == updater_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(UPDATER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    linted_logger logger;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(logger_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   LOGGER_OPTION,
                                   linted_error_string_alloc(errnum));
            try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        logger = fd;
    }

    linted_controller controller;
    {
        int fd;
        int errnum = linted_io_strtofd(controller_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   CONTROLLER_OPTION,
                                   linted_error_string_alloc(errnum));
            try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        controller = fd;
    }

    linted_shutdowner shutdowner;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(shutdowner_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   SHUTDOWNER_OPTION,
                                   linted_error_string_alloc(errnum));
            try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        shutdowner = fd;
    }

    linted_updater updater;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(updater_name, &fd);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "%s: %s argument: %s\n",
                                   program_name,
                                   UPDATER_OPTION,
                                   linted_error_string_alloc(errnum));
            try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        updater = fd;
    }

    fcntl(logger, F_SETFD, fcntl(logger, F_GETFD) | FD_CLOEXEC);
    fcntl(updater, F_SETFD, fcntl(updater, F_GETFD) | FD_CLOEXEC);
    fcntl(shutdowner, F_SETFD, fcntl(shutdowner, F_GETFD) | FD_CLOEXEC);
    fcntl(controller, F_SETFD, fcntl(controller, F_GETFD) | FD_CLOEXEC);

    {
        int kept_fds[] = {
            STDERR_FILENO,
            STDIN_FILENO,
            STDOUT_FILENO,

            logger,
            controller,
            updater,
            shutdowner
        };

        errno_t errnum = linted_util_sanitize_environment(kept_fds,
                                                          LINTED_ARRAY_SIZE(kept_fds));
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s", program_name, linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
    }

    {
        static char const message[] = "starting simulator";
        linted_logger_log(logger, message, sizeof message - 1);
    }

    errno_t error_status = 0;

    struct action_state action_state = {.x = 0,.y = 0,.jumping = false};

    struct simulator_state simulator_state = {
        .update_pending = true, /* Initialize the gui at start */

        .x_position = 0,
        .y_position = 0,
        .z_position = 3 * 255,

        .x_velocity = 0,
        .y_velocity = 0,
        .z_velocity = 0,

        .x_rotation = UINT32_MAX / 2,
        .y_rotation = 0
    };

    int timer = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (-1 == timer) {
        error_status = errno;
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
            error_status = errno;
            goto close_timer;
        }
    }

    for (;;) {
        enum {
            SHUTDOWNER,
            TIMER,
            CONTROLLER,

            UPDATER
        };
        size_t fds_size;

        struct pollfd fds[] = {
            [SHUTDOWNER] = {.fd = shutdowner,.events = POLLIN},
            [TIMER] = {.fd = timer,.events = POLLIN},
            [CONTROLLER] = {.fd = controller,.events = POLLIN},

            [UPDATER] = {.fd = updater,.events = POLLOUT}
        };

        if (simulator_state.update_pending) {
            fds_size = LINTED_ARRAY_SIZE(fds);
        } else {
            fds_size = LINTED_ARRAY_SIZE(fds) - 1;
        }

        errno_t poll_status;
        do {
            int fds_active = poll(fds, fds_size, -1);
            poll_status = -1 == fds_active ? errno : 0;
        } while (EINTR == poll_status);
        if (poll_status != 0) {
            error_status = poll_status;
            goto close_timer;
        }

        if ((fds[SHUTDOWNER].revents & POLLIN) != 0) {
            bool should_exit;
            errno_t errnum = on_shutdowner_readable(shutdowner, &should_exit);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
            if (should_exit) {
                goto exit_main_loop;
            }
        }

        if ((fds[TIMER].revents & POLLIN) != 0) {
            errno_t errnum = on_timer_readable(timer, &action_state,
                                               &simulator_state);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
        }

        if ((fds[CONTROLLER].revents & POLLIN) != 0) {
            errno_t errnum = on_controller_readable(controller,
                                                    &action_state);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
        }

        if (simulator_state.update_pending
            && (fds[UPDATER].revents & POLLOUT) != 0) {
            errno_t errnum = on_updater_writeable(updater, &simulator_state);
            if (errnum != 0) {
                error_status = errnum;
                goto close_timer;
            }
        }
    }

 exit_main_loop:

 close_timer:
    {
        errno_t errnum = linted_io_close(timer);
        if (0 == error_status) {
            assert(errnum != EBADF);

            error_status = errnum;
        }
    }

 exit:
    return error_status;
}

static errno_t on_timer_readable(int timer,
                                 struct action_state const *action_state,
                                 struct simulator_state *simulator_state)
{
    uint64_t ticks;
    {
        errno_t errnum = linted_io_read_all(timer, NULL, &ticks, sizeof ticks);
        if (errnum != 0) {
            return errnum;
        }
    }

    for (size_t ii = 0; ii < ticks; ++ii) {
        simulate_forces(&simulator_state->x_position,
                        &simulator_state->x_velocity,
                        2 * (int_fast32_t) action_state->x);

        simulate_forces(&simulator_state->y_position,
                        &simulator_state->y_velocity,
                        2 * (int_fast32_t) action_state->y);

        simulate_forces(&simulator_state->z_position,
                        &simulator_state->z_velocity,
                        2 * (int_fast32_t) action_state->jumping);

        simulate_rotation(&simulator_state->x_rotation, action_state->x_tilt);
        simulate_rotation(&simulator_state->y_rotation, action_state->y_tilt);

        simulator_state->update_pending = true;
    }

    return 0;
}

static errno_t on_updater_writeable(linted_updater updater,
                                    struct simulator_state *simulator_state)
{
    struct linted_updater_update update = {
        .x_position = simulator_state->x_position,
        .y_position = simulator_state->y_position,
        .z_position = simulator_state->z_position,

        .x_rotation = simulator_state->x_rotation,
        .y_rotation = simulator_state->y_rotation
    };

    errno_t update_status;
    do {
        update_status = linted_updater_send_update(updater, &update);
    } while (EINTR == update_status);

    if (EAGAIN == update_status) {
        return 0;
    }

    if (update_status != 0) {
        return update_status;
    }

    simulator_state->update_pending = false;

    return 0;
}

static errno_t on_shutdowner_readable(linted_shutdowner shutdowner,
                                      bool * should_exit)
{
    errno_t read_status;
    do {
        read_status = linted_shutdowner_receive(shutdowner);
    } while (EINTR == read_status);

    if (EAGAIN == read_status) {
        *should_exit = false;
        return 0;
    }

    if (read_status != 0) {
        return read_status;
    }

    *should_exit = true;
    return 0;
}

static errno_t on_controller_readable(linted_controller controller,
                                      struct action_state *action_state)
{
    struct linted_controller_message message;

    errno_t read_status;
    do {
        read_status = linted_controller_receive(controller, &message);
    } while (EINTR == read_status);

    if (EAGAIN == read_status) {
        return 0;
    }

    if (read_status != 0) {
        return read_status;
    }

    action_state->x = message.right - message.left;
    action_state->y = message.up - message.down;

    action_state->x_tilt = message.x_tilt;
    action_state->y_tilt = message.y_tilt;

    action_state->jumping = message.jumping;

    return 0;
}

static void simulate_forces(int_fast32_t * position,
                            int_fast32_t * velocity,
                            int_fast32_t thrust)
{
    int_fast32_t old_position = *position;
    int_fast32_t old_velocity = *velocity;

    int_fast32_t guess_velocity = saturate(((int_fast64_t)thrust)
                                           + old_velocity);

    int_fast32_t friction = min(absolute(guess_velocity), 1)
            * -sign(guess_velocity);

    int_fast32_t new_velocity = saturate(((int_fast64_t)guess_velocity)
                                         + friction);
    int_fast32_t new_position = saturate(((int_fast64_t)old_position)
                                         + new_velocity);

    *position = new_position;
    *velocity = new_velocity;
}

static void simulate_rotation(uint_fast32_t * rotation, int_fast32_t tilt)
{
    uint_fast32_t step = linted_uint32_to_int32((absolute(tilt) > DEAD_ZONE) * sign(tilt)
        * ROTATION_SPEED);
    *rotation = (*rotation + step) % UINT32_MAX;
}

static int_fast32_t min(int_fast32_t x, int_fast32_t y)
{
    return x < y ? x : y;
}

static int_fast32_t sign(int_fast32_t x)
{
    return x > 0 ? 1 : 0 == x ? 0 : -1;
}

static int_fast32_t saturate(int_fast64_t x)
{
    if (x > INT32_MAX) {
        return INT32_MAX;
    }

    if (x < INT32_MIN) {
        return INT32_MIN;
    }

    return x;
}

static uint_fast32_t absolute(int_fast32_t x)
{
    /* The implicit cast to unsigned is okay, obviously */
    return INT32_MIN == x ? -(int_fast64_t)INT32_MIN : imaxabs(x);
}

static errno_t missing_process_name(int fildes, struct linted_str package_name)
{
    errno_t errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
: missing process name\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t simulator_help(int fildes, char const *program_name,
                              struct linted_str package_name,
                              struct linted_str package_url,
                              struct linted_str package_bugreport)
{
    errno_t errnum;

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" [OPTIONS]\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Run the simulator.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --logger            the logger file descriptor\n\
  --controller        the controller file descriptor\n\
  --updater           the updater file descriptor\n\
  --shutdowner        the shutdowner file descriptor\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t on_bad_option(int fildes, char const *program_name,
                             char const *bad_option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
: unrecognised option '"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_string(fildes, NULL, bad_option)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("'\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t try_for_more_help(int fildes, char const *program_name,
                                 struct linted_str help_option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("Try `"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(" "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, help_option)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
' for more information.\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t version_text(int fildes, struct linted_str package_string,
                            struct linted_str copyright_year)
{
    errno_t errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, package_string)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Copyright (C) "))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, copyright_year)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t missing_option(int fildes, char const *program_name,
                              struct linted_str option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(": missing "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, option)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" option\n"))) != 0) {
        return errnum;
    }

    return 0;
}
