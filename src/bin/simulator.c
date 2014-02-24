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

#include "linted/io.h"
#include "linted/mq.h"
#include "linted/simulator.h"
#include "linted/unimq.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

enum event {
    SHUTDOWN_EVENT,
    TICK_EVENT,
    CONTROLLER_EVENT
};

struct controller_notify_data {
    linted_unimq simulator;
};

struct shutdowner_notify_data {
    linted_unimq simulator;
};

struct timer_data {
    linted_unimq simulator;
};

struct controller_state {
    int32_t x_left;
    int32_t x_right;

    int32_t y_up;
    int32_t y_down;
};

struct simulator_state {
    int32_t x_position;
    int32_t y_position;

    int32_t x_velocity;
    int32_t y_velocity;
};

static void on_controller_notification(union sigval sigval);
static void on_shutdowner_notification(union sigval sigval);
static void on_clock_tick(union sigval sigev_value);

static int handle_tick(linted_updater updater,
                       struct controller_state const * controller_state,
                       struct simulator_state * simulator_state);

static int handle_shutdown_messages(linted_shutdowner shutdowner,
                                    struct sigevent const * sigevent,
                                    bool * should_exit);

static int handle_controller_messages(linted_controller controller,
                                      struct sigevent const * sigevent,
                                      struct controller_state * controller_state);

static int32_t min(int32_t x, int32_t y);
static int32_t sign(int32_t x);

int linted_simulator_run(linted_controller const controller,
                         linted_shutdowner const shutdowner,
                         linted_updater const updater)
{
    int exit_status = -1;

    struct controller_state controller_state = {
        .x_left = 0,
        .x_right = 0,

        .y_up = 0,
        .y_down = 0
    };

    struct simulator_state simulator_state = {
        .x_position = 0,
        .y_position = 0,

        .x_velocity = 0,
        .y_velocity = 0
    };

    linted_unimq simulator;
    struct linted_unimq_attr attr = {
        .max_message_count = 1,
        .message_size = sizeof(enum event)
    };
    if (-1 == linted_unimq_init(&simulator, &attr)) {
        return -1;
    }

    struct timer_data timer_data = {.simulator = simulator };

    struct controller_notify_data controller_notify_data = {
        .simulator = simulator
    };

    struct shutdowner_notify_data shutdowner_notify_data = {
        .simulator = simulator
    };

    struct sigevent shutdowner_sigevent;
    memset(&shutdowner_sigevent, 0, sizeof shutdowner_sigevent);

    shutdowner_sigevent.sigev_notify = SIGEV_THREAD;
    shutdowner_sigevent.sigev_notify_function = on_shutdowner_notification;
    shutdowner_sigevent.sigev_value.sival_ptr = &shutdowner_notify_data;

    struct sigevent controller_sigevent;
    memset(&controller_sigevent, 0, sizeof controller_sigevent);

    controller_sigevent.sigev_notify = SIGEV_THREAD;
    controller_sigevent.sigev_notify_function = on_controller_notification;
    controller_sigevent.sigev_value.sival_ptr = &controller_notify_data;

    timer_t timer;
    {
        struct sigevent sevp;
        memset(&sevp, 0, sizeof sevp);

        sevp.sigev_notify = SIGEV_THREAD;
        sevp.sigev_notify_function = on_clock_tick;
        sevp.sigev_value.sival_ptr = &timer_data;

        if (-1 == timer_create(CLOCK_MONOTONIC, &sevp, &timer)) {
            return -1;
        }

        struct itimerspec itimer_spec;
        memset(&itimer_spec, 0, sizeof itimer_spec);

        long const second = 1000000000;

        /* Strangely, it_value has to be nozero */
        itimer_spec.it_value.tv_nsec = 1;

        itimer_spec.it_interval.tv_sec = 0;
        itimer_spec.it_interval.tv_nsec = second / 60;
        if (-1 == timer_settime(timer, 0, &itimer_spec, NULL)) {
            goto delete_timer;
        }
    }

    /* Start the processes off on already queued messages */
    if (-1 == handle_controller_messages(controller,
                                         &controller_sigevent,
                                         &controller_state)) {
        goto restore_notify;
    }

    {
        bool should_exit;
        if (-1 == handle_shutdown_messages(shutdowner,
                                           &shutdowner_sigevent,
                                           &should_exit)) {
            goto restore_notify;
        }
        if (should_exit) {
            goto exit_main_loop;
        }
    }

    for (;;) {
        enum event event;
        if (-1 == linted_unimq_receive(simulator, &event)) {
            goto restore_notify;
        }

        switch (event) {
        case SHUTDOWN_EVENT:{
                bool should_exit;
                if (-1 == handle_shutdown_messages(shutdowner,
                                                   &shutdowner_sigevent,
                                                   &should_exit)) {
                    goto restore_notify;
                }
                if (should_exit) {
                    goto exit_main_loop;
                }
                break;
            }

        case TICK_EVENT:
            if (-1 == handle_tick(updater, &controller_state,
                                  &simulator_state)) {
                goto restore_notify;
            }
            break;

        case CONTROLLER_EVENT:
            if (-1 == handle_controller_messages(controller,
                                                 &controller_sigevent,
                                                 &controller_state)) {
                goto restore_notify;
            }
            break;

        default:
            syslog(LOG_ERR, "Simulator received unexpected event: %u", event);
        }
    }

 exit_main_loop:
    exit_status = 0;

 restore_notify:
    {
        int errnum = errno;
        int notify_status = linted_shutdowner_notify(shutdowner, NULL);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == notify_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int notify_status = linted_controller_notify(controller, NULL);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == notify_status) {
            exit_status = -1;
        }
    }

 delete_timer:
    {
        int errnum = errno;
        int delete_status = timer_delete(timer);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == delete_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int destroy_status = linted_unimq_destroy(simulator);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == destroy_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}

static int handle_tick(linted_updater updater,
                       struct controller_state const * controller_state,
                       struct simulator_state * simulator_state)
{
    int32_t x_position = simulator_state->x_position;
    int32_t y_position = simulator_state->y_position;

    int32_t x_velocity = simulator_state->x_velocity;
    int32_t y_velocity = simulator_state->y_velocity;

    int32_t x_thrust = 2 * (controller_state->x_right - controller_state->x_left);
    int32_t y_thrust = 2 * (controller_state->y_up - controller_state->y_down);

    int32_t guess_x_velocity = x_thrust + x_velocity;
    int32_t guess_y_velocity = y_thrust + y_velocity;

    int32_t x_friction = min(imaxabs(guess_x_velocity), 1)
        * sign(guess_x_velocity);
    int32_t y_friction = min(imaxabs(guess_y_velocity), 1)
        * sign(guess_y_velocity);

    int32_t new_x_velocity = x_velocity + x_thrust + x_friction;
    int32_t new_y_velocity = y_velocity + y_thrust + y_friction;

    int32_t new_x_position = x_position + new_x_velocity;
    int32_t new_y_position = y_position + new_y_velocity;

    if (x_position != new_x_position || y_position != new_y_position) {
        struct linted_updater_update update = {
            .x_position = new_x_position,
            .y_position = new_y_position
        };

        int update_status;
        do {
            update_status = linted_updater_send_update(updater, &update);
        } while (-1 == update_status && EINTR == errno);
        if (-1 == update_status) {
            return -1;
        }
    }

    simulator_state->x_position = new_x_position;
    simulator_state->y_position = new_y_position;

    simulator_state->x_velocity = new_x_velocity;
    simulator_state->y_velocity = new_y_velocity;

    return 0;
}

static int handle_shutdown_messages(linted_shutdowner shutdowner,
                                    struct sigevent const * sigevent,
                                    bool * should_exit)
{
    if (-1 == linted_shutdowner_notify(shutdowner, sigevent)) {
        return -1;
    }

    bool should_exit_out = false;
    for (;;) {
        int read_status;
        do {
            read_status = linted_shutdowner_receive(shutdowner);
        } while (-1 == read_status && EINTR == errno);
        if (-1 == read_status) {
            if (EAGAIN == errno) {
                break;
            }

            return -1;
        }

        should_exit_out = true;
    }

    *should_exit = should_exit_out;
    return 0;
}

static int handle_controller_messages(linted_controller controller,
                                      struct sigevent const * sigevent,
                                      struct controller_state * controller_state)
{
    if (-1 == linted_controller_notify(controller, sigevent)) {
        return -1;
    }

    for (;;) {
        struct linted_controller_message message;

        int read_status;
        do {
            read_status = linted_controller_receive(controller, &message);
        } while (-1 == read_status && EINTR == errno);
        if (-1 == read_status) {
            if (EAGAIN == errno) {
                break;
            }

            return -1;
        }

        switch (message.type) {
        case LINTED_CONTROLLER_MOVEMENT:
            switch (message.direction) {
            case LINTED_CONTROLLER_LEFT:
                controller_state->x_left = message.moving;
                break;

            case LINTED_CONTROLLER_RIGHT:
                controller_state->x_right = message.moving;
                break;

            case LINTED_CONTROLLER_UP:
                controller_state->y_up = message.moving;
                break;

            case LINTED_CONTROLLER_DOWN:
                controller_state->y_down = message.moving;
                break;
            }
            break;

        default:
            syslog(LOG_ERR,
                   "Simulator received unexpected message type: %i",
                   message.type);
        }
    }

    return 0;
}

static void on_clock_tick(union sigval sigev_value)
{
    struct timer_data *const timer_data = sigev_value.sival_ptr;
    linted_unimq const simulator = timer_data->simulator;

    enum event event = TICK_EVENT;
    if (-1 == linted_unimq_send(simulator, &event)) {
        LINTED_FATAL_ERROR(errno, "could not send simulator event: %s",
                           linted_error_string_alloc(errno));
    }
}

static void on_controller_notification(union sigval sigval)
{
    struct controller_notify_data *notify_data = sigval.sival_ptr;
    linted_unimq const simulator = notify_data->simulator;

    enum event event = CONTROLLER_EVENT;
    if (-1 == linted_unimq_send(simulator, &event)) {
        LINTED_FATAL_ERROR(errno, "could not send simulator event: %s",
                           linted_error_string_alloc(errno));
    }
}

static void on_shutdowner_notification(union sigval sigval)
{
    struct shutdowner_notify_data *notify_data = sigval.sival_ptr;
    linted_unimq const simulator = notify_data->simulator;

    enum event event = SHUTDOWN_EVENT;
    if (-1 == linted_unimq_send(simulator, &event)) {
        LINTED_FATAL_ERROR(errno, "could not send simulator event: %s",
                           linted_error_string_alloc(errno));
    }
}

static int32_t min(int32_t x, int32_t y)
{
    return x < y ? x : y;
}

static int32_t sign(int32_t x)
{
    return x < 0 ? 1 : 0 == x ? 0 : -1;
}
