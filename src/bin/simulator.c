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
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

typedef mqd_t simulator_type;

#define SHUTDOWN_EVENT ((uint8_t) 0)
#define TICK_EVENT ((uint8_t) (SHUTDOWN_EVENT + 1))
#define CONTROLLER_EVENT ((uint8_t) (TICK_EVENT + 1))

struct controller_notify_data {
    int simulator;
};

struct shutdowner_notify_data {
    int simulator;
};

struct timer_data {
    int simulator;
};

static void on_controller_notification(union sigval sigval);
static void on_shutdowner_notification(union sigval sigval);
static void on_clock_tick(union sigval sigev_value);

static int32_t min(int32_t x, int32_t y);
static int32_t sign(int32_t x);

int linted_simulator_run(linted_controller const controller,
                         linted_shutdowner const shutdowner,
                         linted_updater const updater)
{
    int exit_status = -1;

    int32_t x_left = 0;
    int32_t x_right = 0;

    int32_t y_up = 0;
    int32_t y_down = 0;

    int32_t x_position = 0;
    int32_t y_position = 0;

    int32_t x_velocity = 0;
    int32_t y_velocity = 0;

    int simulator_fds[2];
    if (-1 == pipe(simulator_fds)) {
        return -1;
    }

    int simulator_read = simulator_fds[0];
    int simulator_write = simulator_fds[1];

    struct timer_data timer_data = {.simulator = simulator_write };

    struct controller_notify_data controller_notify_data = {
        .simulator = simulator_write
    };

    struct shutdowner_notify_data shutdowner_notify_data = {
        .simulator = simulator_write
    };

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
    {
        union sigval sigvalue;
        sigvalue.sival_ptr = &controller_notify_data;
        on_controller_notification(sigvalue);
    }

    {
        union sigval sigvalue;
        sigvalue.sival_ptr = &shutdowner_notify_data;
        on_shutdowner_notification(sigvalue);
    }

    for (;;) {
        uint8_t event;
        if (-1 == linted_io_read_all(simulator_read, NULL,
                                     &event, sizeof event)) {
            goto restore_notify;
        }

        switch (event) {
        case SHUTDOWN_EVENT:{
            struct sigevent sigevent;
            memset(&sigevent, 0, sizeof sigevent);

            sigevent.sigev_notify = SIGEV_THREAD;
            sigevent.sigev_notify_function = on_shutdowner_notification;
            sigevent.sigev_value.sival_ptr = &shutdowner_notify_data;

            if (-1 == linted_shutdowner_notify(shutdowner, &sigevent)) {
                goto restore_notify;
            }


            for (;;) {
                int read_status;
                do {
                    read_status = linted_shutdowner_receive(shutdowner);
                } while (-1 == read_status && EINTR == errno);
                if (-1 == read_status) {
                    if (EAGAIN == errno) {
                        break;
                    }

                    goto restore_notify;
                }

                goto exit_main_loop;
            }
            break;
        }

        case TICK_EVENT:{
                int32_t x_thrust = 2 * (x_right - x_left);
                int32_t y_thrust = 2 * (y_up - y_down);

                int32_t x_future_velocity = x_thrust + x_velocity;
                int32_t y_future_velocity = y_thrust + y_velocity;

                int32_t x_friction = min(imaxabs(x_future_velocity), 1)
                    * sign(x_future_velocity);
                int32_t y_friction = min(imaxabs(y_future_velocity), 1)
                    * sign(y_future_velocity);

                x_velocity += x_thrust + x_friction;
                y_velocity += y_thrust + y_friction;

                x_position += x_velocity;
                y_position += y_velocity;

                struct linted_updater_update update = {
                    .x_position = x_position,
                    .y_position = y_position
                };

                int update_status;
                do {
                    update_status = linted_updater_send_update(updater, update);
                } while (-1 == update_status && EINTR == errno);
                if (-1 == update_status) {
                    goto restore_notify;
                }
                break;
            }

        case CONTROLLER_EVENT:{
            {
                struct sigevent sigevent;
                memset(&sigevent, 0, sizeof sigevent);

                sigevent.sigev_notify = SIGEV_THREAD;
                sigevent.sigev_notify_function = on_controller_notification;
                sigevent.sigev_value.sival_ptr = &controller_notify_data;

                if (-1 == linted_controller_notify(controller, &sigevent)) {
                    goto restore_notify;
                }
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
                }

                switch (message.type) {
                case LINTED_CONTROLLER_MOVEMENT:
                    switch (message.direction) {
                    case LINTED_CONTROLLER_LEFT:
                        x_left = message.moving;
                        break;

                    case LINTED_CONTROLLER_RIGHT:
                        x_right = message.moving;
                        break;

                    case LINTED_CONTROLLER_UP:
                        y_up = message.moving;
                        break;

                    case LINTED_CONTROLLER_DOWN:
                        y_down = message.moving;
                        break;
                    }
                    break;

                default:
                    syslog(LOG_ERR,
                           "Simulator received unexpected message type: %i",
                           message.type);
                }
            }
            break;
        }

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

    {
        int errnum = errno;
        int close_status = close(simulator_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = close(simulator_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
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

    return exit_status;
}

static void on_clock_tick(union sigval sigev_value)
{
    struct timer_data *const timer_data = sigev_value.sival_ptr;
    int const simulator = timer_data->simulator;

    uint8_t event = TICK_EVENT;
    if (-1 == linted_io_write_all(simulator, NULL, &event, sizeof event)) {
        LINTED_FATAL_ERROR("Could not send simulator event: %s",
                           linted_error_string_alloc(errno));
    }
}

static void on_controller_notification(union sigval sigval)
{
    struct controller_notify_data *notify_data = sigval.sival_ptr;
    int simulator = notify_data->simulator;

    uint8_t event = CONTROLLER_EVENT;
    if (-1 == linted_io_write_all(simulator, NULL, &event, sizeof event)) {
        LINTED_FATAL_ERROR("Could not send simulator event: %s",
                           linted_error_string_alloc(errno));
    }
}

static void on_shutdowner_notification(union sigval sigval)
{
    struct shutdowner_notify_data *notify_data = sigval.sival_ptr;
    int simulator = notify_data->simulator;

    uint8_t event = SHUTDOWN_EVENT;
    if (-1 == linted_io_write_all(simulator, NULL, &event, sizeof event)) {
        LINTED_FATAL_ERROR("Could not send simulator event: %s",
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
