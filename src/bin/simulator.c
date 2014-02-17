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

#include "linted/mq.h"
#include "linted/simulator.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef mqd_t simulator_type;

enum simulator_message_type {
    SIMULATOR_SHUTDOWN,
    SIMULATORICK,

    SIMULATOR_MOVEMENT
};

struct simulator_message {
    enum simulator_message_type type;
    enum linted_controller_direction direction;
    bool moving;
};

struct controller_notify_data {
    simulator_type simulator;
    linted_controller controller;
};

struct shutdowner_notify_data {
    simulator_type simulator;
    linted_shutdowner shutdowner;
};

struct timer_data {
    simulator_type simulator;
};

static void on_controller_notification(union sigval sigval);
static void on_shutdowner_notification(union sigval sigval);
static void on_clock_tick(union sigval sigev_value);

static int32_t min(int32_t x, int32_t y);
static int32_t sign(int32_t x);

static int simulator_pair(simulator_type simulator[2]);
static int simulator_close(simulator_type const simulator);

static int simulator_send_movement(simulator_type simulator,
                                   enum linted_controller_direction direction,
                                   bool moving);
static int simulator_send_tick(simulator_type simulator);
static int simulator_send_shutdown(simulator_type const simulator);

static int simulator_receive(simulator_type simulator,
                             struct simulator_message *message);

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

    simulator_type simulator_mqs[2];
    if (-1 == simulator_pair(simulator_mqs)) {
        return -1;
    }

    int simulator_read = simulator_mqs[0];
    int simulator_write = simulator_mqs[1];

    struct timer_data timer_data = {.simulator = simulator_write };

    struct controller_notify_data controller_notify_data = {
        .controller = controller,
        .simulator = simulator_write
    };

    struct shutdowner_notify_data shutdowner_notify_data = {
        .shutdowner = shutdowner,
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
        struct simulator_message message;

        int read_status;
        do {
            read_status = simulator_receive(simulator_read, &message);
        } while (-1 == read_status && EINTR == errno);
        if (-1 == read_status) {
            goto restore_notify;
        }

        switch (message.type) {
        case SIMULATOR_SHUTDOWN:
            goto exit_main_loop;

        case SIMULATOR_MOVEMENT:
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

        case SIMULATORICK:{
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

        default:
            syslog(LOG_ERR, "Simulator received unexpected message type: %i",
                   message.type);
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
        int close_status = simulator_close(simulator_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = simulator_close(simulator_write);
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
    simulator_type const simulator = timer_data->simulator;

    int tick_status;
    do {
        tick_status = simulator_send_tick(simulator);
    } while (-1 == tick_status && EINTR == errno);
    if (-1 == tick_status) {
        LINTED_FATAL_ERROR("Could not send simulator tick: %s",
                           linted_error_string_alloc(errno));
    }
}

static void on_controller_notification(union sigval sigval)
{
    struct controller_notify_data *notify_data = sigval.sival_ptr;
    simulator_type simulator = notify_data->simulator;
    linted_controller controller = notify_data->controller;
    {
        struct sigevent sigevent;
        memset(&sigevent, 0, sizeof sigevent);

        sigevent.sigev_notify = SIGEV_THREAD;
        sigevent.sigev_notify_function = on_controller_notification;
        sigevent.sigev_value.sival_ptr = notify_data;

        if (-1 == linted_controller_notify(controller, &sigevent)) {
            LINTED_LAZY_DEV_ERROR("Could not reregister for notifications: %s",
                                  linted_error_string_alloc(errno));
        }
    }

    struct linted_controller_message message;

    for (;;) {
        int read_status;
        do {
            read_status = linted_controller_receive(controller, &message);
        } while (-1 == read_status && EINTR == errno);
        if (-1 == read_status) {
            if (EAGAIN == errno) {
                break;
            }

            LINTED_FATAL_ERROR("Could not receive message: %s",
                               linted_error_string_alloc(errno));
        }

        switch (message.type) {
        case LINTED_CONTROLLER_MOVEMENT:{
                int send_status;
                do {
                    send_status = simulator_send_movement(simulator,
                                                          message.direction,
                                                          message.moving);
                } while (-1 == send_status && EINTR == errno);
                if (-1 == send_status) {
                    LINTED_FATAL_ERROR("Could not send message: %s",
                                       linted_error_string_alloc(errno));
                }
                break;
            }

        default:
            syslog(LOG_ERR, "Simulator received unexpected message type: %i",
                   message.type);
        }
    }
}

static void on_shutdowner_notification(union sigval sigval)
{
    struct shutdowner_notify_data *notify_data = sigval.sival_ptr;
    simulator_type simulator = notify_data->simulator;
    linted_shutdowner shutdowner = notify_data->shutdowner;
    {
        struct sigevent sigevent;
        memset(&sigevent, 0, sizeof sigevent);

        sigevent.sigev_notify = SIGEV_THREAD;
        sigevent.sigev_notify_function = on_shutdowner_notification;
        sigevent.sigev_value.sival_ptr = notify_data;

        if (-1 == linted_shutdowner_notify(shutdowner, &sigevent)) {
            LINTED_LAZY_DEV_ERROR
                ("Could not reregister for shutdowner notifications: %s",
                 linted_error_string_alloc(errno));
        }
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

            LINTED_FATAL_ERROR("Could not receive message: %s",
                               linted_error_string_alloc(errno));
        }

        int send_status;
        do {
            send_status = simulator_send_shutdown(simulator);
        } while (-1 == send_status && EINTR == errno);
        if (-1 == send_status) {
            LINTED_FATAL_ERROR("Could not send message: %s",
                               linted_error_string_alloc(errno));
        }
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

static int simulator_pair(simulator_type simulator[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct simulator_message);

    return linted_mq_pair(simulator, &attr, 0, 0);
}

static int simulator_send_movement(simulator_type simulator,
                                   enum linted_controller_direction direction,
                                   bool moving)
{
    struct simulator_message message;
    memset(&message, 0, sizeof message);

    message.type = SIMULATOR_MOVEMENT;
    message.direction = direction;
    message.moving = moving;

    return mq_send(simulator, (char const *)&message, sizeof message, 0);
}

static int simulator_send_tick(simulator_type simulator)
{
    struct simulator_message message;
    memset(&message, 0, sizeof message);
    message.type = SIMULATORICK;
    return mq_send(simulator, (char const *)&message, sizeof message, 0);
}

static int simulator_send_shutdown(simulator_type const simulator)
{
    struct simulator_message message;
    memset(&message, 0, sizeof message);
    message.type = SIMULATOR_SHUTDOWN;
    return mq_send(simulator, (char const *)&message, sizeof message, 0);
}

static int simulator_close(simulator_type const simulator)
{
    return mq_close(simulator);
}

static int simulator_receive(simulator_type simulator,
                             struct simulator_message *message)
{
    return mq_receive(simulator, (char *)message, sizeof *message, NULL);
}
