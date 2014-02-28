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
#include <sys/select.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>

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

static int handle_tick(linted_updater updater,
                       struct controller_state const *controller_state,
                       struct simulator_state *simulator_state);

static int handle_shutdown_messages(linted_shutdowner shutdowner,
                                    bool * should_exit);

static int handle_controller_messages(linted_controller controller, struct controller_state
                                      *controller_state);

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

    int timer = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (-1 == timer) {
        return -1;
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
            goto close_timer;
        }
    }

    for (;;) {
        int fds[] = { controller, shutdowner, timer };
        int greatest = -1;
        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(fds); ++ii) {
            if (greatest < fds[ii]) {
                greatest = fds[ii];
            }
        }

        fd_set watched_fds;
        int select_status;

        do {
            FD_ZERO(&watched_fds);
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(fds); ++ii) {
                FD_SET(fds[ii], &watched_fds);
            }

            select_status = select(greatest + 1, &watched_fds,
                                   NULL, NULL, NULL);
        } while (-1 == select_status && EINTR == errno);
        if (-1 == select_status) {
            goto close_timer;
        }

        if (FD_ISSET(shutdowner, &watched_fds)) {
            bool should_exit;
            if (-1 == handle_shutdown_messages(shutdowner, &should_exit)) {
                goto close_timer;
            }
            if (should_exit) {
                goto exit_main_loop;
            }
        }

        if (FD_ISSET(timer, &watched_fds)) {
            uint64_t ticks;
            if (-1 == read(timer, &ticks, sizeof ticks)) {
                goto close_timer;
            }

            for (size_t ii = 0; ii < ticks; ++ii) {
                if (-1 == handle_tick(updater, &controller_state,
                                      &simulator_state)) {
                    goto close_timer;
                }
            }
        }

        if (FD_ISSET(controller, &watched_fds)) {
            if (-1 == handle_controller_messages(controller, &controller_state)) {
                goto close_timer;
            }
        }
    }

 exit_main_loop:
    exit_status = 0;

 close_timer:
    {
        int errnum = errno;
        int close_status = close(timer);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}

static int handle_tick(linted_updater updater,
                       struct controller_state const *controller_state,
                       struct simulator_state *simulator_state)
{
    int32_t x_position = simulator_state->x_position;
    int32_t y_position = simulator_state->y_position;

    int32_t x_velocity = simulator_state->x_velocity;
    int32_t y_velocity = simulator_state->y_velocity;

    int32_t x_thrust =
        2 * (controller_state->x_right - controller_state->x_left);
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
                                    bool * should_exit)
{
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
                                      struct controller_state *controller_state)
{
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

static int32_t min(int32_t x, int32_t y)
{
    return x < y ? x : y;
}

static int32_t sign(int32_t x)
{
    return x < 0 ? 1 : 0 == x ? 0 : -1;
}
