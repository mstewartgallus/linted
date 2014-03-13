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
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>


struct controller_state {
    unsigned char x_left: 1;
    unsigned char x_right: 1;

    unsigned char y_up: 1;
    unsigned char y_down: 1;
};

struct simulator_state {
    int_fast32_t x_position;
    int_fast32_t y_position;

    int_fast32_t x_velocity;
    int_fast32_t y_velocity;

    bool update_pending;
};

static int on_timer_readable(int timer,
                             struct controller_state const *controller_state,
                             struct simulator_state *simulator_state);

static int on_updater_writeable(linted_updater updater,
                                struct simulator_state *simulator_state);

static int on_shutdowner_readable(linted_shutdowner shutdowner,
                                  bool * should_exit);

static int on_controller_readable(linted_controller controller,
                                  struct controller_state *controller_state);

static int_fast32_t saturate(int_fast64_t x);
static int_fast32_t min(int_fast32_t x, int_fast32_t y);
static int_fast32_t sign(int_fast32_t x);

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
        .update_pending = false,

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
        int read_fds[] = { controller, shutdowner, timer };
        int write_fds[] = { updater };
        int greatest = -1;

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(read_fds); ++ii) {
            if (greatest < read_fds[ii]) {
                greatest = read_fds[ii];
            }
        }

        if (simulator_state.update_pending) {
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(write_fds); ++ii) {
                if (greatest < write_fds[ii]) {
                    greatest = write_fds[ii];
                }
            }
        }

        fd_set watched_read_fds;
        fd_set watched_write_fds;
        int select_status;

        do {
            FD_ZERO(&watched_read_fds);
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(read_fds); ++ii) {
                FD_SET(read_fds[ii], &watched_read_fds);
            }

            FD_ZERO(&watched_write_fds);
            if (simulator_state.update_pending) {
                for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(write_fds); ++ii) {
                    FD_SET(write_fds[ii], &watched_write_fds);
                }
            }

            select_status = select(greatest + 1, &watched_read_fds,
                                   &watched_write_fds, NULL, NULL);
        } while (-1 == select_status && EINTR == errno);
        if (-1 == select_status) {
            goto close_timer;
        }

        if (FD_ISSET(shutdowner, &watched_read_fds)) {
            bool should_exit;
            if (-1 == on_shutdowner_readable(shutdowner, &should_exit)) {
                goto close_timer;
            }
            if (should_exit) {
                goto exit_main_loop;
            }
        }

        if (FD_ISSET(timer, &watched_read_fds)) {
            if (-1 ==
                on_timer_readable(timer, &controller_state, &simulator_state)) {
                goto close_timer;
            }
        }

        if (FD_ISSET(controller, &watched_read_fds)) {
            if (-1 == on_controller_readable(controller, &controller_state)) {
                goto close_timer;
            }
        }

        if (simulator_state.update_pending) {
            if (FD_ISSET(updater, &watched_write_fds)) {
                if (-1 == on_updater_writeable(updater, &simulator_state)) {
                    goto close_timer;
                }
            }
        }
    }

 exit_main_loop:
    exit_status = 0;

 close_timer:
    {
        int errnum = errno;
        if (-1 == close(timer)) {
            LINTED_IMPOSSIBLE_ERROR("could not close created timer: %s",
                                    linted_error_string_alloc(errno));
        }
        errno = errnum;
    }

    return exit_status;
}

static int on_timer_readable(int timer,
                             struct controller_state const *controller_state,
                             struct simulator_state *simulator_state)
{
    uint64_t ticks;
    if (-1 == read(timer, &ticks, sizeof ticks)) {
        return -1;
    }

    for (size_t ii = 0; ii < ticks; ++ii) {
        int_fast32_t x_position = simulator_state->x_position;
        int_fast32_t y_position = simulator_state->y_position;

        int_fast32_t x_velocity = simulator_state->x_velocity;
        int_fast32_t y_velocity = simulator_state->y_velocity;

        int_fast32_t x_thrust = 2 * ((int_fast32_t) controller_state->x_right
                                     - (int_fast32_t) controller_state->x_left);
        int_fast32_t y_thrust = 2 * ((int_fast32_t) controller_state->y_up
                                     - (int_fast32_t) controller_state->y_down);

        int_fast32_t guess_x_velocity = saturate((int_fast64_t) x_thrust + x_velocity);
        int_fast32_t guess_y_velocity = saturate((int_fast64_t) y_thrust + y_velocity);

        int_fast32_t x_friction = min(imaxabs(guess_x_velocity), 1)
            * sign(guess_x_velocity);
        int_fast32_t y_friction = min(imaxabs(guess_y_velocity), 1)
            * sign(guess_y_velocity);

        int_fast32_t new_x_velocity = saturate((int_fast64_t) guess_x_velocity + x_friction);
        int_fast32_t new_y_velocity = saturate((int_fast64_t) guess_y_velocity + y_friction);

        int_fast32_t new_x_position = saturate((int_fast64_t) x_position + new_x_velocity);
        int_fast32_t new_y_position = saturate((int_fast64_t) y_position + new_y_velocity);

        simulator_state->update_pending |= x_position != new_x_position
            || y_position != new_y_position;

        simulator_state->x_position = new_x_position;
        simulator_state->y_position = new_y_position;

        simulator_state->x_velocity = new_x_velocity;
        simulator_state->y_velocity = new_y_velocity;
    }

    return 0;
}

static int on_updater_writeable(linted_updater updater,
                                struct simulator_state *simulator_state)
{
    struct linted_updater_update update = {
        .x_position = simulator_state->x_position,
        .y_position = simulator_state->y_position
    };

    int update_status;
    do {
        update_status = linted_updater_send_update(updater, &update);
    } while (-1 == update_status && EINTR == errno);
    if (-1 == update_status) {
        if (EAGAIN == errno) {
            return 0;
        }

        return -1;
    }

    simulator_state->update_pending = false;

    return 0;
}

static int on_shutdowner_readable(linted_shutdowner shutdowner,
                                  bool * should_exit)
{
    int read_status;
    do {
        read_status = linted_shutdowner_receive(shutdowner);
    } while (-1 == read_status && EINTR == errno);
    if (-1 == read_status) {
        if (EAGAIN == errno) {
            *should_exit = false;
            return 0;
        }

        return -1;
    }

    *should_exit = true;
    return 0;
}

static int on_controller_readable(linted_controller controller,
                                  struct controller_state *controller_state)
{
    struct linted_controller_message message;

    int read_status;
    do {
        read_status = linted_controller_receive(controller, &message);
    } while (-1 == read_status && EINTR == errno);
    if (-1 == read_status) {
        if (EAGAIN == errno) {
            return 0;
        }

        return -1;
    }

    controller_state->x_left = message.left;
    controller_state->x_right = message.right;
    controller_state->y_up = message.up;
    controller_state->y_down = message.down;

    return 0;
}

static int_fast32_t min(int_fast32_t x, int_fast32_t y)
{
    return x < y ? x : y;
}

static int_fast32_t sign(int_fast32_t x)
{
    return x < 0 ? 1 : 0 == x ? 0 : -1;
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
