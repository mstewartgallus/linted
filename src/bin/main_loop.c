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

#include "linted/gui.h"
#include "linted/main_loop.h"
#include "linted/mq.h"
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/spawner.h"
#include "linted/util.h"

#include <errno.h>
#include <mqueue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum message_type {
    MAIN_LOOP_CLOSE_REQUEST,
    MAIN_LOOP_SEND_TICK
};

struct message_data {
    enum message_type type;
};

struct timer_data {
    linted_main_loop_t main_loop;
};

static int linted_main_loop_send_tick(linted_main_loop_t const main_loop);

static void on_clock_tick(union sigval sigev_value);

int linted_main_loop_run(linted_spawner_t spawner)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    mqd_t main_loop_mqs[2];
    if (-1 == linted_mq_pair(main_loop_mqs, &attr, 0)) {
        LINTED_ERROR("Could not create main loop message queue: %s",
                     linted_error_string_alloc(errno));
    }

    linted_main_loop_t const main_loop = main_loop_mqs[1];
    mqd_t const main_loop_read_end = main_loop_mqs[0];

    linted_gui_t const gui = linted_gui_spawn(spawner, main_loop);
    if (-1 == gui) {
        LINTED_ERROR("Could not spawn gui: %s", linted_error_string_alloc(errno));
    }

    linted_simulator_t const simulator = linted_simulator_spawn(spawner, gui);
    if (-1 == simulator) {
        LINTED_ERROR("Could not spawn simulator: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner handle: %s",
                     linted_error_string_alloc(errno));
    }

    struct timer_data timer_data = {
        .main_loop = main_loop
    };

    timer_t timer;
    {
        struct sigevent sevp;
        memset(&sevp, 0, sizeof sevp);

        sevp.sigev_notify = SIGEV_THREAD;
        sevp.sigev_notify_function = on_clock_tick;
        sevp.sigev_value.sival_ptr = &timer_data;

        if (-1 == timer_create(CLOCK_MONOTONIC, &sevp, &timer)) {
            LINTED_ERROR("Could not create timer: %s", linted_error_string_alloc(errno));
        }

        struct itimerspec itimer_spec;
        memset(&itimer_spec, 0, sizeof itimer_spec);

        long const second = 1000000000;

        /* Strangely, it_value has to be nozero */
        itimer_spec.it_value.tv_nsec = 1;

        itimer_spec.it_interval.tv_sec = 0;
        itimer_spec.it_interval.tv_nsec = second / 60;
        if (-1 == timer_settime(timer, 0, &itimer_spec, NULL)) {
            LINTED_ERROR("Could not set timer expiry date: %s",
                         linted_error_string_alloc(errno));
        }
    }

    for (;;) {
        struct message_data message_data;

        ssize_t bytes_read;
        do {
            bytes_read = mq_receive(main_loop_read_end,
                                    (char *)&message_data, sizeof message_data, NULL);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            LINTED_ERROR("Could not read from main loop inbox: %s",
                         linted_error_string_alloc(errno));
        }

        switch (message_data.type) {
        case MAIN_LOOP_SEND_TICK:{
                int tick_status;
                do {
                    tick_status = linted_simulator_send_tick(simulator);
                } while (-1 == tick_status && EINTR == errno);
                if (-1 == tick_status) {
                    LINTED_ERROR("Could not send tick message to simulator: %s",
                                 linted_error_string_alloc(errno));
                }
                break;
            }

        case MAIN_LOOP_CLOSE_REQUEST:
            goto exit_main_loop;

        default:
            LINTED_ERROR("Received unexpected message type: %d", message_data.type);
        }
    }

 exit_main_loop:
    {
        int shutdown_status;
        do {
            shutdown_status = linted_simulator_send_shutdown(simulator);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to simulator: %s",
                         linted_error_string_alloc(errno));
        }
    }

    {
        int shutdown_status;
        do {
            shutdown_status = linted_gui_send_shutdown(gui);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to gui: %s",
                         linted_error_string_alloc(errno));
        }
    }

    if (-1 == timer_delete(timer)) {
        LINTED_ERROR("Could not delete timer: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_simulator_close(simulator)) {
        LINTED_ERROR("Could not close simulator handle: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_gui_close(gui)) {
        LINTED_ERROR("Could not close gui handle: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_main_loop_close(main_loop)) {
        LINTED_ERROR("Could not close main loop write end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(main_loop_read_end)) {
        LINTED_ERROR("Could not close main loop read end: %s",
                     linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}

static void on_clock_tick(union sigval sigev_value)
{
    struct timer_data *const timer_data = sigev_value.sival_ptr;
    linted_main_loop_t const main_loop = timer_data->main_loop;

    int tick_status;
    do {
        tick_status = linted_main_loop_send_tick(main_loop);
    } while (-1 == tick_status && EINTR == errno);
    if (-1 == tick_status) {
        LINTED_ERROR("Could not send main loop tick: %s",
                     linted_error_string_alloc(errno));
    }
}

static int linted_main_loop_send_tick(linted_main_loop_t const main_loop)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = MAIN_LOOP_SEND_TICK;

    return mq_send(main_loop, (char const *)&message_data, sizeof message_data, 0);
}

int linted_main_loop_request_close(linted_main_loop_t const main_loop)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = MAIN_LOOP_CLOSE_REQUEST;

    return mq_send(main_loop, (char const *)&message_data, sizeof message_data, 0);
}

int linted_main_loop_close(linted_main_loop_t const main_loop)
{
    return mq_close(main_loop);
}
