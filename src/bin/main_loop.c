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
#include "linted/task.h"
#include "linted/util.h"

#include <errno.h>
#include <mqueue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define SIMULATOR_CLOCK CLOCK_MONOTONIC

enum message_type {
    MAIN_LOOP_CLOSE_REQUEST
};

struct message_data {
    enum message_type type;
};

int linted_main_loop_run(linted_task_spawner_t spawner)
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
        LINTED_ERROR("Could not spawn gui: %s",
                     linted_error_string_alloc(errno));
    }

    linted_simulator_t const simulator = linted_simulator_spawn(spawner, gui);
    if (-1 == simulator) {
        LINTED_ERROR("Could not spawn simulator: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_main_loop_close(main_loop)) {
        LINTED_ERROR("Could not close main loop read end: %s",
                     linted_error_string_alloc(errno));
    }

    struct timespec next_tick;
    if (-1 == clock_gettime(SIMULATOR_CLOCK, &next_tick)) {
        LINTED_ERROR("Could not get the time: %s",
                     linted_error_string_alloc(errno));
    }

    for (;;) {
        /* TODO: Move away from polling */
        struct message_data message_data;
        struct timespec timespec = {
            .tv_sec = 0,
            .tv_nsec = 0
        };

        ssize_t bytes_read;
        do {
            bytes_read = mq_timedreceive(main_loop_read_end,
                                         (char *)&message_data,
                                         sizeof message_data, NULL, &timespec);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            if (errno != ETIMEDOUT) {
                LINTED_ERROR("Could not read from main loop inbox: %s",
                             linted_error_string_alloc(errno));
            }
        } else {
            switch (message_data.type) {
            case MAIN_LOOP_CLOSE_REQUEST:{
                    goto exit_main_loop;
                }

            default:
                LINTED_ERROR("Received unexpected message type: %d", message_data.type);
            }
        }

        int tick_status;
        do {
            tick_status = linted_simulator_send_tick(simulator);
        } while (-1 == tick_status && EINTR == errno);
        if (-1 == tick_status) {
            LINTED_ERROR("Could not send tick message to simulator: %s",
                         linted_error_string_alloc(errno));
        }

        long const second = 1000000000;
        next_tick.tv_nsec += second / 60;
        if (next_tick.tv_nsec >= second) {
            next_tick.tv_nsec = 0;
            next_tick.tv_sec += 1;
        }
        next_tick.tv_nsec += 1000;

        int sleep_status;
        do {
            sleep_status = clock_nanosleep(SIMULATOR_CLOCK, TIMER_ABSTIME,
                                           &next_tick, NULL);
        } while (-1 == sleep_status && EINTR == errno);
        if (-1 == sleep_status) {
            LINTED_ERROR("Could not sleep: %s",
                         linted_error_string_alloc(errno));
        }
    }

 exit_main_loop:
    if (-1 == linted_simulator_close(simulator)) {
        LINTED_ERROR("Could not close simulator handle: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_gui_close(gui)) {
        LINTED_ERROR("Could not close gui handle: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(main_loop_read_end)) {
        LINTED_ERROR("Could not close main loop handle: %s",
                     linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}

int linted_main_loop_request_close(linted_main_loop_t const main_loop)
{
    struct message_data message_data;

    message_data.type = MAIN_LOOP_CLOSE_REQUEST;

    return mq_send(main_loop, (char const *)&message_data, sizeof message_data, 0);
}

int linted_main_loop_close(linted_main_loop_t const main_loop)
{
    return mq_close(main_loop);
}
