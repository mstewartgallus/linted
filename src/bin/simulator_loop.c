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
#include "linted/mq.h"
#include "linted/simulator_loop.h"
#include "linted/simulator.h"
#include "linted/util.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum message_type {
    SIMULATOR_LOOP_SHUTDOWN
};

struct message_data {
    enum message_type message_type;
};

struct timer_data {
    linted_simulator_t simulator;
};

static void on_clock_tick(union sigval sigev_value);

int linted_simulator_loop_pair(linted_simulator_loop_t simulator_loop[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    return linted_mq_pair(simulator_loop, &attr, 0);
}

int linted_simulator_loop_send_shutdown(linted_simulator_loop_t const simulator_loop)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.message_type = SIMULATOR_LOOP_SHUTDOWN;

    return mq_send(simulator_loop, (char const *)&message_data, sizeof message_data, 0);
}

int linted_simulator_loop_close(linted_simulator_loop_t const simulator_loop)
{
    return mq_close(simulator_loop);
}

int linted_simulator_loop_run(linted_simulator_loop_t const simulator_loop,
                              linted_simulator_t const simulator)
{
    struct timer_data timer_data = { .simulator = simulator };

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
            bytes_read = mq_receive(simulator_loop,
                                    (char *)&message_data, sizeof message_data, NULL);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            LINTED_ERROR("Could not read from simulator loop inbox: %s",
                         linted_error_string_alloc(errno));
        }

        switch (message_data.message_type) {
        case SIMULATOR_LOOP_SHUTDOWN:
            goto exit_main_loop;

        default:
            LINTED_ERROR("Received unexpected message type: %d",
                         message_data.message_type);
        }
    }

exit_main_loop:
    linted_simulator_send_shutdown(simulator);

    if (-1 == timer_delete(timer)) {
        LINTED_ERROR("Could not delete timer: %s",
                     linted_error_string_alloc(errno));
    }

    return 0;
}


static void on_clock_tick(union sigval sigev_value)
{
    struct timer_data *const timer_data = sigev_value.sival_ptr;
    linted_simulator_t const simulator = timer_data->simulator;

    int tick_status;
    do {
        tick_status = linted_simulator_send_tick(simulator);
    } while (-1 == tick_status && EINTR == errno);
    if (-1 == tick_status) {
        LINTED_ERROR("Could not send simulator tick: %s",
                     linted_error_string_alloc(errno));
    }
}
