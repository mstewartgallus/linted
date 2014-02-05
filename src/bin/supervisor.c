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
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/supervisor.h"
#include "linted/util.h"

#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>

#define SIMULATOR_CLOCK CLOCK_MONOTONIC

int linted_supervisor_run(linted_task_spawner_t spawner)
{
    linted_gui_t const gui = linted_gui_spawn(spawner);
    if (-1 == gui) {
        LINTED_ERROR("Could not spawn gui: %m", errno);
    }

    linted_simulator_t const simulator = linted_simulator_spawn(spawner);
    if (-1 == simulator) {
        LINTED_ERROR("Could not spawn simulator: %m", errno);
    }

    struct timespec next_tick;
    if (-1 == clock_gettime(SIMULATOR_CLOCK,  &next_tick)) {
        LINTED_ERROR("Could not get the time: %m", errno);
    }

    for (;;) {
        struct linted_simulator_tick_results tick_results;
        if (-1 == linted_simulator_send_tick(&tick_results, simulator)) {
            LINTED_ERROR("Could not send tick message to simulator: %m", errno);
        }

        int update_status;
        do {
            update_status = linted_gui_send_update(gui,
                                                   tick_results.x_position,
                                                   tick_results.y_position);
        } while (-1 == update_status && EINTR == errno);
        if (-1 == update_status) {
            LINTED_ERROR("Could not send update message to gui: %m", errno);
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
            LINTED_ERROR("Could not sleep: %m", errno);
        }
    }

    if (-1 == linted_simulator_close(simulator)) {
        LINTED_ERROR("Could not close simulator handle: %m", errno);
    }

    if (-1 == linted_gui_close(gui)) {
        LINTED_ERROR("Could not close gui handle: %m", errno);
    }

    return EXIT_SUCCESS;
}
