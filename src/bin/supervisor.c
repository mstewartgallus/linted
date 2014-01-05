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

#include "SDL.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>


int linted_supervisor_run(linted_task_spawner_t spawner) {
    linted_gui_t gui;
    int const gui_status = linted_gui_spawn(&gui, spawner);
    if (-1 == gui_status) {
        LINTED_ERROR("Could not spawn gui: %s\n", strerror(errno));
    }

    linted_simulator_t simulator;
    int const simulator_status = linted_simulator_spawn(&simulator, spawner);
    if (-1 == simulator_status) {
        LINTED_ERROR("Could not spawn simulator: %s\n", strerror(errno));
    }

    Uint32 next_tick = SDL_GetTicks();
    for (;;) {
        Uint32 const now = SDL_GetTicks();
        if (now >= next_tick) {
            next_tick += 1000 / 60;

            struct linted_simulator_tick_results tick_results;
            int const tick_status = linted_simulator_send_tick(&tick_results,
                                                               simulator);
            if (-1 == tick_status) {
                LINTED_ERROR("Could not send tick message to simulator: %s\n",
                             strerror(errno));
            }

            int const update_status = linted_gui_send_update(gui,
                                                             tick_results.x_position,
                                                             tick_results.y_position);
            if (-1 == update_status) {
                LINTED_ERROR("Could not send update message to gui: %s\n",
                             strerror(errno));
            }
        }
    }

    int const simulator_close_status = linted_simulator_close(simulator);
    if (-1 == simulator_close_status) {
        LINTED_ERROR("Could not close simulator handle: %s\n", strerror(errno));
    }

    int const gui_close_status = linted_gui_close(gui);
    if (-1 == gui_close_status) {
        LINTED_ERROR("Could not close gui handle: %s\n", strerror(errno));
    }

    return EXIT_SUCCESS;
}
