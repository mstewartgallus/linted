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
#ifndef LINTED_SIMULATOR_H
#define LINTED_SIMULATOR_H

#include "linted/gui.h"
#include "linted/spawner.h"

#include <mqueue.h>
#include <stdbool.h>

/**
 * A handle to access the simulator. Is safe to share between processes.
 */
typedef mqd_t linted_simulator_t;

enum linted_simulator_direction {
    LINTED_SIMULATOR_UP,
    LINTED_SIMULATOR_DOWN,
    LINTED_SIMULATOR_LEFT,
    LINTED_SIMULATOR_RIGHT
};

int linted_simulator_pair(linted_simulator_t simulator[2]);

int linted_simulator_run(linted_simulator_t inbox, linted_gui_t gui);

int linted_simulator_send_movement(linted_simulator_t simulator,
                                   enum linted_simulator_direction direction,
                                   bool moving);

int linted_simulator_send_tick(linted_simulator_t simulator);
int linted_simulator_send_shutdown(linted_simulator_t simulator);

int linted_simulator_close(linted_simulator_t simulator);

#endif                          /* LINTED_SIMULATOR_H */
