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

#include "linted/task.h"

#include <stdint.h>

struct linted_simulator_tick_results {
    uint8_t x_position;
    uint8_t y_position;
};

typedef struct _linted_simulator {
    linted_task_t _task;
    int _inbox;
} linted_simulator_t;

int linted_simulator_spawn(linted_simulator_t * simulator,
                           linted_task_spawner_t spawner);
int linted_simulator_send_tick(struct linted_simulator_tick_results * tick_results,
                               linted_simulator_t simulator);
int linted_simulator_close(linted_simulator_t simulator);

#endif /* LINTED_SIMULATOR_H */
