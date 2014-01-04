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

#include "linted/actor.h"

#define LINTED_SIMULATOR_NAME "simulator"

int linted_simulator_run(int simulator_fifo, int gui_fifo);

typedef struct _linted_simulator { linted_actor_chan _x; } linted_simulator_t;

linted_simulator_t linted_simulator_from_fildes(int fildes);

void linted_simulator_send_close_request(linted_simulator_t sim);
void linted_simulator_send_tick_request(linted_simulator_t sim);
void linted_simulator_send_gui_closed(linted_simulator_t sim);


#endif /* LINTED_SIMULATOR_H */
