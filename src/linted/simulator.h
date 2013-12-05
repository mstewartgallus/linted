/*
 * Copyright 2013 Steven Stewart-Gallus
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

#include <stdio.h>

#define LINTED_SIMULATOR_NAME "simulator"

int linted_simulator_main(int argc, char * argv[]);

enum {
    LINTED_SIMULATOR_CLOSE_REQUEST = 0,
    LINTED_SIMULATOR_TICK_REQUEST = 1
};

typedef struct {
    linted_actor_byte_fast type;
} linted_simulator_command;

typedef struct { linted_actor_chan x; } linted_simulator_chan;

linted_simulator_chan linted_simulator_chan_from_file(FILE * file);
void linted_simulator_send(linted_simulator_chan chan, linted_simulator_command command);


#endif /* LINTED_SIMULATOR_H */
