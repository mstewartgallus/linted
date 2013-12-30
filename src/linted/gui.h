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
#ifndef LINTED_GUI_H
#define LINTED_GUI_H

#include "linted/actor.h"

#include <stdio.h>

#define LINTED_GUI_NAME "gui"

int linted_gui_main(int argc, char * argv[]);

enum {
    LINTED_GUI_COMMAND_SHUTDOWN = 0,
    LINTED_GUI_COMMAND_TICK_CHANGE = 1
};

typedef struct {
    uint8_t x;
    uint8_t y;
} linted_gui_tick_change;

typedef struct {
    uint8_t type;
    linted_gui_tick_change tick_change;
} linted_gui_command;

typedef struct { linted_actor_chan x; } linted_gui_chan;

linted_gui_chan linted_gui_chan_from_fildes(int fildes);
void linted_gui_send(linted_gui_chan chan, linted_gui_command gui);

#endif /* LINTED_GUI_H */
