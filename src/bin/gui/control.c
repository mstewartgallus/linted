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
#include "config.h"

#include "linted/actor.h"
#include "linted/gui.h"

linted_gui_chan linted_gui_chan_from_fildes(int fildes) {
    return (linted_gui_chan) { .x = (linted_actor_chan) { .x = fildes } };
}

void linted_gui_send_shutdown(linted_gui_chan gui) {
    linted_actor_send_byte(gui.x, LINTED_GUI_COMMAND_SHUTDOWN);
}

void linted_gui_send_tick_change(linted_gui_chan gui, uint8_t x, uint8_t y) {
    linted_actor_send_byte(gui.x, LINTED_GUI_COMMAND_TICK_CHANGE);
    linted_actor_send_byte(gui.x, x);
    linted_actor_send_byte(gui.x, y);
}
