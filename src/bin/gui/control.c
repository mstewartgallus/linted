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

linted_gui_chan linted_gui_chan_from_file(FILE * file) {
    return (linted_gui_chan) { .x = (linted_actor_chan) { .x = file } };
}

void linted_gui_send(linted_gui_chan gui, linted_gui_command command) {
    linted_actor_send_byte(gui.x, command.type);
    linted_actor_send_byte(gui.x, command.data);
    linted_actor_flush(gui.x);
}
