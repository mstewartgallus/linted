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

#include "linted/gui.h"


linted_gui_command_chan linted_gui_command_chan_from_file(FILE * file) {
    return (linted_gui_command_chan) { .x = (linted_actor_chan) { .x = file } };
}

linted_gui_command_port linted_gui_command_port_from_file(FILE * file) {
    return (linted_gui_command_port) { .x = (linted_actor_port) { .x = file } };
}

void linted_gui_command_send(linted_gui_command_chan gui, linted_gui_command command) {
    linted_actor_send_byte(gui.x, command.type);
    linted_actor_send_byte(gui.x, command.data);
    linted_actor_flush(gui.x);
}

linted_gui_command linted_gui_command_recv(linted_gui_command_port gui) {
    const linted_actor_byte_fast type = linted_actor_recv_byte(gui.x);
    const linted_actor_byte_fast data = linted_actor_recv_byte(gui.x);
    return (linted_gui_command) { .type = type, .data = data };
}


linted_gui_event_port linted_gui_event_port_from_file(FILE * const file) {
    return (linted_gui_event_port) { .x = (linted_actor_port) { .x = file } };
}

linted_gui_event_chan linted_gui_event_chan_from_file(FILE * const file) {
    return (linted_gui_event_chan) { .x = (linted_actor_chan) { .x = file } };
}

void linted_gui_event_send(linted_gui_event_chan chan, linted_gui_event event) {
    linted_actor_send_byte(chan.x, event.type);
    linted_actor_flush(chan.x);
}

/*@
  ensures \initialized(&\result);
*/
linted_gui_event linted_gui_event_recv(linted_gui_event_port port) {
    const linted_actor_byte_fast type = linted_actor_recv_byte(port.x);
    return (linted_gui_event) { .type = type };
}
