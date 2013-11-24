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
#include "linted/util.h"

#include "linted/base/stdio.h"

#include <stdlib.h>


int simulator_main(const char * const gui_event_input_string,
                   const char * const gui_command_output_string);

int main(int argc, char * argv[]) {
    /*
     * Frama C's analysis begins at simulator_main so no global state
     * must be modified before then.
     */
    if (argc < 3) {
        return EXIT_FAILURE;
    }

    return simulator_main(argv[1], argv[2]);
}

int simulator_main(const char * const gui_event_input_string,
                   const char * const gui_command_output_string) {
    FILE * const gui_event_fifo = linted_fdopen(atoi(gui_event_input_string), "rb");
    FILE * const gui_command_fifo = linted_fdopen(atoi(gui_command_output_string), "wb");

    const linted_gui_event_port event_port = linted_gui_event_port_from_file(gui_event_fifo);
    const linted_gui_command_chan gui = linted_gui_command_chan_from_file(gui_command_fifo);

    linted_actor_byte_fast_t byte = 0;

    for (;;) {
        const linted_gui_event event = linted_gui_event_recv(event_port);
        const linted_actor_byte_fast_t event_type = event.type.x;
        switch (event_type) {
        case LINTED_GUI_EVENT_TICK_REQUEST:
            byte = (byte + 1) % 256;
            //@ assert byte ≤ 255;

            linted_gui_command_send(gui, (linted_gui_command) {
                    .type = (linted_actor_byte_fast) {
                        .x = LINTED_GUI_COMMAND_TICK_CHANGE
                    },
                    .data = (linted_actor_byte_fast) {
                        .x = byte
                    }
                });
            break;

        case LINTED_GUI_EVENT_CLOSE_REQUEST:
            goto quit_main_loop;

        default:
            LINTED_ERROR("Received unknown event type: %d\n", event_type);
        }
    }

 quit_main_loop:
    linted_gui_command_send(gui, (linted_gui_command) {
            .type = (linted_actor_byte_fast) {
                .x = LINTED_GUI_COMMAND_SHUTDOWN
            }
        });
    linted_fclose(gui_event_fifo);
    linted_fclose(gui_command_fifo);

    return EXIT_SUCCESS;
}
