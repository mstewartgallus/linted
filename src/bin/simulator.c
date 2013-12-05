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
#include "linted/simulator.h"
#include "linted/util.h"

#include "linted/base/stdio.h"

#include <stdlib.h>


linted_simulator_chan linted_simulator_chan_from_file(FILE * file) {
    return (linted_simulator_chan) { .x = (linted_actor_chan) { .x = file } };
}

void linted_simulator_send(linted_simulator_chan simulator, linted_simulator_command command) {
    linted_actor_send_byte(simulator.x, command.type);
    linted_actor_flush(simulator.x);
}

typedef struct { linted_actor_port x; } linted_simulator_port;
static linted_simulator_port linted_simulator_port_from_file(FILE * file);
static linted_simulator_command linted_simulator_recv(linted_simulator_port listener);

static int simulator_main(const char * simulator_string, const char * gui_string);

int linted_simulator_main(int argc, char * argv[]) {
    /* No global state must be modified or Frama C's assumptions will break */
    if (argc < 4) {
        return EXIT_FAILURE;
    }

    return simulator_main(argv[2], argv[3]);
}

static linted_simulator_port linted_simulator_port_from_file(FILE * file) {
    return (linted_simulator_port) { .x = (linted_actor_port) { .x = file } };
}

static linted_simulator_command linted_simulator_recv(linted_simulator_port gui) {
    const linted_actor_byte_fast type = linted_actor_recv_byte(gui.x);
    return (linted_simulator_command) { .type = type };
}

static int simulator_main(const char * const simulator_string,
                          const char * const gui_string) {
    FILE * const simulator_fifo = linted_fdopen(atoi(simulator_string), "rb");
    FILE * const gui_fifo = linted_fdopen(atoi(gui_string), "wb");

    const linted_simulator_port command_port = linted_simulator_port_from_file(simulator_fifo);
    const linted_gui_chan gui = linted_gui_chan_from_file(gui_fifo);

    linted_actor_byte_fast_t byte = 0;

    for (;;) {
        const linted_simulator_command command = linted_simulator_recv(command_port);
        const linted_actor_byte_fast_t command_type = command.type.x;
        switch (command_type) {
        case LINTED_SIMULATOR_TICK_REQUEST:
            byte = (byte + 1) % 256;
            //@ assert byte ≤ 255;

            linted_gui_send(gui, (linted_gui_command) {
                    .type = (linted_actor_byte_fast) {
                        .x = LINTED_GUI_COMMAND_TICK_CHANGE
                    },
                    .data = (linted_actor_byte_fast) {
                        .x = byte
                    }
                });
            break;

        case LINTED_SIMULATOR_CLOSE_REQUEST:
            goto quit_main_loop;

        default:
            LINTED_ERROR("Received unknown event type: %d\n", command_type);
        }
    }

 quit_main_loop:
    linted_gui_send(gui, (linted_gui_command) {
            .type = (linted_actor_byte_fast) {
                .x = LINTED_GUI_COMMAND_SHUTDOWN
            }
        });
    linted_fclose(simulator_fifo);
    linted_fclose(gui_fifo);

    return EXIT_SUCCESS;
}
