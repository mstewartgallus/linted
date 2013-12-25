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

#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " " LINTED_SIMULATOR_NAME " SIMULATOR_PIPE GUI_PIPE\n"\
    "Run the simulator\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

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
    /* Note that in this function no global state must be modified
       until after simulator_main could have executed or Frama C's
       assumptions will break. */
    switch (argc) {
    case 4:
        return simulator_main(argv[2], argv[3]);
    default:
        linted_fprintf(stderr,
                       PACKAGE_TARNAME
                       " "
                       LINTED_SIMULATOR_NAME
                       " did not understand the input\n");
        linted_fputs(USAGE_TEXT, stderr);
        linted_fflush(stderr);
        return EXIT_FAILURE;
    }
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

    linted_actor_byte_fast x_position = { .x = 0 };
    linted_actor_byte_fast y_position = { .x = 0 };

    for (;;) {
        const linted_simulator_command command = linted_simulator_recv(command_port);
        const linted_actor_byte_fast_t command_type = command.type.x;
        switch (command_type) {
        case LINTED_SIMULATOR_TICK_REQUEST: {
            x_position.x = (x_position.x + 1) % 256;
            y_position.x = (y_position.x + 1) % 256;
            //@ assert x_position.x ≤ 255;
            //@ assert y_position.x ≤ 255;

            linted_gui_command gui_command = {
                .type = (linted_actor_byte_fast) { .x = LINTED_GUI_COMMAND_TICK_CHANGE },
                .tick_change = (linted_gui_tick_change) {
                    .x = x_position,
                    .y = y_position
                }
            };
            linted_gui_send(gui, gui_command);
            break;
        }

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

    linted_fclose(gui_fifo);

    /* Drain excess commands to avoid a sigpipe error. */
    for (;;) {
        const linted_simulator_command command = linted_simulator_recv(command_port);
        const linted_actor_byte_fast_t command_type = command.type.x;
        switch (command_type) {
        case LINTED_SIMULATOR_GUI_CLOSED:
            goto exit;

        case LINTED_SIMULATOR_TICK_REQUEST:
        case LINTED_SIMULATOR_CLOSE_REQUEST:
            break;

        default:
            LINTED_ERROR("Received unknown event type: %d\n", command_type);
        }
    }
 exit:
    linted_fclose(simulator_fifo);

    return EXIT_SUCCESS;
}
