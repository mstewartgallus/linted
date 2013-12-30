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

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " " LINTED_SIMULATOR_NAME " SIMULATOR_PIPE GUI_PIPE\n"\
    "Run the simulator\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

linted_simulator_chan linted_simulator_chan_from_fildes(int fildes) {
    return (linted_simulator_chan) { .x = (linted_actor_chan) { .x = fildes } };
}

void linted_simulator_send(linted_simulator_chan simulator,
                           linted_simulator_command command) {
    linted_actor_send_byte(simulator.x, command.type);
}

typedef struct { linted_actor_port x; } linted_simulator_port;
static linted_simulator_port linted_simulator_port_from_fildes(int fildes);
static linted_simulator_command linted_simulator_recv(linted_simulator_port listener);

static int simulator_main(char const * simulator_string,
                          char const * gui_string);

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
        return EXIT_FAILURE;
    }
}

static linted_simulator_port linted_simulator_port_from_fildes(int const fildes) {
    return (linted_simulator_port) { .x = (linted_actor_port) { .x = fildes } };
}

static linted_simulator_command linted_simulator_recv(linted_simulator_port gui) {
    const linted_actor_byte_fast type = linted_actor_recv_byte(gui.x);
    return (linted_simulator_command) { .type = type };
}

static int simulator_main(char const * const simulator_string,
                          char const * const gui_string) {
    int const simulator_fifo = atoi(simulator_string);
    int const gui_fifo = atoi(gui_string);

    linted_simulator_port const command_port = linted_simulator_port_from_fildes(simulator_fifo);
    linted_gui_chan const gui = linted_gui_chan_from_fildes(gui_fifo);

    linted_actor_byte_fast x_position = { .x = 0 };
    linted_actor_byte_fast y_position = { .x = 0 };

    for (;;) {
        linted_simulator_command const command = linted_simulator_recv(command_port);
        linted_actor_byte_fast_t const command_type = command.type.x;
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

    {
        int error_status;
        int error_code;
        do {
            error_status = close(gui_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close gui fifo %s\n",
                         strerror(error_code));
        }
    }


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
    {
        int error_status;
        int error_code;
        do {
            error_status = close(simulator_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close simulator fifo %s\n",
                         strerror(error_code));
        }
    }

    return EXIT_SUCCESS;
}
