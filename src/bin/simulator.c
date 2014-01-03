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

#include <errno.h>
#include <stdlib.h>
#include <string.h>


enum {
    LINTED_SIMULATOR_CLOSE_REQUEST = 0,
    LINTED_SIMULATOR_TICK_REQUEST = 1,
    LINTED_SIMULATOR_GUI_CLOSED = 2
};

typedef struct {
    uint8_t type;
} linted_simulator_command;

#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " " LINTED_SIMULATOR_NAME " SIMULATOR_PIPE GUI_PIPE\n"\
    "Run the simulator\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

typedef struct { linted_actor_port x; } linted_simulator_port;
static linted_simulator_port linted_simulator_port_from_fildes(int fildes);
static linted_simulator_command linted_simulator_recv(linted_simulator_port listener);

static int simulator_main(char const * simulator_string,
                          char const * gui_string);

linted_simulator_chan linted_simulator_chan_from_fildes(int fildes) {
    return (linted_simulator_chan) { .x = (linted_actor_chan) { .x = fildes } };
}

enum { MESSAGE_SIZE = 1 };

void linted_simulator_send_close_request(linted_simulator_chan chan) {
    uint8_t const message[MESSAGE_SIZE] = { LINTED_SIMULATOR_CLOSE_REQUEST };
    linted_actor_send(chan.x, message, MESSAGE_SIZE);
}

void linted_simulator_send_tick_request(linted_simulator_chan chan) {
    uint8_t const message[MESSAGE_SIZE] = { LINTED_SIMULATOR_TICK_REQUEST };
    linted_actor_send(chan.x, message, MESSAGE_SIZE);
}

void linted_simulator_send_gui_closed(linted_simulator_chan chan) {
    uint8_t const message[MESSAGE_SIZE] = { LINTED_SIMULATOR_GUI_CLOSED };
    linted_actor_send(chan.x, message, MESSAGE_SIZE);
}

static linted_simulator_command linted_simulator_recv(linted_simulator_port gui) {
    uint8_t message[MESSAGE_SIZE];
    linted_actor_recv(gui.x, message, MESSAGE_SIZE);
    return (linted_simulator_command) { .type = message[0] };
}

int linted_simulator_main(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after simulator_main could have executed or Frama C's
       assumptions will break. */
    switch (argc) {
    case 4:
        return simulator_main(argv[2], argv[3]);
    default:
        fprintf(stderr,
                PACKAGE_TARNAME
                " "
                LINTED_SIMULATOR_NAME
                " did not understand the input\n");
        fputs(USAGE_TEXT, stderr);
        return EXIT_FAILURE;
    }
}

static linted_simulator_port linted_simulator_port_from_fildes(int fildes) {
    return (linted_simulator_port) { .x = (linted_actor_port) { .x = fildes } };
}

static int simulator_main(char const * const simulator_string,
                          char const * const gui_string) {
    int const simulator_fifo = atoi(simulator_string);
    int const gui_fifo = atoi(gui_string);

    linted_simulator_port const command_port = linted_simulator_port_from_fildes(simulator_fifo);
    linted_gui_chan const gui = linted_gui_chan_from_fildes(gui_fifo);

    uint8_t x_position = 0;
    uint8_t y_position = 0;

    for (;;) {
        linted_simulator_command const command = linted_simulator_recv(command_port);
        uint8_t const command_type = command.type;
        switch (command_type) {
        case LINTED_SIMULATOR_TICK_REQUEST: {
            x_position = (x_position + 1) % 256;
            y_position = (y_position + 1) % 256;
            //@ assert x_position ≤ 255;
            //@ assert y_position ≤ 255;

            linted_gui_send_tick_change(gui, x_position, y_position);
            break;
        }

        case LINTED_SIMULATOR_CLOSE_REQUEST:
            goto quit_main_loop;

        default:
            LINTED_ERROR("Received unknown event type: %d\n", command_type);
        }
    }

 quit_main_loop:
    linted_gui_send_shutdown(gui);

    /* Drain excess commands to avoid a sigpipe error. */
    for (;;) {
        linted_simulator_command const command = linted_simulator_recv(command_port);
        uint8_t const command_type = command.type;
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
            error_status = close(gui_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close gui fifo %s\n",
                         strerror(error_code));
        }
    }

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
