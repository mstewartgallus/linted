/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

typedef struct { linted_actor_port x; } linted_simulator_port;
static linted_simulator_port linted_simulator_port_from_fildes(int fildes);
static linted_simulator_command linted_simulator_recv(linted_simulator_port listener);

linted_simulator_t linted_simulator_from_fildes(int fildes) {
    return (linted_simulator_t) { ._x = (linted_actor_chan) { .x = fildes } };
}

enum { MESSAGE_SIZE = 1 };

void linted_simulator_send_close_request(linted_simulator_t sim) {
    uint8_t const message[MESSAGE_SIZE] = { LINTED_SIMULATOR_CLOSE_REQUEST };
    linted_actor_send(sim._x, message, MESSAGE_SIZE);
}

void linted_simulator_send_tick_request(linted_simulator_t sim) {
    uint8_t const message[MESSAGE_SIZE] = { LINTED_SIMULATOR_TICK_REQUEST };
    linted_actor_send(sim._x, message, MESSAGE_SIZE);
}

void linted_simulator_send_gui_closed(linted_simulator_t sim) {
    uint8_t const message[MESSAGE_SIZE] = { LINTED_SIMULATOR_GUI_CLOSED };
    linted_actor_send(sim._x, message, MESSAGE_SIZE);
}

static linted_simulator_command linted_simulator_recv(linted_simulator_port sim) {
    uint8_t message[MESSAGE_SIZE];
    linted_actor_recv(sim.x, message, MESSAGE_SIZE);
    return (linted_simulator_command) { .type = message[0] };
}

static linted_simulator_port linted_simulator_port_from_fildes(int fildes) {
    return (linted_simulator_port) { .x = (linted_actor_port) { .x = fildes } };
}

int linted_simulator_run(int const simulator_fifo, int const gui_fifo) {
    linted_simulator_port const command_port = linted_simulator_port_from_fildes(simulator_fifo);
    linted_gui_t const gui = linted_gui_from_fildes(gui_fifo);

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
    return EXIT_SUCCESS;
}
