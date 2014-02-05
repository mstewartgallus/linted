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

#include "linted/io.h"
#include "linted/gui.h"
#include "linted/simulator.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

enum message_type {
    SIMULATOR_TICK
};

struct request_data {
    enum message_type message_type;
};

static int simulator_run(linted_task_spawner_t const spawner,
                         int const inboxes[]);

linted_simulator_t linted_simulator_spawn(linted_task_spawner_t const spawner,
                                          linted_gui_t gui)
{
    int sockets[2];
    if (-1 == linted_io_create_local_server(sockets)) {
        return -1;
    }

    int const sim_reader = sockets[0];
    int const sim_writer = sockets[1];

    if (-1 == linted_task_spawn(spawner, simulator_run,
                                (int[]) { sim_reader, gui, -1 })) {
        goto error_and_close_sockets;
    }

    if (-1 == close(sim_reader)) {
        goto error_and_close_socket;
    }

    return sim_writer;

 error_and_close_sockets:
    close(sim_reader);

 error_and_close_socket:
    close(sim_writer);

    return -1;
}

int linted_simulator_send_tick(linted_simulator_t const simulator)
{
    struct request_data request_data = {
        .message_type = SIMULATOR_TICK
    };

    ssize_t bytes_written;
    do {
        bytes_written = write(simulator, &request_data, sizeof request_data);
    } while (-1 == bytes_written && EINTR == errno);
    if (-1 == bytes_written) {
        return -1;
    }

    return 0;
}

int linted_simulator_close(linted_simulator_t const simulator)
{
    return close(simulator);
}

static int simulator_run(linted_task_spawner_t const spawner,
                         int const inboxes[])
{
    int const spawner_close_status = linted_task_spawner_close(spawner);
    if (-1 == spawner_close_status) {
        LINTED_ERROR("Could not close spawner: %m", errno);
    }

    int const inbox = inboxes[0];
    linted_gui_t const gui = inboxes[1];

    uint8_t x_position = 0;
    uint8_t y_position = 0;

    for (;;) {
        struct request_data request_data;
        ssize_t bytes_read;
        do {
            bytes_read = read(inbox, &request_data, sizeof request_data);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            LINTED_ERROR("Could not read from simulator connection: %m", errno);
        }

        if (0 == bytes_read) {
            break;
        }

        switch (request_data.message_type) {
        case SIMULATOR_TICK:{
                x_position = x_position % 255 + 3;
                y_position = y_position % 255 + 5;
                //@ assert x_position ≤ 255;
                //@ assert y_position ≤ 255;

                int update_status;
                do {
                    update_status = linted_gui_send_update(gui,
                                                           x_position,
                                                           y_position);
                } while (-1 == update_status && EINTR == errno);
                if (-1 == update_status) {
                    LINTED_ERROR("Could not send update message to gui: %m",
                                 errno);
                }
                break;
        }

        default:
            LINTED_ERROR("Received unexpected message type: %d",
                         request_data.message_type);
        }
    }


    if (-1 == close(inbox)) {
        LINTED_ERROR("Could not close simulator inbox: %m", errno);
    }

    if (-1 == close(gui)) {
        LINTED_ERROR("Could not close gui: %m", errno);
    }

    return EXIT_SUCCESS;
}
