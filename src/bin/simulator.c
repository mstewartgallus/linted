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

struct reply_data {
    struct linted_simulator_tick_results tick_results;
};

static int simulator_run(linted_task_spawner_t const spawner, int inbox);

int linted_simulator_spawn(linted_simulator_t * const simulator,
                           linted_task_spawner_t const spawner)
{
    int sockets[2];
    if (-1 == linted_io_create_local_server(sockets)) {
        return -1;
    }

    int const sim_reader = sockets[0];
    int const sim_writer = sockets[1];

    if (-1 == linted_task_spawn(spawner, simulator_run, sim_reader)) {
        goto error_and_close_sockets;
    }

    if (-1 == close(sim_reader)) {
        goto error_and_close_socket;
    }

    simulator->_server = sim_writer;

    return 0;

 error_and_close_sockets:
    close(sim_reader);

 error_and_close_socket:
    close(sim_writer);

    return -1;
}

int linted_simulator_send_tick(struct linted_simulator_tick_results *const
                               tick_results, linted_simulator_t const simulator)
{
    {
        struct request_data request_data = {
            .message_type = SIMULATOR_TICK
        };

        ssize_t bytes_written;
        do {
            bytes_written = write(simulator._server, &request_data, sizeof request_data);
        } while (-1 == bytes_written && EINTR == errno);
        if (-1 == bytes_written) {
            return -1;
        }
    }

    {
        struct reply_data reply_data;
        ssize_t bytes_read;
        do {
            bytes_read = read(simulator._server, &reply_data, sizeof reply_data);
        } while (-1 == bytes_read && EINTR == EINTR);
        if (-1 == bytes_read) {
            return -1;
        }

        *tick_results = reply_data.tick_results;
    }

    return 0;
}

int linted_simulator_close(linted_simulator_t const simulator)
{
    return close(simulator._server);
}

static int simulator_run(linted_task_spawner_t const spawner, int const inbox)
{
    int const spawner_close_status = linted_task_spawner_close(spawner);
    if (-1 == spawner_close_status) {
        LINTED_ERROR("Could not close spawner: %m", errno);
    }

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

        struct reply_data reply_data;
        switch (request_data.message_type) {
        case SIMULATOR_TICK:
            x_position = x_position % 255 + 3;
            y_position = y_position % 255 + 5;
            //@ assert x_position ≤ 255;
            //@ assert y_position ≤ 255;

            reply_data.tick_results.x_position = x_position;
            reply_data.tick_results.y_position = y_position;
            break;

        default:
            LINTED_ERROR("Received unexpected message type: %d",
                         request_data.message_type);
        }

        ssize_t bytes_written;
        do {
            bytes_written = write(inbox, &reply_data, sizeof reply_data);
        } while (-1 == bytes_written && errno == EINTR);
        if (-1 == bytes_written) {
            LINTED_ERROR("Could not write to simulator connection: %m", errno);
        }
    }

    int const inbox_close_status = close(inbox);
    if (-1 == inbox_close_status) {
        LINTED_ERROR("Could not close simulator inbox: %m", errno);
    }

    return EXIT_SUCCESS;
}
