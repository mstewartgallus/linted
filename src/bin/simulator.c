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
#include "linted/mq.h"
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/util.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

enum message_type {
    SIMULATOR_TICK
};

struct message_data {
    enum message_type message_type;
};

static int simulator_run(linted_spawner_t const spawner, int const inboxes[]);

linted_simulator_t linted_simulator_spawn(linted_spawner_t const spawner,
                                          linted_gui_t gui)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    mqd_t sim_mqs[2];
    if (-1 == linted_mq_pair(sim_mqs, &attr, 0)) {
        return -1;
    }

    if (-1 == linted_spawner_spawn(spawner, simulator_run, (int[]) {
                                   sim_mqs[0], gui, -1})) {
        goto error_and_close_mqueues;
    }

    mq_close(sim_mqs[0]);

    return sim_mqs[1];

 error_and_close_mqueues:
    mq_close(sim_mqs[0]);

    mq_close(sim_mqs[1]);

    return -1;
}

int linted_simulator_send_tick(linted_simulator_t const simulator)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.message_type = SIMULATOR_TICK;

    return mq_send(simulator, (char const *)&message_data, sizeof message_data, 0);
}

int linted_simulator_close(linted_simulator_t const simulator)
{
    return mq_close(simulator);
}

static int simulator_run(linted_spawner_t const spawner, int const inboxes[])
{
    linted_sandbox();

    int const spawner_close_status = linted_spawner_close(spawner);
    if (-1 == spawner_close_status) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    mqd_t const inbox = inboxes[0];
    linted_gui_t const gui = inboxes[1];

    uint8_t x_position = 0;
    uint8_t y_position = 0;

    for (;;) {
        struct message_data message_data;

        ssize_t bytes_read;
        do {
            bytes_read = mq_receive(inbox,
                                    (char *)&message_data, sizeof message_data, NULL);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            LINTED_ERROR("Could not read from simulator connection: %s",
                         linted_error_string_alloc(errno));
        }

        if (0 == bytes_read) {
            break;
        }

        switch (message_data.message_type) {
        case SIMULATOR_TICK:{
                x_position = x_position % 255 + 3;
                y_position = y_position % 255 + 5;
                //@ assert x_position ≤ 255;
                //@ assert y_position ≤ 255;

                int update_status;
                do {
                    update_status = linted_gui_send_update(gui, x_position, y_position);
                } while (-1 == update_status && EINTR == errno);
                if (-1 == update_status) {
                    LINTED_ERROR("Could not send update message to gui: %s",
                                 linted_error_string_alloc(errno));
                }
                break;
            }

        default:
            LINTED_ERROR("Received unexpected message type: %d",
                         message_data.message_type);
        }
    }

    if (-1 == mq_close(inbox)) {
        LINTED_ERROR("Could not close simulator inbox: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_gui_close(gui)) {
        LINTED_ERROR("Could not close gui: %s", linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}
