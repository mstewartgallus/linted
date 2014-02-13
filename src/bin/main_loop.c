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
#include "linted/main_loop.h"
#include "linted/mq.h"
#include "linted/sandbox.h"
#include "linted/shutdowner.h"
#include "linted/simulator.h"
#include "linted/spawner.h"
#include "linted/util.h"

#include <errno.h>
#include <mqueue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum message_type {
    MAIN_LOOP_CLOSE_REQUEST
};

struct message_data {
    enum message_type type;
};

static int main_loop_pair(linted_main_loop_t mqs[2]);
static int gui_run(linted_spawner_t spawner, int const inboxes[]);
static int simulator_run(linted_spawner_t spawner, int const inboxes[]);

int linted_main_loop_run(linted_spawner_t spawner)
{
    linted_main_loop_t main_loop_mqs[2];
    if (-1 == main_loop_pair(main_loop_mqs)) {
        LINTED_ERROR("Could not create main loop message queue: %s",
                     linted_error_string_alloc(errno));
    }

    linted_gui_t gui_mqs[2];
    if (-1 == linted_gui_pair(gui_mqs)) {
        LINTED_ERROR("Could not create gui message queue: %s",
                     linted_error_string_alloc(errno));
    }

    linted_controller_t controller_mqs[2];
    if (-1 == linted_controller_pair(controller_mqs, O_NONBLOCK, O_NONBLOCK)) {
        LINTED_ERROR("Could not create simulator controller message queue: %s",
                     linted_error_string_alloc(errno));
    }

    linted_shutdowner_t shutdowner_mqs[2];
    if (-1 == linted_shutdowner_pair(shutdowner_mqs, O_NONBLOCK, 0)) {
        LINTED_ERROR("Could not create simulator shutdowner message queue: %s",
                     linted_error_string_alloc(errno));
    }

    linted_main_loop_t const main_loop_read = main_loop_mqs[0];
    linted_main_loop_t const main_loop_write = main_loop_mqs[1];

    linted_gui_t const gui_read = gui_mqs[0];
    linted_gui_t const gui_write = gui_mqs[1];

    linted_controller_t const controller_read = controller_mqs[0];
    linted_controller_t const controller_write = controller_mqs[1];

    linted_shutdowner_t const shutdowner_read = shutdowner_mqs[0];
    linted_shutdowner_t const shutdowner_write = shutdowner_mqs[1];

    if (-1 == linted_spawner_spawn(spawner, gui_run, (int[]) {
                                   gui_read, controller_write, main_loop_write, -1})) {
        LINTED_ERROR("Could not spawn gui: %s", linted_error_string_alloc(errno));
    }
    if (-1 == linted_spawner_spawn(spawner, simulator_run, (int[]) {
                                   controller_read, shutdowner_read, gui_write, -1})) {
        LINTED_ERROR("Could not spawn simulator: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_gui_close(gui_read)) {
        LINTED_ERROR("Could not close gui read end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_main_loop_close(main_loop_write)) {
        LINTED_ERROR("Could not close main loop write end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_controller_close(controller_read)) {
        LINTED_ERROR("Could not close controller read end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_controller_close(controller_write)) {
        LINTED_ERROR("Could not close controller write end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_shutdowner_close(shutdowner_read)) {
        LINTED_ERROR("Could not close shutdowner read end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner handle: %s",
                     linted_error_string_alloc(errno));
    }

    for (;;) {
        struct message_data message_data;

        ssize_t bytes_read;
        do {
            bytes_read = mq_receive(main_loop_read,
                                    (char *)&message_data, sizeof message_data, NULL);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            LINTED_ERROR("Could not read from main loop inbox: %s",
                         linted_error_string_alloc(errno));
        }

        switch (message_data.type) {
        case MAIN_LOOP_CLOSE_REQUEST:
            goto exit_main_loop;

        default:
            LINTED_ERROR("Received unexpected message type: %d", message_data.type);
        }
    }

 exit_main_loop:
    {
        int shutdown_status;
        do {
            shutdown_status = linted_shutdowner_send_shutdown(shutdowner_write);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to simulator: %s",
                         linted_error_string_alloc(errno));
        }
    }

    {
        int shutdown_status;
        do {
            shutdown_status = linted_gui_send_shutdown(gui_write);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to gui: %s",
                         linted_error_string_alloc(errno));
        }
    }

    if (-1 == linted_gui_close(gui_write)) {
        LINTED_ERROR("Could not close gui handle: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_shutdowner_close(shutdowner_write)) {
        LINTED_ERROR("Could not close shutdowner write end: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(main_loop_read)) {
        LINTED_ERROR("Could not close main loop read end: %s",
                     linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}

int linted_main_loop_send_close_request(linted_main_loop_t const main_loop)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = MAIN_LOOP_CLOSE_REQUEST;

    return mq_send(main_loop, (char const *)&message_data, sizeof message_data, 0);
}

int linted_main_loop_close(linted_main_loop_t const main_loop)
{
    return mq_close(main_loop);
}

static int main_loop_pair(linted_main_loop_t mqs[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    return linted_mq_pair(mqs, &attr, 0, 0);
}

static int simulator_run(linted_spawner_t const spawner, int const inboxes[])
{
    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    int controller = inboxes[0];
    int shutdowner = inboxes[1];
    int gui = inboxes[2];
    if (-1 == linted_simulator_run(controller, shutdowner, gui)) {
        LINTED_ERROR("Running the simulator failed: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(gui)) {
        LINTED_ERROR("Could not close gui: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(shutdowner)) {
        LINTED_ERROR("Could not close shutdowner: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(controller)) {
        LINTED_ERROR("Could not close simulator: %s", linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}

static int gui_run(linted_spawner_t const spawner, int const inboxes[])
{
    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    mqd_t gui = inboxes[0];
    mqd_t simulator = inboxes[1];
    mqd_t main_loop = inboxes[2];
    if (-1 == linted_gui_run(gui, simulator, main_loop)) {
        LINTED_ERROR("Running the gui failed: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(gui)) {
        LINTED_ERROR("Could not close gui: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(simulator)) {
        LINTED_ERROR("Could not close simulator: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(main_loop)) {
        LINTED_ERROR("Could not close main loop: %s", linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}
