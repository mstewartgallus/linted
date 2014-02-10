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
#include "linted/simulator_loop.h"
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

static linted_simulator_t simulator_spawn(linted_spawner_t spawner,
                                          linted_gui_t gui);

static linted_simulator_loop_t simulator_loop_spawn(linted_spawner_t spawner,
                                                    linted_simulator_t simulator);
static linted_gui_t gui_spawn(linted_spawner_t spawner, linted_main_loop_t main_loop);

int linted_main_loop_run(linted_spawner_t spawner)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    mqd_t main_loop_mqs[2];
    if (-1 == linted_mq_pair(main_loop_mqs, &attr, 0)) {
        LINTED_ERROR("Could not create main loop message queue: %s",
                     linted_error_string_alloc(errno));
    }

    linted_main_loop_t const main_loop = main_loop_mqs[1];
    mqd_t const main_loop_read_end = main_loop_mqs[0];

    linted_gui_t const gui = gui_spawn(spawner, main_loop);
    if (-1 == gui) {
        LINTED_ERROR("Could not spawn gui: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_main_loop_close(main_loop)) {
        LINTED_ERROR("Could not close main loop write end: %s",
                     linted_error_string_alloc(errno));
    }

    linted_simulator_t const simulator = simulator_spawn(spawner, gui);
    if (-1 == simulator) {
        LINTED_ERROR("Could not spawn simulator: %s", linted_error_string_alloc(errno));
    }

    linted_simulator_loop_t const simulator_loop = simulator_loop_spawn(spawner,
                                                                        simulator);
    if (-1 == simulator_loop) {
        LINTED_ERROR("Could not spawn simulator loop: %s", linted_error_string_alloc(errno));
    }

    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner handle: %s",
                     linted_error_string_alloc(errno));
    }

    for (;;) {
        struct message_data message_data;

        ssize_t bytes_read;
        do {
            bytes_read = mq_receive(main_loop_read_end,
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
            shutdown_status = linted_simulator_loop_send_shutdown(simulator_loop);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to simulator loop: %s",
                         linted_error_string_alloc(errno));
        }
    }

    {
        int shutdown_status;
        do {
            shutdown_status = linted_simulator_send_shutdown(simulator);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to simulator: %s",
                         linted_error_string_alloc(errno));
        }
    }

    {
        int shutdown_status;
        do {
            shutdown_status = linted_gui_send_shutdown(gui);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == shutdown_status) {
            LINTED_ERROR("Could not send shutdown message to gui: %s",
                         linted_error_string_alloc(errno));
        }
    }

    if (-1 == linted_simulator_loop_close(simulator_loop)) {
        LINTED_ERROR("Could not close simulator loop handle: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_gui_close(gui)) {
        LINTED_ERROR("Could not close gui handle: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(main_loop_read_end)) {
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

static int simulator_run(linted_spawner_t const spawner, int const inboxes[])
{
    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    int simulator = inboxes[0];
    int gui = inboxes[1];
    if (-1 == linted_simulator_run(simulator, gui)) {
        LINTED_ERROR("Running the simulator failed: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(gui)) {
        LINTED_ERROR("Could not close gui: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(simulator)) {
        LINTED_ERROR("Could not close simulator: %s", linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}

static linted_simulator_t simulator_spawn(linted_spawner_t const spawner,
                                          linted_gui_t gui)
{
    linted_simulator_t sim_mqs[2];
    if (-1 == linted_simulator_pair(sim_mqs)) {
        goto exit_with_error;
    }

    if (-1 == linted_spawner_spawn(spawner, simulator_run, (int[]) {
                sim_mqs[0], gui, -1})) {
        goto exit_with_error_and_close_mqueues;
    }

    if (-1 == mq_close(sim_mqs[0])) {
        goto exit_with_error_and_close_mqueue;
    }

    return sim_mqs[1];

 exit_with_error_and_close_mqueues:
    {
        int errnum = errno;
        mq_close(sim_mqs[0]);
        errno = errnum;
    }

 exit_with_error_and_close_mqueue:
    {
        int errnum = errno;
        mq_close(sim_mqs[1]);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}

static int simulator_loop_run(linted_spawner_t const spawner, int const inboxes[]);


static linted_simulator_loop_t simulator_loop_spawn(linted_spawner_t const spawner,
                                                    linted_simulator_t simulator)
{
    linted_simulator_loop_t sim_mqs[2];
    if (-1 == linted_simulator_loop_pair(sim_mqs)) {
        goto exit_with_error;
    }

    if (-1 == linted_spawner_spawn(spawner, simulator_loop_run, (int[]) {
                sim_mqs[0], simulator, -1})) {
        goto exit_with_error_and_close_mqueues;
    }

    if (-1 == mq_close(sim_mqs[0])) {
        goto exit_with_error_and_close_mqueue;
    }

    return sim_mqs[1];

 exit_with_error_and_close_mqueues:
    {
        int errnum = errno;
        mq_close(sim_mqs[0]);
        errno = errnum;
    }

 exit_with_error_and_close_mqueue:
    {
        int errnum = errno;
        mq_close(sim_mqs[1]);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}

static int simulator_loop_run(linted_spawner_t const spawner, int const inboxes[])
{
    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    int simulator_loop = inboxes[0];
    int simulator = inboxes[1];
    if (-1 == linted_simulator_loop_run(simulator_loop, simulator)) {
        LINTED_ERROR("Running the simulator loop failed: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(simulator)) {
        LINTED_ERROR("Could not close simulator: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(simulator_loop)) {
        LINTED_ERROR("Could not close simulator loop: %s", linted_error_string_alloc(errno));
    }

    return 0;
}

static int gui_run(linted_spawner_t const spawner, int const inboxes[])
{
    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    int gui = inboxes[0];
    int main_loop = inboxes[1];
    if (-1 == linted_gui_run(gui, main_loop)) {
        LINTED_ERROR("Running the gui failed: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(gui)) {
        LINTED_ERROR("Could not close gui: %s", linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(main_loop)) {
        LINTED_ERROR("Could not close main loop: %s", linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}

static linted_gui_t gui_spawn(linted_spawner_t spawner,
                              linted_main_loop_t main_loop)
{
    linted_gui_t gui_mqs[2];
    if (-1 == linted_gui_pair(gui_mqs)) {
        goto exit_with_error;
    }

    if (-1 == linted_spawner_spawn(spawner, gui_run, (int[]) {
                                   gui_mqs[0], main_loop, -1})) {
        goto exit_with_error_and_close_mqueues;
    }

    if (-1 == mq_close(gui_mqs[0])) {
        goto exit_with_error_and_close_mqueue;
    }

    return gui_mqs[1];

 exit_with_error_and_close_mqueues:
    {
        int errnum = errno;
        mq_close(gui_mqs[0]);
        errno = errnum;
    }

 exit_with_error_and_close_mqueue:
    {
        int errnum = errno;
        mq_close(gui_mqs[1]);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}
