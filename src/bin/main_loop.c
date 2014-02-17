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
#include "linted/syslog.h"
#include "linted/util.h"

#include <errno.h>
#include <mqueue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int gui_run(linted_spawner spawner, int const inboxes[]);
static int simulator_run(linted_spawner spawner, int const inboxes[]);

int linted_main_loop_run(linted_spawner spawner)
{
    linted_syslog_open();

    linted_updater updater_mqs[2];
    if (-1 == linted_updater_pair(updater_mqs, O_NONBLOCK, 0)) {
        LINTED_LAZY_DEV_ERROR("Could not create updater message queue: %s",
                              linted_error_string_alloc(errno));
    }

    linted_controller controller_mqs[2];
    if (-1 == linted_controller_pair(controller_mqs, O_NONBLOCK, 0)) {
        LINTED_LAZY_DEV_ERROR
            ("Could not create simulator controller message queue: %s",
             linted_error_string_alloc(errno));
    }

    linted_shutdowner simulator_shutdowner_mqs[2];
    if (-1 == linted_shutdowner_pair(simulator_shutdowner_mqs, O_NONBLOCK, 0)) {
        LINTED_LAZY_DEV_ERROR
            ("Could not create simulator shutdowner message queue: %s",
             linted_error_string_alloc(errno));
    }

    linted_updater const updater_read = updater_mqs[0];
    linted_updater const updater_write = updater_mqs[1];

    linted_controller const controller_read = controller_mqs[0];
    linted_controller const controller_write = controller_mqs[1];

    linted_shutdowner const simulator_shutdowner_read =
        simulator_shutdowner_mqs[0];
    linted_shutdowner const simulator_shutdowner_write =
        simulator_shutdowner_mqs[1];

    if (-1 == linted_spawner_spawn(spawner, gui_run, (int[]) {
                                   updater_read, simulator_shutdowner_write,
                                   controller_write, -1})) {
        LINTED_LAZY_DEV_ERROR("Could not spawn gui: %s",
                              linted_error_string_alloc(errno));
    }
    if (-1 == linted_spawner_spawn(spawner, simulator_run, (int[]) {
                                   controller_read, simulator_shutdowner_read,
                                   updater_write, -1})) {
        LINTED_LAZY_DEV_ERROR("Could not spawn simulator: %s",
                              linted_error_string_alloc(errno));
    }

    if (-1 == linted_updater_close(updater_read)) {
        LINTED_FATAL_ERROR("Could not close gui read end: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == linted_controller_close(controller_read)) {
        LINTED_FATAL_ERROR("Could not close controller read end: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == linted_controller_close(controller_write)) {
        LINTED_FATAL_ERROR("Could not close controller write end: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == linted_shutdowner_close(simulator_shutdowner_read)) {
        LINTED_FATAL_ERROR("Could not close simulator shutdowner read end: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == linted_spawner_close(spawner)) {
        LINTED_FATAL_ERROR("Could not close spawner handle: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == linted_updater_close(updater_write)) {
        LINTED_FATAL_ERROR("Could not close updater handle: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == linted_shutdowner_close(simulator_shutdowner_write)) {
        LINTED_FATAL_ERROR("Could not close simulator shutdowner write end: %s",
                           linted_error_string_alloc(errno));
    }

    return 0;
}

static int simulator_run(linted_spawner const spawner, int const inboxes[])
{
    linted_syslog_open();

    if (-1 == linted_spawner_close(spawner)) {
        LINTED_FATAL_ERROR("Could not close spawner: %s",
                           linted_error_string_alloc(errno));
    }

    int controller = inboxes[0];
    int shutdowner = inboxes[1];
    int gui = inboxes[2];
    if (-1 == linted_simulator_run(controller, shutdowner, gui)) {
        LINTED_LAZY_DEV_ERROR("Running the simulator failed: %s",
                              linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(gui)) {
        LINTED_FATAL_ERROR("Could not close gui: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(shutdowner)) {
        LINTED_FATAL_ERROR("Could not close shutdowner: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(controller)) {
        LINTED_FATAL_ERROR("Could not close simulator: %s",
                           linted_error_string_alloc(errno));
    }

    return 0;
}

static int gui_run(linted_spawner const spawner, int const inboxes[])
{
    linted_syslog_open();

    if (-1 == linted_spawner_close(spawner)) {
        LINTED_FATAL_ERROR("Could not close spawner: %s",
                           linted_error_string_alloc(errno));
    }

    mqd_t gui = inboxes[0];
    mqd_t shutdowner = inboxes[1];
    mqd_t simulator = inboxes[2];
    if (-1 == linted_gui_run(gui, shutdowner, simulator)) {
        LINTED_LAZY_DEV_ERROR("Running the gui failed: %s",
                              linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(gui)) {
        LINTED_FATAL_ERROR("Could not close gui: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(shutdowner)) {
        LINTED_FATAL_ERROR("Could not close shutdowner: %s",
                           linted_error_string_alloc(errno));
    }

    if (-1 == mq_close(simulator)) {
        LINTED_FATAL_ERROR("Could not close simulator: %s",
                           linted_error_string_alloc(errno));
    }

    return 0;
}
