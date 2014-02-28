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

static int gui_run(void *context, int const inboxes[]);
static int simulator_run(void *context, int const inboxes[]);

int linted_main_loop_run(linted_spawner spawner)
{
    int exit_status = -1;

    linted_updater updater_mqs[2];
    linted_controller controller_mqs[2];
    linted_shutdowner simulator_shutdowner_mqs[2];

    linted_updater updater_read;
    linted_updater updater_write;

    linted_controller controller_read;
    linted_controller controller_write;

    linted_shutdowner simulator_shutdowner_read;
    linted_shutdowner simulator_shutdowner_write;

    if (-1 == linted_updater_pair(updater_mqs, O_NONBLOCK, 0)) {
        return -1;
    }

    updater_read = updater_mqs[0];
    updater_write = updater_mqs[1];

    if (-1 == linted_controller_pair(controller_mqs, O_NONBLOCK, 0)) {
        goto cleanup_updater_pair;
    }

    controller_read = controller_mqs[0];
    controller_write = controller_mqs[1];

    if (-1 == linted_shutdowner_pair(simulator_shutdowner_mqs, O_NONBLOCK, 0)) {
        goto cleanup_controller_pair;
    }

    simulator_shutdowner_read = simulator_shutdowner_mqs[0];
    simulator_shutdowner_write = simulator_shutdowner_mqs[1];

    {
        int gui_fds[] = {
            updater_read,
            simulator_shutdowner_write,
            controller_write
        };
        if (-1 == linted_spawner_spawn(spawner, gui_run,
                                       gui_fds, LINTED_ARRAY_SIZE(gui_fds))) {
            goto cleanup;
        }
    }

    {
        int simulator_fds[] = {
            controller_read,
            simulator_shutdowner_read,
            updater_write
        };
        if (-1 == linted_spawner_spawn(spawner, simulator_run,
                                       simulator_fds,
                                       LINTED_ARRAY_SIZE(simulator_fds))) {
            goto cleanup;
        }
    }

    exit_status = 0;

 cleanup:
    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(simulator_shutdowner_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(simulator_shutdowner_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 cleanup_controller_pair:
    {
        int errnum = errno;
        int close_status = linted_controller_close(controller_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_controller_close(controller_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 cleanup_updater_pair:
    {
        int errnum = errno;
        int close_status = linted_updater_close(updater_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_updater_close(updater_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}

static int simulator_run(void *context, int const inboxes[])
{
    int exit_status = -1;

    linted_controller controller = inboxes[0];
    linted_shutdowner shutdowner = inboxes[1];
    linted_updater updater = inboxes[2];

    if (-1 == linted_simulator_run(controller, shutdowner, updater)) {
        goto cleanup;
    }

    exit_status = 0;

 cleanup:
    {
        int errnum = errno;
        int close_status = linted_updater_close(updater);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(shutdowner);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(controller);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}

static int gui_run(void *context, int const inboxes[])
{
    int exit_status = -1;

    linted_updater updater = inboxes[0];
    linted_shutdowner shutdowner = inboxes[1];
    linted_controller controller = inboxes[2];

    if (-1 == linted_gui_run(updater, shutdowner, controller)) {
        goto cleanup;
    }

    exit_status = 0;

 cleanup:
    {
        int errnum = errno;
        int close_status = linted_updater_close(updater);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(shutdowner);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_controller_close(controller);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}
