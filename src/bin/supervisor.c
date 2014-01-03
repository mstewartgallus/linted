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
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/supervisor.h"
#include "linted/task.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>

#define ARRAY_SIZE(array) ((sizeof (array)) / sizeof ((array)[0]))

int linted_supervisor_run(char * const binary_name) {
    int simulator_fds[2];
    if (-1 == pipe2(simulator_fds, O_CLOEXEC)) {
        LINTED_ERROR("Could not make simulator pipe because of error: %s\n",
                     strerror(errno));
    }

    int gui_fds[2];
    if (-1 == pipe2(gui_fds, O_CLOEXEC)) {
        LINTED_ERROR("Could not make gui pipe because of error: %s\n",
                     strerror(errno));
    }

    int const simulator_reader = simulator_fds[0];
    int const simulator_writer = simulator_fds[1];

    int const gui_reader = gui_fds[0];
    int const gui_writer = gui_fds[1];

    linted_task_t simulator_task;
    int process_status;
    process_status = linted_task_spawn(&simulator_task, binary_name,
                                       LINTED_SIMULATOR_NAME,
                                       (int[]) { simulator_reader, gui_writer, -1 });
    if (-1 == process_status) {
        LINTED_ERROR("Could not spawn simulator process: %s\n",
                     strerror(errno));
    }

    linted_task_t gui_task;
    process_status = linted_task_spawn(&gui_task, binary_name, LINTED_GUI_NAME,
                                       (int[]) { gui_reader, simulator_writer, -1 });
    if (-1 == process_status) {
        LINTED_ERROR("Could not spawn gui process: %s\n",
                     strerror(errno));
    }

    {
        int const fds[] = {
            simulator_writer, simulator_reader,
            gui_writer, gui_reader
        };

        for (size_t ii = 0; ii < ARRAY_SIZE(fds); ++ii) {
            int const fd = fds[ii];
            int const error_status = close(fd);
            int error_code;
            if (-1 == error_status && (error_code = errno, error_code != EINTR)) {
                LINTED_ERROR("Could not close pipe file descriptors because of error: %s\n",
                             strerror(error_code));
            }
        }
    }

    int const sandbox_status = linted_sandbox();
    if (-1 == sandbox_status) {
        LINTED_ERROR("Could not sandbox process because of error: %s\n",
                     strerror(errno));
    }

    for (size_t ii = 0; ii < 2; ++ii) {
        int wait_status;
        pid_t exited_process;
        int exit_code;
        do {
            exited_process = wait(&wait_status);
        } while (-1 == exited_process && (exit_code = errno, exit_code != EINTR));
        if (-1 == exited_process) {
            LINTED_ERROR("Could not wait for a process to exit: %s\n",
                         strerror(errno));
        }

        if (WIFEXITED(wait_status)) {
            int const exit_status = WEXITSTATUS(wait_status);
            if (exit_status != EXIT_SUCCESS) {
                return exit_status;
            }
        } else {
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}
