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

#include "linted/base/stdio.h"
#include "linted/gui.h"
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/spawn.h"
#include "linted/util.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>


#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " [OPTIONS] [SUBCOMMAND]\n"\
    "Play the " PACKAGE_NAME " game\n"\
    "\n"\
    "The following OPTIONS are accepted:\n"\
    "--version\t\tJust print version info and return\n"\
    "--help\t\tPrint this usage information\n"\
    "\n"\
    "The following SUBCOMMANDS are accepted:\n"\
    "simulator\t\tRun the simulator\n"\
    "gui\t\tRun the gui\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

#define VERSION_TEXT \
    PACKAGE_STRING "\n"\
    "Copyright (C) 2013 Steven Stewart-Gallus\n"\
    PACKAGE_NAME " comes with ABSOLUTELY NO WARRANTY.\n"\
    "You may redistribute copies of Linted\n"\
    "under the terms of the Apache License.\n"\
    "For more information about these matters, see the file named COPYING.\n"

#define ARRAY_SIZE(array) ((sizeof (array)) / sizeof ((array)[0]))

static int go(int argc, char * argv[]);
static int spawn_children(char * binary_name);

int main(int argc, char * argv[]) {
    int const exit_status = go(argc, argv);

    int files_status = 0;
    if (EOF == fclose(stdin)) {
        files_status = -1;
        fprintf(stderr, "Could not close standard input: %s\n",
                strerror(errno));
    }

    if (EOF == fclose(stdout)) {
        files_status = -1;
        fprintf(stderr, "Could not close standard output: %s\n",
                strerror(errno));
    }

    if (EOF == fclose(stderr)) {
        /* No error message. An error code is all we can do. */
        files_status = -1;
    }

    if (-1 == files_status && EXIT_SUCCESS == exit_status) {
        return EXIT_FAILURE;
    }

    return exit_status;
}

static int go(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after execute_subcommand could have executed or Frama C's
       assumptions will break. */

    /* Privileged subcommands */
    if (1 == argc) {
        return spawn_children(argv[0]);
    }

    if (argc >= 2) {
        char const * const subcommand = argv[1];
        if (0 == strcmp(subcommand, LINTED_GUI_NAME)) {
            return linted_gui_main(argc, argv);
        }
    }

    /* Unprivileged sub commands */
    int const sandbox_status = linted_sandbox();
    if (-1 == sandbox_status) {
        LINTED_ERROR("Could not sandbox process because of error: %s\n",
                     strerror(errno));
    }

    if (0 == argc) {
        fprintf(stderr, "Did not receive implicit first argument of the binary name\n");
        return EXIT_FAILURE;
    }

    if (argc >= 2) {
        char const * const subcommand = argv[1];
        if (0 == strcmp(subcommand, LINTED_SIMULATOR_NAME)) {
            return linted_simulator_main(argc, argv);
        } else if (0 == strcmp(subcommand, "--help")) {
            linted_fputs(USAGE_TEXT, stdout);
            return EXIT_SUCCESS;
        } else if (0 == strcmp(subcommand, "--version")) {
            linted_fputs(VERSION_TEXT, stdout);
            return EXIT_SUCCESS;
        } else {
            linted_fprintf(stderr,
                           PACKAGE_TARNAME " did not understand the input %s\n"
                           USAGE_TEXT,
                           subcommand);
            return EXIT_FAILURE;
        }
    }
}

static int spawn_children(char * const binary_name) {
    int simulator_fds[2];
    if (-1 == pipe(simulator_fds)) {
        LINTED_ERROR("Could not make simulator pipe because of error: %s\n",
                     strerror(errno));
    }

    int gui_fds[2];
    if (-1 == pipe(gui_fds)) {
        LINTED_ERROR("Could not make gui pipe because of error: %s\n",
                     strerror(errno));
    }

    int const simulator_reader = simulator_fds[0];
    int const simulator_writer = simulator_fds[1];

    int const gui_reader = gui_fds[0];
    int const gui_writer = gui_fds[1];

    pid_t pid;
    int process_status;
    process_status = linted_spawn(&pid, binary_name, LINTED_SIMULATOR_NAME,
                                  (int[]) { simulator_reader, gui_writer, -1 });
    if (-1 == process_status) {
        LINTED_ERROR("Could not spawn simulator process: %s\n",
                     strerror(errno));
    }

    process_status = linted_spawn(&pid, binary_name, LINTED_GUI_NAME,
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

    if (-1 == linted_sandbox()) {
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
