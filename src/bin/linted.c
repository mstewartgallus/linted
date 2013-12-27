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


static const char USAGE_TEXT[];
static const char VERSION_TEXT[];

#define ARRAY_LENGTH(array) ((sizeof (array)) / sizeof ((array)[0]))

static int spawn_children(char * binary_name);

int main(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after execute_subcommand could have executed or Frama C's
       assumptions will break. */

    /* Privileged subcommands */
    if (1 == argc) {
        return spawn_children(argv[0]);
    }

    if (argc >= 2) {
        const char * const subcommand = argv[1];
        if (0 == strcmp(subcommand, LINTED_GUI_NAME)) {
            return linted_gui_main(argc, argv);
        }
    }

    /* Unprivileged sub commands */
    if (-1 == linted_sandbox()) {
        LINTED_ERROR("Could not sandbox process because of error: %s\n",
                     strerror(errno));
    }

    if (0 == argc) {
        fprintf(stderr, "Did not receive implicit first argument of the binary name\n");
        fflush(stderr);
        return EXIT_FAILURE;
    }

    if (argc >= 2) {
        const char * const subcommand = argv[1];
        if (0 == strcmp(subcommand, LINTED_SIMULATOR_NAME)) {
            return linted_simulator_main(argc, argv);
        } else if (0 == strcmp(subcommand, "--help")) {
            linted_fputs(USAGE_TEXT, stdout);
            linted_fflush(stdout);
            return EXIT_SUCCESS;
        } else if (0 == strcmp(subcommand, "--version")) {
            linted_fputs(VERSION_TEXT, stdout);
            linted_fflush(stdout);
            return EXIT_SUCCESS;
        } else {
            linted_fprintf(stderr,
                           PACKAGE_TARNAME " did not understand the input %s\n",
                           subcommand);
            linted_fputs(USAGE_TEXT, stderr);
            linted_fflush(stderr);
            return EXIT_FAILURE;
        }
    }
}

static int spawn_children(char * binary_name) {
    int simulator_fds[2];
    linted_pipe(simulator_fds);

    int gui_fds[2];
    linted_pipe(gui_fds);

    const int simulator_reader = simulator_fds[0];
    const int simulator_writer = simulator_fds[1];

    const int gui_reader = gui_fds[0];
    const int gui_writer = gui_fds[1];

    linted_spawn(binary_name, LINTED_SIMULATOR_NAME,
                 (int[]) { simulator_reader, gui_writer, -1 });
    linted_spawn(binary_name, LINTED_GUI_NAME,
                 (int[]) { gui_reader, simulator_writer, -1 });

    linted_close(simulator_writer);
    linted_close(simulator_reader);
    linted_close(gui_writer);
    linted_close(gui_reader);

    if (-1 == linted_sandbox()) {
        LINTED_ERROR("Could not sandbox process because of error: %s\n",
                     strerror(errno));
    }

    int first_dead_child_status;
    linted_wait(&first_dead_child_status);

    int second_dead_child_status;
    linted_wait(&second_dead_child_status);

    if (first_dead_child_status != EXIT_SUCCESS || second_dead_child_status != EXIT_SUCCESS) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static const char USAGE_TEXT[] =
    "Usage: " PACKAGE_TARNAME " [OPTIONS] [SUBCOMMAND]\n"
    "Play the " PACKAGE_NAME " game\n"
    "\n"
    "The following OPTIONS are accepted:\n"
    "--version\t\tJust print version info and return\n"
    "--help\t\tPrint this usage information\n"
    "\n"
    "The following SUBCOMMANDS are accepted:\n"
    "simulator\t\tRun the simulator\n"
    "gui\t\tRun the gui\n"
    "\n"
    "Report bugs to " PACKAGE_BUGREPORT "\n";

static const char VERSION_TEXT[] =
    PACKAGE_STRING "\n"
    "Copyright (C) 2013 Steven Stewart-Gallus\n"
    PACKAGE_NAME " comes with ABSOLUTELY NO WARRANTY.\n"
    "You may redistribute copies of Linted\n"
    "under the terms of the Apache License.\n"
    "For more information about these matters, see the file named COPYING.\n";
