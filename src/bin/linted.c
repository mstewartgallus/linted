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

#include "linted/linted.h"

#include "linted/gui.h"
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/supervisor.h"
#include "linted/task.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>


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

#define SIMULATOR_USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " " LINTED_SIMULATOR_NAME " SIMULATOR_PIPE GUI_PIPE\n"\
    "Run the simulator\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

#define GUI_USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " " LINTED_GUI_NAME " GUI_PIPE SIMULATOR_PIPE\n"\
    "Run the gui\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

static int go(linted_task_spawner_t spawner, int argc, char * argv[]);

int linted_main(linted_task_spawner_t spawner, int argc, char ** argv) {
    int exit_status;

#ifdef HAVE_UID_T
    uid_t const euid = geteuid();
    if (euid != 0) {
        exit_status = go(spawner, argc, argv);
    } else {
        fputs("Bad administrator!\n", stderr);
        fputs("It is a violation of proper security policy to run a game as root!\n", stderr);
        exit_status = EXIT_FAILURE;
    }
#else
    exit_status = go(spawner, argc, argv);
#endif /* HAS_UID_T */

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

static int go(linted_task_spawner_t spawner, int argc, char * argv[]) {
    if (1 == argc) {
        return linted_supervisor_run(spawner);
    }

    if (2 == argc) {
        if (0 == strcmp(argv[1], "--help")) {
            fputs(USAGE_TEXT, stdout);
            return EXIT_SUCCESS;
        } else if (0 == strcmp(argv[1], "--version")) {
            fputs(VERSION_TEXT, stdout);
            return EXIT_SUCCESS;
        }
    }

    fprintf(stderr,
            PACKAGE_TARNAME " did not understand the command line input\n"
            USAGE_TEXT);
    return EXIT_FAILURE;
}
