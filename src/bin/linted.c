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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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

static int go(int argc, char * argv[]);
static int gui_main(int argc, char * argv[]);
static int simulator_main(int argc, char * argv[]);

int main(int argc, char * argv[]) {
    int exit_status;

#ifdef HAVE_UID_T
    uid_t const euid = geteuid();
    if (euid != 0) {
        exit_status = go(argc, argv);
    } else {
        fputs("Bad administrator!\n", stderr);
        fputs("It is a violation of proper security policy to run a game as root!\n", stderr);
        exit_status = EXIT_FAILURE;
    }
#else
    exit_status = go(argc, argv);
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

static int go(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after execute_subcommand could have executed or Frama C's
       assumptions will break. */

    /* Privileged subcommands */
    if (1 == argc) {
        linted_task_spawner_t const spawner = { ._binary_name = argv[0] };
        return linted_supervisor_run(spawner);
    }

    if (argc >= 2) {
        char const * const subcommand = argv[1];
        if (0 == strcmp(subcommand, LINTED_GUI_NAME)) {
            return gui_main(argc, argv);
        }
    }

    /* Unprivileged sub commands */
    linted_sandbox();

    if (0 == argc) {
        fprintf(stderr, "Did not receive implicit first argument of the binary name\n");
        return EXIT_FAILURE;
    } else {
        char const * const subcommand = argv[1];
        if (0 == strcmp(subcommand, LINTED_SIMULATOR_NAME)) {
            return simulator_main(argc, argv);
        } else if (0 == strcmp(subcommand, "--help")) {
            fputs(USAGE_TEXT, stdout);
            return EXIT_SUCCESS;
        } else if (0 == strcmp(subcommand, "--version")) {
            fputs(VERSION_TEXT, stdout);
            return EXIT_SUCCESS;
        } else {
            fprintf(stderr,
                    PACKAGE_TARNAME " did not understand the input %s\n"
                    USAGE_TEXT,
                    subcommand);
            return EXIT_FAILURE;
        }
    }
}


static int simulator_main(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after simulator_main could have executed or Frama C's
       assumptions will break. */
    if (argc != 4) {
        fprintf(stderr,
                PACKAGE_TARNAME
                " "
                LINTED_SIMULATOR_NAME
                " did not understand the input\n");
        fputs(SIMULATOR_USAGE_TEXT, stderr);
        return EXIT_FAILURE;
    }

    int const simulator_fifo = atoi(argv[2]);
    int const gui_fifo = atoi(argv[3]);
    int const exit_status = linted_simulator_run(simulator_fifo, gui_fifo);

    {
        int error_status;
        int error_code;
        do {
            error_status = close(gui_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close gui fifo %s\n",
                         strerror(error_code));
        }
    }

    {
        int error_status;
        int error_code;
        do {
            error_status = close(simulator_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close simulator fifo %s\n",
                         strerror(error_code));
        }
    }

    return exit_status;
}

static int gui_main(int argc, char * argv[]) {
    if (argc != 4) {
        fprintf(stderr,
                PACKAGE_TARNAME
                " "
                LINTED_GUI_NAME
                " did not understand the input\n");
        fputs(GUI_USAGE_TEXT, stderr);
        return EXIT_FAILURE;
    }

    int const gui_fifo = atoi(argv[2]);
    int const simulator_fifo = atoi(argv[3]);
    int const exit_status = linted_gui_run(gui_fifo, simulator_fifo);

    {
        int error_status;
        int error_code;
        do {
            error_status = close(simulator_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close simulator fifo %s\n",
                         strerror(error_code));
        }
    }

    {
        int error_status;
        int error_code;
        do {
            error_status = close(gui_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close gui fifo %s\n",
                         strerror(error_code));
        }
    }

    return exit_status;
}
