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

#include "linted/util.h"
#include "linted/base/stdio.h"
#include "linted/binaries.h"

#include <fcntl.h>
#include <stdlib.h>
#include <string.h>


const char USAGE_TEXT[];
const char VERSION_TEXT[];

/* TODO: Calculate exactly */
#define LONGEST_FD_STRING 50

#define ARRAY_LENGTH(array) ((sizeof (array)) / sizeof ((array)[0]))


int main(int argc, char * argv[]) {
    /*
      This code occurs before the verified linted_control_main
      code. It must NOT modify any global variables as that would
      invalidate Frama-C's assumptions about the go function.
     */
    for (int ii = 1; ii < argc; ++ii) {
        const char * const arg = argv[ii];
        if (strcmp(arg, "--help") == 0) {
            linted_fputs(USAGE_TEXT, stdout);
            linted_fflush(stdout);
            return 0;
        } else if (strcmp(arg, "--version") == 0) {
            linted_fputs(VERSION_TEXT, stdout);
            linted_fflush(stdout);
            return 0;
        } else {
            linted_fprintf(stderr, "Did not understand the input %s\n", arg);
            linted_fputs(USAGE_TEXT, stderr);
            linted_fflush(stderr);
            return 1;
        }
    }

    int gui_event_fds[2];
    linted_pipe(gui_event_fds);

    int gui_command_fds[2];
    linted_pipe(gui_command_fds);

    const int gui_event_input = gui_event_fds[0];
    const int gui_event_output = gui_event_fds[1];

    const int gui_command_input = gui_command_fds[0];
    const int gui_command_output = gui_command_fds[1];

    const pid_t simulator_pid = linted_fork();
    if (simulator_pid == 0) {
        char gui_event_input_string[LONGEST_FD_STRING];
        char gui_command_output_string[LONGEST_FD_STRING];

        linted_sprintf(gui_event_input_string, "%d", gui_event_input);
        linted_sprintf(gui_command_output_string, "%d", gui_command_output);

        const char filename[] = LINTED_SIMULATOR_BINARY;
        char filename_copy[ARRAY_LENGTH(filename)];
        memcpy(filename_copy, filename, ARRAY_LENGTH(filename));
        char * arguments[] = {
            filename_copy,
            gui_event_input_string,
            gui_command_output_string,
            NULL
        };
        linted_execv(filename, arguments);
    }

    const pid_t gui_pid = linted_fork();
    if (gui_pid == 0) {
        char gui_event_output_string[LONGEST_FD_STRING];
        char gui_command_input_string[LONGEST_FD_STRING];

        linted_sprintf(gui_event_output_string, "%d", gui_event_output);
        linted_sprintf(gui_command_input_string, "%d", gui_command_input);

        const char filename[] = LINTED_GUI_BINARY;
        char filename_copy[ARRAY_LENGTH(filename)];
        memcpy(filename_copy, filename, ARRAY_LENGTH(filename));
        char * arguments[] = {
            filename_copy,
            gui_command_input_string,
            gui_event_output_string,
            NULL
        };
        linted_execv(filename, arguments);
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

const char USAGE_TEXT[] =
    "Usage: " PACKAGE_TARNAME " [OPTIONS]\n"
    "Play the " PACKAGE_NAME " game\n"
    "\n"
    "The following OPTIONS are accepted:\n"
    "--version\t\tJust print version info and return\n"
    "--help\t\tPrint this usage information\n"
    ""
    "Report bugs to " PACKAGE_BUGREPORT "\n";

const char VERSION_TEXT[] =
    PACKAGE_STRING "\n"
    "Copyright (C) 2013 Steven Stewart-Gallus\n"
    PACKAGE_NAME " comes with ABSOLUTELY NO WARRANTY.\n"
    "You may redistribute copies of Linted\n"
    "under the terms of the Apache License.\n"
    "For more information about these matters, see the file named COPYING.\n";
