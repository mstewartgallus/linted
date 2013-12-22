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
#include "linted/simulator.h"
#include "linted/spawn.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>


static const char USAGE_TEXT[];
static const char VERSION_TEXT[];

/* TODO: Calculate exactly */
#define LONGEST_FD_STRING 50

#define ARRAY_LENGTH(array) ((sizeof (array)) / sizeof ((array)[0]))

static int execute_subcommand(int argc, char * argv[]);
static int spawn_children(char * binary_name);
static int addclose_except(posix_spawn_file_actions_t * file_actions,
                           size_t n, const int fildes_to_keep[]);
static bool is_open(int fildes);

int main(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after execute_subcommand could have executed or Frama C's
       assumptions will break. */
    switch (argc) {
    case 0:
        fprintf(stderr, "Did not receive implicit first argument of the binary name\n");
        fflush(stderr);
        return EXIT_FAILURE;
    case 1:
        return spawn_children(argv[0]);
    default:
        return execute_subcommand(argc, argv);
    }
}

static int execute_subcommand(int argc, char * argv[]) {
    /* Note that in this function no global state must be modified
       until after any subcommand that is verified by Frama C 
       assumptions could have executed or Frama C's
       assumptions will break. */
    const char * const subcommand = argv[1];
    if (0 == strcmp(subcommand, LINTED_SIMULATOR_NAME)) {
        linted_setrlimit(RLIMIT_NOFILE, &(struct rlimit) { .rlim_cur = 0, .rlim_max = 0 });
        linted_setrlimit(RLIMIT_NPROC, &(struct rlimit) { .rlim_cur = 0, .rlim_max = 0 });
        return linted_simulator_main(argc, argv);
    } else if (0 == strcmp(subcommand, LINTED_GUI_NAME)) {
        return linted_gui_main(argc, argv);
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

static int spawn_children(char * binary_name) {
    int simulator_fds[2];
    linted_pipe(simulator_fds);

    int gui_fds[2];
    linted_pipe(gui_fds);

    const int simulator_reader = simulator_fds[0];
    const int simulator_writer = simulator_fds[1];

    const int gui_reader = gui_fds[0];
    const int gui_writer = gui_fds[1];

    {
        posix_spawn_file_actions_t file_actions = linted_spawn_file_actions();
        const posix_spawnattr_t attr = linted_spawnattr();

        char simulator_reader_string[LONGEST_FD_STRING];
        char gui_writer_string[LONGEST_FD_STRING];

        linted_sprintf(simulator_reader_string, "%d", simulator_reader);
        linted_sprintf(gui_writer_string, "%d", gui_writer);

        const char subcommand[] = LINTED_SIMULATOR_NAME;
        char subcommand_copy[ARRAY_LENGTH(subcommand)];
        memcpy(subcommand_copy, subcommand, ARRAY_LENGTH(subcommand));

        char * arguments[] = {
            binary_name,
            subcommand_copy,
            simulator_reader_string,
            gui_writer_string,
            NULL
        };

        const int open_fildes[] = { gui_writer, simulator_reader };
        addclose_except(&file_actions, ARRAY_LENGTH(open_fildes), open_fildes);

        linted_spawn(binary_name, file_actions, attr, arguments, environ);

        linted_spawnattr_destroy(attr);
        linted_spawn_file_actions_destroy(file_actions);
    }

    {
        posix_spawn_file_actions_t file_actions = linted_spawn_file_actions();
        const posix_spawnattr_t attr = linted_spawnattr();

        char simulator_writer_string[LONGEST_FD_STRING];
        char gui_reader_string[LONGEST_FD_STRING];

        linted_sprintf(simulator_writer_string, "%d", simulator_writer);
        linted_sprintf(gui_reader_string, "%d", gui_reader);

        const char subcommand[] = LINTED_GUI_NAME;
        char subcommand_copy[ARRAY_LENGTH(subcommand)];
        memcpy(subcommand_copy, subcommand, ARRAY_LENGTH(subcommand));

        char * arguments[] = {
            binary_name,
            subcommand_copy,
            gui_reader_string,
            simulator_writer_string,
            NULL
        };

        const int open_fildes[] = { simulator_writer, gui_reader };
        const int error_code = addclose_except(&file_actions, ARRAY_LENGTH(open_fildes),
                                               open_fildes);
        if (-1 == error_code) {
            LINTED_ERROR("Could not add close file descriptor actions: %s\n",
                         strerror(errno));
        }

        linted_spawn(binary_name, file_actions, attr, arguments, environ);

        linted_spawnattr_destroy(attr);
        linted_spawn_file_actions_destroy(file_actions);
    }

    linted_close(simulator_writer);
    linted_close(simulator_reader);
    linted_close(gui_writer);
    linted_close(gui_reader);

    linted_setrlimit(RLIMIT_NOFILE, &(struct rlimit) { .rlim_cur = 0, .rlim_max = 0 });
    linted_setrlimit(RLIMIT_NPROC, &(struct rlimit) { .rlim_cur = 0, .rlim_max = 0 });

    int first_dead_child_status;
    linted_wait(&first_dead_child_status);

    int second_dead_child_status;
    linted_wait(&second_dead_child_status);

    if (first_dead_child_status != EXIT_SUCCESS || second_dead_child_status != EXIT_SUCCESS) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static bool is_open(int fildes) {
    int error_code;
    do {
        error_code = fcntl(fildes, F_GETFL);
    } while (error_code == -1 && errno == EINTR);
    return error_code != -1;
}

static int addclose_except(posix_spawn_file_actions_t * const file_actions,
                           const size_t n, const int fildes_to_keep[]) {
    int error_code = 0;
    const int max_fd = sysconf(_SC_OPEN_MAX);
    for (int fildes = 0; fildes <= max_fd; ++fildes) {
        if (fildes == STDIN_FILENO || fildes == STDOUT_FILENO || fildes == STDERR_FILENO) {
            continue;
        }

        bool should_keep_fildes = false;
        for (size_t ii = 0; ii < n; ++ii) {
            if (fildes_to_keep[ii] == fildes) {
                should_keep_fildes = true;
                break;
            }
        }
        if (should_keep_fildes) {
            continue;
        }

        if (!is_open(fildes)) {
            continue;
        }

        error_code = posix_spawn_file_actions_addclose(file_actions, fildes);
        if (-1 == error_code) {
            break;
        }
    }
    return error_code;
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
