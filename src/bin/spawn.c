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

#include "linted/spawn.h"

#include "linted/base/stdio.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* TODO: Calculate exactly */
#define LONGEST_FD_STRING 50

static int addclose_except(posix_spawn_file_actions_t * file_actions,
                           size_t n, const int fildes_to_keep[]);
static bool is_open(int fildes);

int linted_spawn(pid_t * const pid, char * const binary_name,
                 char const * const subcommand, int const fildes[const]) {
    int error_status = -1;
    size_t fildes_size = 0;

    int actions_status;
    posix_spawn_file_actions_t file_actions;

    int attr_status;
    posix_spawnattr_t attr;

    char * fildes_strings;
    char * * arguments;
    char * subcommand_copy;

    for (; fildes[fildes_size] != -1; ++fildes_size) {
        /* Do Nothing */
    }

    actions_status = posix_spawn_file_actions_init(&file_actions);
    if (-1 == actions_status) {
        goto finish;
    }

    attr_status = posix_spawnattr_init(&attr);
    if (-1 == attr_status) {
        goto finish_and_free_file_actions;
    }

    fildes_strings = calloc(fildes_size, LONGEST_FD_STRING + 1);
    if (NULL == fildes_strings && fildes_size != 0) {
        goto finish_and_free_attr;
    }

    arguments = calloc(2 + fildes_size + 1, sizeof *arguments);
    if (NULL == arguments) {
        goto finish_and_free_fildes_strings;
    }

    {
        char * last_string = fildes_strings;
        for (size_t ii = 0; ii < fildes_size; ++ii) {
            linted_sprintf(last_string, "%d", fildes[ii]);
            last_string += LONGEST_FD_STRING + 1;
        }
    }

    {
        size_t const subcommand_size = strlen(subcommand) + 1;
        subcommand_copy = malloc(subcommand_size);
        if (NULL == subcommand_copy) {
            goto finish_and_free_arguments;
        }
        memcpy(subcommand_copy, subcommand, subcommand_size);
    }

    arguments[0] = binary_name;
    arguments[1] = subcommand;
    for (size_t ii = 0; ii < fildes_size; ++ii) {
        arguments[2 + ii] = fildes_strings + (LONGEST_FD_STRING + 1) * ii;
    }
    arguments[2 + fildes_size] = NULL;

    addclose_except(&file_actions, fildes_size, fildes);

    do {
        error_status = posix_spawn(pid, binary_name, &file_actions, &attr,
                                   arguments, environ);
    } while (-1 == error_status && errno == EINTR);

    free(subcommand_copy);

 finish_and_free_arguments:
    free(arguments);

 finish_and_free_fildes_strings:
    free(fildes_strings);

 finish_and_free_attr:;
    {
        int const dest_status = posix_spawnattr_destroy(&attr);
        if (-1 == dest_status) {
            error_status = -1;
        }
    }

 finish_and_free_file_actions:;
    {
        int const dest_status = posix_spawn_file_actions_destroy(&file_actions);
        if (-1 == dest_status) {
            error_status = -1;
        }
    }

 finish:
    return error_status;
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

static bool is_open(int fildes) {
    int error_code;
    do {
        error_code = fcntl(fildes, F_GETFL);
    } while (error_code == -1 && errno == EINTR);
    return error_code != -1;
}
