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

static posix_spawn_file_actions_t check_spawn_file_actions(void);

static posix_spawnattr_t check_spawnattr(void);

static void check_spawn_file_actions_addclose(posix_spawn_file_actions_t * file_actions,
                                              int fildes);

static void check_spawn_file_actions_destroy(posix_spawn_file_actions_t file_actions);

static void check_spawnattr_destroy(posix_spawnattr_t attrp);

static pid_t check_spawn(char * path,
                         posix_spawn_file_actions_t file_actions,
                         posix_spawnattr_t attrp,
                         char *const argv[], char *const envp[]);


pid_t linted_spawn(char * const binary_name, char const * const subcommand,
                   int const fildes[const]) {
    size_t fildes_size = 0;
    for (; fildes[fildes_size] != -1; ++fildes_size) {
        /* Do Nothing */
    }

    posix_spawn_file_actions_t file_actions = check_spawn_file_actions();
    posix_spawnattr_t const attr = check_spawnattr();

    char * const fildes_strings = calloc(fildes_size, LONGEST_FD_STRING + 1);
    if (NULL == fildes_strings && fildes_size != 0) {
        LINTED_ERROR("Could not allocate memory to spawn process: %s\n",
                     strerror(errno));
    }

    char * * const arguments = calloc(2 + fildes_size + 1, sizeof *arguments);
    if (NULL == arguments) {
        LINTED_ERROR("Could not allocate memory to spawn process: %s\n",
                     strerror(errno));
    }

    char * last_string = fildes_strings;
    for (size_t ii = 0; ii < fildes_size; ++ii) {
        linted_sprintf(last_string, "%d", fildes[ii]);
        last_string += LONGEST_FD_STRING + 1;
    }

    size_t const subcommand_size = strlen(subcommand) + 1;
    char * const subcommand_copy = malloc(subcommand_size);
    memcpy(subcommand_copy, subcommand, subcommand_size);

    arguments[0] = binary_name;
    arguments[1] = subcommand;
    for (size_t ii = 0; ii < fildes_size; ++ii) {
        arguments[2 + ii] = fildes_strings + (LONGEST_FD_STRING + 1) * ii;
    }
    arguments[2 + fildes_size] = NULL;

    addclose_except(&file_actions, fildes_size, fildes);

    pid_t const process = check_spawn(binary_name, file_actions, attr,
                                      arguments, environ);

    free(arguments);
    free(fildes_strings);

    check_spawnattr_destroy(attr);
    check_spawn_file_actions_destroy(file_actions);

    return process;
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

static posix_spawn_file_actions_t check_spawn_file_actions(void) {
    posix_spawn_file_actions_t file_actions;
    const int error_code = posix_spawn_file_actions_init(&file_actions);
    if (-1 == error_code) {
        LINTED_ERROR("Could not create spawn file actions object: %s\n", strerror(errno));
    }
    return file_actions;
}

static posix_spawnattr_t check_spawnattr(void) {
    posix_spawnattr_t attrp;
    const int error_code = posix_spawnattr_init(&attrp);
    if (-1 == error_code) {
        LINTED_ERROR("Could not create spawn attributes object: %s\n", strerror(errno));
    }
    return attrp;
}

static void check_spawn_file_actions_addclose(posix_spawn_file_actions_t * file_actions,
                                              int fildes) {
    const int error_code = posix_spawn_file_actions_addclose(file_actions, fildes);
    if (-1 == error_code) {
        LINTED_ERROR("Could not add close file descriptor action: %s\n", strerror(errno));
    }
}

static void check_spawn_file_actions_destroy(posix_spawn_file_actions_t file_actions) {
    const int error_code = posix_spawn_file_actions_destroy(&file_actions);
    if (-1 == error_code) {
        LINTED_ERROR("Could not destroy spawn file actions object: %s\n", strerror(errno));
    }
}

static void check_spawnattr_destroy(posix_spawnattr_t attrp) {
    const int error_code = posix_spawnattr_destroy(&attrp);
    if (-1 == error_code) {
        LINTED_ERROR("Could not destroy spawn attributes object: %s\n", strerror(errno));
    }
}

static pid_t check_spawn(char * path,
                         posix_spawn_file_actions_t file_actions,
                         posix_spawnattr_t attrp,
                         char *const argv[], char *const envp[]) {
    pid_t pid;
    int error_code;
    do {
        error_code = posix_spawn(&pid, path, &file_actions, &attrp, argv, envp);
    } while (-1 == error_code && errno == EINTR);
    if (-1 == error_code) {
        LINTED_ERROR("Could not spawn process: %s\n", strerror(errno));
    }
    return pid;
}
