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
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* TODO: Calculate exactly */
#define LONGEST_FD_STRING 50

int linted_spawn(pid_t * const pid, char * const binary_name,
                 char const * const subcommand, int const fildes[]) {
    int error_status = -1;
    size_t fildes_size = 0;

    int * new_fildes;
    char * fildes_strings;
    char * * arguments;
    char * subcommand_copy;

    for (; fildes[fildes_size] != -1; ++fildes_size) {
        /* Do Nothing */
    }

    fildes_strings = calloc(fildes_size, LONGEST_FD_STRING + 1);
    if (NULL == fildes_strings && fildes_size != 0) {
        goto finish;
    }

    new_fildes = calloc(fildes_size, sizeof *new_fildes);
    if (NULL == new_fildes) {
        goto finish_and_free_new_fildes;
    }

    arguments = calloc(2 + fildes_size + 1, sizeof *arguments);
    if (NULL == arguments) {
        goto finish_and_free_fildes_strings;
    }

    {
        size_t const subcommand_size = strlen(subcommand) + 1;
        subcommand_copy = malloc(subcommand_size);
        if (NULL == subcommand_copy) {
            goto finish_and_free_arguments;
        }
        memcpy(subcommand_copy, subcommand, subcommand_size);
    }

    pid_t const child_pid = fork();
    switch (child_pid) {
    case -1:
        goto finish_and_free_subcommand_copy;

    case 0:
        for (size_t ii = 0; ii < fildes_size; ++ii) {
            int const fd = dup(fildes[ii]);
            if (-1 == fd) {
                LINTED_ERROR("Could not duplicate file descriptor: %s",
                             strerror(errno));
            }
            new_fildes[ii] = fd;
        }

        {
            char * last_string = fildes_strings;
            for (size_t ii = 0; ii < fildes_size; ++ii) {
                linted_sprintf(last_string, "%d", new_fildes[ii]);
                last_string += LONGEST_FD_STRING + 1;
            }
        }

        arguments[0] = binary_name;
        arguments[1] = subcommand_copy;
        for (size_t ii = 0; ii < fildes_size; ++ii) {
            arguments[2 + ii] = fildes_strings + (LONGEST_FD_STRING + 1) * ii;
        }
        arguments[2 + fildes_size] = NULL;


        execv(binary_name, arguments);
        /* If couldn't execute. */
        LINTED_ERROR("Could not execute: %s", strerror(errno));

    default:
        error_status = 0;
    }

 finish_and_free_subcommand_copy:
    free(subcommand_copy);

 finish_and_free_arguments:
    free(arguments);

 finish_and_free_new_fildes:
    free(new_fildes);

 finish_and_free_fildes_strings:
    free(fildes_strings);

 finish:
    return error_status;
}
