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

#include "linted/task.h"

#include "linted/sprintf.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* TODO: Calculate exactly */
#define LONGEST_FD_STRING 50

int linted_task_spawn(linted_task_t * const task,
                      linted_task_spawner_t const spawner,
                      char const * const subcommand, int const fildes[]) {
    int error_status = -1;
    size_t fildes_size = 0;

    char (* fildes_strings)[LONGEST_FD_STRING];
    char ** arguments;
    char * subcommand_copy;

    pid_t child_pid;

    for (; fildes[fildes_size] != -1; ++fildes_size) {
        /* Do Nothing */
    }

    fildes_strings = calloc(fildes_size, sizeof fildes_strings[0]);
    if (NULL == fildes_strings && fildes_size != 0) {
        goto finish;
    }

    arguments = calloc(2 + fildes_size + 1, sizeof arguments[0]);
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

    child_pid = fork();
    task->_process_id = child_pid;
    switch (child_pid) {
    case -1:
        goto finish_and_free_subcommand_copy;

    case 0: {
        for (size_t ii = 0; ii < fildes_size; ++ii) {
            int const new_fildes = dup(fildes[ii]);
            if (-1 == new_fildes) {
                LINTED_ERROR("Could not duplicate file descriptor: %s",
                             strerror(errno));
            }

            linted_sprintf(fildes_strings[ii], "%d", new_fildes);
        }

        arguments[0] = spawner._binary_name;
        arguments[1] = subcommand_copy;
        for (size_t ii = 0; ii < fildes_size; ++ii) {
            arguments[2 + ii] = fildes_strings[ii];
        }
        arguments[2 + fildes_size] = NULL;

        execv(spawner._binary_name, arguments);
        /* If execv does not succeed. */
        LINTED_ERROR("Could not execute: %s", strerror(errno));
    }

    default:
        error_status = 0;
    }

 finish_and_free_subcommand_copy:
    free(subcommand_copy);

 finish_and_free_arguments:
    free(arguments);

 finish_and_free_fildes_strings:
    free(fildes_strings);

 finish:
    return error_status;
}
