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

#include "linted/io.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern char **environ;

static char const no_memory_string[] =
    "could not allocate memory for error string";

char const *linted_error_string_alloc(int errnum)
{
    size_t size = 40;
    char *string = NULL;
    int strerror_status;
    do {
        size_t const multiplicand = 3;

        if (size > SIZE_MAX / multiplicand) {
            errno = ENOMEM;
            goto out_of_memory;
        }

        size = (multiplicand * size) / 2;

        char *const new_string = realloc(string, size);
        if (NULL == new_string) {
            goto out_of_memory;
        }
        string = new_string;

        strerror_status = strerror_r(errnum, string, size);
    } while (-1 == strerror_status && ERANGE == errno);
    assert(strerror_status != -1);

    return string;

 out_of_memory:
    {
        int new_errnum = errno;
        free(string);
        errno = new_errnum;
    }
    return no_memory_string;
}

void linted_error_string_free(char const *error_string)
{
    if (error_string != no_memory_string) {
        free((void *)error_string);
    }
}

errno_t linted_util_sanitize_environment(fd_set const *essential_fds)
{
    if (-1 == linted_io_close_fds_except(essential_fds)) {
        return errno;
    }

    if (-1 == chdir("/")) {
        return errno;
    }

    /* Sanitize the environment */
    for (char **env = environ; *env != NULL; ++env) {
        memset(*env, '\0', strlen(*env));
    }
    environ = NULL;

    return 0;
}
