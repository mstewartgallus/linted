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
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern char **environ;

static char const no_memory_string[] = "\
could not allocate memory for error string";

static errno_t close_fds_except(int const *kept_fds, size_t size);

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

errno_t linted_util_sanitize_environment(int const *kept_fds, size_t size)
{
    errno_t errnum = close_fds_except(kept_fds, size);
    if (errnum != 0) {
        return errnum;
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

static errno_t close_fds_except(int const *kept_fds, size_t size)
{
    errno_t error_status = 0;
    DIR *const fds_dir = opendir("/proc/self/fd");
    if (NULL == fds_dir) {
        return errno;
    }

    {
        size_t fds_to_close_count = 0;
        int *fds_to_close = NULL;

        for (;;) {
            /*
             * Use readdir because this function isn't thread safe
             * anyways and readdir_r has a very broken interface.
             */
            errno = 0;
            struct dirent *const result = readdir(fds_dir);
            {
                int errnum = errno;
                if (errnum != 0) {
                    error_status = errnum;
                    goto free_fds_to_close;
                }
            }

            if (NULL == result) {
                break;
            }

            char const *const d_name = result->d_name;
            if (0 == strcmp(d_name, ".")) {
                continue;
            }

            if (0 == strcmp(d_name, "..")) {
                continue;
            }

            int const fd = atoi(d_name);

            /*
             * This is Linux specific code so we can rely on dirfd to
             * not return ENOTSUP here.
             */

            if (fd == dirfd(fds_dir)) {
                continue;
            }

            for (size_t ii = 0; ii < size; ++ii) {
                if (kept_fds[ii] == fd) {
                    goto kept_fd;
                }
            }
            goto not_kept;
 kept_fd:
            continue;
 not_kept:

            ++fds_to_close_count;
            int *new_fds = realloc(fds_to_close,
                                   fds_to_close_count * sizeof fds_to_close[0]);
            if (NULL == new_fds) {
                error_status = errno;
                goto free_fds_to_close;
            }
            fds_to_close = new_fds;

            fds_to_close[fds_to_close_count - 1] = fd;
        }

        for (size_t ii = 0; ii < fds_to_close_count; ++ii) {
            errno_t errnum = linted_io_close(fds_to_close[ii]);
            assert(errnum != EBADF);

            /*
             * Otherwise ignore the error. This function is called
             * for security reasons and an EIO error only means
             * that the spawner of this process leaked an open
             * handle to /dev/full.
             */
        }

 free_fds_to_close:
        free(fds_to_close);
    }

    if (-1 == closedir(fds_dir)) {
        int errnum = errno;

        assert(errnum != EBADF);

        if (0 == error_status) {
            error_status = errnum;
        }
    }

    return error_status;
}
