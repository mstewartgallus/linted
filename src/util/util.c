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

#include "linted/error.h"
#include "linted/ko.h"

#include <assert.h>
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

extern char **environ;

static linted_error close_fds_except(int const *kept_fds, size_t size);

linted_error linted_util_sanitize_environment(int const *kept_fds, size_t size)
{
    linted_error errnum = close_fds_except(kept_fds, size);
    if (errnum != 0) {
        return errnum;
    }

    /* Sanitize the environment */
    for (char **env = environ; *env != NULL; ++env) {
        memset(*env, '\0', strlen(*env));
    }
    environ = NULL;

    return 0;
}

#ifndef NDEBUG
static linted_error close_fds_except(int const *kept_fds, size_t size)
{
    /* Valgrind has trouble with closing fds*/
    return 0;
}
#else
static linted_error close_fds_except(int const *kept_fds, size_t size)
{
    linted_error error_status = 0;
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
            int *new_fds = linted_mem_realloc_array(&error_status,
                                                    fds_to_close,
                                                    fds_to_close_count,
                                                    sizeof fds_to_close[0]);
            if (error_status != 0) {
                goto free_fds_to_close;
            }
            fds_to_close = new_fds;

            fds_to_close[fds_to_close_count - 1] = fd;
        }

        for (size_t ii = 0; ii < fds_to_close_count; ++ii) {
            linted_error errnum = linted_ko_close(fds_to_close[ii]);
            assert(errnum != EBADF);

            /*
       * Otherwise ignore the error. This function is called
       * for security reasons and an EIO error only means
       * that the spawner of this process leaked an open
       * handle to /dev/full.
       */
        }

    free_fds_to_close:
        linted_mem_free(fds_to_close);
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
#endif
