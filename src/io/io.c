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

#include "linted/io.h"

#include "linted/util.h"

#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

errno_t linted_io_read_all(int fd, size_t * bytes_read_out,
                           void *buf, size_t count)
{
    errno_t error_status = 0;
    size_t total_bytes_read = 0;

    do {
        ssize_t bytes_read = read(fd, (char *)buf + total_bytes_read,
                                    count - total_bytes_read);

        if (0 == bytes_read) {
            /* Hang up */
            goto output_bytes_read;
        }

        errno_t read_status = -1 == bytes_read ? errno : 0;
        if (EINTR == read_status) {
            continue;
        }

        if (read_status != 0) {
            error_status = EWOULDBLOCK == read_status ? EAGAIN : read_status;
            goto output_bytes_read;
        }

        total_bytes_read += bytes_read;
    } while (total_bytes_read != count);

 output_bytes_read:
    if (bytes_read_out != NULL) {
        *bytes_read_out = total_bytes_read;
    }
    return error_status;
}

errno_t linted_io_write_all(int fd, size_t * bytes_wrote_out,
                            void const *buf, size_t count)
{
    errno_t error_status = 0;
    size_t total_bytes_wrote = 0;

    do {
        ssize_t bytes_wrote = write(fd, (char const *)buf + total_bytes_wrote,
                                    count - total_bytes_wrote);
        errno_t write_status = -1 == bytes_wrote ? errno : 0;
        if (EINTR == write_status) {
            continue;
        }

        if (write_status != 0) {
            error_status = EWOULDBLOCK == write_status ? EAGAIN : write_status;
            goto output_bytes_wrote;
        }

        total_bytes_wrote += bytes_wrote;
    } while (total_bytes_wrote != count);

 output_bytes_wrote:
    if (bytes_wrote_out != NULL) {
        *bytes_wrote_out = total_bytes_wrote;
    }
    return error_status;
}

errno_t linted_io_write_str(int fd, size_t * bytes_wrote, struct linted_str str)
{
    return linted_io_write_all(fd, bytes_wrote, str.bytes, str.size);
}

errno_t linted_io_write_string(int fd, size_t * bytes_wrote_out, char const *s)
{
    return linted_io_write_all(fd, bytes_wrote_out, s, strlen(s));
}

errno_t linted_io_write_format(int fd, size_t * bytes_wrote_out,
                               char const *format_str, ...)
{
    errno_t error_status = 0;

    va_list ap;
    va_start(ap, format_str);

    va_list ap_copy;
    va_copy(ap_copy, ap);

    int bytes_should_write = vsnprintf(NULL, 0, format_str, ap);
    if (bytes_should_write < 0) {
        error_status = errno;
        goto free_va_lists;
    }

    {
        size_t string_size = bytes_should_write + 1;

        char *string = malloc(string_size);
        if (NULL == string) {
            error_status = errno;
            goto free_va_lists;
        }

        if (vsnprintf(string, string_size, format_str, ap_copy) < 0) {
            error_status = errno;
            goto free_string;
        }

        {
            errno_t errnum = linted_io_write_string(fd, bytes_wrote_out,
                                                    string);
            if (errnum != 0) {
                error_status = errnum;
                goto free_string;
            }
        }

 free_string:
        free(string);
    }

 free_va_lists:
    va_end(ap);
    va_end(ap_copy);

    return error_status;
}

errno_t linted_io_strtofd(char const *str, int *fd)
{
    size_t length = strlen(str);
    unsigned position = 1u;

    if ('0' == str[0u] && length != 1u) {
        return EINVAL;
    }

    unsigned total = 0u;
    for (; length > 0u; --length) {
        char const digit = str[length - 1u];

        if ('0' <= digit && digit <= '9') {
            unsigned long sum = total + ((unsigned)(digit - '0')) * position;
            if (sum > INT_MAX) {
                return ERANGE;
            }

            total = sum;
        } else {
            return EINVAL;
        }

        unsigned long next_position = 10u * position;
        if (next_position > INT_MAX) {
            return ERANGE;
        }
        position = next_position;
    }

    *fd = total;
    return 0;
}

errno_t linted_io_close_fds_except(fd_set const *fds)
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

            if (fd < FD_SETSIZE && FD_ISSET(fd, fds)) {
                continue;
            }

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

errno_t linted_io_close(int fd)
{
    sigset_t full_set;
    sigfillset(&full_set);

    sigset_t old_set;
    pthread_sigmask(SIG_BLOCK, &full_set, &old_set);

    int close_status = close(fd);

    pthread_sigmask(SIG_SETMASK, &old_set, NULL);

    return -1 == close_status ? errno : 0;
}
