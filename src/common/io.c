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

int linted_io_read_all(int fd, size_t * bytes_read_out, void *buf, size_t count)
{
    int exit_status = -1;
    size_t total_bytes_read = 0;

    do {
        ssize_t bytes_read = read(fd, (char *)buf + total_bytes_read,
                                  count - total_bytes_read);
        if (-1 == bytes_read) {
            if (EINTR == errno) {
                continue;
            }

            goto output_bytes_read;
        }

        if (0 == bytes_read) {
            /* File empty or pipe hangup */
            exit_status = 0;
            goto output_bytes_read;
        }

        total_bytes_read += bytes_read;
    } while (total_bytes_read != count);

    exit_status = 0;

 output_bytes_read:
    if (bytes_read_out != NULL) {
        *bytes_read_out = total_bytes_read;
    }
    return exit_status;
}

int linted_io_write_all(int fd, size_t * bytes_wrote_out,
                        void const *buf, size_t count)
{
    int exit_status = -1;
    size_t total_bytes_wrote = 0;

    do {
        ssize_t bytes_wrote = write(fd, (char const *)buf + total_bytes_wrote,
                                    count - total_bytes_wrote);
        if (-1 == bytes_wrote) {
            if (EINTR == errno) {
                continue;
            }

            goto output_bytes_wrote;
        }

        total_bytes_wrote += bytes_wrote;
    } while (total_bytes_wrote != count);

    exit_status = 0;

 output_bytes_wrote:
    if (bytes_wrote_out != NULL) {
        *bytes_wrote_out = total_bytes_wrote;
    }
    return exit_status;
}

int linted_io_write_string(int fd, size_t * bytes_wrote_out, char const *s)
{
    return linted_io_write_all(fd, bytes_wrote_out, s, strlen(s));
}

int linted_io_write_format(int fd, size_t * bytes_wrote_out,
                           char const *format_str, ...)
{
    int exit_status = -1;

    va_list ap;
    va_start(ap, format_str);

    va_list ap_copy;
    va_copy(ap_copy, ap);

    int bytes_should_write = vsnprintf(NULL, 0, format_str, ap);
    if (bytes_should_write < 0) {
        goto free_va_lists;
    }

    size_t string_size = bytes_should_write + 1;

    char *string = malloc(string_size);
    if (NULL == string) {
        goto free_va_lists;
    }

    if (vsnprintf(string, string_size, format_str, ap_copy) < 0) {
        goto free_string;
    }

    if (-1 == linted_io_write_string(fd, bytes_wrote_out, string)) {
        goto free_string;
    }

    exit_status = 0;

 free_string:
    {
        int errnum = errno;
        free(string);
        errno = errnum;
    }

 free_va_lists:
    va_end(ap);
    va_end(ap_copy);

    return exit_status;
}

int linted_io_strtofd(char const * str)
{
    size_t length = strlen(str);
    unsigned position = 1u;

    if ('0' == str[0u] && length != 1u) {
        errno = EINVAL;
        return -1;
    }

    unsigned total = 0u;
    for (; length > 0u; --length) {
        char const digit = str[length - 1u];

        if ('0' <= digit && digit <= '9') {
            unsigned long sum = total + ((unsigned) (digit - '0')) * position;
            if (sum > INT_MAX) {
                errno = ERANGE;
                return -1;
            }

            total = sum;
        } else {
            errno = EINVAL;
            return -1;
        }

        unsigned long next_position = 10u * position;
        if (next_position > INT_MAX) {
            errno = ERANGE;
            return -1;
        }
    }

    return total;
}

int linted_io_close_fds_except(fd_set const *fds)
{
    int exit_status = -1;
    DIR *const fds_dir = opendir("/proc/self/fd");
    if (NULL == fds_dir) {
        return -1;
    }

    long const name_max = fpathconf(dirfd(fds_dir), _PC_NAME_MAX);
    if (-1 == name_max) {
        LINTED_IMPOSSIBLE_ERROR
            ("could not find the maximum name length of a subdirectory: %s",
             linted_error_string_alloc(errno));
    }

    struct dirent *const entry =
        malloc(offsetof(struct dirent, d_name) + name_max + 1);
    if (NULL == entry) {
        goto close_fds_dir;
    }

    {
        size_t fds_to_close_count = 0;
        int *fds_to_close = NULL;

        for (;;) {
            struct dirent *result;
            int const errnum = readdir_r(fds_dir, entry, &result);
            if (errnum != 0) {
                errno = errnum;
                goto free_fds_to_close;
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
                goto free_fds_to_close;
            }
            fds_to_close = new_fds;

            fds_to_close[fds_to_close_count - 1] = fd;
        }

        for (size_t ii = 0; ii < fds_to_close_count; ++ii) {
            if (-1 == linted_io_close(fds_to_close[ii])) {
                if (EBADF == errno) {
                    LINTED_IMPOSSIBLE_ERROR("could not close open file: %s",
                                            linted_error_string_alloc(errno));
                }

                /*
                 * Otherwise ignore the error. This function is called
                 * for security reasons and an EIO error only means
                 * that the spawner of this process leaked an open
                 * handle to /dev/full.
                 */
            }
        }

        exit_status = 0;

 free_fds_to_close:
        free(fds_to_close);

        free(entry);
    }

 close_fds_dir:
    if (-1 == closedir(fds_dir)) {
        if (EBADF == errno) {
            LINTED_IMPOSSIBLE_ERROR("could not close opened directory: %s",
                                    linted_error_string_alloc(errno));
        }

        exit_status = -1;
    }

    return exit_status;
}

int linted_io_close(int fd)
{
    sigset_t full_set;
    sigfillset(&full_set);

    sigset_t old_set;
    pthread_sigmask(SIG_BLOCK, &full_set, &old_set);

    int close_status = close(fd);

    pthread_sigmask(SIG_SETMASK, &old_set, NULL);

    return close_status;
}
