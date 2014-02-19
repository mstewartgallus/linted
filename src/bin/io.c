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

#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

int linted_io_send_fildes(int const sock, int const fildes)
{
    char dummy_data = 0;

    struct iovec iovecs[] = {
        (struct iovec){
                       .iov_base = &dummy_data,
                       .iov_len = sizeof dummy_data}
    };

    struct msghdr message;
    memset(&message, 0, sizeof message);

    message.msg_iov = iovecs;
    message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

    int const sent_fildes[] = { fildes };
    char control_message[CMSG_SPACE(sizeof sent_fildes)];
    memset(control_message, 0, sizeof control_message);

    message.msg_control = control_message;
    message.msg_controllen = sizeof control_message;

    struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
    control_message_header->cmsg_level = SOL_SOCKET;
    control_message_header->cmsg_type = SCM_RIGHTS;
    control_message_header->cmsg_len = CMSG_LEN(sizeof sent_fildes);

    void *const control_message_data = CMSG_DATA(control_message_header);
    memcpy(control_message_data, sent_fildes, sizeof sent_fildes);

    return -1 == sendmsg(sock, &message, 0) ? -1 : 0;
}

ssize_t linted_io_recv_fildes(int *fildes, int const inbox)
{
    char dummy_data = 0;

    struct iovec iovecs[] = {
        (struct iovec){
                       .iov_base = &dummy_data,
                       .iov_len = sizeof dummy_data}
    };

    struct msghdr message;
    memset(&message, 0, sizeof message);

    message.msg_iov = iovecs;
    message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

    int sent_fildes[1] = { -1 };
    char control_message[CMSG_SPACE(sizeof sent_fildes)];
    memset(control_message, 0, sizeof control_message);

    message.msg_control = control_message;
    message.msg_controllen = sizeof control_message;

    ssize_t const bytes_read = recvmsg(inbox, &message,
                                       MSG_CMSG_CLOEXEC | MSG_WAITALL);
    if (-1 == bytes_read) {
        return -1;
    }

    if (0 == bytes_read) {
        return 0;
    }

    struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
    void *const control_message_data = CMSG_DATA(control_message_header);

    memcpy(sent_fildes, control_message_data, sizeof sent_fildes);

    *fildes = sent_fildes[0];

    return bytes_read;
}

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

int linted_io_close_fds_except(fd_set const *fds)
{
    int exit_status = -1;
    char const fds_path[] = "/proc/self/fd";
    DIR *const fds_dir = opendir(fds_path);
    if (NULL == fds_dir) {
        return -1;
    }

    struct dirent *const entry = malloc(offsetof(struct dirent, d_name)
                                        + pathconf(fds_path, _PC_NAME_MAX) + 1);
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

            if (FD_ISSET(fd, fds)) {
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
            if (-1 == close(fds_to_close[ii])) {
                LINTED_IMPOSSIBLE_ERROR("could not close open file: %s",
                                        linted_error_string_alloc(errno));
            }
        }

        exit_status = 0;

 free_fds_to_close:
        free(fds_to_close);

        free(entry);
    }

 close_fds_dir:
    if (-1 == closedir(fds_dir)) {
        LINTED_IMPOSSIBLE_ERROR("could not close opened directory: %s",
                                linted_error_string_alloc(errno));
    }

    return exit_status;
}
