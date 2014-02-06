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

#include "linted/io.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

struct request_header {
    linted_task_func_t func;
    size_t fildes_count;
};

struct reply_data {
    int error_status;
};

static int fork_server_run(linted_task_spawner_t spawner, int inbox);

linted_task_spawner_t linted_task_spawner_init(void)
{
    /* First we fork from a known good state and serve out forks of
     * this known good state. This avoids several problems with
     * inheritance of corrupted state that aren't even fixable with
     * exec. It also allows us to avoid the nasty command line
     * interface exec forces us into.
     */
    int sockets[2];
    if (-1 == linted_io_create_local_server(sockets)) {
        return -1;
    }

    int const spawner_writer = sockets[0];
    int const spawner_reader = sockets[1];

    {
        pid_t const child_pid = fork();
        if (0 == child_pid) {
            int const exit_status = fork_server_run(spawner_writer, spawner_reader);
            exit(exit_status);
        }

        if (-1 == child_pid) {
            goto error_and_close_sockets;
        }
    }

    if (-1 == close(spawner_reader)) {
        goto error_and_close_socket;
    }

    return spawner_writer;

 error_and_close_sockets:
    close(spawner_reader);

 error_and_close_socket:
    close(spawner_writer);

    return -1;
}

int linted_task_spawner_close(linted_task_spawner_t spawner)
{
    return close(spawner);
}

int linted_task_spawn(linted_task_spawner_t const spawner,
                      linted_task_func_t const func, int const fildes_to_send[])
{
    int error_status = -1;

    size_t fildes_count = 0;
    for (; fildes_to_send[fildes_count] != -1; ++fildes_count) {
        /* Do nothing */
    }

    int const connection = linted_io_connect_to_local_socket(spawner);
    if (-1 == connection) {
        goto finish;
    }

    {
        struct request_header request_header = {
            .func = func,
            .fildes_count = fildes_count
        };

        ssize_t bytes_sent;
        do {
            bytes_sent = send(connection, &request_header, sizeof request_header, 0);
        } while (-1 == bytes_sent && EINTR == errno);
        if (-1 == bytes_sent) {
            goto finish_and_close_connection;
        }
    }

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int const sent_fildes = fildes_to_send[ii];

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

        char control_message_buffer[CMSG_SPACE(sizeof sent_fildes)];
        memset(control_message_buffer, 0, sizeof control_message_buffer);

        message.msg_control = control_message_buffer;
        message.msg_controllen = sizeof control_message_buffer;

        struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
        control_message_header->cmsg_level = SOL_SOCKET;
        control_message_header->cmsg_type = SCM_RIGHTS;
        control_message_header->cmsg_len = CMSG_LEN(sizeof sent_fildes);

        void *const control_message_data = CMSG_DATA(control_message_header);
        memcpy(control_message_data, &sent_fildes, sizeof sent_fildes);

        ssize_t bytes_written;
        do {
            bytes_written = sendmsg(connection, &message, 0);
        } while (-1 == bytes_written && EINTR == errno);
        if (-1 == bytes_written) {
            goto finish_and_close_connection;
        }
    }

    {
        struct reply_data reply_data;
        ssize_t bytes_read;
        do {
            bytes_read = read(connection, &reply_data, sizeof reply_data);
        } while (-1 == bytes_read && EINTR == EINTR);
        if (-1 == bytes_read) {
            goto finish_and_close_connection;
        }

        int const reply_error_status = reply_data.error_status;
        if (reply_error_status != 0) {
            errno = reply_error_status;
            goto finish_and_close_connection;
        }
    }

    error_status = 0;

 finish_and_close_connection:
    if (-1 == close(connection)) {
        error_status = -1;
    }

 finish:
    return error_status;
}

static int fork_server_run(linted_task_spawner_t const spawner, int inbox)
{
    /* Posix requires an exact copy of process memory so passing
     * around function pointers through pipes is allowed.
     */

    struct sigaction action;
    memset(&action, 0, sizeof action);
    action.sa_handler = SIG_IGN;

    struct sigaction old_action;
    int const sig_status = sigaction(SIGCHLD, &action, &old_action);
    if (-1 == sig_status) {
        LINTED_ERROR("Could not ignore child processes: %m\n", errno);
    }

    /* TODO: Handle multiple connections at once */
    for (;;) {
        int const connection = linted_io_recv_socket(inbox);
        if (-1 == connection) {
            if (0 == errno) {
                break;
            }

            LINTED_ERROR("Could not accept fork request connection: %m", errno);
        }

        linted_task_func_t fork_func;
        size_t fildes_count;
        {
            struct request_header request_header;

            ssize_t bytes_received;
            do {
                bytes_received = recv(connection,
                                      &request_header, sizeof request_header,
                                      MSG_WAITALL);
            } while (-1 == bytes_received && EINTR == errno);
            if (-1 == bytes_received) {
                LINTED_ERROR("Could not receive fork request header: %m", errno);
            }

            fork_func = request_header.func;
            fildes_count = request_header.fildes_count;
        }

        if (fildes_count > 255) {
            /* TODO: Send an EINVAL error back */
            LINTED_ERROR("Fildes sent was too high: %lu", fildes_count);
        }

        int sent_inboxes[fildes_count + 1];
        sent_inboxes[fildes_count] = -1;
        for (size_t ii = 0; ii < fildes_count; ++ii) {
            struct msghdr message;
            memset(&message, 0, sizeof message);

            char dummy_data;

            struct iovec iov[] = {
                (struct iovec){
                               .iov_base = &dummy_data,
                               .iov_len = sizeof dummy_data}
            };
            message.msg_iov = iov;
            message.msg_iovlen = LINTED_ARRAY_SIZE(iov);

            int sent_fildes;

            char control_message_buffer[CMSG_SPACE(sizeof sent_fildes)];
            memset(control_message_buffer, 0, sizeof control_message_buffer);

            message.msg_control = control_message_buffer;
            message.msg_controllen = sizeof control_message_buffer;

            ssize_t bytes_read;
            do {
                bytes_read =
                    recvmsg(connection, &message, MSG_CMSG_CLOEXEC | MSG_WAITALL);
            } while (-1 == bytes_read && EINTR == errno);
            if (-1 == bytes_read) {
                LINTED_ERROR("Could not read bytes from fork request: %m", errno);
            }

            struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
            void *const control_message_data = CMSG_DATA(control_message_header);

            memcpy(&sent_fildes, control_message_data, sizeof sent_fildes);

            sent_inboxes[ii] = sent_fildes;
        }

        pid_t const child_pid = fork();
        if (0 == child_pid) {
            /* Restore the old signal behaviour */
            int const retry_sig_status = sigaction(SIGCHLD,
                                                   &old_action,
                                                   &action);
            if (-1 == retry_sig_status) {
                LINTED_ERROR("Could not restore child signal behaviour: %m", errno);
            }

            if (-1 == close(connection)) {
                LINTED_ERROR("Forked child could not close connection: %m", errno);
            }
            return fork_func(spawner, sent_inboxes);
        }

        for (size_t ii = 0; ii < fildes_count; ++ii) {
            if (-1 == close(sent_inboxes[ii])) {
                LINTED_ERROR("Fork server could not close inbox file descriptor: %m",
                             errno);
            }
        }

        {
            struct reply_data reply_data;
            if (-1 == child_pid) {
                reply_data.error_status = errno;
            } else {
                reply_data.error_status = 0;
            }

            int const reply_write_status = write(connection,
                                                 &reply_data,
                                                 sizeof reply_data);
            if (-1 == reply_write_status) {
                LINTED_ERROR
                    ("Fork server could not write reply to child requester: %m", errno);
            }
        }

        if (-1 == close(connection)) {
            LINTED_ERROR("Fork server could not close connection: %m", errno);
        }
    }

    if (-1 == close(inbox)) {
        LINTED_ERROR("Could not close inbox: %m", errno);
    }

    if (-1 == linted_task_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %m", errno);
    }

    return EXIT_SUCCESS;
}
