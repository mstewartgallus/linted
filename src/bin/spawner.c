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

#include "linted/spawner.h"

#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/un.h>
#include <unistd.h>

/*
 * We fork from a known good state and serve out forks of this known
 * good state. This avoids several problems with inheritance of
 * corrupted state that aren't even fixable with exec.
 *
 * We send over function pointers which allows us to avoid the nasty
 * command line interface exec forces us into.
 *
 * Posix requires an exact copy of process memory so I believe passing
 * around function pointers through pipes is allowed.
 */

struct request_header {
    linted_spawner_task_t func;
    size_t fildes_count;
};

struct reply_data {
    int error_status;
};

static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[]);
static void exec_task_from_connection(linted_spawner_t spawner, int connection);
static int connect_socket(int const local);
static int send_fildes(int const socket, int const fildes);
static ssize_t recv_fildes(int *fildes, int const inbox);

int linted_spawner_run(linted_spawner_task_t main_loop,
                       int const fildes[])
{
    int sockets[2];
    if (-1 == socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sockets)) {
        return -1;
    }

    linted_spawner_t const spawner = sockets[0];
    int const inbox = sockets[1];

    switch (fork()) {
    case 0:
        if (-1 == close(inbox)) {
            LINTED_ERROR("Could not close inbox: %s",
                         linted_error_string_alloc(errno));
        }

        exec_task(main_loop, spawner, fildes);

    case -1:
        goto exit_with_error_and_close_sockets;
    }

    for (;;) {
        int connection;
        ssize_t bytes_read;

        do {
            bytes_read = recv_fildes(&connection, inbox);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            goto exit_with_error_and_close_sockets;
        }

        /* Luckily, because we already must fork for a fork server we
         * get asynchronous behaviour for free.
         */
        switch (fork()) {
        case 0:
            if (-1 == close(inbox)) {
                LINTED_ERROR("Could not close inbox: %s",
                             linted_error_string_alloc(errno));
            }

            exec_task_from_connection(spawner, connection);

        case -1:{
                struct reply_data reply = {.error_status = errno };

                int write_status;
                do {
                    write_status = write(connection, &reply, sizeof reply);
                } while (-1 == write_status && EINTR == errno);
                if (-1 == write_status) {
                    goto exit_with_error_and_close_sockets;
                }
            }
        }

        if (-1 == close(connection)) {
            goto exit_with_error_and_close_sockets;
        }
    }

 exit_fork_server:

    if (-1 == close(inbox)) {
        goto exit_with_error_and_close_spawner;
    }

    if (-1 == linted_spawner_close(spawner)) {
        goto exit_with_error;
    }

    return 0;

 exit_with_error_and_close_sockets:
    {
        int errnum = errno;
        close(inbox);
        errno = errnum;
    }

 exit_with_error_and_close_spawner:
    {
        int errnum = errno;
        close(spawner);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}

int linted_spawner_close(linted_spawner_t spawner)
{
    return close(spawner);
}

int linted_spawner_spawn(linted_spawner_t const spawner,
                         linted_spawner_task_t const func, int const fildes_to_send[])
{
    size_t fildes_count = 0;
    for (; fildes_to_send[fildes_count] != -1; ++fildes_count) {
        /* Do nothing */
    }

    int const connection = connect_socket(spawner);
    if (-1 == connection) {
        goto exit_with_error;
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
            goto exit_with_error_and_close_connection;
        }
    }

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int send_status;
        do {
            send_status = send_fildes(connection, fildes_to_send[ii]);
        } while (-1 == send_status && EINTR == errno);
        if (-1 == send_status) {
            goto exit_with_error_and_close_connection;
        }
    }

    {
        struct reply_data reply_data;
        ssize_t bytes_read;
        do {
            bytes_read = read(connection, &reply_data, sizeof reply_data);
        } while (-1 == bytes_read && EINTR == errno);
        if (-1 == bytes_read) {
            goto exit_with_error_and_close_connection;
        }

        int const reply_error_status = reply_data.error_status;
        if (reply_error_status != 0) {
            errno = reply_error_status;
            goto exit_with_error_and_close_connection;
        }
    }

    if (-1 == close(connection)) {
        goto exit_with_error;
    }

    return 0;

 exit_with_error_and_close_connection:
    {
        int errnum = errno;
        close(connection);
        errno = errnum;
    }

 exit_with_error:
    return -1;
}


static void exec_task(linted_spawner_task_t task,
                      linted_spawner_t spawner, int const fildes[])
{
    exit(task(spawner, fildes));
}

static void exec_task_from_connection(linted_spawner_t const spawner,
                                      int connection)
{
    linted_spawner_task_t task;
    size_t fildes_count;
    {
        struct request_header request_header;

        ssize_t bytes_received;
        do {
            bytes_received = recv(connection,
                                  &request_header, sizeof request_header, MSG_WAITALL);
        } while (-1 == bytes_received && EINTR == errno);
        if (-1 == bytes_received) {
            goto reply_with_error;
        }

        task = request_header.func;
        fildes_count = request_header.fildes_count;
    }

    if (fildes_count > 20) {
        errno = EINVAL;
        goto reply_with_error;
    }

    {
        int sent_inboxes[fildes_count + 1];
        sent_inboxes[fildes_count] = -1;
        for (size_t ii = 0; ii < fildes_count; ++ii) {
            int fildes;
            ssize_t bytes_read;

            do {
                bytes_read = recv_fildes(&fildes, connection);
            } while (-1 == bytes_read && EINTR == errno);
            switch (bytes_read) {
            case -1:
                goto reply_with_error;

            case 0:
                errno = EINVAL;
                goto reply_with_error;
            }

            sent_inboxes[ii] = fildes;
        }

        {
            struct reply_data reply = {.error_status = 0 };

            int write_status;
            do {
                write_status = write(connection, &reply, sizeof reply);
            } while (-1 == write_status && EINTR == errno);
            if (-1 == write_status) {
                LINTED_ERROR("Fork server could not reply to request: %s",
                             linted_error_string_alloc(errno));
            }
        }

        if (-1 == close(connection)) {
            LINTED_ERROR("Forked child could not close connection: %s",
                         linted_error_string_alloc(errno));
        }

        exec_task(task, spawner, sent_inboxes);
    }

 reply_with_error:;
    struct reply_data reply = {.error_status = errno };

    int write_status;
    do {
        write_status = write(connection, &reply, sizeof reply);
    } while (-1 == write_status && EINTR == errno);
    if (-1 == write_status) {
        LINTED_ERROR("Fork server could not reply to request: %s",
                     linted_error_string_alloc(errno));
    }

    exit(EXIT_FAILURE);
}

static int connect_socket(int const sock)
{
    int new_sockets[2];
    if (-1 == socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, new_sockets)) {
        return -1;
    }

    int send_status;
    do {
        send_status = send_fildes(sock, new_sockets[0]);
    } while (-1 == send_status && EINTR == errno);
    if (-1 == send_status) {
        int errnum = errno;

        close(new_sockets[0]);
        close(new_sockets[1]);

        errno = errnum;
        return -1;
    }

    if (-1 == close(new_sockets[0])) {
        int errnum = errno;

        close(new_sockets[1]);

        errno = errnum;
        return -1;
    }

    return new_sockets[1];
}

static int send_fildes(int const sock, int const fildes)
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

static ssize_t recv_fildes(int *fildes, int const inbox)
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

    ssize_t const bytes_read = recvmsg(inbox, &message, MSG_CMSG_CLOEXEC | MSG_WAITALL);
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
