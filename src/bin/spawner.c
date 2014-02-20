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

#include "linted/spawner.h"

#include "linted/io.h"
#include "linted/util.h"

#include <errno.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

struct request_header {
    linted_spawner_task func;
    size_t fildes_count;
};

struct reply {
    int error_status;
};

static int server_connect(int local);

static int send_fildes(int socket, int fildes);
static ssize_t recv_fildes(int *fildes, int socket);

int linted_spawner_pair(linted_spawner spawners[2])
{
    return socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, spawners);
}

int linted_spawner_close(linted_spawner spawner)
{
    return close(spawner);
}

int linted_spawner_spawn(linted_spawner const spawner,
                         linted_spawner_task const func,
                         int const fildes_to_send[], size_t fildes_count)
{
    int spawn_status = -1;

    int const connection = server_connect(spawner);
    if (-1 == connection) {
        goto cleanup_nothing;
    }

    {
        struct request_header request_header = {
            .func = func,
            .fildes_count = fildes_count
        };

        if (-1 == linted_io_write_all(connection, NULL,
                                      &request_header, sizeof request_header)) {
            goto cleanup_connection;
        }
    }

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int send_status;
        do {
            send_status = send_fildes(connection, fildes_to_send[ii]);
        } while (-1 == send_status && EINTR == errno);
        if (-1 == send_status) {
            goto cleanup_connection;
        }
    }

    {
        struct reply reply;
        size_t bytes_read;
        if (-1 == linted_io_read_all(connection, &bytes_read,
                                     &reply, sizeof reply)) {
            goto cleanup_connection;
        }
        if (bytes_read != sizeof reply) {
            /* The connection hung up on us instead of replying */
            errno = EIO;
            goto cleanup_connection;
        }

        int const reply_error_status = reply.error_status;
        if (reply_error_status != 0) {
            errno = reply_error_status;
            goto cleanup_connection;
        }
    }

    spawn_status = 0;

 cleanup_connection:
    {
        int errnum = errno;

        int close_status = close(connection);

        if (-1 == spawn_status) {
            errno = errnum;
        }

        if (-1 == close_status) {
            spawn_status = -1;
        }
    }

 cleanup_nothing:
    return spawn_status;
}

ssize_t linted_spawner_recv_future(linted_spawner spawner,
                                   linted_spawner_future * future)
{
    return recv_fildes(future, spawner);
}

int linted_spawner_recv_request(linted_spawner_future connection,
                                struct linted_spawner_request *request)
{
    linted_spawner_task task;
    size_t fildes_count;
    {
        struct request_header request_header;
        size_t bytes_read;
        if (-1 == linted_io_read_all(connection, &bytes_read,
                                     &request_header, sizeof request_header)) {
            goto reply_with_error;
        }
        if (bytes_read != sizeof request_header) {
            /* The connection hung up on us instead of replying */
            errno = EINVAL;
            goto reply_with_error;
        }

        task = request_header.func;
        fildes_count = request_header.fildes_count;
    }

    if (fildes_count > LINTED_SPAWNER_MAX_FILDES_COUNT) {
        errno = EINVAL;
        goto reply_with_error;
    }

    request->task = task;
    request->fildes_count = fildes_count;

    for (size_t ii = 0; ii < fildes_count; ++ii) {
        int new_fildes;
        ssize_t bytes_read;

        do {
            bytes_read = recv_fildes(&new_fildes, connection);
        } while (-1 == bytes_read && EINTR == errno);
        switch (bytes_read) {
        case -1:
            goto reply_with_error;

        case 0:
            errno = EINVAL;
            goto reply_with_error;
        }

        request->fildes[ii] = new_fildes;
    }

    {
        struct reply reply = {.error_status = 0 };

        if (-1 == linted_io_write_all(connection, NULL, &reply, sizeof reply)) {
            return -1;
        }
    }

    if (-1 == close(connection)) {
        return -1;
    }

    return 0;

 reply_with_error:;
    int errnum = errno;
    struct reply reply = {.error_status = errnum };

    if (-1 == linted_io_write_all(connection, NULL, &reply, sizeof reply)) {
        return -1;
    }

    close(connection);

    return -1;
}

int linted_spawner_deny_request(linted_spawner_future connection, int errnum)
{
    struct reply reply = {.error_status = errnum };

    return linted_io_write_all(connection, NULL, &reply, sizeof reply);
}

static int server_connect(int const sock)
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
