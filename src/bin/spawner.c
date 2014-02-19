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

#include "linted/io.h"
#include "linted/util.h"

#include <errno.h>
#include <unistd.h>

struct request_header {
    linted_spawner_task func;
    size_t fildes_count;
};

struct reply {
    int error_status;
};

int linted_spawner_pair(linted_spawner spawners[2])
{
    return linted_server_pair(spawners);
}

int linted_spawner_close(linted_spawner spawner)
{
    return linted_server_close(spawner);
}

int linted_spawner_spawn(linted_spawner const spawner,
                         linted_spawner_task const func,
                         int const fildes_to_send[], size_t fildes_count)
{
    int spawn_status = -1;

    int const connection = linted_server_connect(spawner);
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
            send_status = linted_io_send_fildes(connection, fildes_to_send[ii]);
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

        int close_status = linted_server_conn_close(connection);

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
    return linted_io_recv_fildes(future, spawner);
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
            bytes_read = linted_io_recv_fildes(&new_fildes, connection);
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
