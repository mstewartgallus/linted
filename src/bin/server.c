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

#include "linted/server.h"

#include "linted/fildes.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

int linted_server(linted_server_t servers[2])
{
    return socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, servers);
}

int linted_server_close(linted_server_t server)
{
    return close(server);
}

linted_server_conn_t linted_server_connect(linted_server_t const sock)
{
    int new_sockets[2];
    if (-1 == socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, new_sockets)) {
        return -1;
    }

    int send_status;
    do {
        send_status = linted_fildes_send(sock, new_sockets[0]);
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

int linted_server_conn_close(linted_server_conn_t server)
{
    return close(server);
}
