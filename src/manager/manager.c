/*
 * Copyright 2014 Steven Stewart-Gallus
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
#include "linted/manager.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <sys/socket.h>

errno_t linted_manager_bind(linted_manager * manager, int backlog,
                            char const * path, size_t path_len)
{
    if (NULL == path && path_len != 0) {
        return EINVAL;
    }

    if (0 == path_len && path != NULL) {
        return EINVAL;
    }

    if (path_len > LINTED_MANAGER_PATH_MAX) {
        return ENAMETOOLONG;
    }

    int sock = socket(AF_UNIX,
                      SOCK_SEQPACKET | SOCK_NONBLOCK | SOCK_CLOEXEC,
                      0);
    if (-1 == sock) {
        return errno;
    }

    if (-1 == shutdown(sock, SHUT_WR)) {
        goto close_sock;
    }

    {
        struct sockaddr_un address;
        memset(&address, 0, sizeof address);

        address.sun_family = AF_UNIX;
        memcpy(address.sun_path, path, path_len);

        if (-1 == bind(sock, (void *)&address,
                       sizeof(sa_family_t) + path_len)) {
            goto close_sock;
        }
    }

    if (-1 == listen(sock, backlog)) {
        goto close_sock;
    }

    *manager = sock;
    return 0;

close_sock:
    {
        errno_t errnum = errno;
        linted_io_close(sock);
        return errnum;
    }
}

errno_t linted_manager_connect(linted_manager * manager,
                               char const * path, size_t path_len)
{
    if (path_len > LINTED_MANAGER_PATH_MAX) {
        return ENAMETOOLONG;
    }

    int sock = socket(AF_UNIX,
                      SOCK_SEQPACKET | SOCK_CLOEXEC,
                      0);
    if (-1 == sock) {
        return errno;
    }

    {
        struct sockaddr_un address;
        memset(&address, 0, sizeof address);

        address.sun_family = AF_UNIX;
        memcpy(address.sun_path, path, path_len);

        if (-1 == connect(sock, (void *)&address,
                          sizeof(sa_family_t) + path_len)) {
            goto close_sock;
        }
    }

    *manager = sock;
    return 0;

 close_sock:
    {
        errno_t errnum = errno;
        linted_io_close(sock);
        return errnum;
    }
}

errno_t linted_manager_close(linted_manager manager)
{
    return linted_io_close(manager);
}

errno_t linted_manager_path(linted_manager manager,
                            char buf[static LINTED_MANAGER_PATH_MAX],
                            size_t * len)
{
    struct sockaddr_un address;
    memset(&address, 0, sizeof address);

    socklen_t addr_len = sizeof address;
    if (-1 == getsockname(manager, (void *)&address, &addr_len)) {
        return errno;
    }

    *len = addr_len - sizeof (sa_family_t);
    memcpy(buf, address.sun_path, *len);

    return 0;
}

errno_t linted_manager_recv_request(linted_manager manager,
                                    union linted_manager_request *request)
{
    return linted_io_read_all(manager, NULL, request, sizeof *request);
}

errno_t linted_manager_send_reply(linted_manager manager,
                                  union linted_manager_reply const *reply)
{
    return linted_io_write_all(manager, NULL, reply, sizeof *reply);
}

errno_t linted_manager_send_request(linted_manager manager,
                                    union linted_manager_request const *request)
{
    return linted_io_write_all(manager, NULL, request, sizeof *request);
}

errno_t linted_manager_recv_reply(linted_manager manager,
                                  union linted_manager_reply *reply)
{
    return linted_io_read_all(manager, NULL, reply, sizeof *reply);
}
