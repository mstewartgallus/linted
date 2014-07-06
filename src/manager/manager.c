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
#define _GNU_SOURCE

#include "config.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/manager.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <sys/socket.h>

linted_error linted_manager_bind(linted_manager *manager, int backlog,
                                 char const *path, size_t path_len)
{
    linted_error errnum = 0;

    if (NULL == path && path_len != 0u) {
        return EINVAL;
    }

    if (path_len > LINTED_MANAGER_PATH_MAX) {
        return ENAMETOOLONG;
    }

    int sock
        = socket(AF_UNIX, SOCK_SEQPACKET | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
    if (-1 == sock) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    if (-1 == shutdown(sock, SHUT_WR)) {
        errnum = errno;
        assert(errnum != 0);
        goto close_sock;
    }

    {
        struct sockaddr_un address;
        memset(&address, 0, sizeof address);

        address.sun_family = AF_UNIX;

        if (path != NULL) {
            memcpy(address.sun_path, path, path_len);
            if ('@' == address.sun_path[0u]) {
                address.sun_path[0u] = '\0';
            }
        }

        if (-1
            == bind(sock, (void *)&address, sizeof(sa_family_t) + path_len)) {
            errnum = errno;
            assert(errnum != 0);
            goto close_sock;
        }
    }

    if (-1 == listen(sock, backlog)) {
        errnum = errno;
        assert(errnum != 0);
        goto close_sock;
    }

    *manager = sock;
    return 0;

close_sock:
    linted_ko_close(sock);
    return errnum;
}

void linted_manager_accept(struct linted_manager_task_accept *task,
                           unsigned task_action, linted_manager manager)
{
    linted_ko_task_accept(LINTED_UPCAST(task), task_action, manager);
}

linted_error linted_manager_connect(linted_manager *manager, char const *path,
                                    size_t path_len)
{
    linted_error errnum = 0;

    if (path_len > LINTED_MANAGER_PATH_MAX) {
        return ENAMETOOLONG;
    }

    int sock = socket(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0);
    if (-1 == sock) {
        errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    {
        struct sockaddr_un address;
        memset(&address, 0, sizeof address);

        address.sun_family = AF_UNIX;
        memcpy(address.sun_path, path, path_len);

        if ('@' == address.sun_path[0u]) {
            address.sun_path[0u] = '\0';
        }

        if (-1 == connect(sock, (void *)&address,
                          sizeof(sa_family_t) + path_len)) {
            errnum = errno;
            assert(errnum != 0);
            goto close_sock;
        }
    }

    *manager = sock;
    return 0;

close_sock:
    linted_ko_close(sock);
    return errnum;
}

linted_error linted_manager_path(linted_manager manager,
                                 char buf[static LINTED_MANAGER_PATH_MAX],
                                 size_t *len)
{
    struct sockaddr_un address;
    memset(&address, 0, sizeof address);

    socklen_t addr_len = sizeof address;
    {
        socklen_t xx = sizeof address;
        if (-1 == getsockname(manager, (void *)&address, &xx)) {
            linted_error errnum = errno;
            assert(errnum != 0);
            return errnum;
        }
        addr_len = xx;
    }

    *len = addr_len - sizeof(sa_family_t);
    memcpy(buf, address.sun_path, *len);

    if ('\0' == buf[0u]) {
        buf[0u] = '@';
    }

    return 0;
}

void linted_manager_recv_request(struct linted_manager_task_recv_request *task,
                                 unsigned task_action, linted_manager manager)
{
    linted_ko_task_read(LINTED_UPCAST(task), task_action, manager,
                        (char *)&task->request, sizeof task->request);
}

void linted_manager_send_reply(struct linted_manager_task_send_reply *task,
                               unsigned task_action, linted_manager manager,
                               union linted_manager_reply const *reply)
{
    task->reply = *reply;
    linted_ko_task_write(LINTED_UPCAST(task), task_action, manager,
                         (char const *)&task->reply, sizeof task->reply);
}

linted_error linted_manager_send_request(linted_manager manager,
                                         union linted_manager_request const
                                         *request)
{
    return linted_io_write_all(manager, NULL, request, sizeof *request);
}

linted_error linted_manager_recv_reply(linted_manager manager,
                                       union linted_manager_reply *reply,
                                       size_t *size)
{
    linted_error errnum
        = linted_io_read_all(manager, size, reply, sizeof *reply);

    if (errnum != 0) {
        return errnum;
    }

    /* Sent malformed input */
    if (*size != sizeof *reply) {
        return EPROTO;
    }

    return 0;
}
