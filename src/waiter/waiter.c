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
#include "linted/waiter.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/wait.h>

struct linted_waiter_data {
    pid_t process;
    int fd;
};

static void *waiter_routine(void *data);

errno_t linted_waiter_init(struct linted_waiter * waiter, pid_t pid)
{
    errno_t errnum;

    int init_wait_fd;
    int waiter_wait_fd;
    {
        int waiter_fds[2];
        if (-1 == socketpair(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0,
                             waiter_fds)) {
            return errno;
        }
        init_wait_fd = waiter_fds[0];
        waiter_wait_fd = waiter_fds[1];
    }

    if (-1 == shutdown(init_wait_fd, SHUT_WR)) {
        errnum = errno;
        goto close_fds;
    }

    if (-1 == shutdown(waiter_wait_fd, SHUT_RD)) {
        errnum = errno;
        goto close_fds;
    }

    struct linted_waiter_data * waiter_data = malloc(sizeof *waiter_data);
    if (NULL == waiter_data) {
        errnum = errno;
        goto close_fds;
    }

    waiter_data->fd = waiter_wait_fd;
    waiter_data->process = pid;

    if (-1 == pthread_create(&waiter->pthread, NULL,
                             waiter_routine, waiter_data)) {
        errnum = errno;
        goto free_waiter;
    }

    waiter->waiter_data = waiter_data;
    waiter->init_wait_fd = init_wait_fd;
    waiter->waiter_wait_fd = waiter_wait_fd;

    return 0;

free_waiter:;
    free(waiter_data);

close_fds:;
    linted_io_close(init_wait_fd);
    linted_io_close(waiter_wait_fd);

    return errnum;
}

int linted_waiter_fd(struct linted_waiter const * waiter)
{
    return waiter->init_wait_fd;
}

errno_t linted_waiter_destroy(struct linted_waiter const * waiter)
{
    errno_t errnum = 0;

    pthread_cancel(waiter->pthread);
    /* ESRCH is perfectly fine here */

    errnum = pthread_join(waiter->pthread, NULL);
    assert(errnum != ESRCH);

    free(waiter->waiter_data);

    {
        errno_t close_errnum = linted_io_close(waiter->init_wait_fd);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    {
        errno_t close_errnum = linted_io_close(waiter->waiter_wait_fd);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    return errnum;
}

static void *waiter_routine(void *data)
{
    struct linted_waiter_data *waiter_data = data;

    siginfo_t exit_info;
    memset(&exit_info, 0, sizeof exit_info);

    errno_t errnum;
    do {
        int wait_status = waitid(P_PID, waiter_data->process,
                                 &exit_info, WEXITED);
        errnum = -1 == wait_status ? errno : 0;
        assert(errnum != EINVAL);
    } while (EINTR == errnum);

    {
        struct linted_waiter_message message;
        memset(&message, 0, sizeof message);

        message.exit_info = exit_info;
        message.errnum = errnum;

        linted_io_write_all(waiter_data->fd, NULL,
                            &message, sizeof message);
        /* TODO: Handle the error */
    }

    return NULL;
}
