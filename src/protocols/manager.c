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

#include "linted/manager.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <sys/uio.h>

struct linted_manager_req {
    unsigned type;
    char data[];
};

int linted_manager_send_signal(void)
{
    return SIGRTMIN;
}

int linted_manager_wait_signal(void)
{
    return SIGRTMIN + 1;
}

int linted_manager_req_type(pid_t pid, struct linted_manager_req const *request)
{
    unsigned type;
    struct iovec local_iov[] = {
        {.iov_base = &type,
         .iov_len = sizeof type}
    };

    struct iovec remote_iov[] = {
        {.iov_base = (void *)&request->type,
         .iov_len = sizeof request->type}
    };

    size_t ii = 0;

    size_t bytes_not_read = 0;
    for (size_t jj = 0; jj < LINTED_ARRAY_SIZE(local_iov); ++jj) {
        bytes_not_read += local_iov[jj].iov_len;
    }

    ssize_t bytes;
    do {
        bytes = process_vm_readv(pid,
                                 local_iov + ii,
                                 LINTED_ARRAY_SIZE(local_iov) - ii,
                                 remote_iov + ii,
                                 LINTED_ARRAY_SIZE(remote_iov) - ii, 0);
        if (-1 == bytes) {
            return -1;
        }

        bytes_not_read -= bytes;
        ++ii;
    } while (bytes_not_read != 0);

    return 0;
}

int linted_manager_start_req_args(pid_t pid,
                                  struct linted_manager_req const *request,
                                  struct linted_manager_start_args *args)
{
    struct linted_manager_start_req *start_request = (void *)request;
    struct iovec local_iov[] = {
        {.iov_base = &args->service,
         .iov_len = sizeof args->service}
    };

    struct iovec remote_iov[] = {
        {.iov_base = (void *)&start_request->args.service,
         .iov_len = sizeof start_request->args.service}
    };

    size_t ii = 0;

    size_t bytes_not_read = 0;
    for (size_t jj = 0; jj < LINTED_ARRAY_SIZE(local_iov); ++jj) {
        bytes_not_read += local_iov[jj].iov_len;
    }

    ssize_t bytes;
    do {
        bytes = process_vm_readv(pid,
                                 local_iov + ii,
                                 LINTED_ARRAY_SIZE(local_iov) - ii,
                                 remote_iov + ii,
                                 LINTED_ARRAY_SIZE(remote_iov) - ii, 0);
        if (-1 == bytes) {
            return -1;
        }

        bytes_not_read -= bytes;
        ++ii;
    } while (bytes_not_read != 0);

    return 0;
}

int linted_manager_start_req_reply(pid_t pid,
                                   struct linted_manager_req *request,
                                   struct linted_manager_start_reply const
                                   *reply)
{
    struct linted_manager_start_req *start_request = (void *)request;
    struct iovec local_iov[] = {
        {.iov_base = (void *)&reply->is_up,
         .iov_len = sizeof reply->is_up}
    };
    struct iovec remote_iov[] = {
        {.iov_base = &start_request->reply.is_up,
         .iov_len = sizeof start_request->reply.is_up}
    };

    size_t ii = 0;

    size_t bytes_not_written = 0;
    for (size_t jj = 0; jj < LINTED_ARRAY_SIZE(local_iov); ++jj) {
        bytes_not_written += local_iov[jj].iov_len;
    }

    ssize_t bytes;
    do {
        bytes = process_vm_writev(pid,
                                  local_iov + ii,
                                  LINTED_ARRAY_SIZE(local_iov) - ii,
                                  remote_iov + ii,
                                  LINTED_ARRAY_SIZE(remote_iov) - ii, 0);
        if (-1 == bytes) {
            return -1;
        }

        bytes_not_written -= bytes;
        ++ii;
    } while (bytes_not_written != 0);

    return 0;
}

int linted_manager_finish_reply(pid_t pid, int errnum)
{
    union sigval value = {.sival_int = errnum };

    int queue_status;
    do {
        queue_status = sigqueue(pid, linted_manager_wait_signal(), value);
    } while (-1 == queue_status && EAGAIN == errno);
    return queue_status;
}

int linted_manager_send_request(pid_t pid, struct linted_manager_req *request)
{
    int exit_status = -1;

    sigset_t sigwait_set;
    sigemptyset(&sigwait_set);
    sigaddset(&sigwait_set, linted_manager_wait_signal());

    sigset_t sigold_set;
    pthread_sigmask(SIG_BLOCK, &sigwait_set, &sigold_set);

    union sigval value = {.sival_ptr = request };
    int queue_status;
    do {
        queue_status = sigqueue(pid, linted_manager_send_signal(), value);
    } while (-1 == queue_status && EAGAIN == errno);
    if (-1 == queue_status) {
        goto restore_sigmask;
    }

    siginfo_t info;
    int signal_number;
    do {
        signal_number = sigtimedwait(&sigwait_set, &info, NULL);
    } while (-1 == signal_number && EINTR == errno);
    if (-1 == signal_number) {
        assert(errno != EAGAIN);
        assert(errno != EINVAL);

        goto restore_sigmask;
    }

    int errnum = info.si_value.sival_int;
    if (errnum != 0) {
        errno = errnum;
        goto restore_sigmask;
    }

    exit_status = 0;

 restore_sigmask:
    pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);

    return exit_status;
}
