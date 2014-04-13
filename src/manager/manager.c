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

errno_t linted_manager_req_type(pid_t pid,
                                struct linted_manager_req const *request,
                                unsigned *type)
{
    struct iovec local_iov[] = {
        {.iov_base = type,
         .iov_len = sizeof *type}
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
            return errno;
        }

        bytes_not_read -= bytes;
        ++ii;
    } while (bytes_not_read != 0);

    return 0;
}

errno_t linted_manager_start_req_args(pid_t pid,
                                      struct linted_manager_req const *request,
                                      struct linted_manager_start_args * args)
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
            return errno;
        }

        bytes_not_read -= bytes;
        ++ii;
    } while (bytes_not_read != 0);

    return 0;
}

errno_t linted_manager_start_req_reply(pid_t pid,
                                       struct linted_manager_req * request,
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
            return errno;
        }

        bytes_not_written -= bytes;
        ++ii;
    } while (bytes_not_written != 0);

    return 0;
}

errno_t linted_manager_finish_reply(pid_t pid, int errnum)
{
    union sigval value = {.sival_int = errnum };

    int queue_status;
    do {
        queue_status = sigqueue(pid, linted_manager_wait_signal(), value);
    } while (-1 == queue_status && EAGAIN == errno);
    return -1 == queue_status ? errno : 0;
}

errno_t linted_manager_send_request(pid_t pid,
                                    struct linted_manager_req * request)
{
    int error_status = 0;

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
        error_status = errno;
        goto restore_sigmask;
    }

    siginfo_t info;
    errno_t errnum;
    do {
        int signal_number = sigtimedwait(&sigwait_set, &info, NULL);
        errnum = -1 == signal_number ? errno : 0;
    } while (EINTR == errnum);
    if (errnum != 0) {
        assert(errnum != EAGAIN);
        assert(errnum != EINVAL);

        error_status = errnum;
        goto restore_sigmask;
    }

    error_status = info.si_value.sival_int;

 restore_sigmask:
    pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);

    return error_status;
}
