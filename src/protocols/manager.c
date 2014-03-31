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
#include <signal.h>
#include <sys/uio.h>

int linted_manager_send_signal(void)
{
    return SIGRTMIN;
}

int linted_manager_wait_signal(void)
{
    return SIGRTMIN + 1;
}

int linted_manager_receive_message(siginfo_t * info,
                                   struct linted_manager_message * message)
{
    struct iovec local_iov[] = {
        { .iov_base = message, .iov_len = sizeof *message }
    };
    struct iovec remote_iov[] = {
        {
            .iov_base = info->si_ptr,
            .iov_len = sizeof *message
        }
    };

    pid_t pid = info->si_pid;

    int read_status = process_vm_readv(pid,
                                       local_iov, LINTED_ARRAY_SIZE(local_iov),
                                       remote_iov, LINTED_ARRAY_SIZE(remote_iov),
                                       0);

    union sigval value = {
        .sival_int = -1 == read_status ? errno : 0
    };

    return sigqueue(pid, linted_manager_wait_signal(), value);
}

int linted_manager_send_message(pid_t pid,
                                struct linted_manager_message const * message)
{
    int exit_status = -1;

    sigset_t sigwait_set;
    sigemptyset(&sigwait_set);
    sigaddset(&sigwait_set, linted_manager_wait_signal());

    sigset_t sigold_set;
    pthread_sigmask(SIG_BLOCK, &sigwait_set, &sigold_set);

    union sigval value = {.sival_ptr = (void *)message};
    if (-1 == sigqueue(pid, linted_manager_send_signal(), value)) {
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
        goto restore_sigmask;
    }

    exit_status = 0;

 restore_sigmask:
    pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);

    return exit_status;
}
