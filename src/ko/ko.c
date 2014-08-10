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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/ko.h"

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop)
{
    size_t length = strlen(str);
    unsigned position = 1U;

    if ('0' == str[0U] && length != 1U) {
        return EINVAL;
    }

    unsigned total = 0U;
    for (; length > 0U; --length) {
        char const digit = str[length - 1U];

        if ('0' <= digit && digit <= '9') {
            unsigned long sum = total + ((unsigned)(digit - '0')) * position;
            if (sum > INT_MAX) {
                return ERANGE;
            }

            total = sum;
        } else {
            return EINVAL;
        }

        unsigned long next_position = 10U * position;
        if (next_position > INT_MAX) {
            return ERANGE;
        }
        position = next_position;
    }

    *kop = total;
    return 0;
}

linted_error linted_ko_dummy(linted_ko * restrict kop)
{
    return linted_ko_open(kop, -1, "/dev/null", LINTED_KO_RDONLY);
}

linted_error linted_ko_open(linted_ko * restrict kop, linted_ko dirko,
                            char const *pathname, int flags)
{
    linted_error errnum;

    if ((flags & ~LINTED_KO_RDONLY & ~LINTED_KO_WRONLY & ~LINTED_KO_RDWR
         & ~LINTED_KO_SYNC) != 0U) {
        return EINVAL;
    }

    bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0U;
    bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0U;
    bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0U;

    bool ko_sync = (flags & LINTED_KO_SYNC) != 0U;

    if (ko_rdonly && ko_wronly) {
        return EINVAL;
    }

    if (ko_rdwr && ko_rdonly) {
        return EINVAL;
    }

    if (ko_rdwr && ko_wronly) {
        return EINVAL;
    }

    /*
     * Always, be safe for execs and use O_NONBLOCK because asynch
     * functions handle that anyways and open may block otherwise.
     */
    int oflags = O_CLOEXEC | O_NONBLOCK | O_NOCTTY;

    if (ko_rdonly) {
        oflags |= O_RDONLY;
    }

    if (ko_wronly) {
        oflags |= O_WRONLY;
    }

    if (ko_rdwr) {
        oflags |= O_RDWR;
    }

    if (ko_sync) {
        oflags |= O_SYNC;
    }

    int fildes;
    do {
        fildes = openat(dirko, pathname, oflags);
        if (-1 == fildes) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EINTR == errnum);
    if (errnum != 0) {
        return errnum;
    }

    *kop = fildes;

    return 0;
}

linted_error linted_ko_reopen(linted_ko * restrict kooutp,
                              linted_ko koin, int flags)
{
    char pathname[sizeof "/proc/self/fd/" + 10U];
    sprintf(pathname, "/proc/self/fd/%i", koin);
    return linted_ko_open(kooutp, -1, pathname, flags);
}

linted_error linted_ko_close(linted_ko ko)
{
    linted_error errnum;
    /*
     * The state of a file descriptor after close gives an EINTR error
     * is unspecified by POSIX so this function avoids the problem by
     * simply blocking all signals.
     */

    sigset_t sigset;

    /* First use the signal set for the full set */
    sigfillset(&sigset);

    pthread_sigmask(SIG_BLOCK, &sigset, &sigset);

    /* Then reuse the signal set for the old set */

    if (-1 == close(ko)) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
    } else {
        errnum = 0;
    }

    pthread_sigmask(SIG_SETMASK, &sigset, NULL);

    return errnum;
}

void linted_ko_task_poll(struct linted_ko_task_poll *task, unsigned task_action,
                         linted_ko ko, short events)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_POLL,
                       task_action);

    task->ko = ko;
    task->events = events;
}

void linted_ko_task_read(struct linted_ko_task_read *task, unsigned task_action,
                         linted_ko ko, char *buf, size_t size)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_READ,
                       task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->current_position = 0U;
    task->bytes_read = 0U;
}

void linted_ko_task_write(struct linted_ko_task_write *task,
                          unsigned task_action, linted_ko ko, char const *buf,
                          size_t size)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_WRITE,
                       task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->current_position = 0U;
    task->bytes_wrote = 0U;
}

void linted_ko_task_accept(struct linted_ko_task_accept *task,
                           unsigned task_action, linted_ko ko)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_ACCEPT,
                       task_action);

    task->ko = ko;
}
