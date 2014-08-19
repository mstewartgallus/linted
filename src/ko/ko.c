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
#define _GNU_SOURCE

#include "config.h"

#include "linted/ko.h"

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);

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

linted_error linted_ko_dummy(linted_ko *restrict kop)
{
    return linted_ko_open(kop, -1, "/dev/null", LINTED_KO_RDONLY);
}

linted_error linted_ko_open(linted_ko *restrict kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
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

linted_error linted_ko_reopen(linted_ko *restrict kooutp, linted_ko koin,
                              unsigned long flags)
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

void linted_ko_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *restrict task)
{
    struct linted_ko_task_poll *restrict task_poll
        = LINTED_DOWNCAST(struct linted_ko_task_poll, task);
    linted_error errnum;

    short revents = 0;

    do {
        errnum = poll_one(task_poll->ko, task_poll->events, &revents);
    } while (EINTR == errnum);

    task_poll->revents = revents;
    task->errnum = errnum;

    linted_asynch_pool_complete(pool, task);
}

void linted_ko_do_read(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *restrict task)
{
    struct linted_ko_task_read *restrict task_read
        = LINTED_DOWNCAST(struct linted_ko_task_read, task);
    size_t bytes_read = task_read->current_position;
    size_t bytes_left = task_read->size - bytes_read;

    linted_error errnum = 0;
    for (;;) {
        for (;;) {
            ssize_t result
                = read(task_read->ko, task_read->buf + bytes_read, bytes_left);
            if (-1 == result) {
                errnum = errno;
                LINTED_ASSUME(errnum != 0);

                if (EINTR == errnum) {
                    continue;
                }

                break;
            }

            size_t bytes_read_delta = result;
            if (0U == bytes_read_delta) {
                break;
            }

            bytes_read += bytes_read_delta;
            bytes_left -= bytes_read_delta;
            if (0U == bytes_left) {
                break;
            }
        }

        if (errnum != EAGAIN && errnum != EWOULDBLOCK) {
            break;
        }

        short revents = 0;
        do {
            errnum = poll_one(task_read->ko, POLLIN, &revents);
        } while (EINTR == errnum);
        if (errnum != 0) {
            break;
        }

        if ((errnum = check_for_poll_error(task_read->ko, revents)) != 0) {
            break;
        }
    }

    task->errnum = errnum;
    task_read->bytes_read = bytes_read;
    task_read->current_position = 0U;

    linted_asynch_pool_complete(pool, task);
}

void linted_ko_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *restrict task)
{
    struct linted_ko_task_write *restrict task_write
        = LINTED_DOWNCAST(struct linted_ko_task_write, task);
    size_t bytes_wrote = task_write->current_position;
    size_t bytes_left = task_write->size - bytes_wrote;

    linted_error errnum = 0;

    sigset_t oldset;
    /* Get EPIPEs */
    /* SIGPIPE may not be blocked already */
    /* Reuse oldset to save on stack space */
    sigemptyset(&oldset);
    sigaddset(&oldset, SIGPIPE);

    pthread_sigmask(SIG_BLOCK, &oldset, &oldset);

    for (;;) {
        for (;;) {
            ssize_t result = write(task_write->ko,
                                   task_write->buf + bytes_wrote, bytes_left);
            if (-1 == result) {
                errnum = errno;
                LINTED_ASSUME(errnum != 0);

                if (EINTR == errnum) {
                    continue;
                }

                break;
            }

            size_t bytes_wrote_delta = result;

            bytes_wrote += bytes_wrote_delta;
            bytes_left -= bytes_wrote_delta;
            if (0U == bytes_left) {
                break;
            }
        }

        if (errnum != EAGAIN && errnum != EWOULDBLOCK) {
            break;
        }

        short revents = 0;
        do {
            errnum = poll_one(task_write->ko, POLLOUT, &revents);
        } while (EINTR == errnum);
        if (errnum != 0) {
            break;
        }

        if ((errnum = check_for_poll_error(task_write->ko, revents)) != 0) {
            break;
        }
    }

    /* Consume SIGPIPEs */
    {
        sigset_t sigpipeset;

        sigemptyset(&sigpipeset);
        sigaddset(&sigpipeset, SIGPIPE);

        {
            struct timespec timeout = { 0 };

            sigtimedwait(&sigpipeset, NULL, &timeout);
        }
    }

    pthread_sigmask(SIG_SETMASK, &oldset, NULL);

    task->errnum = errnum;
    task_write->bytes_wrote = bytes_wrote;
    task_write->current_position = 0U;

    linted_asynch_pool_complete(pool, task);
}

void linted_ko_do_accept(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *restrict task)
{
    struct linted_ko_task_accept *restrict task_accept
        = LINTED_DOWNCAST(struct linted_ko_task_accept, task);

    linted_ko new_ko = -1;
    linted_error errnum;

    for (;;) {
    retry_accept:

        new_ko
            = accept4(task_accept->ko, NULL, 0, SOCK_NONBLOCK | SOCK_CLOEXEC);
        if (-1 == new_ko) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
        } else {
            errnum = 0;
        }

        /**
         * @bug On BSD accept returns the same file description as
         * passed into connect so this file descriptor shares NONBLOCK
         * status with the connector. I'm not sure of a way to sever
         * shared file descriptions on BSD.
         */

        /* Retry on network error */
        switch (errnum) {
        case EINTR:
        case ENETDOWN:
        case EPROTO:
        case ENOPROTOOPT:
        case EHOSTDOWN:
        case ENONET:
        case EHOSTUNREACH:
        case EOPNOTSUPP:
        case ENETUNREACH:
            goto retry_accept;
        }

        if (errnum != EAGAIN && errnum != EWOULDBLOCK) {
            break;
        }

        short revents = 0;
        do {
            errnum = poll_one(task_accept->ko, POLLIN, &revents);
        } while (EINTR == errnum);
        if (errnum != 0) {
            break;
        }

        if ((errnum = check_for_poll_error(task_accept->ko, revents)) != 0) {
            break;
        }
    }

    task->errnum = errnum;
    task_accept->returned_ko = new_ko;

    linted_asynch_pool_complete(pool, task);
}

static linted_error poll_one(linted_ko ko, short events, short *revents)
{
    struct pollfd pollfd = { .fd = ko, .events = events };
    if (-1 == poll(&pollfd, 1U, -1)) {
        linted_error errnum = errno;
        LINTED_ASSUME(errnum != 0);
        return errnum;
    }

    *revents = pollfd.revents;
    return 0;
}

static linted_error check_for_poll_error(linted_ko ko, short revents)
{
    linted_error errnum = 0;

    if ((revents & POLLNVAL) != 0) {
        errnum = EBADF;
    } else if ((revents & POLLHUP) != 0) {
        errnum = EPIPE;
    } else if ((revents & POLLERR) != 0) {
        linted_error xx = errnum;
        socklen_t optlen = sizeof xx;
        if (-1 == getsockopt(ko, SOL_SOCKET, SO_ERROR, &xx, &optlen)) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
        } else {
            errnum = xx;
        }
    }

    return errnum;
}
