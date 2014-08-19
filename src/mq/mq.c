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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/error.h"
#include "linted/mq.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <mqueue.h>
#include <poll.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#define FILE_MAX 255U
#define RANDOM_BYTES 8U

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);


/**
 * Implemented using POSIX message queues.
 */
linted_error linted_mq_create(linted_mq *restrict mqp,
                              char const *restrict debugpath,
                              struct linted_mq_attr const *attr,
                              unsigned long flags)
{
    linted_error errnum;
    linted_mq ko;
    int unlink_status;

    if (flags != 0U) {
        return EINVAL;
    }

    if (debugpath[0U] != '/') {
        return EINVAL;
    }

    size_t path_size = strlen(debugpath + 1U);

    if (path_size > FILE_MAX - 1U - RANDOM_BYTES) {
        return ENAMETOOLONG;
    }

    size_t maxmsg = attr->maxmsg;
    size_t msgsize = attr->msgsize;

    {
        char random_mq_name[FILE_MAX + 1U];

        memcpy(1U + random_mq_name, 1U + debugpath, path_size);
        random_mq_name[0U] = '/';
        random_mq_name[1U + path_size] = '-';
        random_mq_name[1U + path_size + 1U + RANDOM_BYTES] = '\0';

        do {
            for (size_t ii = 0U; ii < RANDOM_BYTES; ++ii) {
                char random_char;
                for (;;) {
                    /* Normally using the modulus would give a bad
                     * distribution but CHAR_MAX + 1U is a power of two
                     */
                    random_char = linted_random_fast() % (CHAR_MAX + 1U);

                    /* Throw out results and retry for an even
                     * distribution
                     */
                    if ((random_char >= 'a' && random_char <= 'z')
                        || (random_char >= 'A' && random_char <= 'Z')
                        || (random_char >= '0' && random_char <= '9')) {
                        break;
                    }
                }

                random_mq_name[1U + path_size + 1U + ii] = random_char;
            }

            {
                struct mq_attr mq_attr;
                mq_attr.mq_flags = 0;
                mq_attr.mq_curmsgs = 0;
                mq_attr.mq_maxmsg = maxmsg;
                mq_attr.mq_msgsize = msgsize;

                ko = mq_open(random_mq_name,
                             O_RDWR | O_CREAT | O_EXCL | O_NONBLOCK, 0,
                             &mq_attr);
            }
            if (-1 == ko) {
                errnum = errno;
                LINTED_ASSUME(errnum != 0);
            } else {
                errnum = 0;
            }
        } while (EEXIST == errnum);
        if (errnum != 0) {
            goto exit_with_error;
        }

        unlink_status = mq_unlink(random_mq_name);
    }
    if (-1 == unlink_status) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        goto exit_with_error_and_close;
    }

    if (-1 == fchmod(ko, S_IRUSR | S_IWUSR)) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        goto exit_with_error_and_close;
    }

    *mqp = ko;

    return 0;

exit_with_error_and_close:
    mq_close(ko);

exit_with_error:
    return errnum;
}

void linted_mq_task_receive(struct linted_mq_task_receive *task,
                            unsigned task_action, linted_ko ko, char *buf,
                            size_t size)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_MQ_RECEIVE,
                       task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->bytes_read = 0U;
}

void linted_mq_task_send(struct linted_mq_task_send *task, unsigned task_action,
                         linted_ko ko, char const *buf, size_t size)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_MQ_SEND,
                       task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->bytes_wrote = 0U;
}

void linted_mq_do_receive(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *restrict task)
{
    struct linted_mq_task_receive *restrict task_receive
        = LINTED_DOWNCAST(struct linted_mq_task_receive, task);
    size_t bytes_read = 0U;
    linted_error errnum = 0;
    for (;;) {
        do {
            ssize_t result = mq_receive(task_receive->ko, task_receive->buf,
                                        task_receive->size, NULL);
            if (-1 == result) {
                errnum = errno;
                LINTED_ASSUME(errnum != 0);
                continue;
            }

            bytes_read = result;
        } while (EINTR == errnum);

        if (errnum != EAGAIN) {
            break;
        }

        short revents = 0;
        do {
            errnum = poll_one(task_receive->ko, POLLIN, &revents);
        } while (EINTR == errnum);
        if (errnum != 0) {
            break;
        }

        if ((errnum = check_for_poll_error(task_receive->ko, revents)) != 0) {
            break;
        }
    }

    task->errnum = errnum;
    task_receive->bytes_read = bytes_read;

    linted_asynch_pool_complete(pool, task);
}

void linted_mq_do_send(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *restrict task)
{
    struct linted_mq_task_send *restrict task_send
        = LINTED_DOWNCAST(struct linted_mq_task_send, task);
    size_t bytes_wrote = 0U;
    linted_error errnum = 0;
    for (;;) {
        do {
            if (-1
                == mq_send(task_send->ko, task_send->buf, task_send->size, 0)) {
                errnum = errno;
                LINTED_ASSUME(errnum != 0);
                continue;
            }

            bytes_wrote = task_send->size;
        } while (EINTR == errnum);

        if (errnum != EAGAIN) {
            break;
        }

        short revents = 0;
        do {
            errnum = poll_one(task_send->ko, POLLOUT, &revents);
        } while (EINTR == errnum);
        if (errnum != 0) {
            break;
        }

        if ((errnum = check_for_poll_error(task_send->ko, revents)) != 0) {
            break;
        }
    }

    task->errnum = errnum;
    task_send->bytes_wrote = bytes_wrote;

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
    }

    /* Not a socket, can't use getsockopt to get errors */
    assert (0 == (revents & POLLERR));

    return errnum;
}
