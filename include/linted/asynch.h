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
#ifndef LINTED_ASYNCH_H
#define LINTED_ASYNCH_H

#include "linted/error.h"
#include "linted/queue.h"
#include "linted/ko.h"

#include <stddef.h>
#include <sys/types.h>

#if _POSIX_C_SOURCE >= 199309L
#include <time.h>
#endif

#if _POSIX_C_SOURCE >= 200809L
#include <sys/wait.h>
#endif

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asynchronously.
 *
 * @todo Make the array queue a synchronous (not buffering at all
 *       queue).
 *
 * @todo Dynamically resize the worker pool based upon if commands are
 *       successfully transferred or not (the worker pool will need a
 *       set and a lock).
 */

struct linted_asynch_pool;

enum {
    LINTED_ASYNCH_TASK_POLL,
    LINTED_ASYNCH_TASK_READ,
    LINTED_ASYNCH_TASK_WRITE,
    LINTED_ASYNCH_TASK_MQ_RECEIVE,
    LINTED_ASYNCH_TASK_MQ_SEND,
    LINTED_ASYNCH_TASK_WAITID,
    LINTED_ASYNCH_TASK_ACCEPT,
    LINTED_ASYNCH_TASK_SLEEP_UNTIL
};

struct linted_asynch_task
{
    struct linted_queue_node parent;
    linted_error errnum;
    unsigned type;
    unsigned task_action;
};

struct linted_asynch_task_poll
{
    struct linted_asynch_task parent;
    linted_ko ko;
    short events;
    short revents;
};

struct linted_asynch_task_read
{
    struct linted_asynch_task parent;
    char *buf;
    size_t size;
    size_t current_position;
    size_t bytes_read;
    linted_ko ko;
};

struct linted_asynch_task_write
{
    struct linted_asynch_task parent;
    char const *buf;
    size_t size;
    size_t current_position;
    size_t bytes_wrote;
    linted_ko ko;
};

struct linted_asynch_task_mq_receive
{
    struct linted_asynch_task parent;
    char *buf;
    size_t size;
    size_t bytes_read;
    linted_ko ko;
};

struct linted_asynch_task_mq_send
{
    struct linted_asynch_task parent;
    char const *buf;
    size_t size;
    size_t bytes_wrote;
    linted_ko ko;
};

#if _POSIX_C_SOURCE >= 200809L
struct linted_asynch_task_waitid
{
    struct linted_asynch_task parent;
    siginfo_t info;
    idtype_t idtype;
    id_t id;
    int options;
};
#endif

struct linted_asynch_task_accept
{
    struct linted_asynch_task parent;
    linted_ko ko;
    linted_ko returned_ko;
};

#if _POSIX_C_SOURCE >= 199309L
struct linted_asynch_task_sleep_until
{
    struct linted_asynch_task parent;
    int flags;
    struct timespec request;
};
#endif

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks);
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool);

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task);

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completions,
                                     size_t size, size_t *task_countp);

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completions,
                                     size_t size, size_t *task_countp);

void linted_asynch_poll(struct linted_asynch_task_poll *task, unsigned task_action,
                        linted_ko ko, short events);

void linted_asynch_read(struct linted_asynch_task_read *task, unsigned task_action,
                        linted_ko ko, char *buf, size_t size);

void linted_asynch_write(struct linted_asynch_task_write *task, unsigned task_action,
                         linted_ko ko, char const *buf, size_t size);

void linted_asynch_mq_receive(struct linted_asynch_task_mq_receive *task,
                              unsigned task_action, linted_ko ko, char *buf,
                              size_t size);

void linted_asynch_mq_send(struct linted_asynch_task_mq_send *task,
                           unsigned task_action, linted_ko ko, char const *buf,
                           size_t size);

#if _POSIX_C_SOURCE >= 200809L
void linted_asynch_waitid(struct linted_asynch_task_waitid *task,
                          unsigned task_action, idtype_t idtype, id_t id,
                          int options);
#endif

void linted_asynch_accept(struct linted_asynch_task_accept *task,
                          unsigned task_action, linted_ko ko);

#if _POSIX_C_SOURCE >= 199309L
void linted_asynch_sleep_until(struct linted_asynch_task_sleep_until *task,
                               unsigned task_action, int flags,
                               struct timespec const *request);
#endif

#endif /* LINTED_ASYNCH_H */
