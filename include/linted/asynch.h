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
#include "linted/ko.h"
#include "linted/linked_queue.h"

#include <stddef.h>
#include <poll.h>
#include <pthread.h>

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asynchronously.
 *
 * @todo Make the allocated notes part of the task structures.
 *
 * @todo Make the array queue a synchronous (not buffering at all
 *       queue).
 *
 * @todo Dynamically resize the worker pool based upon if commands are
 *       successfully transferred or not (the worker pool will need a
 *       set and a lock).
 */

struct linted_asynch_worker_pool;

struct linted_asynch_pool
{
    struct linted_asynch_worker_pool* worker_pool;

    /**
     * A one writer to many readers queue.
     */
    struct linted_linked_queue* command_queue;

    /**
     * A one reader to many writers queue. Should be able to retrieve
     * many values at once. As all writes are a direct result of
     * submitted commands there is no need to worry about it growing
     * too large.
     */
    struct linted_linked_queue* event_queue;
};

enum {
    LINTED_ASYNCH_TASK_POLL,
    LINTED_ASYNCH_TASK_READ,
    LINTED_ASYNCH_TASK_MQ_RECEIVE,
    LINTED_ASYNCH_TASK_MQ_SEND
};

struct linted_asynch_task
{
    struct linted_linked_queue_node parent;
    linted_error errnum;
    unsigned type;
    unsigned task_action;
};

struct linted_asynch_task_poll
{
    struct linted_asynch_task parent;
    struct pollfd* fds;
    size_t size;
};

struct linted_asynch_task_read
{
    struct linted_asynch_task parent;
    char* buf;
    size_t size;
    size_t bytes_read;
    linted_ko ko;
};

struct linted_asynch_task_mq_receive
{
    struct linted_asynch_task parent;
    char* buf;
    size_t size;
    size_t bytes_read;
    linted_ko ko;
};

struct linted_asynch_task_mq_send
{
    struct linted_asynch_task parent;
    char const* buf;
    size_t size;
    size_t bytes_wrote;
    linted_ko ko;
};

linted_error linted_asynch_pool_create(struct linted_asynch_pool* pool,
                                       unsigned max_tasks);
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool* pool);

void linted_asynch_pool_submit(struct linted_asynch_pool* pool,
                               struct linted_asynch_task* task);

linted_error linted_asynch_pool_wait(struct linted_asynch_pool* pool,
                                     struct linted_asynch_task
                                     ** completed_tasks,
                                     size_t size, size_t* task_countp);

linted_error linted_asynch_pool_poll(struct linted_asynch_pool* pool,
                                     struct linted_asynch_task
                                     ** completed_tasks,
                                     size_t size, size_t* task_countp);

#endif /* LINTED_ASYNCH_H */
