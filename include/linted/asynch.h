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

#include <stddef.h>
#include <poll.h>
#include <pthread.h>

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asynchronously.

 * @todo Make the array queue a synchronous (not buffering at all
 *       queue).
 *
 * @todo Dynamically resize the worker pool based upon if commands are
 *       successfully transferred or not (the worker pool will need a
 *       set and a lock).
 */

struct linted_linked_queue;
struct linted_asynch_worker_pool;

struct linted_asynch_pool
{
    struct linted_asynch_worker_pool* worker_pool;

    /**
     * A one writer to many readers queue.
     */
    struct linted_array_queue* command_queue;

    /**
     * A one reader to many writers queue. Should be able to retrieve
     * many values at once. As all writes are a direct result of
     * submitted commands there is no need to worry about it growing
     * too large.
     */
    struct linted_linked_queue* event_queue;
};

enum {
    LINTED_ASYNCH_EVENT_POLL,
    LINTED_ASYNCH_EVENT_READ,
    LINTED_ASYNCH_EVENT_WRITE
};

struct linted_asynch_event_typical
{
    unsigned type;
    unsigned task_id;
    int errnum;
};

struct linted_asynch_event_poll
{
    unsigned type;
    unsigned task_id;
    int errnum;
};

struct linted_asynch_event_read
{
    unsigned type;
    unsigned task_id;
    int errnum;
    size_t bytes_read;
};

struct linted_asynch_event_write
{
    unsigned type;
    unsigned task_id;
    int errnum;
    size_t bytes_wrote;
};

union linted_asynch_event
{
    struct linted_asynch_event_typical typical;
    struct linted_asynch_event_poll poll;
    struct linted_asynch_event_read read;
    struct linted_asynch_event_write write;
};

enum {
    LINTED_ASYNCH_TASK_POLL,
    LINTED_ASYNCH_TASK_READ,
    LINTED_ASYNCH_TASK_MQ_RECEIVE,
    LINTED_ASYNCH_TASK_MQ_SEND
};

struct linted_linked_queue_node;

struct linted_asynch_task_typical
{
    struct linted_linked_queue_node* reply_node;
    unsigned type;
    unsigned task_id;
};

struct linted_asynch_task_poll
{
    struct linted_linked_queue_node* reply_node;
    unsigned type;
    int task_id;
    struct pollfd* fds;
    size_t size;
};

struct linted_asynch_task_read
{
    struct linted_linked_queue_node* reply_node;
    unsigned type;
    int task_id;
    linted_ko ko;
    char* buf;
    size_t size;
};

struct linted_asynch_task_mq_receive
{
    struct linted_linked_queue_node* reply_node;
    unsigned type;
    int task_id;
    linted_ko ko;
    char* buf;
    size_t size;
};

struct linted_asynch_task_mq_send
{
    struct linted_linked_queue_node* reply_node;
    unsigned type;
    int task_id;
    linted_ko ko;
    char const* buf;
    size_t size;
};

union linted_asynch_task
{
    struct linted_asynch_task_typical typical;
    struct linted_asynch_task_poll poll;
    struct linted_asynch_task_read read;
    struct linted_asynch_task_mq_receive mq_receive;
    struct linted_asynch_task_mq_send mq_send;
};

linted_error linted_asynch_pool_create(struct linted_asynch_pool* pool);
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool* pool);

linted_error linted_asynch_pool_submit(struct linted_asynch_pool* pool,
                                       union linted_asynch_task* task);

linted_error linted_asynch_pool_wait(struct linted_asynch_pool* pool,
                                     union linted_asynch_event* events,
                                     size_t size, size_t* event_count);

#endif /* LINTED_ASYNCH_H */
