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

#include "linted/asynch.h"

#include "linted/linked_queue.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <mqueue.h>
#include <poll.h>
#include <sched.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

struct linted_asynch_pool
{
    /**
     * A one writer to many readers queue.
     */
    struct linted_linked_queue command_queue;

    /**
     * A one reader to many writers queue. Should be able to retrieve
     * many values at once. As all writes are a direct result of
     * submitted commands there is no need to worry about it growing
     * too large.
     */
    struct linted_linked_queue event_queue;

    size_t worker_count;
    pthread_t workers[];
};

static void* worker_routine(void* arg);

int linted_asynch_pool_create(struct linted_asynch_pool** poolp,
                              unsigned max_tasks)
{
    int errnum;
    size_t created_threads = 0;
    struct linted_asynch_pool* pool;

    size_t workers_size = max_tasks * sizeof pool->workers[0];
    pool = malloc(sizeof *pool + workers_size);
    if (NULL == pool) {
        return errno;
    }
    if ((errnum = linted_linked_queue_create(&pool->command_queue)) != 0) {
        goto free_pool;
    }

    if ((errnum = linted_linked_queue_create(&pool->event_queue)) != 0) {
        goto destroy_command_queue;
    }

    pool->worker_count = max_tasks;

    for (; created_threads < max_tasks; ++created_threads) {
        if ((errnum = pthread_create(&pool->workers[created_threads], NULL,
                                     worker_routine, pool)) != 0) {
            goto destroy_threads;
        }
    }

    *poolp = pool;

    return 0;

destroy_threads:
    for (size_t ii = 0; ii < created_threads; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0; ii < created_threads; ++ii) {
        pthread_join(pool->workers[ii], NULL);
    }

    linted_linked_queue_destroy(&pool->event_queue);

destroy_command_queue:
    linted_linked_queue_destroy(&pool->command_queue);

free_pool:
    free(pool);

    return errnum;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool* pool)
{
    linted_error errnum = 0;

    size_t worker_count = pool->worker_count;

    for (size_t ii = 0; ii < worker_count; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0; ii < worker_count; ++ii) {
        pthread_join(pool->workers[ii], NULL);
    }

    linted_linked_queue_destroy(&pool->command_queue);
    linted_linked_queue_destroy(&pool->event_queue);

    free(pool);

    return errnum;
}

void linted_asynch_pool_submit(struct linted_asynch_pool* pool,
                               struct linted_asynch_task* task)
{
    linted_linked_queue_send(&pool->command_queue, LINTED_UPCAST(task));
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool* pool,
                                     struct linted_asynch_task
                                     ** completed_tasks,
                                     size_t size, size_t* task_countp)
{
    linted_error errnum;
    size_t task_count = 0;

    if (0 == size) {
        return EINVAL;
    }

    /* Wait for one event */
    {
        struct linted_linked_queue_node* node;
        linted_linked_queue_recv(&pool->event_queue, &node);

        /* The node is the first member of the task */
        completed_tasks[task_count]
            = LINTED_DOWNCAST(struct linted_asynch_task, node);
        ++task_count;
    }

    /* Then poll for more */
    for (; task_count < size; ++task_count) {
        struct linted_linked_queue_node* node;
        errnum = linted_linked_queue_try_recv(&pool->event_queue, &node);
        if (EAGAIN == errnum) {
            break;
        }

        completed_tasks[task_count]
            = LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    *task_countp = task_count;

    return 0;
}

linted_error linted_asynch_pool_poll(struct linted_asynch_pool* pool,
                                     struct linted_asynch_task
                                     ** completed_tasks,
                                     size_t size, size_t* task_countp)
{
    linted_error errnum;
    size_t task_count = 0;

    if (0 == size) {
        return EINVAL;
    }

    for (; task_count < size; ++task_count) {
        struct linted_linked_queue_node* node;
        errnum = linted_linked_queue_try_recv(&pool->event_queue, &node);
        if (EAGAIN == errnum) {
            break;
        }

        /* The node is the first member of the task */
        completed_tasks[task_count]
            = LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    *task_countp = task_count;

    if (0 == task_count) {
        return EAGAIN;
    }

    return 0;
}

static void* worker_routine(void* arg)
{
    /*
     * Set the thread to be asynchronously killable at any time.
     */
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

    struct linted_asynch_pool* pool = arg;

    for (;;) {
        struct linted_asynch_task* task;
        {
            struct linted_linked_queue_node* node;
            linted_linked_queue_recv(&pool->command_queue, &node);
            task = LINTED_DOWNCAST(struct linted_asynch_task, node);
        }

        switch (task->type) {
        case LINTED_ASYNCH_TASK_POLL: {
            struct linted_asynch_task_poll* task_poll
                = LINTED_DOWNCAST(struct linted_asynch_task_poll, task);
            linted_error errnum;

            struct pollfd fd = {.fd = task_poll->ko,
                                .events = task_poll->events};
            do {
                int poll_status = poll(&fd, 1, -1);
                errnum = -1 == poll_status ? errno : 0;
            } while (EINTR == errnum);

            task_poll->revents = fd.revents;
            task->errnum = errnum;
            break;
        }

        case LINTED_ASYNCH_TASK_READ: {
            struct linted_asynch_task_read* task_read
                = LINTED_DOWNCAST(struct linted_asynch_task_read, task);
            size_t bytes_read = 0;
            size_t bytes_left = task_read->size;
            linted_error errnum;
            do {
                {
                    struct pollfd fd
                        = { .fd = task_read->ko, .events = POLLIN };

                    int poll_status = poll(&fd, 1, -1);
                    errnum = -1 == poll_status ? errno : 0;
                    if (errnum != 0) {
                        continue;
                    }
                }

                for (;;) {
                    ssize_t result = read(
                        task_read->ko, task_read->buf + bytes_read, bytes_left);
                    if (-1 == result) {
                        errnum = errno;
                        break;
                    }

                    size_t bytes_read_delta = result;
                    if (0 == bytes_read_delta) {
                        break;
                    }

                    bytes_read += bytes_read_delta;
                    bytes_left -= bytes_read_delta;
                    if (0 == bytes_left) {
                        break;
                    }
                }
            } while (EAGAIN == errnum || EINTR == errnum);

            task->errnum = errnum;
            task_read->bytes_read = bytes_read;
            break;
        }

        case LINTED_ASYNCH_TASK_MQ_RECEIVE: {
            struct linted_asynch_task_mq_receive* task_receive
                = LINTED_DOWNCAST(struct linted_asynch_task_mq_receive, task);
            size_t bytes_read = 0;
            linted_error errnum;
            do {
                {
                    struct pollfd fd
                        = { .fd = task_receive->ko, .events = POLLIN };

                    int poll_status = poll(&fd, 1, -1);
                    errnum = -1 == poll_status ? errno : 0;
                    if (errnum != 0) {
                        continue;
                    }
                }

                ssize_t result = mq_receive(task_receive->ko, task_receive->buf,
                                            task_receive->size, NULL);
                if (-1 == result) {
                    errnum = errno;
                    continue;
                }

                bytes_read = result;
            } while (EAGAIN == errnum || EINTR == errnum);

            task->errnum = errnum;
            task_receive->bytes_read = bytes_read;
            break;
        }

        case LINTED_ASYNCH_TASK_MQ_SEND: {
            struct linted_asynch_task_mq_send* task_send
                = LINTED_DOWNCAST(struct linted_asynch_task_mq_send, task);
            size_t bytes_wrote = 0;
            linted_error errnum;
            do {
                {
                    struct pollfd fd
                        = { .fd = task_send->ko, .events = POLLOUT };

                    int poll_status = poll(&fd, 1, -1);
                    errnum = -1 == poll_status ? errno : 0;
                    if (errnum != 0) {
                        continue;
                    }
                }

                if (-1 == mq_send(task_send->ko, task_send->buf,
                                  task_send->size, 0)) {
                    errnum = errno;
                    continue;
                }

                bytes_wrote = task_send->size;
            } while (EAGAIN == errnum || EINTR == errnum);

            task->errnum = errnum;
            task_send->bytes_wrote = bytes_wrote;
            break;
        }

        case LINTED_ASYNCH_TASK_WAITID: {
            struct linted_asynch_task_waitid* task_wait
                = LINTED_DOWNCAST(struct linted_asynch_task_waitid, task);
            linted_error errnum;
            do {
                int wait_status = waitid(task_wait->idtype, task_wait->id,
                                         &task_wait->info,
                                         task_wait->options);
                errnum = -1 == wait_status ? errno : 0;
            } while (EINTR == errnum);

            task->errnum = errnum;
            break;
        }

        default:
            assert(false);
        }

        linted_linked_queue_send(&pool->event_queue, LINTED_UPCAST(task));
    }
    return NULL;
}
