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
#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/asynch.h"

#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <process.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <string.h>
#include <windows.h>

struct linted_asynch_pool
{
    /**
     * A one writer to many readers queue.
     */
    struct linted_queue *worker_command_queue;

    /**
     * A one reader to many writers queue. Should be able to retrieve
     * many values at once. As all writes are a direct result of
     * submitted commands there is no need to worry about it growing
     * too large.
     */
    struct linted_queue *event_queue;

    size_t worker_count;

    linted_ko workers[];
};

static noreturn DWORD WINAPI worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task);

static void run_task_read(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);
static void run_task_write(struct linted_asynch_pool *pool,
                           struct linted_asynch_task *task);

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks)
{
    linted_error errnum;
    size_t worker_count = 0;
    struct linted_asynch_pool *pool;

    size_t workers_size = max_tasks * sizeof pool->workers[0u];
    {
        void *xx;
        if ((errnum = linted_mem_alloc(&xx, sizeof *pool + workers_size))
            != 0) {
            return errnum;
        }
        pool = xx;
    }

    if ((errnum = linted_queue_create(&pool->worker_command_queue)) != 0) {
        goto free_pool;
    }

    if ((errnum = linted_queue_create(&pool->event_queue)) != 0) {
        goto destroy_worker_command_queue;
    }

    pool->worker_count = max_tasks;

    for (; worker_count < max_tasks; ++worker_count) {
        linted_ko thread = CreateThread(NULL, 0, worker_routine, pool, 0, NULL);
        if (NULL == thread) {
            DWORD error = GetLastError();
            errnum = HRESULT_FROM_WIN32(error);
            goto destroy_threads;
        }
    }

    *poolp = pool;

    return 0;

destroy_threads:
    for (size_t ii = 0u; ii < worker_count; ++ii) {
        TerminateThread(pool->workers[ii], 0);
    }

    for (size_t ii = 0u; ii < worker_count; ++ii) {
        CloseHandle(pool->workers[ii]);
    }

    linted_queue_destroy(pool->event_queue);

destroy_worker_command_queue:
    linted_queue_destroy(pool->worker_command_queue);

free_pool:
    linted_mem_free(pool);

    return errnum;
}

/**
 * @bug The queue isn't safe to terminate. Use an OS queue or list
 *      that isn't corruptible.
 */
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
    linted_error errnum = 0;

    size_t worker_count = pool->worker_count;

    for (size_t ii = 0u; ii < worker_count; ++ii) {
        TerminateThread(pool->workers[ii], 0);
    }

    for (size_t ii = 0u; ii < worker_count; ++ii) {
        CloseHandle(pool->workers[ii]);
    }

    linted_queue_destroy(pool->worker_command_queue);
    linted_queue_destroy(pool->event_queue);

    linted_mem_free(pool);

    return errnum;
}

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task)
{
    if (NULL == pool) {
        run_task(NULL, task);
    } else {
        linted_queue_send(pool->worker_command_queue, LINTED_UPCAST(task));
    }
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completions,
                                     size_t size, size_t *task_countp)
{
    linted_error errnum;
    size_t task_count = 0u;

    if (0u == size) {
        return EINVAL;
    }

    /* Wait for one event */
    {
        struct linted_queue_node *node;
        linted_queue_recv(pool->event_queue, &node);

        /* The node is the first member of the task */
        completions[task_count]
            = LINTED_DOWNCAST(struct linted_asynch_task, node);
        ++task_count;
    }

    /* Then poll for more */
    for (; task_count < size; ++task_count) {
        struct linted_queue_node *node;
        errnum = linted_queue_try_recv(pool->event_queue, &node);
        if (EAGAIN == errnum) {
            break;
        }

        completions[task_count]
            = LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    *task_countp = task_count;

    return 0u;
}

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completions,
                                     size_t size, size_t *task_countp)
{
    linted_error errnum;
    size_t task_count = 0u;

    if (0u == size) {
        return EINVAL;
    }

    for (; task_count < size; ++task_count) {
        struct linted_queue_node *node;
        errnum = linted_queue_try_recv(pool->event_queue, &node);
        if (EAGAIN == errnum) {
            break;
        }

        /* The node is the first member of the task */
        completions[task_count]
            = LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    *task_countp = task_count;

    if (0u == task_count) {
        return EAGAIN;
    }

    return 0;
}

void linted_asynch_task(struct linted_asynch_task *task, unsigned type,
                        unsigned task_action)
{
    linted_queue_node(LINTED_UPCAST(task));

    task->type = type;
    task->errnum = 0;
    task->task_action = task_action;
}

static noreturn DWORD WINAPI worker_routine(void *arg)
{
    struct linted_asynch_pool *pool = arg;

    for (;;) {
        struct linted_asynch_task *task;
        {
            struct linted_queue_node *node;
            linted_queue_recv(pool->worker_command_queue, &node);
            task = LINTED_DOWNCAST(struct linted_asynch_task, node);
        }

        run_task(pool, task);
    }
}

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task)
{
    switch (task->type) {
    case LINTED_ASYNCH_TASK_READ:
        run_task_read(pool, task);
        break;

    case LINTED_ASYNCH_TASK_WRITE:
        run_task_write(pool, task);
        break;

    default:
        assert(false);
    }
}

static void run_task_read(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
    struct linted_ko_task_read *task_read
        = LINTED_DOWNCAST(struct linted_ko_task_read, task);

    DWORD bytes_read = 0u;

    linted_error errnum = 0;
    if (!ReadFile(task_read->ko, task_read->buf, task_read->size, &bytes_read,
                  NULL)) {
        DWORD error = GetLastError();
        errnum = HRESULT_FROM_WIN32(error);
    }

    task->errnum = errnum;
    task_read->bytes_read = bytes_read;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_write(struct linted_asynch_pool *pool,
                           struct linted_asynch_task *task)
{
    struct linted_ko_task_write *task_write
        = LINTED_DOWNCAST(struct linted_ko_task_write, task);

    DWORD bytes_write = 0u;

    linted_error errnum = 0;
    if (!WriteFile(task_write->ko, task_write->buf, task_write->size,
                   &bytes_write, NULL)) {
        DWORD error = GetLastError();
        errnum = HRESULT_FROM_WIN32(error);
    }

    task->errnum = errnum;
    task_write->bytes_wrote = bytes_write;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}
