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
#define _GNU_SOURCE

#include "config.h"

#include "linted/asynch.h"

#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>

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

    _Bool stopped : 1U;

    pthread_t workers[];
};

static void *worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *restrict task);

static void run_task_waitid(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *restrict task);
static void run_task_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *restrict task);

linted_error linted_asynch_pool_create(struct linted_asynch_pool
                                       **restrict poolp,
                                       unsigned max_tasks)
{
    linted_error errnum;
    size_t created_threads = 0U;
    struct linted_asynch_pool *pool;

    size_t workers_size = max_tasks * sizeof pool->workers[0U];

    {
        void *xx;
        if ((errnum = linted_mem_alloc(&xx, sizeof *pool + workers_size))
            != 0) {
            return errnum;
        }
        pool = xx;
    }

    pool->stopped = false;

    if ((errnum = linted_queue_create(&pool->worker_command_queue)) != 0) {
        goto free_pool;
    }

    if ((errnum = linted_queue_create(&pool->event_queue)) != 0) {
        goto destroy_worker_command_queue;
    }

    pool->worker_count = max_tasks;

    {
        pthread_attr_t worker_attributes;

        if ((errnum = pthread_attr_init(&worker_attributes)) != 0) {
            goto destroy_event_queue;
        }

        /*
         * Our tasks are only I/O tasks and have extremely tiny stacks.
         */
        pthread_attr_setstacksize(&worker_attributes,
                                  sysconf(_SC_THREAD_STACK_MIN));

        for (; created_threads < max_tasks; ++created_threads) {
            if ((errnum = pthread_create(&pool->workers[created_threads],
                                         &worker_attributes, worker_routine,
                                         pool)) != 0) {
                break;
            }
        }

        {
            linted_error destroy_errnum
                = pthread_attr_destroy(&worker_attributes);
            if (0 == errnum) {
                errnum = destroy_errnum;
            }
        }
    }

    if (errnum != 0) {
        goto destroy_threads;
    }

    *poolp = pool;

    return 0;

destroy_threads:
    for (size_t ii = 0U; ii < created_threads; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0U; ii < created_threads; ++ii) {
        pthread_join(pool->workers[ii], NULL);
    }

destroy_event_queue:
    linted_queue_destroy(pool->event_queue);

destroy_worker_command_queue:
    linted_queue_destroy(pool->worker_command_queue);

free_pool:
    linted_mem_free(pool);

    return errnum;
}

linted_error linted_asynch_pool_stop(struct linted_asynch_pool *pool)
{
    linted_error errnum = 0;

    if (pool->stopped) {
        return EINVAL;
    }

    pool->stopped = true;

    size_t worker_count = pool->worker_count;

    for (size_t ii = 0U; ii < worker_count; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0U; ii < worker_count; ++ii) {
        pthread_join(pool->workers[ii], NULL);
    }

    linted_queue_destroy(pool->worker_command_queue);

    return errnum;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
    linted_error errnum = 0;

    if (!pool->stopped) {
        return EBUSY;
    }

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
        assert(!pool->stopped);

        task->errnum = EINPROGRESS;
        linted_queue_send(pool->worker_command_queue, LINTED_UPCAST(task));
    }
}

void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
    if (pool != NULL) {
        int oldstate;
        pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldstate);
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
        pthread_setcancelstate(oldstate, NULL);
    }
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
    /* Wait for one event */
    {
        struct linted_queue_node *node;
        linted_queue_recv(pool->event_queue, &node);

        /* The node is the first member of the task */
        *completionp = LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    return 0U;
}

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
    linted_error errnum;

    {
        struct linted_queue_node *node;
        errnum = linted_queue_try_recv(pool->event_queue, &node);
        if (EAGAIN == errnum) {
            return EAGAIN;
        }

        *completionp = LINTED_DOWNCAST(struct linted_asynch_task, node);
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

void linted_asynch_task_waitid(struct linted_asynch_task_waitid *task,
                               unsigned task_action, idtype_t idtype, id_t id,
                               int options)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_WAITID,
                       task_action);

    task->idtype = idtype;
    task->id = id;
    task->options = options;
}

void linted_asynch_task_sleep_until(struct linted_asynch_task_sleep_until *task,
                                    unsigned task_action, int flags,
                                    struct timespec const *request)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_SLEEP_UNTIL,
                       task_action);

    task->flags = flags;
    task->request = *request;
}

static void *worker_routine(void *arg)
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
                     struct linted_asynch_task *restrict task)
{
    switch (task->type) {
    case LINTED_ASYNCH_TASK_POLL:
        linted_ko_do_poll(pool, task);
        break;

    case LINTED_ASYNCH_TASK_READ:
        linted_ko_do_read(pool, task);
        break;

    case LINTED_ASYNCH_TASK_WRITE:
        linted_ko_do_write(pool, task);
        break;

    case LINTED_ASYNCH_TASK_MQ_RECEIVE:
        linted_mq_do_receive(pool, task);
        break;

    case LINTED_ASYNCH_TASK_MQ_SEND:
        linted_mq_do_send(pool, task);
        break;

    case LINTED_ASYNCH_TASK_WAITID:
        run_task_waitid(pool, task);
        break;

    case LINTED_ASYNCH_TASK_ACCEPT:
        linted_ko_do_accept(pool, task);
        break;

    case LINTED_ASYNCH_TASK_SLEEP_UNTIL:
        run_task_sleep_until(pool, task);
        break;

    default:
        LINTED_ASSUME_UNREACHABLE();
    }
}

static void run_task_waitid(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *restrict task)
{
    struct linted_asynch_task_waitid *restrict task_wait
        = LINTED_DOWNCAST(struct linted_asynch_task_waitid, task);
    linted_error errnum;
    int status;
    pid_t child;
    do {
        pid_t id;
        switch (task_wait->idtype) {
        case P_PID:
            id = task_wait->id;
            break;

        case P_ALL:
            id = -1;
            break;

        case P_PGID:
            id = -(pid_t)task_wait->id;
            break;

        default:
            errnum = EINVAL;
            goto finish;
        }

        {
            int xx;
            child = waitpid(id, &xx, task_wait->options);
            if (-1 == child) {
                errnum = errno;
                LINTED_ASSUME(errnum != 0);
            } else {
                errnum = 0;
            }
            status = xx;
        }
    } while (EINTR == errnum);

finish:
    if (0 == errnum) {
        task_wait->info.si_uid = 0;
        task_wait->info.si_pid = child;
        task_wait->info.si_signo = SIGCHLD;

        if (WIFEXITED(status)) {
            task_wait->info.si_code = CLD_EXITED;
            task_wait->info.si_status = WEXITSTATUS(status);
        } else if (WIFSIGNALED(status)) {
            if (WCOREDUMP(status)) {
                task_wait->info.si_code = CLD_DUMPED;
            } else {
                task_wait->info.si_code = CLD_KILLED;
            }
            task_wait->info.si_status = WTERMSIG(status);
        } else if (WIFSTOPPED(status)) {
            if (SIGTRAP == WSTOPSIG(status)) {
                task_wait->info.si_code = CLD_TRAPPED;
            } else {
                task_wait->info.si_code = CLD_STOPPED;
            }

#if defined __linux__
            task_wait->info.si_status = (status >> 8) & 0xffff;
#else
#error How to get special ptrace information from signal is unknown on this platform
#endif
        } else if (WIFCONTINUED(status)) {
            task_wait->info.si_code = CLD_CONTINUED;
            task_wait->info.si_status = 0;
        } else {
            LINTED_ASSUME_UNREACHABLE();
        }
    }

    task->errnum = errnum;

    linted_asynch_pool_complete(pool, task);
}

static void run_task_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *restrict task)
{
    struct linted_asynch_task_sleep_until *restrict task_sleep
        = LINTED_DOWNCAST(struct linted_asynch_task_sleep_until, task);
    linted_error errnum;
    struct timespec time_remaining = task_sleep->request;
    do {
        if (-1 == clock_nanosleep(CLOCK_MONOTONIC, task_sleep->flags,
                                  &time_remaining, &time_remaining)) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EINTR == errnum);

    task->errnum = errnum;

    linted_asynch_pool_complete(pool, task);
}
