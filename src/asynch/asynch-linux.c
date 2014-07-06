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

#include "linted/asynch.h"

#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <mqueue.h>
#include <poll.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
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

    pthread_t workers[];
};

static noreturn void *worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task);

static void run_task_poll(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);
static void run_task_read(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);
static void run_task_write(struct linted_asynch_pool *pool,
                           struct linted_asynch_task *task);
static void run_task_mq_receive(struct linted_asynch_pool *pool,
                                struct linted_asynch_task *task);
static void run_task_mq_send(struct linted_asynch_pool *pool,
                             struct linted_asynch_task *task);
static void run_task_waitid(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *task);
static void run_task_accept(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *task);
static void run_task_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks)
{
    linted_error errnum;
    size_t created_threads = 0u;
    struct linted_asynch_pool *pool;

    size_t workers_size = max_tasks * sizeof pool->workers[0u];

    {
        linted_error xx;
        pool = linted_mem_alloc(&xx, sizeof *pool + workers_size);
        errnum = xx;
    }
    if (errnum != 0) {
        return errnum;
    }

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
    for (size_t ii = 0u; ii < created_threads; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0u; ii < created_threads; ++ii) {
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

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
    linted_error errnum = 0;

    size_t worker_count = pool->worker_count;

    for (size_t ii = 0u; ii < worker_count; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0u; ii < worker_count; ++ii) {
        pthread_join(pool->workers[ii], NULL);
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
        task->errnum = EINPROGRESS;
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

void linted_asynch_task_waitid(struct linted_asynch_task_waitid *task,
                          unsigned task_action, idtype_t idtype, id_t id,
                          int options)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_WAITID, task_action);

    task->idtype = idtype;
    task->id = id;
    task->options = options;
}

void linted_asynch_task_sleep_until(struct linted_asynch_task_sleep_until *task,
                                    unsigned task_action, int flags,
                                    struct timespec const *request)
{
    linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_SLEEP_UNTIL, task_action);

    task->flags = flags;
    task->request = *request;
}

static noreturn void *worker_routine(void *arg)
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
    case LINTED_ASYNCH_TASK_POLL:
        run_task_poll(pool, task);
        break;

    case LINTED_ASYNCH_TASK_READ:
        run_task_read(pool, task);
        break;

    case LINTED_ASYNCH_TASK_WRITE:
        run_task_write(pool, task);
        break;

    case LINTED_ASYNCH_TASK_MQ_RECEIVE:
        run_task_mq_receive(pool, task);
        break;

    case LINTED_ASYNCH_TASK_MQ_SEND:
        run_task_mq_send(pool, task);
        break;

    case LINTED_ASYNCH_TASK_WAITID:
        run_task_waitid(pool, task);
        break;

    case LINTED_ASYNCH_TASK_ACCEPT:
        run_task_accept(pool, task);
        break;

    case LINTED_ASYNCH_TASK_SLEEP_UNTIL:
        run_task_sleep_until(pool, task);
        break;

    default:
        assert(false);
    }
}

static void run_task_poll(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
    struct linted_ko_task_poll *task_poll
        = LINTED_DOWNCAST(struct linted_ko_task_poll, task);
    linted_error errnum;

    short revents = 0;

    do {
        errnum = poll_one(task_poll->ko, task_poll->events, &revents);
    } while (EINTR == errnum);

    task_poll->revents = revents;
    task->errnum = errnum;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_read(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
    struct linted_ko_task_read *task_read
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
                assert(errnum != 0);

                if (EINTR == errnum) {
                    continue;
                }

                break;
            }

            size_t bytes_read_delta = result;
            if (0u == bytes_read_delta) {
                break;
            }

            bytes_read += bytes_read_delta;
            bytes_left -= bytes_read_delta;
            if (0u == bytes_left) {
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
    task_read->current_position = 0u;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_write(struct linted_asynch_pool *pool,
                           struct linted_asynch_task *task)
{
    struct linted_ko_task_write *task_write
        = LINTED_DOWNCAST(struct linted_ko_task_write, task);
    size_t bytes_wrote = task_write->current_position;
    size_t bytes_left = task_write->size - bytes_wrote;

    linted_error errnum = 0;
    for (;;) {
        for (;;) {
            ssize_t result = write(task_write->ko,
                                   task_write->buf + bytes_wrote, bytes_left);
            if (-1 == result) {
                errnum = errno;
                assert(errnum != 0);

                if (EINTR == errnum) {
                    continue;
                }

                break;
            }

            size_t bytes_wrote_delta = result;

            bytes_wrote += bytes_wrote_delta;
            bytes_left -= bytes_wrote_delta;
            if (0u == bytes_left) {
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

    task->errnum = errnum;
    task_write->bytes_wrote = bytes_wrote;
    task_write->current_position = 0u;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_mq_receive(struct linted_asynch_pool *pool,
                                struct linted_asynch_task *task)
{
    struct linted_mq_task_receive *task_receive
        = LINTED_DOWNCAST(struct linted_mq_task_receive, task);
    size_t bytes_read = 0u;
    linted_error errnum = 0;
    for (;;) {
        do {
            ssize_t result = mq_receive(task_receive->ko, task_receive->buf,
                                        task_receive->size, NULL);
            if (-1 == result) {
                errnum = errno;
                assert(errnum != 0);
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

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_mq_send(struct linted_asynch_pool *pool,
                             struct linted_asynch_task *task)
{
    struct linted_mq_task_send *task_send
        = LINTED_DOWNCAST(struct linted_mq_task_send, task);
    size_t bytes_wrote = 0u;
    linted_error errnum = 0;
    for (;;) {
        do {
            if (-1
                == mq_send(task_send->ko, task_send->buf, task_send->size, 0)) {
                errnum = errno;
                assert(errnum != 0);
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

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_waitid(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *task)
{
    struct linted_asynch_task_waitid *task_wait
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
                assert(errnum != 0);
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
            assert(false);
        }
    }

    task->errnum = errnum;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_accept(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *task)
{
    struct linted_ko_task_accept *task_accept
        = LINTED_DOWNCAST(struct linted_ko_task_accept, task);

    linted_ko new_ko = -1;
    linted_error errnum;

    for (;;) {
    retry_accept:

        new_ko
            = accept4(task_accept->ko, NULL, 0, SOCK_NONBLOCK | SOCK_CLOEXEC);
        if (-1 == new_ko) {
            errnum = errno;
            assert(errnum != 0);
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

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static void run_task_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
    struct linted_asynch_task_sleep_until *task_sleep
        = LINTED_DOWNCAST(struct linted_asynch_task_sleep_until, task);
    linted_error errnum;
    struct timespec time_remaining = task_sleep->request;
    do {
        if (-1 == clock_nanosleep(CLOCK_MONOTONIC, task_sleep->flags,
                                  &time_remaining, &time_remaining)) {
            errnum = errno;
            assert(errnum != 0);
        } else {
            errnum = 0;
        }
    } while (EINTR == errnum);

    task->errnum = errnum;

    if (pool != NULL) {
        linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
    }
}

static linted_error poll_one(linted_ko ko, short events, short *revents)
{
    struct pollfd pollfd = { .fd = ko, .events = events };
    if (-1 == poll(&pollfd, 1u, -1)) {
        linted_error errnum = errno;
        assert(errnum != 0);
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
            assert(errnum != 0);
        } else {
            errnum = xx;
        }
    }

    return errnum;
}
