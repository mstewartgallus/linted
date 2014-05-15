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

#include "linted/queue.h"
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
#include <sys/eventfd.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

struct linted_asynch_pool
{
    struct linted_queue io_command_queue;

    /**
     * A one writer to many readers queue.
     */
    struct linted_queue worker_command_queue;

    /**
     * A one reader to many writers queue. Should be able to retrieve
     * many values at once. As all writes are a direct result of
     * submitted commands there is no need to worry about it growing
     * too large.
     */
    struct linted_queue event_queue;

    size_t worker_count;

    linted_ko notifier;
    linted_ko on_io_manager_event;

    pthread_t io_manager;
    pthread_t workers[];
};

struct io_poll_data {
    struct linted_queue waiters;
    linted_ko ko;
};

static void *io_manager_routine(void *arg);
static void *worker_routine(void *arg);

static uint32_t task_notifier_flags(struct linted_asynch_task * task);
static linted_ko task_ko(struct linted_asynch_task * task);

static void asynch_task(struct linted_asynch_task *task, unsigned type,
                        unsigned task_action);

static void asynch_task_poll(struct linted_asynch_pool* pool,
                             struct linted_asynch_task *task);
static void asynch_task_read(struct linted_asynch_pool* pool,
                             struct linted_asynch_task *task);
static void asynch_task_mq_receive(struct linted_asynch_pool* pool,
                                   struct linted_asynch_task *task);
static void asynch_task_mq_send(struct linted_asynch_pool* pool,
                                struct linted_asynch_task *task);
static void asynch_task_waitid(struct linted_asynch_pool* pool,
                               struct linted_asynch_task *task);
static void asynch_task_accept(struct linted_asynch_pool* pool,
                               struct linted_asynch_task *task);

int linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                              unsigned max_tasks)
{
    int errnum;
    size_t created_threads = 0;
    struct linted_asynch_pool *pool;

    size_t workers_size = max_tasks * sizeof pool->workers[0];
    pool = malloc(sizeof *pool + workers_size);
    if (NULL == pool) {
        return errno;
    }

    if ((errnum = linted_queue_create(&pool->worker_command_queue)) != 0) {
        goto free_pool;
    }

    if ((errnum = linted_queue_create(&pool->event_queue)) != 0) {
        goto destroy_worker_command_queue;
    }

    if ((errnum = linted_queue_create(&pool->io_command_queue)) != 0) {
        goto destroy_event_queue;
    }

    linted_ko notifier = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == notifier) {
        errnum = errno;
        goto destroy_io_command_queue;
    }

    linted_ko on_io_manager_event = eventfd(0,
                                            EFD_SEMAPHORE | EFD_NONBLOCK | EFD_CLOEXEC);
    if (-1 == on_io_manager_event) {
        errnum = errno;
        goto close_notifier;
    }

    {
        struct epoll_event event = {
            .events = EPOLLIN,
            .data = {.ptr = NULL}
        };
        if (-1 == epoll_ctl(notifier, EPOLL_CTL_ADD, on_io_manager_event, &event)) {
            errnum = errno;
            goto close_notifier;
        }
    }

    pool->notifier = notifier;
    pool->on_io_manager_event = on_io_manager_event;
    pool->worker_count = max_tasks;

    if ((errnum = pthread_create(&pool->io_manager, NULL,
                                 io_manager_routine, pool)) != 0) {
        goto close_on_io_manager_event;
    }

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

    pthread_cancel(pool->io_manager);
    pthread_join(pool->io_manager, NULL);

close_on_io_manager_event:
    linted_ko_close(on_io_manager_event);

close_notifier:
    linted_ko_close(notifier);

destroy_io_command_queue:
    linted_queue_destroy(&pool->io_command_queue);

destroy_event_queue:
    linted_queue_destroy(&pool->event_queue);

destroy_worker_command_queue:
    linted_queue_destroy(&pool->worker_command_queue);

free_pool:
    free(pool);

    return errnum;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
    linted_error errnum = 0;

    size_t worker_count = pool->worker_count;

    for (size_t ii = 0; ii < worker_count; ++ii) {
        pthread_cancel(pool->workers[ii]);
    }

    for (size_t ii = 0; ii < worker_count; ++ii) {
        pthread_join(pool->workers[ii], NULL);
    }

    pthread_cancel(pool->io_manager);
    pthread_join(pool->io_manager, NULL);

    linted_ko_close(pool->notifier);
    linted_ko_close(pool->on_io_manager_event);

    linted_queue_destroy(&pool->io_command_queue);
    linted_queue_destroy(&pool->worker_command_queue);
    linted_queue_destroy(&pool->event_queue);

    free(pool);

    return errnum;
}

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task)
{
    linted_queue_send(&pool->worker_command_queue, LINTED_UPCAST(task));
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completions,
                                     size_t size, size_t *task_countp)
{
    linted_error errnum;
    size_t task_count = 0;

    if (0 == size) {
        return EINVAL;
    }

    /* Wait for one event */
    {
        struct linted_queue_node *node;
        linted_queue_recv(&pool->event_queue, &node);

        /* The node is the first member of the task */
        completions[task_count] =
            LINTED_DOWNCAST(struct linted_asynch_task, node);
        ++task_count;
    }

    /* Then poll for more */
    for (; task_count < size; ++task_count) {
        struct linted_queue_node *node;
        errnum = linted_queue_try_recv(&pool->event_queue, &node);
        if (EAGAIN == errnum) {
            break;
        }

        completions[task_count] =
            LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    *task_countp = task_count;

    return 0;
}

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completions,
                                     size_t size, size_t *task_countp)
{
    linted_error errnum;
    size_t task_count = 0;

    if (0 == size) {
        return EINVAL;
    }

    for (; task_count < size; ++task_count) {
        struct linted_queue_node *node;
        errnum = linted_queue_try_recv(&pool->event_queue, &node);
        if (EAGAIN == errnum) {
            break;
        }

        /* The node is the first member of the task */
        completions[task_count] =
            LINTED_DOWNCAST(struct linted_asynch_task, node);
    }

    *task_countp = task_count;

    if (0 == task_count) {
        return EAGAIN;
    }

    return 0;
}

void linted_asynch_poll(struct linted_asynch_task_poll *task, int task_action,
                        linted_ko ko, short events)
{
    asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_POLL, task_action);

    task->ko = ko;
    task->events = events;
}

void linted_asynch_read(struct linted_asynch_task_read *task, int task_action,
                        linted_ko ko, char *buf, size_t size)
{
    asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_READ, task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->bytes_read = 0;
}

void linted_asynch_mq_receive(struct linted_asynch_task_mq_receive *task,
                              int task_action, linted_ko ko, char *buf,
                              size_t size)
{
    asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_MQ_RECEIVE,
                task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->bytes_read = 0;
}

void linted_asynch_mq_send(struct linted_asynch_task_mq_send *task,
                           int task_action, linted_ko ko, char const *buf,
                           size_t size)
{
    asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_MQ_SEND, task_action);

    task->ko = ko;
    task->buf = buf;
    task->size = size;
    task->bytes_wrote = 0;
}

void linted_asynch_waitid(struct linted_asynch_task_waitid *task,
                          int task_action, idtype_t idtype, id_t id,
                          int options)
{
    asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_WAITID, task_action);

    task->idtype = idtype;
    task->id = id;
    task->options = options;
}

void linted_asynch_accept(struct linted_asynch_task_accept *task,
                          int task_action, linted_ko ko)
{
    asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_ACCEPT, task_action);

    task->ko = ko;
}

static void asynch_task(struct linted_asynch_task *task, unsigned type,
                        unsigned task_action)
{
    linted_queue_node(LINTED_UPCAST(task));

    task->type = type;
    task->errnum = 0;
    task->task_action = task_action;
}

static void *io_manager_routine(void *arg)
{
    struct linted_asynch_pool *pool = arg;

    linted_ko notifier = pool->notifier;

    for (;;) {
        bool has_commands_ready = false;
        struct epoll_event events[20];
        int event_count;
        {
            linted_error errnum;
            do {
                event_count = epoll_wait(notifier, events,
                                         LINTED_ARRAY_SIZE(events), -1);
                errnum = -1 == event_count ? errno : 0;
            } while (EINTR == errnum);
            if (errnum != 0) {
                assert(errnum != EBADF);
                assert(errnum != EFAULT);
                assert(errnum != EINVAL);
                assert(false);
            }
        }

        for (size_t ii = 0; ii < (size_t)event_count; ++ii) {
            struct epoll_event * event = &events[ii];
            uint32_t event_flags = event->events;
            struct linted_asynch_task * task = event->data.ptr;
            if (NULL == task) {
                has_commands_ready = true;
                continue;
            }

            linted_ko ko = task_ko(task);

            uint32_t task_flags = task_notifier_flags(task);
            if ((event_flags & task_flags) != 0) {
                linted_queue_send(&pool->worker_command_queue, LINTED_UPCAST(task));
            } else if ((event_flags & EPOLLERR) != 0) {
                socklen_t size = sizeof task->errnum;
                getsockopt(ko, SOL_SOCKET, SO_ERROR,
                           (void *)&task->errnum, &size);
                linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
            } else {
                assert(false);
            }

            epoll_ctl(notifier, EPOLL_CTL_DEL, ko, NULL);
        }

        if (has_commands_ready) {
            for (;;) {
                uint64_t tick;
                if (-1 == read(pool->on_io_manager_event, &tick, sizeof tick)) {
                    break;
                }
            }

            for (;;) {
                linted_error errnum;

                struct linted_asynch_task *task;
                {
                    struct linted_queue_node *node;
                    if (EAGAIN == linted_queue_try_recv(&pool->io_command_queue, &node)) {
                        break;
                    }
                    task = LINTED_DOWNCAST(struct linted_asynch_task, node);
                }

                linted_ko ko = task_ko(task);

                struct epoll_event event = {
                    .events = task_notifier_flags(task),
                    .data = {.ptr = task}
                };
                if (-1 == epoll_ctl(notifier, EPOLL_CTL_ADD, ko, &event)) {
                    errnum = errno;
                    assert(errnum != ENOENT);

                    goto send_error;
                }
                continue;

            send_error:
                task->errnum = errnum;
                linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
            }
        }
    }
}

static uint32_t task_notifier_flags(struct linted_asynch_task * task)
{
    switch (task->task_action) {
    case LINTED_ASYNCH_TASK_POLL: {
        struct linted_asynch_task_poll *task_poll =
            LINTED_DOWNCAST(struct linted_asynch_task_poll, task);

        uint32_t flags = 0;
        if ((task_poll->events & POLLIN) != 0) {
            flags |= EPOLLIN;
        }
        if ((task_poll->events & POLLPRI) != 0) {
            flags |= EPOLLPRI;
        }
        if ((task_poll->events & POLLOUT) != 0) {
            flags |= EPOLLOUT;
        }
        if ((task_poll->events & POLLRDHUP) != 0) {
            flags |= EPOLLRDHUP;
        }
        return flags;
    }

    case LINTED_ASYNCH_TASK_READ:
    case LINTED_ASYNCH_TASK_MQ_RECEIVE:
    case LINTED_ASYNCH_TASK_ACCEPT:
        return EPOLLIN;

    case LINTED_ASYNCH_TASK_MQ_SEND:
        return EPOLLOUT;

    default:
        assert(false);
    }
}

static linted_ko task_ko(struct linted_asynch_task * task)
{
    switch (task->task_action) {
    case LINTED_ASYNCH_TASK_POLL:
        return LINTED_DOWNCAST(struct linted_asynch_task_poll, task)->ko;

    case LINTED_ASYNCH_TASK_READ:
        return LINTED_DOWNCAST(struct linted_asynch_task_read, task)->ko;

    case LINTED_ASYNCH_TASK_MQ_RECEIVE:
        return LINTED_DOWNCAST(struct linted_asynch_task_mq_receive, task)->ko;

    case LINTED_ASYNCH_TASK_MQ_SEND:
        return LINTED_DOWNCAST(struct linted_asynch_task_mq_send, task)->ko;

    case LINTED_ASYNCH_TASK_ACCEPT:
        return LINTED_DOWNCAST(struct linted_asynch_task_accept, task)->ko;

    default:
        assert(false);
    }
}


static void *worker_routine(void *arg)
{
    /*
     * Set the thread to be asynchronously killable at any time.
     */
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

    struct linted_asynch_pool *pool = arg;

    for (;;) {
        struct linted_asynch_task *task;
        {
            struct linted_queue_node *node;
            linted_queue_recv(&pool->worker_command_queue, &node);
            task = LINTED_DOWNCAST(struct linted_asynch_task, node);
        }

        switch (task->type) {
        case LINTED_ASYNCH_TASK_POLL:
            asynch_task_poll(pool, task);
            break;

        case LINTED_ASYNCH_TASK_READ:
            asynch_task_read(pool, task);
            break;

        case LINTED_ASYNCH_TASK_MQ_RECEIVE:
            asynch_task_mq_receive(pool, task);
            break;

        case LINTED_ASYNCH_TASK_MQ_SEND:
            asynch_task_mq_send(pool, task);
            break;

        case LINTED_ASYNCH_TASK_WAITID:
            asynch_task_waitid(pool, task);
            break;

        case LINTED_ASYNCH_TASK_ACCEPT:
            asynch_task_accept(pool, task);
            break;

        default:
            assert(false);
        }
    }
    return NULL;
}

static void send_io_command(struct linted_asynch_pool* pool,
                            struct linted_asynch_task* task)
{
    linted_queue_send(&pool->io_command_queue, LINTED_UPCAST(task));

    uint64_t tick = 1;
    write(pool->on_io_manager_event, &tick, sizeof tick);
}


static void asynch_task_poll(struct linted_asynch_pool* pool,
                             struct linted_asynch_task *task)
{
    struct linted_asynch_task_poll *task_poll =
        LINTED_DOWNCAST(struct linted_asynch_task_poll, task);
    linted_error errnum;

    struct pollfd fd = { .fd = task_poll->ko, .events = task_poll->events };
    do {
        int poll_status = poll(&fd, 1, 0);
        errnum = -1 == poll_status ? errno : 0;
    } while (EINTR == errnum);

    task_poll->revents = fd.revents;
    task->errnum = errnum;

    if (0 == fd.revents) {
        send_io_command(pool, task);
    } else {
        linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
    }
}

static void asynch_task_read(struct linted_asynch_pool* pool,
                             struct linted_asynch_task *task)
{
    struct linted_asynch_task_read *task_read =
        LINTED_DOWNCAST(struct linted_asynch_task_read, task);
    size_t bytes_read = 0;
    size_t bytes_left = task_read->size;
    linted_error errnum;
    do {
        {
            struct pollfd fd = { .fd = task_read->ko, .events = POLLIN };

            int poll_status = poll(&fd, 1, -1);
            errnum = -1 == poll_status ? errno : 0;
            if (errnum != 0) {
                continue;
            }
        }

        for (;;) {
            ssize_t result =
                read(task_read->ko, task_read->buf + bytes_read, bytes_left);
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

    linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
}

static void asynch_task_mq_receive(struct linted_asynch_pool* pool,
                                   struct linted_asynch_task *task)
{
    struct linted_asynch_task_mq_receive *task_receive =
        LINTED_DOWNCAST(struct linted_asynch_task_mq_receive, task);
    size_t bytes_read = 0;
    linted_error errnum;
    do {
        {
            struct pollfd fd = { .fd = task_receive->ko, .events = POLLIN };

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

    linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
}

static void asynch_task_mq_send(struct linted_asynch_pool* pool,
                                struct linted_asynch_task *task)
{
    struct linted_asynch_task_mq_send *task_send =
        LINTED_DOWNCAST(struct linted_asynch_task_mq_send, task);
    size_t bytes_wrote = 0;
    linted_error errnum;
    do {
        {
            struct pollfd fd = { .fd = task_send->ko, .events = POLLOUT };

            int poll_status = poll(&fd, 1, -1);
            errnum = -1 == poll_status ? errno : 0;
            if (errnum != 0) {
                continue;
            }
        }

        if (-1 == mq_send(task_send->ko, task_send->buf, task_send->size, 0)) {
            errnum = errno;
            continue;
        }

        bytes_wrote = task_send->size;
    } while (EAGAIN == errnum || EINTR == errnum);

    task->errnum = errnum;
    task_send->bytes_wrote = bytes_wrote;

    linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
}

static void asynch_task_waitid(struct linted_asynch_pool* pool,
                               struct linted_asynch_task *task)
{
    struct linted_asynch_task_waitid *task_wait =
        LINTED_DOWNCAST(struct linted_asynch_task_waitid, task);
    linted_error errnum;
    do {
        int wait_status = waitid(task_wait->idtype, task_wait->id,
                                 &task_wait->info, task_wait->options);
        errnum = -1 == wait_status ? errno : 0;
    } while (EINTR == errnum);

    task->errnum = errnum;

    linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
}

static void asynch_task_accept(struct linted_asynch_pool* pool,
                               struct linted_asynch_task *task)
{
    struct linted_asynch_task_accept *task_accept =
        LINTED_DOWNCAST(struct linted_asynch_task_accept, task);
    linted_error errnum;
    linted_ko returned_ko = -1;
    do {
        {
            struct pollfd fd = { .fd = task_accept->ko, .events = POLLIN };

            int poll_status = poll(&fd, 1, -1);
            errnum = -1 == poll_status ? errno : 0;
            if (errnum != 0) {
                continue;
            }
        }

        returned_ko =
            accept4(task_accept->ko, NULL, 0, SOCK_NONBLOCK | SOCK_CLOEXEC);
        errnum = -1 == returned_ko ? errno : 0;
    } while (EAGAIN == errnum || EINTR == errnum);

    task->errnum = errnum;
    task_accept->returned_ko = returned_ko;

    linted_queue_send(&pool->event_queue, LINTED_UPCAST(task));
}
