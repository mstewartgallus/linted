#include "config.h"

#include "linted/asynch.h"

#include "linted/array_queue.h"
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

#define POOL_SIZE 20u

struct linted_asynch_worker_pool
{
    struct linted_array_queue* command_queue;
    struct linted_array_queue* event_queue;
    size_t worker_count;
    pthread_t workers[];
};

static void* worker_routine(void* arg);

int linted_asynch_pool_create(struct linted_asynch_pool* pool)
{
    int errnum;
    size_t created_threads = 0;

    struct linted_array_queue* command_queue;
    if ((errnum = linted_array_queue_create(
             &command_queue, sizeof(union linted_asynch_task))) != 0) {
        return errnum;
    }

    struct linted_array_queue* event_queue;
    if ((errnum = linted_array_queue_create(
             &event_queue, sizeof(union linted_asynch_event))) != 0) {
        goto destroy_command_queue;
    }

    struct linted_asynch_worker_pool* worker_pool = malloc(
        sizeof *worker_pool + POOL_SIZE * sizeof worker_pool->workers[0]);
    if (NULL == worker_pool) {
        errnum = errno;
        goto destroy_event_queue;
    }

    worker_pool->worker_count = POOL_SIZE;
    worker_pool->command_queue = command_queue;
    worker_pool->event_queue = event_queue;

    for (; created_threads < POOL_SIZE; ++created_threads) {
        if ((errnum = pthread_create(&worker_pool->workers[created_threads],
                                     NULL, worker_routine, worker_pool)) != 0) {
            goto destroy_threads;
        }
    }

    pool->worker_pool = worker_pool;
    pool->command_queue = command_queue;
    pool->event_queue = event_queue;

    return 0;

destroy_threads:
    for (size_t ii = 0; ii < created_threads; ++ii) {
        pthread_t worker = worker_pool->workers[ii];
        pthread_cancel(worker);
        pthread_join(worker, NULL);
    }

destroy_event_queue:
    linted_array_queue_destroy(event_queue);

destroy_command_queue:
    linted_array_queue_destroy(command_queue);

    return errnum;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool* pool)
{
    linted_error errnum = 0;

    struct linted_asynch_worker_pool* worker_pool = pool->worker_pool;
    size_t worker_count = worker_pool->worker_count;
    pthread_t* workers = worker_pool->workers;

    for (size_t ii = 0; ii < worker_count; ++ii) {
        pthread_t worker = workers[ii];
        pthread_cancel(worker);
        pthread_join(worker, NULL);
    }

    free(pool->worker_pool);

    linted_array_queue_destroy(pool->command_queue);
    linted_array_queue_destroy(pool->event_queue);

    return errnum;
}

linted_error linted_asynch_pool_submit(struct linted_asynch_pool* pool,
                                       union linted_asynch_task* task)
{
    int errnum;

    // Unfortunately, I am not smart enough to create a dynamically
    // sized thread pool.
    do {
        errnum = linted_array_queue_send(pool->command_queue, task);
    } while (EINTR == errnum);
    return errnum;
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool* pool,
                                     union linted_asynch_event* events,
                                     size_t size, size_t* event_count)
{
    linted_error errnum;

    if (0 == size) {
        return EINVAL;
    }

    /* Just receive one at a time right now */
    if ((errnum = linted_array_queue_recv(pool->event_queue, &events[0]))
        != 0) {
        return errnum;
    }

    *event_count = 1;
    return 0;
}

static void* worker_routine(void* arg)
{
    struct linted_asynch_worker_pool* worker_pool = arg;

    for (;;) {
        union linted_asynch_task task;
        linted_error recv_errnum;
        do {
            recv_errnum
                = linted_array_queue_recv(worker_pool->command_queue, &task);
        } while (EINTR == recv_errnum);

        union linted_asynch_event event;
        memset(&event, 0, sizeof event);

        switch (task.typical.type) {
        case LINTED_ASYNCH_TASK_POLL: {
            struct linted_asynch_task_poll* task_poll = &task.poll;
            linted_error errnum;
            do {
                int poll_status = poll(task_poll->fds, task_poll->size, -1);
                errnum = -1 == poll_status ? errno : 0;
            } while (EINTR == errnum);

            event.poll.type = LINTED_ASYNCH_EVENT_POLL;
            event.poll.task_id = task_poll->task_id;
            event.poll.errnum = errnum;
            break;
        }

        case LINTED_ASYNCH_TASK_READ: {
            struct linted_asynch_task_read* task_read = &task.read;
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

            event.read.type = LINTED_ASYNCH_EVENT_READ;
            event.read.task_id = task_read->task_id;
            event.read.errnum = errnum;
            event.read.bytes_read = bytes_read;
            break;
        }

        case LINTED_ASYNCH_TASK_MQ_RECEIVE: {
            struct linted_asynch_task_mq_receive* task_receive =
                &task.mq_receive;
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
                    break;
                }

                bytes_read = result;
            } while (EAGAIN == errnum || EINTR == errnum);

            event.read.type = LINTED_ASYNCH_EVENT_READ;
            event.read.task_id = task_receive->task_id;
            event.read.errnum = errnum;
            event.read.bytes_read = bytes_read;
            break;
        }

        case LINTED_ASYNCH_TASK_MQ_SEND: {
            struct linted_asynch_task_mq_send* task_send = &task.mq_send;
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
                    break;
                }

                bytes_wrote = task_send->size;
            } while (EAGAIN == errnum || EINTR == errnum);

            event.write.type = LINTED_ASYNCH_EVENT_WRITE;
            event.write.task_id = task_send->task_id;
            event.write.errnum = errnum;
            event.write.bytes_wrote = bytes_wrote;
            break;
        }

        default:
            assert(false);
        }

        linted_error send_errnum;
        do {
            send_errnum
                = linted_array_queue_send(worker_pool->event_queue, &event);
        } while (EINTR == send_errnum);
    }
}
