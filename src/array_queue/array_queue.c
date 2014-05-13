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

#include "linted/array_queue.h"

#include "linted/error.h"
#include "linted/util.h"

#include <pthread.h>
#include <stdbool.h>
#include <string.h>

struct linted_array_queue
{
    pthread_mutex_t mutex;
    pthread_cond_t on_empty;
    pthread_cond_t on_full;
    size_t message_size;
    bool occupied;
    char message_buffer[];
};

static void unlock_routine(void* arg);

linted_error linted_array_queue_create(struct linted_array_queue** queuep,
                                       size_t msgsize)
{
    if (0 == msgsize) {
        return EINVAL;
    }

    struct linted_array_queue* queue = malloc(sizeof *queue + msgsize);
    if (NULL == queue) {
        return errno;
    }

    pthread_mutex_init(&queue->mutex, NULL);
    pthread_cond_init(&queue->on_empty, NULL);
    pthread_cond_init(&queue->on_full, NULL);

    queue->message_size = msgsize;
    queue->occupied = false;

    *queuep = queue;

    return 0;
}

void linted_array_queue_destroy(struct linted_array_queue* queue)
{
    pthread_cond_destroy(&queue->on_full);
    pthread_cond_destroy(&queue->on_empty);

    /*
     * The mutex is destroyed last to make Valgrind debugging easier.
     */
    pthread_mutex_destroy(&queue->mutex);

    free(queue);
}

linted_error linted_array_queue_try_send(struct linted_array_queue* queue,
                                         void const* message)
{
    linted_error errnum = 0;
    int old_cancel_state;

    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &old_cancel_state);

    pthread_mutex_lock(&queue->mutex);
    pthread_cleanup_push(unlock_routine, &queue->mutex);

    if (queue->occupied) {
        errnum = EAGAIN;
        goto unlock_mutex;
    }
    queue->occupied = true;

    memcpy(queue->message_buffer, message, queue->message_size);

    pthread_cond_signal(&queue->on_full);

unlock_mutex:
    pthread_cleanup_pop(true);

    pthread_setcanceltype(old_cancel_state, NULL);

    return errnum;
}

linted_error linted_array_queue_try_recv(struct linted_array_queue* queue,
                                         void* message)
{
    linted_error errnum = 0;
    int old_cancel_state;

    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &old_cancel_state);

    pthread_mutex_lock(&queue->mutex);
    pthread_cleanup_push(unlock_routine, &queue->mutex);

    if (!queue->occupied) {
        errnum = EAGAIN;
        goto unlock_mutex;
    }

    queue->occupied = false;
    memcpy(message, queue->message_buffer, queue->message_size);

    pthread_cond_signal(&queue->on_empty);

unlock_mutex:
    pthread_cleanup_pop(true);

    pthread_setcanceltype(old_cancel_state, NULL);

    return errnum;
}

void linted_array_queue_send(struct linted_array_queue* queue,
                             void const* message)
{
    int old_cancel_state;
    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &old_cancel_state);

    pthread_mutex_lock(&queue->mutex);
    pthread_cleanup_push(unlock_routine, &queue->mutex);

    while (queue->occupied) {
        pthread_cond_wait(&queue->on_empty, &queue->mutex);
    }

    queue->occupied = true;
    memcpy(queue->message_buffer, message, queue->message_size);

    pthread_cond_signal(&queue->on_full);

    pthread_cleanup_pop(true);

    pthread_setcanceltype(old_cancel_state, NULL);
}

void linted_array_queue_recv(struct linted_array_queue* queue, void* message)
{
    int old_cancel_state;
    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &old_cancel_state);

    pthread_mutex_lock(&queue->mutex);
    pthread_cleanup_push(unlock_routine, &queue->mutex);

    while (!queue->occupied) {
        pthread_cond_wait(&queue->on_full, &queue->mutex);
    }

    queue->occupied = false;
    memcpy(message, queue->message_buffer, queue->message_size);

    pthread_cond_signal(&queue->on_empty);

    pthread_cleanup_pop(true);

    pthread_setcanceltype(old_cancel_state, NULL);
}

static void unlock_routine(void* arg)
{
    pthread_mutex_t* mutex = arg;
    pthread_mutex_unlock(mutex);
}
