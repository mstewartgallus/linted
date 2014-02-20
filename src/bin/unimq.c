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

#include "linted/unimq.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

struct linted__unimq {
    struct linted_unimq_attr attributes;
    pthread_mutex_t mutex;
    pthread_cond_t is_empty;
    pthread_cond_t is_full;
    size_t message_count;
    char messages[];
};

int linted_unimq_init(linted_unimq * mq, struct linted_unimq_attr * attr)
{
    if (NULL == attr) {
        errno = EINVAL;
        return -1;
    }

    size_t const max_message_count = attr->max_message_count;
    size_t const message_size = attr->message_size;

    if (0 == message_size) {
        errno = EINVAL;
        return -1;
    }

    if (0 == max_message_count) {
        errno = EINVAL;
        return -1;
    }

    size_t const message_buffer_size = message_size * max_message_count;
    if (message_buffer_size / max_message_count != message_size) {
        errno = ENOMEM;
        return -1;
    }

    size_t const unimq_size = sizeof (struct linted__unimq) + message_buffer_size;
    if (unimq_size < sizeof (struct linted__unimq)) {
        return -1;
    }

    linted_unimq allocated = malloc(unimq_size);
    if (NULL == allocated) {
        return -1;
    }

    allocated->attributes = *attr;
    pthread_mutex_init(&allocated->mutex, NULL);
    pthread_cond_init(&allocated->is_empty, NULL);
    pthread_cond_init(&allocated->is_full, NULL);
    allocated->message_count = 0;

    *mq = allocated;

    return 0;
}

int linted_unimq_send(linted_unimq mq, void const *msg_ptr)
{
    struct linted_unimq_attr const * const attr = &mq->attributes;
    pthread_mutex_t * const mutex = &mq->mutex;
    pthread_cond_t * const is_empty = &mq->is_empty;
    pthread_cond_t * const is_full = &mq->is_full;
    size_t * const message_count = &mq->message_count;
    char * const messages = mq->messages;

    int lock_status = pthread_mutex_lock(mutex);
    if (lock_status != 0) {
        errno = lock_status;
        return -1;
    }

    while (*message_count >= attr->max_message_count) {
        pthread_cond_wait(is_full, mutex);
    }

    memcpy(messages + *message_count * attr->message_size, msg_ptr,
           attr->message_size);

    ++*message_count;

    pthread_cond_signal(is_empty);

    int unlock_status = pthread_mutex_unlock(mutex);
    if (unlock_status != 0) {
        errno = unlock_status;
        return -1;
    }

    return 0;
}

int linted_unimq_receive(linted_unimq mq, void *msg_ptr)
{
    struct linted_unimq_attr const * const attr = &mq->attributes;
    pthread_mutex_t * const mutex = &mq->mutex;
    pthread_cond_t * const is_empty = &mq->is_empty;
    pthread_cond_t * const is_full = &mq->is_full;
    size_t * const message_count = &mq->message_count;
    char const * const messages = mq->messages;

    int lock_status = pthread_mutex_lock(mutex);
    if (lock_status != 0) {
        errno = lock_status;
        return -1;
    }

    while (0 == *message_count) {
        pthread_cond_wait(is_empty, mutex);
    }

    --*message_count;

    memcpy(msg_ptr, messages + *message_count * attr->message_size,
           attr->message_size);

    pthread_cond_signal(is_full);

    int unlock_status = pthread_mutex_unlock(mutex);
    if (unlock_status != 0) {
        errno = unlock_status;
        return -1;
    }

    return 0;
}

int linted_unimq_destroy(linted_unimq mq)
{
    int exit_status = 0;

    pthread_mutex_t * const mutex = &mq->mutex;
    pthread_cond_t * const is_empty = &mq->is_empty;
    pthread_cond_t * const is_full = &mq->is_full;

    int lock_status = pthread_mutex_lock(mutex);
    if (lock_status != 0) {
        errno = lock_status;
        exit_status = -1;
    }

    {
        int errnum = errno;
        int destroy_status = pthread_cond_destroy(is_empty);
        errno = destroy_status;
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (destroy_status != 0) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int destroy_status = pthread_cond_destroy(is_full);
        errno = destroy_status;
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (destroy_status != 0) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int unlock_status = pthread_mutex_unlock(mutex);
        errno = unlock_status;
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (unlock_status != 0) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int destroy_status = pthread_mutex_destroy(mutex);
        errno = destroy_status;
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (destroy_status != 0) {
            exit_status = -1;
        }
    }

    free(mq);

    return exit_status;
}
