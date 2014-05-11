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

#include "linted/linked_queue.h"

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static struct timespec get_the_end(void);
static void unlock_routine(void* arg);

linted_error linted_linked_queue_create(struct linted_linked_queue* queue)
{
    struct linted_linked_queue_node* tip = malloc(sizeof *tip);
    if (NULL == tip) {
        return errno;
    }

    tip->prev = tip;
    tip->next = tip;

    queue->tip = tip;

    pthread_mutex_init(&queue->lock, NULL);
    pthread_cond_init(&queue->gains_member, NULL);

    return 0;
}

void linted_linked_queue_destroy(struct linted_linked_queue* queue)
{
    for (;;) {
        struct linted_linked_queue_node* node;
        linted_error errnum = linted_linked_queue_try_recv(queue, &node);
        if (EAGAIN == errnum) {
            break;
        }
        free(node);
    }
    pthread_cond_destroy(&queue->gains_member);
    pthread_mutex_destroy(&queue->lock);
}

void linted_linked_queue_send(struct linted_linked_queue* queue,
                              struct linted_linked_queue_node* node)
{
    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    struct linted_linked_queue_node* tip = queue->tip;

    /* The nodes previous to the tip are the tail */
    struct linted_linked_queue_node* tail = tip->prev;
    tail->next = node;
    node->prev = tail;
    node->next = tip;
    tip->prev = node;

    pthread_cond_broadcast(&queue->gains_member);

    pthread_cleanup_pop(true);
}

linted_error linted_linked_queue_recv(struct linted_linked_queue* queue,
                                      struct linted_linked_queue_node** nodep)
{
    linted_error errnum = 0;
    struct linted_linked_queue_node* head;

    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    struct linted_linked_queue_node* tip = queue->tip;

    /* The nodes next to the tip are the head */
    for (;;) {
        head = tip->next;
        if (head != tip) {
            break;
        }

        struct timespec the_end = get_the_end();
        if ((errnum = pthread_cond_timedwait(&queue->gains_member, &queue->lock,
                                             &the_end)) != 0) {
            goto pop_cleanup;
        }
    }

    struct linted_linked_queue_node* next = head->next;
    tip->next = next;
    next->prev = tip;

pop_cleanup:
    pthread_cleanup_pop(true);

    if (errnum != 0) {
        return errnum;
    }

    *nodep = head;

    return 0;
}

linted_error linted_linked_queue_try_recv(struct linted_linked_queue* queue,
                                          struct linted_linked_queue_node
                                          ** nodep)
{
    linted_error errnum = 0;
    struct linted_linked_queue_node* head;

    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    struct linted_linked_queue_node* tip = queue->tip;

    /* The nodes next to the tip are the head */
    head = tip->next;
    if (head == tip) {
        errnum = EAGAIN;
        goto pop_cleanup;
    }

    struct linted_linked_queue_node* next = head->next;
    tip->next = next;
    next->prev = tip;

pop_cleanup:
    pthread_cleanup_pop(true);

    if (errnum != 0) {
        return errnum;
    }

    *nodep = head;

    return 0;
}

static inline struct timespec get_the_end(void)
{
    struct timespec the_end;
    struct tm the_end_tm;

    memset(&the_end_tm, 0, sizeof the_end_tm);

    the_end_tm.tm_year = INT_MAX;
    the_end.tv_sec = mktime(&the_end_tm);
    the_end.tv_nsec = 0;

    return the_end;
}

static void unlock_routine(void* arg)
{
    pthread_mutex_t* mutex = arg;
    pthread_mutex_unlock(mutex);
}
