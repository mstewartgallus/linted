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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <string.h>

struct linted_queue
{
    struct linted_queue_node tip;
    pthread_mutex_t lock;
    pthread_cond_t gains_member;
};

static void unlock_routine(void *arg);

void linted_queue_node(struct linted_queue_node *node)
{
    node->prev = NULL;
    node->next = NULL;
}

linted_error linted_queue_create(struct linted_queue **queuep)
{
    linted_error errnum;
    struct linted_queue *queue = linted_mem_alloc(&errnum, sizeof *queue);
    if (errnum != 0) {
        return errnum;
    }

    struct linted_queue_node *tip = &queue->tip;

    tip->prev = tip;
    tip->next = tip;

    pthread_mutex_init(&queue->lock, NULL);
    pthread_cond_init(&queue->gains_member, NULL);

    *queuep = queue;

    return 0;
}

void linted_queue_destroy(struct linted_queue *queue)
{
    pthread_cond_destroy(&queue->gains_member);
    pthread_mutex_destroy(&queue->lock);

    linted_mem_free(queue);
}

void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
    assert(NULL == node->next);
    assert(NULL == node->prev);

    struct linted_queue_node *tip = &queue->tip;

    /* Guard against double insertions */
    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    /* The nodes previous to the tip are the tail */
    struct linted_queue_node *tail = tip->prev;
    tail->next = node;
    node->prev = tail;
    node->next = tip;
    tip->prev = node;

    pthread_cond_broadcast(&queue->gains_member);

    pthread_cleanup_pop(true);
}

void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **nodep)
{
    struct linted_queue_node *head;

    struct linted_queue_node *tip = &queue->tip;

    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    /* The nodes next to the tip are the head */
    for (;;) {
        head = tip->next;
        if (head != tip) {
            break;
        }

        pthread_cond_wait(&queue->gains_member, &queue->lock);
    }

    struct linted_queue_node *next = head->next;
    tip->next = next;
    next->prev = tip;

    pthread_cleanup_pop(true);

    /* Refresh the node for reuse later */
    linted_queue_node(head);

    *nodep = head;
}

linted_error linted_queue_try_recv(struct linted_queue *queue,
                                   struct linted_queue_node **nodep)
{
    linted_error errnum = 0;
    struct linted_queue_node *head;

    struct linted_queue_node *tip = &queue->tip;

    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    /* The nodes next to the tip are the head */
    head = tip->next;
    if (head == tip) {
        errnum = EAGAIN;
        goto pop_cleanup;
    }

    struct linted_queue_node *next = head->next;
    tip->next = next;
    next->prev = tip;

pop_cleanup:
    pthread_cleanup_pop(true);

    if (0 == errnum) {
        /* Refresh the node for reuse later */
        linted_queue_node(head);

        *nodep = head;
    }

    return errnum;
}

static void unlock_routine(void *arg)
{
    pthread_mutex_t *mutex = arg;
    pthread_mutex_unlock(mutex);
}
