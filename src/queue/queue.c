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

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void unlock_routine(void *arg);

void linted_queue_node(struct linted_queue_node *node)
{
    node->prev = NULL;
    node->next = NULL;
}

linted_error linted_queue_create(struct linted_queue *queue)
{
    struct linted_queue_node *tip = malloc(sizeof *tip);
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

void linted_queue_destroy(struct linted_queue *queue)
{
    pthread_cond_destroy(&queue->gains_member);
    pthread_mutex_destroy(&queue->lock);

    free(queue->tip);
}

void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
    assert(NULL == node->next);
    assert(NULL == node->prev);

    /* Guard against double insertions */
    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    struct linted_queue_node *tip = queue->tip;

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

    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    struct linted_queue_node *tip = queue->tip;

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
    int old_cancel_state;
    struct linted_queue_node *head;

    pthread_mutex_lock(&queue->lock);
    pthread_cleanup_push(unlock_routine, &queue->lock);

    struct linted_queue_node *tip = queue->tip;

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
