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
#ifndef LINTED_LINKED_QUEUE_H
#define LINTED_LINKED_QUEUE_H

#include "linted/error.h"

#include <pthread.h>
#include <stddef.h>

/**
 * @file
 *
 * Mediates one-by-one communication between threads.
 */

struct linted_linked_queue_node;

struct linted_linked_queue
{
    struct linted_linked_queue_node* tip;
    pthread_mutex_t lock;
    pthread_cond_t gains_member;
};

struct linted_linked_queue_node
{
    struct linted_linked_queue_node* prev;
    struct linted_linked_queue_node* next;
    char contents[];
};

linted_error linted_linked_queue_create(struct linted_linked_queue* queue);
void linted_linked_queue_destroy(struct linted_linked_queue* queue);

/**
 * deferred cancellation safe
 */
void linted_linked_queue_send(struct linted_linked_queue* queue,
                              struct linted_linked_queue_node* node);

/**
 * deferred cancellation safe
 */
linted_error linted_linked_queue_recv(struct linted_linked_queue* queue,
                                      struct linted_linked_queue_node** node);

/**
 * deferred cancellation safe
 */
linted_error linted_linked_queue_try_recv(struct linted_linked_queue* queue,
                                          struct linted_linked_queue_node
                                          ** node);

#endif /* LINTED_LINKED_QUEUE_H */
