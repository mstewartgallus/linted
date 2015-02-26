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
#ifndef LINTED_QUEUE_H
#define LINTED_QUEUE_H

#include "linted/error.h"

/**
 * @file
 *
 * Mediates one-by-one communication between threads.
 *
 * @warning Is not cancellation safe.
 */
struct linted_queue;

struct linted_queue_node
{
	struct linted_queue_node *next;
};

void linted_queue_node(struct linted_queue_node *node);

linted_error linted_queue_create(struct linted_queue **queuep);

/**
 * @warning It is the responsibility of the caller to fetch and
 * destroy all nodes in the queue.
 */
void linted_queue_destroy(struct linted_queue *queue);

void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node);

void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **node);

linted_error linted_queue_try_recv(struct linted_queue *queue,
                                   struct linted_queue_node **node);

#endif /* LINTED_QUEUE_H */
