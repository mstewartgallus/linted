/*
 * Copyright 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

struct linted_queue
{
	struct linted_queue_node *head;
	struct linted_queue_node **tailp;
};

void linted_queue_node(struct linted_queue_node *node)
{
	node->next = 0;
}

linted_error linted_queue_create(struct linted_queue **queuep)
{
	linted_error err;
	struct linted_queue *queue;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *queue);
		if (err != 0)
			return err;
		queue = xx;
	}

	queue->head = 0;
	queue->tailp = &queue->head;

	*queuep = queue;

	return 0;
}

void linted_queue_destroy(struct linted_queue *queue)
{
	linted_mem_free(queue);
}

/* Attach to the tail */
void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
	linted_error err;

	assert(queue != 0);
	assert(node != 0);

	/* Guard against double insertions */
	assert(0 == node->next);

	*queue->tailp = node;

	queue->tailp = &node->next;
}

void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **nodep)
{
	/* Unsupported */
	abort();
}

linted_error linted_queue_try_recv(struct linted_queue *queue,
                                   struct linted_queue_node **nodep)
{
	/* No cancellation points in the critical section */

	/* The nodes next to the tip are the head */
	struct linted_queue_node *removed = queue->head;
	if (0 == removed)
		return LINTED_ERROR_AGAIN;

	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

	/* Refresh the node for reuse later */
	linted_queue_node(removed);

	*nodep = removed;

	return 0;
}
