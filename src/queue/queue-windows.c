/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <windows.h>

struct linted_queue
{
	CRITICAL_SECTION lock;
	CONDITION_VARIABLE gains_member;
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

	InitializeCriticalSection(&queue->lock);
	InitializeConditionVariable(&queue->gains_member);

	*queuep = queue;

	return 0;
}

void linted_queue_destroy(struct linted_queue *queue)
{
	DeleteCriticalSection(&queue->lock);

	linted_mem_free(queue);
}

void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
	/* Guard against double insertions */
	assert(0 == node->next);

	EnterCriticalSection(&queue->lock);

	*queue->tailp = node;

	queue->tailp = &node->next;

	WakeConditionVariable(&queue->gains_member);

	LeaveCriticalSection(&queue->lock);
}

void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **nodep)
{
	struct linted_queue_node *removed;

	CRITICAL_SECTION *lock = &queue->lock;
	CONDITION_VARIABLE *gains_member = &queue->gains_member;

	EnterCriticalSection(lock);

	/* The nodes next to the tip are the head */
	removed = queue->head;
	if (removed != 0)
		goto got_node;

	do {
		if (!SleepConditionVariableCS(gains_member, lock,
		                              INFINITE)) {
			assert(false);
		}

		removed = queue->head;
	} while (removed == 0);
got_node:

	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

	LeaveCriticalSection(lock);

	/* Refresh the node for reuse later */
	linted_queue_node(removed);

	*nodep = removed;
}

linted_error linted_queue_try_recv(struct linted_queue *queue,
                                   struct linted_queue_node **nodep)
{
	linted_error err = 0;
	struct linted_queue_node *removed;

	CRITICAL_SECTION *lock = &queue->lock;

	EnterCriticalSection(lock);

	/* The nodes next to the tip are the head */
	removed = queue->head;
	if (0 == removed) {
		err = LINTED_ERROR_AGAIN;
		goto leave_section;
	}

	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

leave_section:
	LeaveCriticalSection(lock);

	if (0 == err) {
		/* Refresh the node for reuse later */
		linted_queue_node(removed);

		*nodep = removed;
	}

	return err;
}
