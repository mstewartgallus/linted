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
#define _WIN32_WINNT 0x0600

#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <windows.h>
#include <stdbool.h>
#include <string.h>

struct linted_queue
{
	struct linted_queue_node tip;
	CRITICAL_SECTION lock;
	CONDITION_VARIABLE gains_member;
};

void linted_queue_node(struct linted_queue_node *node)
{
	node->prev = NULL;
	node->next = NULL;
}

linted_error linted_queue_create(struct linted_queue **restrict queuep)
{
	linted_error errnum;
	struct linted_queue *queue;
	{
		void *xx;
		if ((errnum = linted_mem_alloc(&xx, sizeof *queue)) != 0) {
			return errnum;
		}
		queue = xx;
	}

	struct linted_queue_node *tip = &queue->tip;

	tip->prev = tip;
	tip->next = tip;

	if (0 == InitializeCriticalSectionAndSpinCount(&queue->lock, 4000)) {
		return GetLastError();
	}

	InitializeConditionVariable(&queue->gains_member);

	*queuep = queue;

	return 0;
}

void linted_queue_destroy(struct linted_queue *restrict queue)
{
	DeleteCriticalSection(&queue->lock);

	linted_mem_free(queue);
}

void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
	/* Guard against double insertions */
	assert(NULL == node->next);
	assert(NULL == node->prev);

	struct linted_queue_node *tip = &queue->tip;

	EnterCriticalSection(&queue->lock);

	/* The nodes previous to the tip are the tail */
	struct linted_queue_node *tail = tip->prev;
	tail->next = node;
	node->prev = tail;
	node->next = tip;
	tip->prev = node;

	WakeConditionVariable(&queue->gains_member);

	LeaveCriticalSection(&queue->lock);
}

void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **nodep)
{
	struct linted_queue_node *head;

	struct linted_queue_node *tip = &queue->tip;

	EnterCriticalSection(&queue->lock);

	/* The nodes next to the tip are the head */
	for (;;) {
		head = tip->next;
		if (head != tip) {
			break;
		}

		if (!SleepConditionVariableCS(&queue->gains_member,
		                              &queue->lock, INFINITE)) {
			LINTED_ASSUME_UNREACHABLE();
		}
	}

	struct linted_queue_node *next = head->next;
	tip->next = next;
	next->prev = tip;

	LeaveCriticalSection(&queue->lock);

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

	EnterCriticalSection(&queue->lock);

	/* The nodes next to the tip are the head */
	head = tip->next;
	if (head == tip) {
		errnum = EAGAIN;
		goto leave_section;
	}

	struct linted_queue_node *next = head->next;
	tip->next = next;
	next->prev = tip;

leave_section:
	LeaveCriticalSection(&queue->lock);

	if (0 == errnum) {
		/* Refresh the node for reuse later */
		linted_queue_node(head);

		*nodep = head;
	}

	return errnum;
}
