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

linted_error linted_queue_create(struct linted_queue **restrict queuep)
{
	linted_error errnum;
	struct linted_queue *queue;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *queue);
		if (errnum != 0)
			return errnum;
		queue = xx;
	}

	struct linted_queue_node *tip = &queue->tip;

	tip->prev = tip;
	tip->next = tip;

	{
		pthread_mutexattr_t attr;

		errnum = pthread_mutexattr_init(&attr);
		if (errnum != 0)
			goto free_queue;

#if !defined NDBEBUG
		errnum =
		    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
		assert(errnum != EINVAL);
		if (errnum != 0)
			goto destroy_attr;
#endif

		errnum = pthread_mutex_init(&queue->lock, &attr);
		assert(errnum != EINVAL);

	destroy_attr:
		;
		linted_error dest_errnum = pthread_mutexattr_destroy(&attr);
		if (0 == errnum)
			errnum = dest_errnum;

		if (errnum != 0)
			goto free_queue;
	}

	errnum = pthread_cond_init(&queue->gains_member, NULL);
	if (errnum != 0) {
		assert(errnum != EINVAL);
		assert(false);
	}

	*queuep = queue;

	return 0;

free_queue:
	linted_mem_free(queue);

	return errnum;
}

void linted_queue_destroy(struct linted_queue *restrict queue)
{
	linted_error errnum;

	errnum = pthread_cond_destroy(&queue->gains_member);
	if (errnum != 0) {
		assert(errnum != EBUSY);
		assert(false);
	}

	errnum = pthread_mutex_destroy(&queue->lock);
	if (errnum != 0) {
		assert(errnum != EBUSY);
		assert(false);
	}

	linted_mem_free(queue);
}

void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
	linted_error errnum;

	/* Guard against double insertions */
	assert(NULL == node->next);
	assert(NULL == node->prev);

	struct linted_queue_node *tip = &queue->tip;
	pthread_mutex_t *lock = &queue->lock;
	pthread_cond_t *gains_member = &queue->gains_member;

	node->next = tip;

	errnum = pthread_mutex_lock(lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	/* The nodes previous to the tip are the tail */
	struct linted_queue_node *tail = tip->prev;
	tail->next = node;
	node->prev = tail;
	tip->prev = node;

	/* Not a cancellation point */
	pthread_cond_signal(gains_member);

	errnum = pthread_mutex_unlock(lock);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}
}

void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **nodep)
{
	linted_error errnum;
	struct linted_queue_node *head;

	struct linted_queue_node *tip = &queue->tip;
	pthread_mutex_t *lock = &queue->lock;
	pthread_cond_t *gains_member = &queue->gains_member;

	errnum = pthread_mutex_lock(lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	/* The nodes next to the tip are the head */
	head = tip->next;
	if (head != tip)
		goto got_node;

	/* The slow path, only bother to push the cancellation point
	 * handler if we ever start waiting.*/
	pthread_cleanup_push(unlock_routine, lock);
	do {
		errnum = pthread_cond_wait(gains_member, lock);
		if (errnum != 0) {
			assert(errnum != EINVAL);
			assert(false);
		}

		head = tip->next;
	} while (tip == head);
	pthread_cleanup_pop(false);
got_node:
	;

	struct linted_queue_node *next = head->next;
	tip->next = next;
	next->prev = tip;

	/* The fast path doesn't bother with the handler */

	errnum = pthread_mutex_unlock(lock);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}

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
	pthread_mutex_t *lock = &queue->lock;

	errnum = pthread_mutex_lock(lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	/* No cancellation points in the critical section */

	/* The nodes next to the tip are the head */
	head = tip->next;
	if (head == tip) {
		errnum = EAGAIN;
		goto pop_cleanup;
	}

	struct linted_queue_node *next = head->next;
	tip->next = next;
	next->prev = tip;

pop_cleanup : {
	linted_error unlock_errnum = pthread_mutex_unlock(lock);
	if (unlock_errnum != 0) {
		assert(unlock_errnum != EPERM);
		assert(false);
	}
}

	if (0 == errnum) {
		/* Refresh the node for reuse later */
		linted_queue_node(head);

		*nodep = head;
	}

	return errnum;
}

static void unlock_routine(void *mutex)
{
	linted_error errnum;

	errnum = pthread_mutex_unlock(mutex);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}
}
