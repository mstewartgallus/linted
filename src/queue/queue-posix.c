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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 200809L

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <pthread.h>
#include <stdbool.h>

#if defined NDEBUG
#define ENABLE_ERRORCHECK 0
#else
#define ENABLE_ERRORCHECK 1
#endif

struct linted_queue
{
	pthread_mutex_t lock;
	pthread_cond_t gains_member;
	struct linted_queue_node *head;
	struct linted_queue_node **tailp;
};

void linted_queue_node(struct linted_queue_node *node)
{
	node->next = 0;
}

linted_error linted_queue_create(struct linted_queue **queuep)
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

	queue->head = 0;
	queue->tailp = &queue->head;

	if (ENABLE_ERRORCHECK) {
		pthread_mutexattr_t attr;

		errnum = pthread_mutexattr_init(&attr);
		if (errnum != 0)
			goto free_queue;

		errnum = pthread_mutexattr_settype(
		    &attr, PTHREAD_MUTEX_ERRORCHECK);
		assert(errnum != EINVAL);
		if (errnum != 0)
			goto destroy_attr;

		errnum = pthread_mutex_init(&queue->lock, &attr);
		assert(errnum != EINVAL);

	destroy_attr:
		;
		linted_error dest_errnum =
		    pthread_mutexattr_destroy(&attr);
		if (0 == errnum)
			errnum = dest_errnum;
	} else {
		errnum = pthread_mutex_init(&queue->lock, 0);
		assert(errnum != EINVAL);
	}
	if (errnum != 0)
		goto free_queue;

	errnum = pthread_cond_init(&queue->gains_member, 0);
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

void linted_queue_destroy(struct linted_queue *queue)
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

/* Attach to the tail */
void linted_queue_send(struct linted_queue *queue,
                       struct linted_queue_node *node)
{
	linted_error errnum;

	/* Guard against double insertions */
	assert(0 == node->next);

	errnum = pthread_mutex_lock(&queue->lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	*queue->tailp = node;

	queue->tailp = &node->next;

	pthread_cond_signal(&queue->gains_member);

	errnum = pthread_mutex_unlock(&queue->lock);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}
}

/* Remove from the head */
void linted_queue_recv(struct linted_queue *queue,
                       struct linted_queue_node **nodep)
{
	linted_error errnum;

	errnum = pthread_mutex_lock(&queue->lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	/* The nodes next to the tip are the head */
	struct linted_queue_node *removed = queue->head;
	if (removed != 0)
		goto got_node;

	do {
		errnum = pthread_cond_wait(&queue->gains_member,
		                           &queue->lock);
		if (errnum != 0) {
			assert(errnum != EINVAL);
			assert(false);
		}

		removed = queue->head;
	} while (removed == 0);

got_node:
	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

	/* The fast path doesn't bother with the handler */

	errnum = pthread_mutex_unlock(&queue->lock);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}

	/* Refresh the node for reuse later */
	linted_queue_node(removed);

	*nodep = removed;
}

linted_error linted_queue_try_recv(struct linted_queue *queue,
                                   struct linted_queue_node **nodep)
{
	linted_error errnum;

	errnum = pthread_mutex_lock(&queue->lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	/* No cancellation points in the critical section */

	/* The nodes next to the tip are the head */
	struct linted_queue_node *removed = queue->head;
	if (0 == removed) {
		errnum = LINTED_ERROR_AGAIN;
		goto unlock_mutex;
	}

	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

unlock_mutex : {
	linted_error unlock_errnum = pthread_mutex_unlock(&queue->lock);
	if (unlock_errnum != 0) {
		assert(unlock_errnum != EPERM);
		assert(false);
	}
}

	if (0 == errnum) {
		/* Refresh the node for reuse later */
		linted_queue_node(removed);

		*nodep = removed;
	}

	return errnum;
}
