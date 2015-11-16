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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/node.h"
#include "linted/util.h"

#include <errno.h>
#include <pthread.h>
#include <stdbool.h>

#if defined NDEBUG
#define ENABLE_ERRORCHECK 0
#else
#define ENABLE_ERRORCHECK 1
#endif

struct linted_queue {
	pthread_mutex_t lock;
	pthread_cond_t gains_member;
	struct linted_node *head;
	struct linted_node **tailp;
};

static void refresh_node(struct linted_node *node)
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

	if (ENABLE_ERRORCHECK) {
		pthread_mutexattr_t attr;

		err = pthread_mutexattr_init(&attr);
		if (err != 0)
			goto free_queue;

		err = pthread_mutexattr_settype(
		    &attr, PTHREAD_MUTEX_ERRORCHECK);
		LINTED_ASSERT(err != EINVAL);
		if (err != 0)
			goto destroy_attr;

		err = pthread_mutex_init(&queue->lock, &attr);
		LINTED_ASSERT(err != EINVAL);

	destroy_attr:
		;
		linted_error dest_err =
		    pthread_mutexattr_destroy(&attr);
		if (0 == err)
			err = dest_err;
	} else {
		err = pthread_mutex_init(&queue->lock, 0);
		LINTED_ASSERT(err != EINVAL);
	}
	if (err != 0)
		goto free_queue;

	err = pthread_cond_init(&queue->gains_member, 0);
	if (err != 0) {
		LINTED_ASSERT(err != EINVAL);
		LINTED_ASSERT(false);
	}

	*queuep = queue;

	return 0;

free_queue:
	linted_mem_free(queue);

	return err;
}

void linted_queue_destroy(struct linted_queue *queue)
{
	linted_error err;

	err = pthread_cond_destroy(&queue->gains_member);
	if (err != 0) {
		LINTED_ASSERT(err != EBUSY);
		LINTED_ASSERT(false);
	}

	err = pthread_mutex_destroy(&queue->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EBUSY);
		LINTED_ASSERT(false);
	}

	linted_mem_free(queue);
}

/* Attach to the tail */
void linted_queue_send(struct linted_queue *queue,
                       struct linted_node *node)
{
	refresh_node(node);

	pthread_mutex_t *lockp = &queue->lock;
	pthread_cond_t *gains_memberp = &queue->gains_member;
	struct linted_node ***tailpp = &queue->tailp;

	linted_error err;

	/* Guard against double insertions */
	LINTED_ASSERT(0 == node->next);

	struct linted_node **node_nextp = &node->next;

	err = pthread_mutex_lock(lockp);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	struct linted_node **tailp = *tailpp;

	*tailp = node;
	*tailpp = node_nextp;

	pthread_cond_signal(gains_memberp);

	err = pthread_mutex_unlock(lockp);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}
}

/* Remove from the head */
void linted_queue_recv(struct linted_queue *queue,
                       struct linted_node **nodep)
{
	linted_error err;

	pthread_mutex_t *lockp = &queue->lock;
	pthread_cond_t *gains_memberp = &queue->gains_member;
	struct linted_node **headp = &queue->head;
	struct linted_node ***tailpp = &queue->tailp;

	err = pthread_mutex_lock(lockp);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	/* The nodes next to the tip are the head */
	struct linted_node *removed;
	for (;;) {
		removed = *headp;
		if (removed != 0)
			break;

		err = pthread_cond_wait(gains_memberp, lockp);
		if (err != 0) {
			LINTED_ASSERT(err != EINVAL);
			LINTED_ASSERT(false);
		}
	}

	struct linted_node **nextp = &removed->next;

	struct linted_node **tailp = *tailpp;
	if (tailp == nextp)
		tailp = headp;

	struct linted_node *head = *nextp;

	*headp = head;
	*tailpp = tailp;

	err = pthread_mutex_unlock(lockp);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}

	/* Refresh the node for reuse later */
	refresh_node(removed);

	*nodep = removed;
}

linted_error linted_queue_try_recv(struct linted_queue *queue,
                                   struct linted_node **nodep)
{
	linted_error err;

	err = pthread_mutex_lock(&queue->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	/* No cancellation points in the critical section */

	/* The nodes next to the tip are the head */
	struct linted_node *removed = queue->head;
	if (0 == removed) {
		err = LINTED_ERROR_AGAIN;
		goto unlock_mutex;
	}

	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

unlock_mutex : {
	linted_error unlock_err = pthread_mutex_unlock(&queue->lock);
	if (unlock_err != 0) {
		LINTED_ASSERT(unlock_err != EPERM);
		LINTED_ASSERT(false);
	}
}

	if (0 == err) {
		refresh_node(removed);

		*nodep = removed;
	}

	return err;
}
