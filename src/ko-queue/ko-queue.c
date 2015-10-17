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
#define _GNU_SOURCE

#include "config.h"

#include "linted/ko.h"
#include "linted/ko-queue.h"
#include "linted/mem.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <pthread.h>
#include <sys/eventfd.h>
#include <unistd.h>

#if defined _POSIX_SPIN_LOCKS
typedef pthread_spinlock_t spinlock;
#else
typedef pthread_mutex_t spinlock;
#endif

static inline void spinlock_init(spinlock *lock);
static inline void spinlock_destroy(spinlock *lock);
static inline linted_error spinlock_lock(spinlock *lock);
static inline linted_error spinlock_unlock(spinlock *lock);

struct linted_ko_queue {
	struct linted_queue_node *head;
	struct linted_queue_node **tailp;
	spinlock lock;
	int waiter_fd;
};

void linted_ko_queue_node(struct linted_queue_node *node)
{
	node->next = 0;
}

linted_error linted_ko_queue_create(struct linted_ko_queue **queuep)
{
	linted_error err = 0;

	struct linted_ko_queue *queue;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *queue);
		if (err != 0)
			return err;
		queue = xx;
	}

	queue->head = 0;
	queue->tailp = &queue->head;

	spinlock_init(&queue->lock);

	int waiter_fd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
	if (-1 == waiter_fd) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto free_queue;
	}
	queue->waiter_fd = waiter_fd;

	*queuep = queue;

	return 0;

free_queue:
	linted_mem_free(queue);

	return err;
}

void linted_ko_queue_destroy(struct linted_ko_queue *queue)
{
	spinlock_destroy(&queue->lock);

	linted_ko_close(queue->waiter_fd);

	linted_mem_free(queue);
}

/* Attach to the tail */
void linted_ko_queue_send(struct linted_ko_queue *queue,
                          struct linted_queue_node *node)
{
	linted_error err = 0;

	/* Guard against double insertions */
	LINTED_ASSERT(0 == node->next);

	spinlock_lock(&queue->lock);

	*queue->tailp = node;

	queue->tailp = &node->next;

	spinlock_unlock(&queue->lock);

	for (;;) {
		uint64_t xx = 0xFF;
		if (-1 == write(queue->waiter_fd, &xx, sizeof xx)) {
			err = errno;
			LINTED_ASSERT(err != 0);
			if (EINTR == err)
				continue;

			LINTED_ASSERT(false);
		}
		break;
	}
}

linted_error linted_ko_queue_try_recv(struct linted_ko_queue *queue,
                                      struct linted_queue_node **nodep)
{
	linted_error err = 0;

	spinlock_lock(&queue->lock);

	/* No cancellation points in the critical section */

	/* The nodes next to the tip are the head */
	struct linted_queue_node *removed = queue->head;
	if (0 == removed) {
		err = LINTED_ERROR_AGAIN;
		goto unlock_mutex;
	}

	if (queue->tailp == &removed->next)
		queue->tailp = &queue->head;

	queue->head = removed->next;

unlock_mutex:
	spinlock_unlock(&queue->lock);

	if (0 == err) {
		/* Refresh the node for reuse later */
		linted_queue_node(removed);

		*nodep = removed;
	}

	return err;
}

linted_ko linted_ko_queue_ko(struct linted_ko_queue *queue)
{
	return queue->waiter_fd;
}

#if defined _POSIX_SPIN_LOCKS
static inline void spinlock_init(spinlock *lock)
{
	pthread_spin_init(lock, false);
}

static inline void spinlock_destroy(spinlock *lock)
{
	pthread_spin_destroy(lock);
}

static inline linted_error spinlock_lock(spinlock *lock)
{
	return pthread_spin_lock(lock);
}

static inline linted_error spinlock_unlock(spinlock *lock)
{
	return pthread_spin_unlock(lock);
}
#else
static inline void spinlock_init(spinlock *lock)
{
	pthread_mutex_init(lock, 0);
}

static inline void spinlock_destroy(spinlock *lock)
{
	pthread_mutex_destroy(lock);
}

static inline linted_error spinlock_lock(spinlock *lock)
{
	return pthread_mutex_lock(lock);
}

static inline linted_error spinlock_unlock(spinlock *lock)
{
	return pthread_mutex_unlock(lock);
}
#endif
