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

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/ko-stack.h"
#include "linted/mem.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/eventfd.h>
#include <unistd.h>

struct linted_ko_stack {
	struct linted_queue_node *root;
	int waiter_fd;
};

static void refresh_node(struct linted_queue_node *node)
{
	node->next = 0;
}

linted_error linted_ko_stack_create(struct linted_ko_stack **queuep)
{
	linted_error err = 0;

	struct linted_ko_stack *queue;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *queue);
		if (err != 0)
			return err;
		queue = xx;
	}

	queue->root = 0;

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

void linted_ko_stack_destroy(struct linted_ko_stack *queue)
{
	linted_ko_close(queue->waiter_fd);

	linted_mem_free(queue);
}

/* Attach to the tail */
void linted_ko_stack_send(struct linted_ko_stack *queue,
                          struct linted_queue_node *node)
{
	linted_error err = 0;

	/* Guard against double insertions */
	LINTED_ASSERT(0 == node->next);

	linted_ko waiter_fd = queue->waiter_fd;
	struct linted_queue_node *next;

	__atomic_thread_fence(__ATOMIC_RELEASE);

	for (;;) {
		next = __atomic_load_n(&queue->root, __ATOMIC_ACQUIRE);

		node->next = next;

		if (__atomic_compare_exchange_n(
		        &queue->root, &next, node, false,
		        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
			break;
		}
	}

	for (;;) {
		uint64_t xx = 0xFF;
		if (-1 == write(waiter_fd, &xx, sizeof xx)) {
			err = errno;
			LINTED_ASSERT(err != 0);
			if (EINTR == err)
				continue;

			LINTED_ASSERT(false);
		}
		break;
	}
}

linted_error linted_ko_stack_try_recv(struct linted_ko_stack *queue,
                                      struct linted_queue_node **nodep)
{
	struct linted_queue_node *node;
	for (;;) {
		node = __atomic_load_n(&queue->root, __ATOMIC_ACQUIRE);
		if (0 == node)
			return EAGAIN;

		struct linted_queue_node *next =
		    __atomic_load_n(&node->next, __ATOMIC_ACQUIRE);

		if (__atomic_compare_exchange_n(
		        &queue->root, &node, next, false,
		        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
			break;
		}
	}

	__atomic_thread_fence(__ATOMIC_ACQUIRE);

	refresh_node(node);

	*nodep = node;

	return 0;
}

linted_ko linted_ko_stack_ko(struct linted_ko_stack *queue)
{
	return queue->waiter_fd;
}
