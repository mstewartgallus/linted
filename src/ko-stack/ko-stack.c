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
#include "linted/node.h"
#include "linted/stack.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/eventfd.h>
#include <unistd.h>

struct linted_ko_stack {
	struct linted_node *inbox;
	struct linted_node *outbox;
	int waiter_fd;
};

static void refresh_node(struct linted_node *node)
{
	node->next = 0;
}

linted_error linted_ko_stack_create(struct linted_ko_stack **stackp)
{
	linted_error err = 0;

	struct linted_ko_stack *stack;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *stack);
		if (err != 0)
			return err;
		stack = xx;
	}

	stack->inbox = 0;
	stack->outbox = 0;

	int waiter_fd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
	if (-1 == waiter_fd) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto free_stack;
	}
	stack->waiter_fd = waiter_fd;

	*stackp = stack;

	return 0;

free_stack:
	linted_mem_free(stack);

	return err;
}

void linted_ko_stack_destroy(struct linted_ko_stack *stack)
{
	linted_ko_close(stack->waiter_fd);

	linted_mem_free(stack);
}

/* Attach to the tail */
void linted_ko_stack_send(struct linted_ko_stack *stack,
                          struct linted_node *node)
{
	linted_error err = 0;

	refresh_node(node);

	linted_ko waiter_fd = stack->waiter_fd;

	__atomic_thread_fence(__ATOMIC_RELEASE);

	for (;;) {
		struct linted_node *next =
		    __atomic_load_n(&stack->inbox, __ATOMIC_ACQUIRE);

		node->next = next;

		if (__atomic_compare_exchange_n(
		        &stack->inbox, &next, node, true,
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

linted_error linted_ko_stack_try_recv(struct linted_ko_stack *stack,
                                      struct linted_node **nodep)
{
	{
		struct linted_node *node;
		for (;;) {
			node = __atomic_load_n(&stack->inbox,
			                       __ATOMIC_ACQUIRE);
			if (0 == node)
				break;

			if (__atomic_compare_exchange_n(
			        &stack->inbox, &node, 0, true,
			        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
				break;
		}

		__atomic_thread_fence(__ATOMIC_ACQUIRE);

		struct linted_node *next;
		for (; node != 0; node = next) {
			next = node->next;
			node->next = stack->outbox;
			stack->outbox = node;
		}
	}

	struct linted_node *node = stack->outbox;
	if (0 == node)
		return EAGAIN;

	stack->outbox = node->next;

	refresh_node(node);

	*nodep = node;

	return 0;
}

linted_ko linted_ko_stack_ko(struct linted_ko_stack *stack)
{
	return stack->waiter_fd;
}
