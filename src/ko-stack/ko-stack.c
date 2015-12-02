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
#include "linted/sched.h"
#include "linted/util.h"

#include <errno.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/eventfd.h>
#include <unistd.h>

typedef _Atomic(struct linted_node *) atomic_node;

struct linted_ko_stack {
	atomic_node inbox;
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

	atomic_node ptr = ATOMIC_VAR_INIT((void *)0);
	stack->inbox = ptr;
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

void linted_ko_stack_send(struct linted_ko_stack *stack,
                          struct linted_node *node)
{
	linted_error err = 0;

	linted_ko waiter_fd = stack->waiter_fd;

	for (;;) {
		struct linted_node *next = atomic_load_explicit(
		    &stack->inbox, memory_order_relaxed);

		node->next = next;

		atomic_thread_fence(memory_order_release);

		if (atomic_compare_exchange_weak_explicit(
		        &stack->inbox, &next, node,
		        memory_order_relaxed, memory_order_relaxed)) {
			break;
		}

		linted_sched_light_yield();
	}

	for (;;) {
		static uint64_t const xx = 0xFF;
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
	struct linted_node *ret = atomic_exchange_explicit(
	    &stack->inbox, 0, memory_order_relaxed);
	if (ret != 0)
		goto put_on_outbox;

	ret = stack->outbox;
	if (ret != 0) {
		stack->outbox = ret->next;
		goto give_node;
	}
	return EAGAIN;

put_on_outbox:
	atomic_thread_fence(memory_order_acquire);

	struct linted_node *start = ret->next;
	if (start != 0) {
		struct linted_node *end = start;
		for (;;) {
			struct linted_node *next = end->next;
			if (0 == next)
				break;
			end = next;
		}
		end->next = stack->outbox;
		stack->outbox = start;
	}

give_node:
	refresh_node(ret);

	*nodep = ret;
	return 0;
}

linted_ko linted_ko_stack_ko(struct linted_ko_stack *stack)
{
	return stack->waiter_fd;
}
