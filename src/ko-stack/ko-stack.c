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

#include "lntd/error.h"
#include "lntd/ko-stack.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/node.h"
#include "lntd/sched.h"
#include "lntd/util.h"

#include <errno.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/eventfd.h>
#include <unistd.h>

typedef _Atomic(struct lntd_node *) atomic_node;

struct lntd_ko_stack {
	int waiter_fd;

	char __padding1[64U - sizeof(int)];

	atomic_node inbox;

	char __padding2[64U - sizeof(atomic_node)];

	struct lntd_node *outbox;
};

static inline void refresh_node(struct lntd_node *node)
{
	node->next = 0;
}

lntd_error lntd_ko_stack_create(struct lntd_ko_stack **stackp)
{
	lntd_error err = 0;

	struct lntd_ko_stack *stack;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *stack);
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
		LNTD_ASSUME(err != 0);
		goto free_stack;
	}
	stack->waiter_fd = waiter_fd;

	*stackp = stack;

	return 0;

free_stack:
	lntd_mem_free(stack);

	return err;
}

void lntd_ko_stack_destroy(struct lntd_ko_stack *stack)
{
	lntd_ko_close(stack->waiter_fd);

	lntd_mem_free(stack);
}

void lntd_ko_stack_send(struct lntd_ko_stack *stack,
                        struct lntd_node *node)
{
	lntd_error err = 0;

	lntd_ko waiter_fd = stack->waiter_fd;

	for (;;) {
		struct lntd_node *next = atomic_load_explicit(
		    &stack->inbox, memory_order_relaxed);

		node->next = next;

		atomic_thread_fence(memory_order_release);

		if (atomic_compare_exchange_weak_explicit(
		        &stack->inbox, &next, node,
		        memory_order_relaxed, memory_order_relaxed)) {
			break;
		}

		lntd_sched_light_yield();
	}

	for (;;) {
		static uint64_t const xx = 0xFF;
		if (-1 == write(waiter_fd, &xx, sizeof xx)) {
			err = errno;
			LNTD_ASSERT(err != 0);
			if (EINTR == err)
				continue;

			LNTD_ASSERT(false);
		}
		break;
	}
}

lntd_error lntd_ko_stack_try_recv(struct lntd_ko_stack *stack,
                                  struct lntd_node **nodep)
{
	struct lntd_node *ret = atomic_exchange_explicit(
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

	struct lntd_node *start = ret->next;
	if (start != 0) {
		struct lntd_node *end = start;
		for (;;) {
			struct lntd_node *next = end->next;
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

lntd_ko lntd_ko_stack_ko(struct lntd_ko_stack *stack)
{
	return stack->waiter_fd;
}
