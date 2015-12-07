/*
 * Copyright 2015 Steven Stewart-Gallus
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
#include "config.h"

#include "lntd/stack.h"

#include "lntd/error.h"
#include "lntd/mem.h"
#include "lntd/node.h"
#include "lntd/sched.h"
#include "lntd/trigger.h"

#include <errno.h>
#include <stdatomic.h>
#include <stdint.h>

typedef _Atomic(struct lntd_node *) atomic_node;

struct lntd_stack {
	atomic_node inbox;
	struct lntd_trigger inbox_filled;

	char __padding[64U - sizeof(struct lntd_trigger)];

	struct lntd_node *outbox;
};

static inline void refresh_node(struct lntd_node *node)
{
	node->next = 0;
}

lntd_error lntd_stack_create(struct lntd_stack **stackp)
{
	lntd_error err = 0;

	struct lntd_stack *stack;
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

	lntd_trigger_create(&stack->inbox_filled);

	*stackp = stack;

	return 0;
}

void lntd_stack_destroy(struct lntd_stack *stack)
{
	lntd_trigger_destroy(&stack->inbox_filled);

	lntd_mem_free(stack);
}

void lntd_stack_send(struct lntd_stack *stack, struct lntd_node *node)
{
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

	lntd_trigger_set(&stack->inbox_filled);
}

void lntd_stack_recv(struct lntd_stack *stack, struct lntd_node **nodep)
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

	for (;;) {
		for (uint_fast8_t ii = 0U; ii < 20U; ++ii) {
			ret = atomic_exchange_explicit(
			    &stack->inbox, 0, memory_order_relaxed);
			if (ret != 0)
				goto put_on_outbox;

			lntd_sched_light_yield();
		}

		lntd_trigger_wait(&stack->inbox_filled);
	}

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
}

lntd_error lntd_stack_try_recv(struct lntd_stack *stack,
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
