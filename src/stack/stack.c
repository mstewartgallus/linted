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

#include "linted/stack.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/node.h"
#include "linted/trigger.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>

#include <emmintrin.h>

struct linted_stack {
	struct linted_node *inbox;
	struct linted_node *outbox;
	struct linted_trigger inbox_filled;
};

static void refresh_node(struct linted_node *node)
{
	node->next = 0;
}

linted_error linted_stack_create(struct linted_stack **stackp)
{
	linted_error err = 0;

	struct linted_stack *stack;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *stack);
		if (err != 0)
			return err;
		stack = xx;
	}

	stack->inbox = 0;
	stack->outbox = 0;

	linted_trigger_create(&stack->inbox_filled);

	*stackp = stack;

	return 0;
}

void linted_stack_destroy(struct linted_stack *stack)
{
	linted_trigger_destroy(&stack->inbox_filled);

	linted_mem_free(stack);
}

void linted_stack_send(struct linted_stack *stack,
                       struct linted_node *node)
{
	for (;;) {
		struct linted_node *next =
		    __atomic_load_n(&stack->inbox, __ATOMIC_ACQUIRE);

		node->next = next;

		__atomic_thread_fence(__ATOMIC_RELEASE);

		if (__atomic_compare_exchange_n(
		        &stack->inbox, &next, node, true,
		        __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
			break;
		}
	}

	linted_trigger_set(&stack->inbox_filled);
}

void linted_stack_recv(struct linted_stack *stack,
                       struct linted_node **nodep)
{
	linted_error err = 0;

	for (;;) {
		for (uint_fast8_t ii = 0U; ii < 20U; ++ii) {
			err = linted_stack_try_recv(stack, nodep);
			if (0 == err)
				return;

			_mm_pause();
		}

		linted_trigger_wait(&stack->inbox_filled);
	}
}

linted_error linted_stack_try_recv(struct linted_stack *stack,
                                   struct linted_node **nodep)
{
	{
		struct linted_node *node = __atomic_exchange_n(
		    &stack->inbox, 0, __ATOMIC_ACQ_REL);

		__atomic_thread_fence(__ATOMIC_ACQUIRE);

		for (;;) {
			if (0 == node)
				break;

			struct linted_node *next = node->next;

			node->next = stack->outbox;
			stack->outbox = node;

			node = next;
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
