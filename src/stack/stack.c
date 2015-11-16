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
#define _GNU_SOURCE

#include "config.h"

#include "linted/stack.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>

#include <linux/futex.h>
#include <immintrin.h>

struct trigger {
	int triggered;
};

static void trigger_init(struct trigger *trigger);
static void trigger_set(struct trigger *trigger);
static void trigger_wait(struct trigger *trigger);

struct linted_stack {
	struct linted_queue_node *inbox;
	struct linted_queue_node *outbox;
	struct trigger inbox_filled;
};

static linted_error wait_until_different(int const *uaddr, int val);
static linted_error hint_wakeup(int const *uaddr);

static linted_error futex_wait(int const *uaddr, int val,
                               struct timespec const *timeout);
static linted_error futex_wake(unsigned *restrict wokeupp,
                               int const *uaddr, int val);

static void refresh_node(struct linted_queue_node *node)
{
	node->next = 0;
}

linted_error linted_stack_create(struct linted_stack **stackp)
{
	linted_error err;
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

	trigger_init(&stack->inbox_filled);

	*stackp = stack;

	return 0;
}

void linted_stack_destroy(struct linted_stack *stack)
{
	linted_mem_free(stack);
}

void linted_stack_send(struct linted_stack *stack,
                       struct linted_queue_node *node)
{
	struct linted_queue_node *next;

	__atomic_thread_fence(__ATOMIC_RELEASE);

	for (;;) {
		next = __atomic_load_n(&stack->inbox, __ATOMIC_ACQUIRE);

		node->next = next;

		if (__atomic_compare_exchange_n(
		        &stack->inbox, &next, node, false,
		        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
			break;
		}
	}

	trigger_set(&stack->inbox_filled);
}

/* Remove from the head */
void linted_stack_recv(struct linted_stack *stack,
                       struct linted_queue_node **nodep)
{
	linted_error err = 0;

	for (;;) {
		for (uint_fast8_t ii = 0U; ii < 20U; ++ii) {
			err = linted_stack_try_recv(stack, nodep);
			if (0 == err)
				return;

			_mm_pause();
		}

		trigger_wait(&stack->inbox_filled);
	}
}

linted_error linted_stack_try_recv(struct linted_stack *stack,
                                   struct linted_queue_node **nodep)
{
	for (;;) {
		struct linted_queue_node *node;

		node = __atomic_load_n(&stack->inbox, __ATOMIC_ACQUIRE);
		if (0 == node)
			break;

		struct linted_queue_node *next =
		    __atomic_load_n(&node->next, __ATOMIC_ACQUIRE);

		if (__atomic_compare_exchange_n(
		        &stack->inbox, &node, next, false,
		        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
			node->next = stack->outbox;
			stack->outbox = node;
		}
	}

	struct linted_queue_node *node = stack->outbox;
	if (0 == node)
		return EAGAIN;

	__atomic_thread_fence(__ATOMIC_ACQUIRE);

	stack->outbox = node->next;

	refresh_node(node);

	*nodep = node;
	return 0;
}

static void trigger_init(struct trigger *trigger)
{
	trigger->triggered = 0;
}

static void trigger_set(struct trigger *trigger)
{
	__atomic_store_n(&trigger->triggered, 0, __ATOMIC_RELEASE);
	hint_wakeup(&trigger->triggered);
}

static void trigger_wait(struct trigger *trigger)
{
	wait_until_different(&trigger->triggered, 0);

	__atomic_store_n(&trigger->triggered, 0, __ATOMIC_RELEASE);
}

static linted_error wait_until_different(int const *uaddr, int val)
{
	return futex_wait(uaddr, val, NULL);
}

static linted_error hint_wakeup(int const *uaddr)
{
	return futex_wake(NULL, uaddr, 1);
}

static linted_error futex_wait(int const *uaddr, int val,
                               struct timespec const *timeout)
{
	int xx =
	    syscall(__NR_futex, (intptr_t)uaddr, (intptr_t)FUTEX_WAIT,
	            (intptr_t)val, (intptr_t)timeout);
	if (xx < 0) {
		return errno;
	}

	return 0;
}

static linted_error futex_wake(unsigned *restrict wokeupp,
                               int const *uaddr, int val)
{
	int xx = syscall(__NR_futex, (intptr_t)uaddr,
	                 (intptr_t)FUTEX_WAKE, (intptr_t)val);
	if (xx < 0) {
		return errno;
	}

	if (wokeupp != NULL) {
		*wokeupp = xx;
	}
	return 0;
}
