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

#include "linted/channel.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/sched.h"
#include "linted/trigger.h"
#include "linted/util.h"

#include <stdatomic.h>
#include <stdint.h>

typedef _Atomic(void *) atomic_voidptr;

struct linted_channel {
	atomic_voidptr value;
	struct linted_trigger filled;
};

linted_error linted_channel_create(struct linted_channel **channelp)
{
	linted_error err;
	struct linted_channel *channel;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *channel);
		if (err != 0)
			return err;
		channel = xx;
	}

	atomic_voidptr ptr = ATOMIC_VAR_INIT((void *)0);
	channel->value = ptr;
	linted_trigger_create(&channel->filled);

	*channelp = channel;

	return 0;
}

void linted_channel_destroy(struct linted_channel *channel)
{
	linted_trigger_destroy(&channel->filled);

	linted_mem_free(channel);
}

linted_error linted_channel_try_send(struct linted_channel *channel,
                                     void *node)
{
	LINTED_ASSERT_NOT_NULL(channel);
	LINTED_ASSERT_NOT_NULL(node);

	atomic_thread_fence(memory_order_release);

	void *expected = 0;
	if (!atomic_compare_exchange_strong_explicit(
	        &channel->value, &expected, node, memory_order_acq_rel,
	        memory_order_acquire))
		return LINTED_ERROR_AGAIN;

	linted_trigger_set(&channel->filled);

	return 0;
}

/* Remove from the head */
void linted_channel_recv(struct linted_channel *channel, void **nodep)
{
	LINTED_ASSERT_NOT_NULL(channel);
	LINTED_ASSERT_NOT_NULL(nodep);

	void *node;
	for (;;) {
		for (uint_fast8_t ii = 0U; ii < 20U; ++ii) {
			node = atomic_exchange_explicit(
			    &channel->value, 0, memory_order_acq_rel);
			if (node != 0)
				goto exit_loop;
			linted_sched_light_yield();
		}

		linted_trigger_wait(&channel->filled);
	}
exit_loop:
	atomic_thread_fence(memory_order_acquire);

	*nodep = node;
}
