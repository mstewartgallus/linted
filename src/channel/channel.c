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

#include "lntd/channel.h"

#include "lntd/error.h"
#include "lntd/mem.h"
#include "lntd/sched.h"
#include "lntd/trigger.h"
#include "lntd/util.h"

#include <stdatomic.h>
#include <stdint.h>

typedef _Atomic(void *) atomic_voidptr;

struct lntd_channel {
	atomic_voidptr value;
	struct lntd_trigger filled;
};

lntd_error lntd_channel_create(struct lntd_channel **channelp)
{
	lntd_error err;
	struct lntd_channel *channel;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *channel);
		if (err != 0)
			return err;
		channel = xx;
	}

	atomic_voidptr ptr = ATOMIC_VAR_INIT((void *)0);
	channel->value = ptr;
	lntd_trigger_create(&channel->filled);

	*channelp = channel;

	return 0;
}

void lntd_channel_destroy(struct lntd_channel *channel)
{
	lntd_trigger_destroy(&channel->filled);

	lntd_mem_free(channel);
}

lntd_error lntd_channel_try_send(struct lntd_channel *channel,
                                 void *node)
{
	LNTD_ASSERT_NOT_NULL(channel);
	LNTD_ASSERT_NOT_NULL(node);

	void *expected = 0;

	atomic_thread_fence(memory_order_release);

	if (!atomic_compare_exchange_strong_explicit(
	        &channel->value, &expected, node, memory_order_relaxed,
	        memory_order_relaxed))
		return LNTD_ERROR_AGAIN;

	lntd_trigger_set(&channel->filled);

	return 0;
}

/* Remove from the head */
void lntd_channel_recv(struct lntd_channel *channel, void **nodep)
{
	LNTD_ASSERT_NOT_NULL(channel);
	LNTD_ASSERT_NOT_NULL(nodep);

	void *node;
	for (;;) {
		for (uint_fast8_t ii = 0U; ii < 20U; ++ii) {
			node = atomic_exchange_explicit(
			    &channel->value, 0, memory_order_relaxed);
			if (node != 0)
				goto exit_loop;
			lntd_sched_light_yield();
		}

		lntd_trigger_wait(&channel->filled);
	}
exit_loop:
	atomic_thread_fence(memory_order_acquire);

	*nodep = node;
}
