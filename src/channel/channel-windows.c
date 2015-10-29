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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <stdbool.h>

#include <windows.h>

struct linted_channel {
	CRITICAL_SECTION lock;
	CONDITION_VARIABLE filled;
	void **waiter;
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

	channel->waiter = 0;

	InitializeCriticalSection(&channel->lock);
	InitializeConditionVariable(&channel->filled);

	*channelp = channel;

	return 0;
}

void linted_channel_destroy(struct linted_channel *channel)
{
	DeleteCriticalSection(&channel->lock);

	linted_mem_free(channel);
}

linted_error linted_channel_try_send(struct linted_channel *channel,
                                     void *node)
{
	linted_error err = 0;

	LINTED_ASSERT(channel != 0);
	LINTED_ASSERT(node != 0);

	EnterCriticalSection(&channel->lock);

	void **waiter = channel->waiter;
	if (0 == waiter) {
		err = LINTED_ERROR_AGAIN;
		goto unlock_mutex;
	}

	*waiter = node;
	channel->waiter = 0;

	/* Not a cancellation point */
	WakeConditionVariable(&channel->filled);

unlock_mutex:
	LeaveCriticalSection(&channel->lock);

	return err;
}

/* Remove from the head */
void linted_channel_recv(struct linted_channel *channel, void **nodep)
{
	LINTED_ASSERT(channel != 0);
	LINTED_ASSERT(nodep != 0);

	EnterCriticalSection(&channel->lock);

	LINTED_ASSERT(0 == channel->waiter);

	*nodep = 0;
	channel->waiter = nodep;

	do {
		if (!SleepConditionVariableCS(
		        &channel->filled, &channel->lock, INFINITE)) {
			LINTED_ASSERT(false);
		}
	} while (0 == *nodep);

	LeaveCriticalSection(&channel->lock);
}
