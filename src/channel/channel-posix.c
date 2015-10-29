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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <pthread.h>
#include <stdbool.h>

#if defined NDEBUG
#define ENABLE_ERRORCHECK 0
#else
#define ENABLE_ERRORCHECK 1
#endif

struct linted_channel {
	pthread_mutex_t lock;
	pthread_cond_t filled;
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

	if (ENABLE_ERRORCHECK) {
		pthread_mutexattr_t attr;

		err = pthread_mutexattr_init(&attr);
		if (err != 0)
			goto free_channel;

		err = pthread_mutexattr_settype(
		    &attr, PTHREAD_MUTEX_ERRORCHECK);
		LINTED_ASSERT(err != EINVAL);
		if (err != 0)
			goto destroy_attr;

		err = pthread_mutex_init(&channel->lock, &attr);
		LINTED_ASSERT(err != EINVAL);

	destroy_attr:
		;
		linted_error dest_err =
		    pthread_mutexattr_destroy(&attr);
		if (0 == err)
			err = dest_err;
	} else {
		err = pthread_mutex_init(&channel->lock, 0);
		LINTED_ASSERT(err != EINVAL);
	}
	if (err != 0)
		goto free_channel;

	err = pthread_cond_init(&channel->filled, 0);
	if (err != 0) {
		LINTED_ASSERT(err != EINVAL);
		LINTED_ASSERT(false);
	}

	*channelp = channel;

	return 0;

free_channel:
	linted_mem_free(channel);

	return err;
}

void linted_channel_destroy(struct linted_channel *channel)
{
	linted_error err;

	err = pthread_cond_destroy(&channel->filled);
	if (err != 0) {
		LINTED_ASSERT(err != EBUSY);
		LINTED_ASSERT(false);
	}

	err = pthread_mutex_destroy(&channel->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EBUSY);
		LINTED_ASSERT(false);
	}

	linted_mem_free(channel);
}

linted_error linted_channel_try_send(struct linted_channel *channel,
                                     void *node)
{
	linted_error err = 0;

	LINTED_ASSERT(channel != 0);
	LINTED_ASSERT(node != 0);

	err = pthread_mutex_lock(&channel->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	void **waiter = channel->waiter;
	if (0 == waiter) {
		err = LINTED_ERROR_AGAIN;
		goto unlock_mutex;
	}

	*waiter = node;
	channel->waiter = 0;

	/* Not a cancellation point */
	pthread_cond_signal(&channel->filled);

unlock_mutex : {
	linted_error unlock_err = pthread_mutex_unlock(&channel->lock);
	if (unlock_err != 0) {
		LINTED_ASSERT(unlock_err != EPERM);
		LINTED_ASSERT(false);
	}
}

	return err;
}

/* Remove from the head */
void linted_channel_recv(struct linted_channel *channel, void **nodep)
{
	linted_error err;

	LINTED_ASSERT(channel != 0);
	LINTED_ASSERT(nodep != 0);

	err = pthread_mutex_lock(&channel->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	LINTED_ASSERT(0 == channel->waiter);

	*nodep = 0;
	channel->waiter = nodep;

	do {
		err =
		    pthread_cond_wait(&channel->filled, &channel->lock);
		if (err != 0) {
			LINTED_ASSERT(err != EINVAL);
			LINTED_ASSERT(false);
		}
	} while (0 == *nodep);

	err = pthread_mutex_unlock(&channel->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}
}
