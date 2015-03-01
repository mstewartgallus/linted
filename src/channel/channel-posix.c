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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 200809L

#include "linted/queue.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <pthread.h>
#include <stdbool.h>

#if defined NDEBUG
#define ENABLE_ERRORCHECK 0
#else
#define ENABLE_ERRORCHECK 1
#endif

struct linted_channel
{
	pthread_mutex_t lock;
	pthread_cond_t filled;
	void **waiter;
};

linted_error linted_channel_create(struct linted_channel **channelp)
{
	linted_error errnum;
	struct linted_channel *channel;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *channel);
		if (errnum != 0)
			return errnum;
		channel = xx;
	}

	channel->waiter = 0;

	if (ENABLE_ERRORCHECK) {
		pthread_mutexattr_t attr;

		errnum = pthread_mutexattr_init(&attr);
		if (errnum != 0)
			goto free_channel;

		errnum =
		    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
		assert(errnum != EINVAL);
		if (errnum != 0)
			goto destroy_attr;

		errnum = pthread_mutex_init(&channel->lock, &attr);
		assert(errnum != EINVAL);

	destroy_attr:
		;
		linted_error dest_errnum = pthread_mutexattr_destroy(&attr);
		if (0 == errnum)
			errnum = dest_errnum;
	} else {
		errnum = pthread_mutex_init(&channel->lock, 0);
		assert(errnum != EINVAL);
	}
	if (errnum != 0)
		goto free_channel;

	errnum = pthread_cond_init(&channel->filled, 0);
	if (errnum != 0) {
		assert(errnum != EINVAL);
		assert(false);
	}

	*channelp = channel;

	return 0;

free_channel:
	linted_mem_free(channel);

	return errnum;
}

void linted_channel_destroy(struct linted_channel *channel)
{
	linted_error errnum;

	errnum = pthread_cond_destroy(&channel->filled);
	if (errnum != 0) {
		assert(errnum != EBUSY);
		assert(false);
	}

	errnum = pthread_mutex_destroy(&channel->lock);
	if (errnum != 0) {
		assert(errnum != EBUSY);
		assert(false);
	}

	linted_mem_free(channel);
}

linted_error linted_channel_try_send(struct linted_channel *channel, void *node)
{
	linted_error errnum = 0;

	assert(channel != 0);
	assert(node != 0);

	errnum = pthread_mutex_lock(&channel->lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	void **waiter = channel->waiter;
	if (0 == waiter) {
		errnum = LINTED_ERROR_AGAIN;
		goto unlock_mutex;
	}

	*waiter = node;
	channel->waiter = 0;

	/* Not a cancellation point */
	pthread_cond_signal(&channel->filled);

unlock_mutex : {
	linted_error unlock_errnum = pthread_mutex_unlock(&channel->lock);
	if (unlock_errnum != 0) {
		assert(unlock_errnum != EPERM);
		assert(false);
	}
}

	return errnum;
}

/* Remove from the head */
void linted_channel_recv(struct linted_channel *channel, void **nodep)
{
	linted_error errnum;

	assert(channel != 0);
	assert(nodep != 0);

	errnum = pthread_mutex_lock(&channel->lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	assert(0 == channel->waiter);

	*nodep = 0;
	channel->waiter = nodep;

	do {
		errnum = pthread_cond_wait(&channel->filled, &channel->lock);
		if (errnum != 0) {
			assert(errnum != EINVAL);
			assert(false);
		}
	} while (0 == *nodep);

	errnum = pthread_mutex_unlock(&channel->lock);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}
}
