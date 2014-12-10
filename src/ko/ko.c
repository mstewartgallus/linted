/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#define _GNU_SOURCE

#include "config.h"

#include "linted/ko.h"

#include "linted/mem.h"
#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

struct linted_ko_task_accept
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	linted_ko ko;
	linted_ko returned_ko;
};

struct linted_ko_task_poll
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	linted_ko ko;
	short events;
	short revents;
};

static void fd_to_str(char *buf, linted_ko fd);

static linted_error poll_one(linted_ko ko, short events, short *revents);

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop)
{
	size_t length = strlen(str);
	unsigned position = 1U;

	if ('0' == str[0U] && length != 1U)
		return EINVAL;

	unsigned total = 0U;
	for (; length > 0U; --length) {
		char const digit = str[length - 1U];

		if ('0' <= digit && digit <= '9') {
			unsigned long sum =
			    total + ((unsigned)(digit - '0')) * position;
			if (sum > INT_MAX)
				return ERANGE;

			total = sum;
		} else {
			return EINVAL;
		}

		unsigned long next_position = 10U * position;
		if (next_position > INT_MAX)
			return ERANGE;
		position = next_position;
	}

	*kop = total;
	return 0;
}

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
{
	linted_error errnum;

	if (LINTED_KO_CWD == dirko) {
		dirko = AT_FDCWD;
	} else if (dirko < 0) {
		return EINVAL;
	}

	if ((flags & ~LINTED_KO_RDONLY & ~LINTED_KO_WRONLY & ~LINTED_KO_RDWR &
	     ~LINTED_KO_SYNC & ~LINTED_KO_DIRECTORY) != 0U)
		return EINVAL;

	bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0U;
	bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0U;
	bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0U;

	bool ko_sync = (flags & LINTED_KO_SYNC) != 0U;

	bool ko_directory = (flags & LINTED_KO_DIRECTORY) != 0U;

	if (ko_rdonly && ko_wronly)
		return EINVAL;

	if (ko_rdwr && ko_rdonly)
		return EINVAL;

	if (ko_rdwr && ko_wronly)
		return EINVAL;

	if ((ko_directory && ko_rdonly) || (ko_directory && ko_wronly) ||
	    (ko_directory && ko_rdwr) || (ko_directory && ko_sync))
		return EINVAL;

	/*
	 * Always, be safe for execs and use O_NONBLOCK because asynch
	 * functions handle that anyways and open may block otherwise.
	 */
	int oflags = O_CLOEXEC | O_NONBLOCK | O_NOCTTY;

	if (ko_rdonly)
		oflags |= O_RDONLY;

	if (ko_wronly)
		oflags |= O_WRONLY;

	if (ko_rdwr)
		oflags |= O_RDWR;

	if (ko_sync)
		oflags |= O_SYNC;

	if (ko_directory)
		oflags |= O_DIRECTORY;

	int fildes;
	do {
		fildes = openat(dirko, pathname, oflags);
		if (-1 == fildes) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		} else {
			errnum = 0;
		}
	} while (EINTR == errnum);
	if (errnum != 0)
		return errnum;

	*kop = fildes;

	return 0;
}

linted_error linted_ko_reopen(linted_ko *kooutp, linted_ko koin,
                              unsigned long flags)
{
	char pathname[] = "/proc/self/fd/XXXXXXXXXXX";
	fd_to_str(pathname + strlen("/proc/self/fd/"), koin);
	return linted_ko_open(kooutp, LINTED_KO_CWD, pathname, flags);
}

linted_error linted_ko_close(linted_ko ko)
{
	linted_error errnum;
	/*
	 * The state of a file descriptor after close gives an EINTR error
	 * is unspecified by POSIX so this function avoids the problem by
	 * simply blocking all signals.
	 */

	sigset_t sigset;

	/* First use the signal set for the full set */
	sigfillset(&sigset);

	errnum = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
	if (errnum != 0)
		return errnum;

	/* Then reuse the signal set for the old set */

	if (-1 == close(ko)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	} else {
		errnum = 0;
	}

	linted_error mask_errnum = pthread_sigmask(SIG_SETMASK, &sigset, NULL);
	if (0 == errnum)
		errnum = mask_errnum;

	return errnum;
}

linted_error linted_ko_task_poll_create(struct linted_ko_task_poll **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_ko_task_poll *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(&xx, task,
		                                   LINTED_ASYNCH_TASK_POLL);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_ko_task_poll_destroy(struct linted_ko_task_poll *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_ko_task_poll_prepare(struct linted_ko_task_poll *task,
                                 unsigned task_action, linted_ko ko, int flags)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->events = flags;
}

struct linted_ko_task_poll *
linted_ko_task_poll_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_ko_task_poll_to_asynch(struct linted_ko_task_poll *task)
{
	return task->parent;
}

void *linted_ko_task_poll_data(struct linted_ko_task_poll *task)
{
	return task->data;
}

void linted_ko_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_ko_task_poll *task_poll =
	    linted_ko_task_poll_from_asynch(task);
	linted_error errnum;

	linted_ko ko = task_poll->ko;
	short events = task_poll->events;

	short revents = 0;
	{
		short xx;
		errnum = poll_one(ko, events, &xx);
		if (0 == errnum)
			revents = xx;
	}

	if (EINTR == errnum)
		goto submit_retry;

	task_poll->revents = revents;
	linted_asynch_task_seterrnum(task, errnum);

	linted_asynch_pool_complete(pool, task);
	return;

submit_retry:
	linted_asynch_pool_submit(pool, task);
}

static struct timespec const zero_timeout = { 0 };

static linted_error poll_one(linted_ko ko, short events, short *reventsp)
{
	linted_error errnum;

	short revents;
	{
		struct pollfd pollfd = { .fd = ko, .events = events };
		int poll_status = poll(&pollfd, 1U, -1);
		if (-1 == poll_status)
			goto poll_failed;

		revents = pollfd.revents;
		goto poll_succeeded;
	}

poll_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);
	return errnum;

poll_succeeded:
	*reventsp = revents;
	return 0;
}

static void fd_to_str(char *buf, linted_ko fd)
{
	size_t strsize = 0U;

	assert(fd >= 0);

	for (;;) {
		memmove(buf + 1U, buf, strsize);

		linted_ko digit = fd % 10;

		*buf = '0' + digit;

		fd /= 10;
		++strsize;

		if (0 == fd)
			break;
	}

	buf[strsize] = '\0';
}
