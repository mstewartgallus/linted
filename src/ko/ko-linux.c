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

static void fd_to_str(char *buf, linted_ko fd);

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);

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

linted_error linted_ko_dummy(linted_ko *kop)
{
	return linted_ko_open(kop, LINTED_KO_CWD, "/dev/null",
	                      LINTED_KO_RDONLY);
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

void linted_ko_task_poll(struct linted_ko_task_poll *task, unsigned task_action,
                         linted_ko ko, short events)
{
	linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_POLL,
	                   task_action);

	task->ko = ko;
	task->events = events;
}

void linted_ko_task_read(struct linted_ko_task_read *task, unsigned task_action,
                         linted_ko ko, char *buf, size_t size)
{
	linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_READ,
	                   task_action);

	task->ko = ko;
	task->buf = buf;
	task->size = size;
	task->current_position = 0U;
	task->bytes_read = 0U;
}

void linted_ko_task_write(struct linted_ko_task_write *task,
                          unsigned task_action, linted_ko ko, char const *buf,
                          size_t size)
{
	linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_WRITE,
	                   task_action);

	task->ko = ko;
	task->buf = buf;
	task->size = size;
	task->current_position = 0U;
	task->bytes_wrote = 0U;
}

void linted_ko_task_accept(struct linted_ko_task_accept *task,
                           unsigned task_action, linted_ko ko)
{
	linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_ACCEPT,
	                   task_action);

	task->ko = ko;
}

void linted_ko_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_ko_task_poll *restrict task_poll =
	    LINTED_DOWNCAST(struct linted_ko_task_poll, task);
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

	if (EINTR == errnum) {
		linted_asynch_pool_submit(pool, task);
		return;
	}

	task_poll->revents = revents;
	task->errnum = errnum;

	linted_asynch_pool_complete(pool, task);
}

void linted_ko_do_read(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_ko_task_read *task_read =
	    LINTED_DOWNCAST(struct linted_ko_task_read, task);
	size_t bytes_read = task_read->current_position;
	size_t bytes_left = task_read->size - bytes_read;

	linted_ko ko = task_read->ko;
	char *buf = task_read->buf;

	linted_error errnum = 0;

	ssize_t result = read(ko, buf + bytes_read, bytes_left);
	if (-1 == result) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum) {
		linted_asynch_pool_submit(pool, task);
		return;
	}

	if (EAGAIN == errnum || EWOULDBLOCK == errnum) {
		short revents = 0;
		{
			short xx;
			errnum = poll_one(ko, POLLIN, &xx);
			if (0 == errnum)
				revents = xx;
		}

		if (EINTR == errnum) {
			linted_asynch_pool_submit(pool, task);
			return;
		}

		if (errnum != 0)
			goto complete_task;

		errnum = check_for_poll_error(ko, revents);
	}

	if (errnum != 0)
		goto complete_task;

	size_t bytes_read_delta = result;
	if (0U == bytes_read_delta)
		goto complete_task;

	bytes_read += bytes_read_delta;
	bytes_left -= bytes_read_delta;
	if (0U == bytes_left)
		goto complete_task;

	task->errnum = 0;
	task_read->bytes_read = bytes_read;
	task_read->current_position = bytes_read;
	linted_asynch_pool_submit(pool, task);
	return;

complete_task:
	task->errnum = errnum;
	task_read->bytes_read = bytes_read;
	task_read->current_position = 0U;

	linted_asynch_pool_complete(pool, task);
}

void linted_ko_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *task)
{
	struct linted_ko_task_write *task_write =
	    LINTED_DOWNCAST(struct linted_ko_task_write, task);
	size_t bytes_wrote = task_write->current_position;
	size_t bytes_left = task_write->size - bytes_wrote;

	linted_error errnum = 0;

	linted_ko ko = task_write->ko;
	char const *buf = task_write->buf;

	sigset_t oldset;
	/* Get EPIPEs */
	/* SIGPIPE may not be blocked already */
	/* Reuse oldset to save on stack space */
	sigemptyset(&oldset);
	sigaddset(&oldset, SIGPIPE);

	if ((errnum = pthread_sigmask(SIG_BLOCK, &oldset, &oldset)) != 0)
		goto complete_task;

	ssize_t result = write(ko, buf + bytes_wrote, bytes_left);
	if (-1 == result) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	/* Consume SIGPIPEs */
	{
		sigset_t sigpipeset;

		sigemptyset(&sigpipeset);
		sigaddset(&sigpipeset, SIGPIPE);

		linted_error wait_errnum;
		do {
			struct timespec timeout = { 0 };

			if (-1 == sigtimedwait(&sigpipeset, NULL, &timeout)) {
				wait_errnum = errno;
				LINTED_ASSUME(wait_errnum != 0);
			} else {
				wait_errnum = 0;
			}
		} while (EINTR == wait_errnum);
		if (wait_errnum != 0 && wait_errnum != EAGAIN) {
			if (0 == errnum)
				errnum = wait_errnum;
		}
	}

	{
		linted_error mask_errnum =
		    pthread_sigmask(SIG_SETMASK, &oldset, NULL);
		if (0 == errnum)
			errnum = mask_errnum;
	}

	if (EINTR == errnum) {
		linted_asynch_pool_submit(pool, task);
		return;
	}

	if (EAGAIN == errnum || EWOULDBLOCK == errnum) {
		short revents = 0;
		{
			short xx;
			errnum = poll_one(ko, POLLOUT, &xx);
			if (0 == errnum)
				revents = xx;
		}

		if (EINTR == errnum) {
			linted_asynch_pool_submit(pool, task);
			return;
		}

		if (errnum != 0)
			goto complete_task;

		errnum = check_for_poll_error(ko, revents);
	}

	if (errnum != 0)
		goto complete_task;

	size_t bytes_wrote_delta = result;

	bytes_wrote += bytes_wrote_delta;
	bytes_left -= bytes_wrote_delta;
	if (0U == bytes_left)
		goto complete_task;

	task_write->bytes_wrote = bytes_wrote;
	task_write->current_position = bytes_wrote;

	linted_asynch_pool_submit(pool, task);
	return;

complete_task:
	task->errnum = errnum;
	task_write->bytes_wrote = bytes_wrote;
	task_write->current_position = 0U;

	linted_asynch_pool_complete(pool, task);
}

void linted_ko_do_accept(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task)
{
	struct linted_ko_task_accept *task_accept =
	    LINTED_DOWNCAST(struct linted_ko_task_accept, task);

	linted_ko new_ko = -1;
	linted_error errnum = 0;
	linted_ko ko = task_accept->ko;

	new_ko = accept4(ko, NULL, 0, SOCK_NONBLOCK | SOCK_CLOEXEC);
	if (-1 == new_ko) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	/**
	 * @bug On BSD accept returns the same file description as
	 * passed into connect so this file descriptor shares NONBLOCK
	 * status with the connector. I'm not sure of a way to sever
	 * shared file descriptions on BSD.
	 */

	/* Retry on network error */
	switch (errnum) {
	case EINTR:
	case ENETDOWN:
	case EPROTO:
	case ENOPROTOOPT:
	case EHOSTDOWN:
	case ENONET:
	case EHOSTUNREACH:
	case EOPNOTSUPP:
	case ENETUNREACH:
		linted_asynch_pool_submit(pool, task);
		return;
	}

	if (EAGAIN == errnum || EWOULDBLOCK == errnum) {
		short revents = 0;
		{
			short xx;
			errnum = poll_one(ko, POLLIN, &xx);
			if (0 == errnum)
				revents = xx;
		}
		if (EINTR == errnum) {
			linted_asynch_pool_submit(pool, task);
			return;
		}

		if (errnum != 0)
			goto complete_task;

		errnum = check_for_poll_error(ko, revents);
	}

complete_task:
	task->errnum = errnum;
	task_accept->returned_ko = new_ko;

	linted_asynch_pool_complete(pool, task);
}

static linted_error poll_one(linted_ko ko, short events, short *revents)
{
	struct pollfd pollfd = { .fd = ko, .events = events };
	if (-1 == poll(&pollfd, 1U, -1)) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*revents = pollfd.revents;
	return 0;
}

static linted_error check_for_poll_error(linted_ko ko, short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = EBADF;

	return errnum;
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
