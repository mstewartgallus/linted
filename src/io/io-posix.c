/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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

#include "linted/io.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/log.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

/* Android's libc does not have the pthread_sigmask declaration in
 * signal.h as mandated by POSIX. */
#if defined __BIONIC__
#include <pthread.h>
#endif

#if defined __BIONIC__
#include <sys/syscall.h>
#endif

struct linted_io_task_poll {
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	linted_ko ko;
	short events;
	short revents;
};

struct linted_io_task_read {
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t current_position;
	size_t bytes_read;
	linted_ko ko;
};

struct linted_io_task_write {
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t current_position;
	size_t bytes_wrote;
	linted_ko ko;
};

static sigset_t get_pipe_set(void);
static linted_error eat_sigpipes(void);

static linted_error poll_one(linted_ko ko, short events,
                             short *revents);
static linted_error check_for_poll_error(short revents);

linted_error linted_io_read_all(linted_ko ko, size_t *bytes_read_out,
                                void *buf, size_t size)
{
	size_t bytes_read = 0U;
	size_t bytes_left = size;
	char *buf_offset = buf;

	linted_error err = 0;

restart_reading:
	;
	ssize_t result = read(ko, buf_offset, bytes_left);
	if (result < 0) {
		err = errno;
		LINTED_ASSUME(err != 0);
		if (EINTR == err)
			goto restart_reading;
		if (EAGAIN == err)
			goto poll_for_readability;
		if (EWOULDBLOCK == err)
			goto poll_for_readability;
		return err;
	}

	size_t bytes_read_delta = result;
	if (0U == bytes_read_delta)
		goto finish_reading;

	buf_offset += bytes_read_delta;
	bytes_read += bytes_read_delta;
	bytes_left -= bytes_read_delta;

	if (bytes_left != 0)
		goto restart_reading;

finish_reading:
	if (bytes_read_out != 0)
		*bytes_read_out = bytes_read;
	return err;

poll_for_readability:
	;
	short revents;
	{
		short xx;
		err = poll_one(ko, POLLIN, &xx);
		if (EINTR == err)
			goto poll_for_readability;
		if (EAGAIN == err)
			goto poll_for_readability;
		if (err != 0)
			goto finish_reading;
		revents = xx;
	}

	err = check_for_poll_error(revents);
	if (err != 0)
		goto finish_reading;

	if ((revents & POLLIN) != 0)
		goto restart_reading;

	if ((revents & POLLHUP) != 0)
		goto finish_reading;

	LINTED_ASSUME_UNREACHABLE();
}

linted_error linted_io_write_all(linted_ko ko, size_t *bytes_wrote_out,
                                 void const *buf, size_t size)
{
	linted_error err = 0;
	size_t bytes_wrote = 0U;
	size_t bytes_left = size;
	char const *buf_offset = buf;

	sigset_t oldset;
	/* Get EPIPEs */
	/* SIGPIPE may not be blocked already */
	/* Reuse oldset to save on stack space */
	sigemptyset(&oldset);
	sigaddset(&oldset, SIGPIPE);

	err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (err != 0)
		goto write_bytes_wrote;

restart_writing:
	;
	ssize_t result = write(ko, buf_offset, bytes_left);
	if (-1 == result) {
		err = errno;
		LINTED_ASSUME(err != 0);
		if (EINTR == err)
			goto restart_writing;
		if (EAGAIN == err)
			goto poll_for_writeability;
		goto get_sigpipe;
	}

	size_t bytes_wrote_delta = result;

	buf_offset += bytes_wrote_delta;
	bytes_wrote += bytes_wrote_delta;
	bytes_left -= bytes_wrote_delta;
	if (bytes_left != 0)
		goto restart_writing;

/* Consume SIGPIPEs */
get_sigpipe : {
	linted_error eat_err = eat_sigpipes();
	if (0 == err)
		err = eat_err;
}

	{
		linted_error mask_err =
		    pthread_sigmask(SIG_SETMASK, &oldset, 0);
		if (0 == err)
			err = mask_err;
	}

write_bytes_wrote:
	if (bytes_wrote_out != 0)
		*bytes_wrote_out = bytes_wrote;
	return err;

poll_for_writeability:
	;
	short revents;
	{
		short xx;
		err = poll_one(ko, POLLOUT, &xx);
		if (EINTR == err)
			goto poll_for_writeability;
		if (EAGAIN == err)
			goto poll_for_writeability;
		if (err != 0)
			goto get_sigpipe;
		revents = xx;
	}

	err = check_for_poll_error(revents);
	if (err != 0)
		goto get_sigpipe;

	if ((revents & POLLOUT) != 0)
		goto restart_writing;

	LINTED_ASSUME_UNREACHABLE();
}

linted_error linted_io_write_string(linted_ko ko,
                                    size_t *bytes_wrote_out,
                                    char const *s)
{
	return linted_io_write_all(ko, bytes_wrote_out, s, strlen(s));
}

linted_error linted_io_write_format(linted_ko ko,
                                    size_t *bytes_wrote_out,
                                    char const *format_str, ...)
{
	va_list ap;
	va_start(ap, format_str);

	linted_error err = linted_io_write_va_list(ko, bytes_wrote_out,
	                                           format_str, ap);

	va_end(ap);

	return err;
}

linted_error linted_io_write_va_list(linted_ko ko,
                                     size_t *bytes_wrote_out,
                                     char const *format_str,
                                     va_list list)
{
	int bytes;

#if defined __BIONIC__
	bytes = vfdprintf(ko, format_str, list);
#else
	bytes = vdprintf(ko, format_str, list);
#endif

	if (bytes < 0) {
		linted_error err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	if (bytes_wrote_out != 0)
		*bytes_wrote_out = bytes;

	return 0;
}

static linted_error poll_one(linted_ko ko, short events,
                             short *reventsp)
{
	linted_error err;

	short revents;
	{
		struct pollfd pollfd = {.fd = ko, .events = events};
		int poll_status = poll(&pollfd, 1U, -1);
		if (-1 == poll_status)
			goto poll_failed;

		revents = pollfd.revents;
		goto poll_succeeded;
	}

poll_failed:
	err = errno;
	LINTED_ASSUME(err != 0);
	return err;

poll_succeeded:
	*reventsp = revents;
	return 0;
}

static linted_error check_for_poll_error(short revents)
{
	linted_error err = 0;

	if ((revents & POLLNVAL) != 0)
		err = EBADF;

	return err;
}

linted_error
linted_io_task_poll_create(struct linted_io_task_poll **taskp,
                           void *data)
{
	linted_error err;
	struct linted_io_task_poll *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		err = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_POLL);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		err = linted_asynch_waiter_create(&xx);
		if (err != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_parent:
	linted_mem_free(parent);
free_task:
	linted_mem_free(task);
	return err;
}

void linted_io_task_poll_destroy(struct linted_io_task_poll *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_poll_prepare(struct linted_io_task_poll *task,
                                 union linted_asynch_action task_action,
                                 linted_ko ko, int flags)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->events = flags;
}

struct linted_io_task_poll *
linted_io_task_poll_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_io_task_poll_to_asynch(struct linted_io_task_poll *task)
{
	return task->parent;
}

void *linted_io_task_poll_data(struct linted_io_task_poll *task)
{
	return task->data;
}

linted_error
linted_io_task_read_create(struct linted_io_task_read **taskp,
                           void *data)
{
	linted_error err;
	struct linted_io_task_read *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		err = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_READ);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		err = linted_asynch_waiter_create(&xx);
		if (err != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	task->current_position = 0U;

	*taskp = task;
	return 0;
free_parent:
	linted_mem_free(parent);
free_task:
	linted_mem_free(task);
	return err;
}

void linted_io_task_read_destroy(struct linted_io_task_read *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_read_prepare(struct linted_io_task_read *task,
                                 union linted_asynch_action task_action,
                                 linted_ko ko, char *buf, size_t size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;
}

struct linted_io_task_read *
linted_io_task_read_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_io_task_read_to_asynch(struct linted_io_task_read *task)
{
	return task->parent;
}

void *linted_io_task_read_data(struct linted_io_task_read *task)
{
	return task->data;
}

linted_ko linted_io_task_read_ko(struct linted_io_task_read *task)
{
	return task->ko;
}

size_t linted_io_task_read_bytes_read(struct linted_io_task_read *task)
{
	return task->bytes_read;
}

linted_error
linted_io_task_write_create(struct linted_io_task_write **taskp,
                            void *data)
{
	linted_error err;
	struct linted_io_task_write *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		err = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_WRITE);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		err = linted_asynch_waiter_create(&xx);
		if (err != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	task->current_position = 0U;
	*taskp = task;
	return 0;

free_parent:
	linted_asynch_task_destroy(parent);

free_task:
	linted_mem_free(task);
	return err;
}

void linted_io_task_write_destroy(struct linted_io_task_write *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_write_prepare(
    struct linted_io_task_write *task,
    union linted_asynch_action task_action, linted_ko ko,
    char const *buf, size_t size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;
}

struct linted_io_task_write *
linted_io_task_write_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_io_task_write_to_asynch(struct linted_io_task_write *task)
{
	return task->parent;
}

void *linted_io_task_write_data(struct linted_io_task_write *task)
{
	return task->data;
}

void linted_io_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_io_task_poll *task_poll =
	    linted_io_task_poll_from_asynch(task);

	struct linted_asynch_waiter *waiter = task_poll->waiter;
	linted_ko ko = task_poll->ko;
	short events = task_poll->events;

	short revents = linted_asynch_waiter_revents(waiter);
	if (0 == revents) {
		linted_asynch_pool_wait_on_poll(pool, waiter, task, ko,
		                                events);
		return;
	}

	task_poll->revents = revents;
	linted_asynch_pool_complete(pool, task, 0);
}

void linted_io_do_read(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_io_task_read *task_read =
	    linted_io_task_read_from_asynch(task);
	size_t bytes_read = task_read->current_position;
	size_t bytes_left = task_read->size - bytes_read;

	linted_ko ko = task_read->ko;
	char *buf = task_read->buf;

	struct linted_asynch_waiter *waiter = task_read->waiter;

	linted_error err = 0;

	ssize_t result = read(ko, buf + bytes_read, bytes_left);
	if (result < 0) {
		err = errno;
		LINTED_ASSUME(err != 0);
	}

	if (EINTR == err)
		goto submit_retry;

	if (EAGAIN == err || EWOULDBLOCK == err)
		goto wait_on_poll;

	if (err != 0)
		goto complete_task;

	/* Hangup or nothing more to read */
	if (0 == result)
		goto complete_task;

	size_t bytes_read_delta = result;

	bytes_read += bytes_read_delta;
	bytes_left -= bytes_read_delta;

	if (bytes_read_delta != 0U && bytes_left != 0U)
		goto submit_retry;

complete_task:
	task_read->bytes_read = bytes_read;
	task_read->current_position = 0U;

	linted_asynch_pool_complete(pool, task, err);
	return;

submit_retry:
	task_read->bytes_read = bytes_read;
	task_read->current_position = bytes_read;
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, waiter, task, ko, POLLIN);
}

void linted_io_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *task)
{
	struct linted_io_task_write *task_write =
	    linted_io_task_write_from_asynch(task);
	size_t bytes_wrote = task_write->current_position;
	size_t bytes_left = task_write->size - bytes_wrote;

	linted_error err = 0;

	linted_ko ko = task_write->ko;
	char const *buf = task_write->buf;

	char const *buf_offset = buf + bytes_wrote;

	/* We can assume that SIGPIPEs are blocked here. */
	ssize_t result = write(ko, buf_offset, bytes_left);
	if (-1 == result) {
		err = errno;
		LINTED_ASSUME(err != 0);
	}

	if (EINTR == err)
		goto submit_retry;

	if (EAGAIN == err || EWOULDBLOCK == err)
		goto wait_on_poll;

	if (err != 0)
		goto complete_task;

	size_t bytes_wrote_delta = result;

	bytes_wrote += bytes_wrote_delta;
	bytes_left -= bytes_wrote_delta;
	if (bytes_left != 0U)
		goto submit_retry;

complete_task:
	task_write->bytes_wrote = bytes_wrote;
	task_write->current_position = 0U;

	linted_asynch_pool_complete(pool, task, err);
	return;

submit_retry:
	task_write->bytes_wrote = bytes_wrote;
	task_write->current_position = bytes_wrote;

	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, task_write->waiter, task,
	                                ko, POLLOUT);
}

static struct timespec const zero_timeout = {0};

static linted_error eat_sigpipes(void)
{
	linted_error err = 0;

	for (;;) {
		int wait_status;

		sigset_t const pipe_set = get_pipe_set();
		struct timespec const *timeoutp = &zero_timeout;

#if defined __BIONIC__
		wait_status = syscall(__NR_rt_sigtimedwait, &pipe_set,
		                      0, timeoutp);
#else
		wait_status = sigtimedwait(&pipe_set, 0, timeoutp);
#endif
		if (wait_status != -1)
			continue;

		linted_error wait_err = errno;
		LINTED_ASSUME(wait_err != 0);

		if (EAGAIN == wait_err)
			break;

		if (wait_err != EINTR) {
			if (0 == err)
				err = wait_err;
			break;
		}
	}

	return err;
}

static sigset_t get_pipe_set(void)
{
	sigset_t pipeset;
	sigemptyset(&pipeset);
	sigaddset(&pipeset, SIGPIPE);
	return pipeset;
}
