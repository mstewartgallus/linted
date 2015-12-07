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
#define _GNU_SOURCE 200809L

#include "config.h"

#include "lntd/io.h"

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/util.h"

#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Android's libc does not have the pthread_sigmask declaration in
 * signal.h as mandated by POSIX. */
#if defined __BIONIC__
#include <pthread.h>
#endif

#if defined __BIONIC__
#include <sys/syscall.h>
#endif

struct lntd_io_task_poll {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	lntd_ko ko;
	short events;
	short revents;
};

struct lntd_io_task_read {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t current_position;
	size_t bytes_read;
	lntd_ko ko;
};

struct lntd_io_task_write {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t current_position;
	size_t bytes_wrote;
	lntd_ko ko;
};

static sigset_t get_pipe_set(void);
static lntd_error eat_sigpipes(void);

static lntd_error poll_one(lntd_ko ko, short events, short *revents);
static lntd_error check_for_poll_error(short revents);

lntd_error lntd_io_read_all(lntd_ko ko, size_t *bytes_read_out,
                            void *buf, size_t size)
{
	size_t bytes_read = 0U;
	size_t bytes_left = size;
	char *buf_offset = buf;

	lntd_error err = 0;

restart_reading:
	;
	ssize_t result = read(ko, buf_offset, bytes_left);
	if (result < 0) {
		err = errno;
		LNTD_ASSUME(err != 0);
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

	LNTD_ASSUME_UNREACHABLE();
}

lntd_error lntd_io_write_all(lntd_ko ko, size_t *bytes_wrote_out,
                             void const *buf, size_t size)
{
	lntd_error err = 0;
	size_t bytes_wrote = 0U;
	size_t bytes_left = size;
	char const *buf_offset = buf;

	{
		sigset_t oldset;
		/* Get EPIPEs */
		/* SIGPIPE may not be blocked already */
		/* Reuse oldset to save on stack space */
		sigemptyset(&oldset);
		sigaddset(&oldset, SIGPIPE);

		err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
		if (err != 0)
			goto write_bytes_wrote;

		for (;;) {
			ssize_t result =
			    write(ko, buf_offset, bytes_left);
			if (-1 == result) {
				err = errno;
				LNTD_ASSUME(err != 0);
				if (EINTR == err)
					continue;
				if (EAGAIN == err)
					goto poll_for_writeability;
				break;
			}

			size_t bytes_wrote_delta = result;

			buf_offset += bytes_wrote_delta;
			bytes_wrote += bytes_wrote_delta;
			bytes_left -= bytes_wrote_delta;
			if (bytes_left != 0)
				continue;

			break;

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
					break;
				revents = xx;
			}

			err = check_for_poll_error(revents);
			if (err != 0)
				break;

			if ((revents & POLLOUT) != 0)
				continue;

			LNTD_ASSUME_UNREACHABLE();
		}

		/* Consume SIGPIPEs */
		lntd_error eat_err = eat_sigpipes();
		if (0 == err)
			err = eat_err;

		lntd_error mask_err =
		    pthread_sigmask(SIG_SETMASK, &oldset, 0);
		if (0 == err)
			err = mask_err;

		goto write_bytes_wrote;
	}

write_bytes_wrote:
	if (bytes_wrote_out != 0)
		*bytes_wrote_out = bytes_wrote;
	return err;
}

lntd_error lntd_io_write_string(lntd_ko ko, size_t *bytes_wrote_out,
                                char const *s)
{
	return lntd_io_write_all(ko, bytes_wrote_out, s, strlen(s));
}

lntd_error lntd_io_write_format(lntd_ko ko, size_t *bytes_wrote_out,
                                char const *format_str, ...)
{
	va_list ap;
	va_start(ap, format_str);

	lntd_error err =
	    lntd_io_write_va_list(ko, bytes_wrote_out, format_str, ap);

	va_end(ap);

	return err;
}

lntd_error lntd_io_write_va_list(lntd_ko ko, size_t *bytes_wrote_out,
                                 char const *format_str, va_list list)
{
	lntd_error err = 0;

	int bytes;
	{
		sigset_t oldset;
		/* Get EPIPEs */
		/* SIGPIPE may not be blocked already */
		/* Reuse oldset to save on stack space */
		sigemptyset(&oldset);
		sigaddset(&oldset, SIGPIPE);

		err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
		if (err != 0)
			return err;

#if defined __BIONIC__
		bytes = vfdprintf(ko, format_str, list);
#else
		bytes = vdprintf(ko, format_str, list);
#endif

		if (bytes < 0) {
			err = errno;
			LNTD_ASSUME(err != 0);
		}

		lntd_error eat_err = eat_sigpipes();
		if (0 == err)
			err = eat_err;

		lntd_error mask_err =
		    pthread_sigmask(SIG_SETMASK, &oldset, 0);
		if (0 == err)
			err = mask_err;
	}

	if (err != 0)
		return err;

	if (bytes_wrote_out != 0)
		*bytes_wrote_out = bytes;

	return 0;
}

static lntd_error poll_one(lntd_ko ko, short events, short *reventsp)
{
	lntd_error err;

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
	LNTD_ASSUME(err != 0);
	return err;

poll_succeeded:
	*reventsp = revents;
	return 0;
}

static lntd_error check_for_poll_error(short revents)
{
	lntd_error err = 0;

	if ((revents & POLLNVAL) != 0)
		err = EBADF;

	return err;
}

lntd_error lntd_io_task_poll_create(struct lntd_io_task_poll **taskp,
                                    void *data)
{
	lntd_error err;
	struct lntd_io_task_poll *task;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct lntd_async_task *parent;
	{
		struct lntd_async_task *xx;
		err = lntd_async_task_create(&xx, task,
		                             LNTD_ASYNCH_TASK_POLL);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct lntd_async_waiter *xx;
		err = lntd_async_waiter_create(&xx);
		if (err != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_parent:
	lntd_mem_free(parent);
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_io_task_poll_destroy(struct lntd_io_task_poll *task)
{
	lntd_async_waiter_destroy(task->waiter);
	lntd_async_task_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *
lntd_io_task_poll_prepare(struct lntd_io_task_poll *task,
                          union lntd_async_ck task_ck, void *userstate,
                          lntd_ko ko, int flags)
{
	task->ko = ko;
	task->events = flags;
	return lntd_async_task_prepare(task->parent, task_ck,
	                               userstate);
}

struct lntd_async_task *
lntd_io_task_poll_to_async(struct lntd_io_task_poll *task)
{
	return task->parent;
}

void *lntd_io_task_poll_data(struct lntd_io_task_poll *task)
{
	return task->data;
}

lntd_error lntd_io_task_read_create(struct lntd_io_task_read **taskp,
                                    void *data)
{
	lntd_error err;
	struct lntd_io_task_read *task;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct lntd_async_task *parent;
	{
		struct lntd_async_task *xx;
		err = lntd_async_task_create(&xx, task,
		                             LNTD_ASYNCH_TASK_READ);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct lntd_async_waiter *xx;
		err = lntd_async_waiter_create(&xx);
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
	lntd_mem_free(parent);
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_io_task_read_destroy(struct lntd_io_task_read *task)
{
	lntd_async_waiter_destroy(task->waiter);
	lntd_async_task_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *
lntd_io_task_read_prepare(struct lntd_io_task_read *task,
                          union lntd_async_ck task_ck, void *userstate,
                          lntd_ko ko, char *buf, size_t size)
{
	task->ko = ko;
	task->buf = buf;
	task->size = size;
	return lntd_async_task_prepare(task->parent, task_ck,
	                               userstate);
}

struct lntd_async_task *
lntd_io_task_read_to_async(struct lntd_io_task_read *task)
{
	return task->parent;
}

void *lntd_io_task_read_data(struct lntd_io_task_read *task)
{
	return task->data;
}

lntd_ko lntd_io_task_read_ko(struct lntd_io_task_read *task)
{
	return task->ko;
}

size_t lntd_io_task_read_bytes_read(struct lntd_io_task_read *task)
{
	return task->bytes_read;
}

lntd_error lntd_io_task_write_create(struct lntd_io_task_write **taskp,
                                     void *data)
{
	lntd_error err;
	struct lntd_io_task_write *task;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct lntd_async_task *parent;
	{
		struct lntd_async_task *xx;
		err = lntd_async_task_create(&xx, task,
		                             LNTD_ASYNCH_TASK_WRITE);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct lntd_async_waiter *xx;
		err = lntd_async_waiter_create(&xx);
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
	lntd_async_task_destroy(parent);

free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_io_task_write_destroy(struct lntd_io_task_write *task)
{
	lntd_async_waiter_destroy(task->waiter);
	lntd_async_task_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *
lntd_io_task_write_prepare(struct lntd_io_task_write *task,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_ko ko, char const *buf, size_t size)
{
	task->ko = ko;
	task->buf = buf;
	task->size = size;
	return lntd_async_task_prepare(task->parent, task_ck,
	                               userstate);
}

struct lntd_async_task *
lntd_io_task_write_to_async(struct lntd_io_task_write *task)
{
	return task->parent;
}

void *lntd_io_task_write_data(struct lntd_io_task_write *task)
{
	return task->data;
}

void lntd_io_do_poll(struct lntd_async_pool *pool,
                     struct lntd_async_task *task)
{
	struct lntd_io_task_poll *task_poll =
	    lntd_async_task_data(task);

	struct lntd_async_waiter *waiter = task_poll->waiter;
	lntd_ko ko = task_poll->ko;
	short events = task_poll->events;

	short revents = lntd_async_waiter_revents(waiter);
	if (0 == revents) {
		lntd_async_pool_wait_on_poll(pool, waiter, task, ko,
		                             events);
		return;
	}

	task_poll->revents = revents;
	lntd_async_pool_complete(pool, task, 0);
}

void lntd_io_do_read(struct lntd_async_pool *pool,
                     struct lntd_async_task *task)
{
	struct lntd_io_task_read *task_read =
	    lntd_async_task_data(task);

	size_t bytes_read = task_read->current_position;
	size_t bytes_left = task_read->size - bytes_read;

	lntd_ko ko = task_read->ko;
	char *buf = task_read->buf;

	struct lntd_async_waiter *waiter = task_read->waiter;

	lntd_error err = 0;

	ssize_t result = read(ko, buf + bytes_read, bytes_left);
	if (result < 0) {
		err = errno;
		LNTD_ASSUME(err != 0);
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

	lntd_async_pool_complete(pool, task, err);
	return;

submit_retry:
	task_read->bytes_read = bytes_read;
	task_read->current_position = bytes_read;
	lntd_async_pool_resubmit(pool, task);
	return;

wait_on_poll:
	lntd_async_pool_wait_on_poll(pool, waiter, task, ko, POLLIN);
}

void lntd_io_do_write(struct lntd_async_pool *pool,
                      struct lntd_async_task *task)
{
	struct lntd_io_task_write *task_write =
	    lntd_async_task_data(task);

	size_t bytes_wrote = task_write->current_position;
	size_t bytes_left = task_write->size - bytes_wrote;

	lntd_error err = 0;

	lntd_ko ko = task_write->ko;
	char const *buf = task_write->buf;

	char const *buf_offset = buf + bytes_wrote;

	/* We can assume that SIGPIPEs are blocked here. */
	ssize_t result = write(ko, buf_offset, bytes_left);
	if (-1 == result) {
		err = errno;
		LNTD_ASSUME(err != 0);
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

	lntd_async_pool_complete(pool, task, err);
	return;

submit_retry:
	task_write->bytes_wrote = bytes_wrote;
	task_write->current_position = bytes_wrote;

	lntd_async_pool_resubmit(pool, task);
	return;

wait_on_poll:
	lntd_async_pool_wait_on_poll(pool, task_write->waiter, task, ko,
	                             POLLOUT);
}

static struct timespec const zero_timeout = {0};

static lntd_error eat_sigpipes(void)
{
	lntd_error err = 0;

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

		lntd_error wait_err = errno;
		LNTD_ASSUME(wait_err != 0);

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
