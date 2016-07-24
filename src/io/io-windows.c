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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "lntd/io.h"

#include "lntd/async.h"
#include "lntd/mem.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/util.h"

#include <conio.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <windows.h>
#include <winsock2.h>

struct lntd_io_task_accept {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	lntd_ko ko;
	lntd_ko returned_ko;
};

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

struct lntd_io_task_recv {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t bytes_read;
	lntd_ko ko;
};

struct lntd_io_task_sendto {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t bytes_wrote;
	lntd_ko ko;
	struct sockaddr_storage dest_addr;
	size_t dest_addr_size;
};

static lntd_error poll_one(lntd_ko ko, short events, short *revents);
static lntd_error check_for_poll_error(short revents);

lntd_error lntd_io_read_all(lntd_ko ko, size_t *bytes_read_out,
                            void *buf, size_t size)
{
	size_t bytes_read = 0U;
	size_t bytes_left = size;

	lntd_error err = 0;

restart_reading:
	;
	size_t bytes_read_delta;
	{
		DWORD xx;
		if (!ReadFile(ko, (char *)buf + bytes_read, bytes_left,
		              &xx, 0)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);

			if (HRESULT_FROM_WIN32(ERROR_HANDLE_EOF) ==
			    err) {
				err = 0;
				goto finish_reading;
			}

			if (HRESULT_FROM_WIN32(WSAEINTR) == err)
				goto restart_reading;

			if (HRESULT_FROM_WIN32(WSAEWOULDBLOCK) == err)
				goto poll_for_readability;

			goto finish_reading;
		}
		bytes_read_delta = xx;
	}

	bytes_read += bytes_read_delta;
	bytes_left -= bytes_read_delta;

	if (bytes_read_delta != 0U && bytes_left != 0U)
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
		if (HRESULT_FROM_WIN32(WSAEINTR) == err)
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

restart_writing:
	;
	size_t bytes_wrote_delta;
	{
		DWORD xx;
		if (!WriteFile(ko, (char const *)buf + bytes_wrote,
		               bytes_left, &xx, 0)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);

			if (HRESULT_FROM_WIN32(WSAEINTR) == err)
				goto restart_writing;

			if (HRESULT_FROM_WIN32(WSAEWOULDBLOCK) == err)
				goto poll_for_writeability;

			if (err != 0)
				goto write_bytes_wrote;

			goto write_bytes_wrote;
		}
		bytes_wrote_delta = xx;
	}

	bytes_wrote += bytes_wrote_delta;
	bytes_left -= bytes_wrote_delta;
	if (bytes_left != 0)
		goto restart_writing;

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
		if (HRESULT_FROM_WIN32(WSAEINTR) == err)
			goto poll_for_writeability;
		if (err != 0)
			goto write_bytes_wrote;
		revents = xx;
	}

	err = check_for_poll_error(revents);
	if (err != 0)
		goto write_bytes_wrote;

	if ((revents & POLLOUT) != 0)
		goto restart_writing;

	LNTD_ASSUME_UNREACHABLE();
}

lntd_error lntd_io_write_string(lntd_ko ko, size_t *bytes_wrote_out,
                                char const *s)
{
	return lntd_io_write_format(ko, bytes_wrote_out, "%s", s);
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

	int maybe_bytes = vsprintf_s(0, 0U, format_str, list);
	if (maybe_bytes < 0)
		return LNTD_ERROR_INVALID_PARAMETER;

	size_t bytes = (size_t)maybe_bytes;

	char *str;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, bytes + 1U);
		if (err != 0)
			return err;
		str = xx;
	}
	str[bytes] = 0U;

	if (vsprintf_s(str, bytes + 1U, format_str, list) < 0) {
		err = LNTD_ERROR_INVALID_PARAMETER;
		goto free_str;
	}

	err = lntd_io_write_all(ko, bytes_wrote_out, str, bytes);

free_str:
	lntd_mem_free(str);

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

void lntd_io_task_poll_submit(struct lntd_async_pool *pool,
                              struct lntd_io_task_poll *task,
                              union lntd_async_ck task_ck,
                              void *userstate, lntd_ko ko, int flags)
{
	task->ko = ko;
	task->events = flags;
	lntd_async_task_submit(pool, task->parent, task_ck, userstate);
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

void lntd_io_task_read_submit(struct lntd_async_pool *pool,
                              astruct lntd_io_task_read *task,
                              union lntd_async_ck task_ck,
                              void *userstate, lntd_ko ko, char *buf,
                              size_t size)
{
	task->ko = ko;
	task->buf = buf;
	task->size = size;
	lntd_async_task_submit(pool, task->parent, task_ck, userstate);
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

void lntd_io_task_write_submit(struct lntd_async_pool *pool,
                               struct lntd_io_task_write *task,
                               union lntd_async_ck task_ck,
                               void *userstate, lntd_ko ko,
                               char const *buf, size_t size)
{
	task->ko = ko;
	task->buf = buf;
	task->size = size;
	return lntd_async_task_submit(pool, task->parent, task_ck,
	                              userstate);
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

	size_t bytes_read_delta;
	{
		DWORD xx;
		if (!ReadFile(ko, buf + bytes_read, bytes_left, &xx,
		              0)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);

			if (HRESULT_FROM_WIN32(ERROR_HANDLE_EOF) ==
			    err) {
				err = 0;
				goto complete_task;
			}

			if (HRESULT_FROM_WIN32(WSAEINTR) == err)
				goto submit_retry;

			if (HRESULT_FROM_WIN32(WSAEWOULDBLOCK) == err)
				goto wait_on_poll;

			goto complete_task;
		}
		bytes_read_delta = xx;
	}

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

	size_t bytes_wrote_delta;
	{
		DWORD xx;
		if (!WriteFile(ko, buf + bytes_wrote, bytes_left, &xx,
		               0)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);

			if (HRESULT_FROM_WIN32(WSAEINTR) == err)
				goto submit_retry;

			if (HRESULT_FROM_WIN32(WSAEWOULDBLOCK) == err)
				goto wait_on_poll;

			if (err != 0)
				goto complete_task;

			goto complete_task;
		}
		bytes_wrote_delta = xx;
	}

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

static lntd_error poll_one(lntd_ko ko, short events, short *reventsp)
{
	lntd_error err;

	short revents;
	{
		struct pollfd pollfd = {.fd = (SOCKET)ko,
		                        .events = events};
		int poll_status = WSAPoll(&pollfd, 1U, -1);
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
		err = LNTD_ERROR_INVALID_KO;

	return err;
}
