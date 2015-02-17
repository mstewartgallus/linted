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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/io.h"

#include "linted/asynch.h"
#include "linted/mem.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/util.h"

#include <assert.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <windows.h>
#include <winsock2.h>

struct linted_io_task_accept
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	linted_ko ko;
	linted_ko returned_ko;
};

struct linted_io_task_poll
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	linted_ko ko;
	short events;
	short revents;
};

struct linted_io_task_read
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t current_position;
	size_t bytes_read;
	linted_ko ko;
};

struct linted_io_task_write
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t current_position;
	size_t bytes_wrote;
	linted_ko ko;
};

struct linted_io_task_recv
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t bytes_read;
	linted_ko ko;
};

struct linted_io_task_sendto
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t bytes_wrote;
	linted_ko ko;
	struct sockaddr_storage dest_addr;
	size_t dest_addr_size;
};

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(short revents);

linted_error linted_io_read_all(linted_ko ko, size_t *bytes_read_out, void *buf,
                                size_t size)
{
	size_t bytes_read = 0U;
	size_t bytes_left = size;

	linted_error errnum = 0;

restart_reading:
	;
	size_t bytes_read_delta;
	{
		DWORD xx;
		if (!ReadFile(ko, (char *)buf + bytes_read, bytes_left, &xx,
		              0)) {
			errnum = GetLastError();
			LINTED_ASSUME(errnum != 0);

			if (ERROR_HANDLE_EOF == errnum) {
				errnum = 0;
				goto finish_reading;
			}

			if (WSAEINTR == errnum)
				goto restart_reading;

			if (WSAEWOULDBLOCK == errnum)
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
	return errnum;

poll_for_readability:
	;
	short revents;
	{
		short xx;
		errnum = poll_one(ko, POLLIN, &xx);
		if (WSAEINTR == errnum)
			goto poll_for_readability;
		if (errnum != 0)
			goto finish_reading;
		revents = xx;
	}

	errnum = check_for_poll_error(revents);
	if (errnum != 0)
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
	linted_error errnum = 0;
	size_t bytes_wrote = 0U;
	size_t bytes_left = size;

restart_writing:
	;
	size_t bytes_wrote_delta;
	{
		DWORD xx;
		if (!WriteFile(ko, (char const *)buf + bytes_wrote, bytes_left,
		               &xx, 0)) {
			errnum = GetLastError();
			LINTED_ASSUME(errnum != 0);

			if (WSAEINTR == errnum)
				goto restart_writing;

			if (WSAEWOULDBLOCK == errnum)
				goto poll_for_writeability;

			if (errnum != 0)
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
	return errnum;

poll_for_writeability:
	;
	short revents;
	{
		short xx;
		errnum = poll_one(ko, POLLOUT, &xx);
		if (WSAEINTR == errnum)
			goto poll_for_writeability;
		if (errnum != 0)
			goto write_bytes_wrote;
		revents = xx;
	}

	errnum = check_for_poll_error(revents);
	if (errnum != 0)
		goto write_bytes_wrote;

	if ((revents & POLLOUT) != 0)
		goto restart_writing;

	LINTED_ASSUME_UNREACHABLE();
}

linted_error linted_io_write_string(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s)
{
	return linted_io_write_all(ko, bytes_wrote_out, s, strlen(s));
}

linted_error linted_io_write_format(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *format_str, ...)
{
	return LINTED_ERROR_UNIMPLEMENTED;
}

linted_error linted_io_task_poll_create(struct linted_io_task_poll **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_io_task_poll *task;
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

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
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
	return errnum;
}

void linted_io_task_poll_destroy(struct linted_io_task_poll *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_poll_prepare(struct linted_io_task_poll *task,
                                 unsigned task_action, linted_ko ko, int flags)
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

linted_error linted_io_task_read_create(struct linted_io_task_read **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_io_task_read *task;
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
		                                   LINTED_ASYNCH_TASK_READ);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
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
	return errnum;
}

void linted_io_task_read_destroy(struct linted_io_task_read *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_read_prepare(struct linted_io_task_read *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t size)
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

linted_error linted_io_task_write_create(struct linted_io_task_write **taskp,
                                         void *data)
{
	linted_error errnum;
	struct linted_io_task_write *task;
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
		                                   LINTED_ASYNCH_TASK_WRITE);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
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
	return errnum;
}

void linted_io_task_write_destroy(struct linted_io_task_write *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_write_prepare(struct linted_io_task_write *task,
                                  unsigned task_action, linted_ko ko,
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

linted_error linted_io_task_recv_create(struct linted_io_task_recv **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_io_task_recv *task;
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
		                                   LINTED_ASYNCH_TASK_RECV);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
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
	return errnum;
}

void linted_io_task_recv_destroy(struct linted_io_task_recv *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_recv_prepare(struct linted_io_task_recv *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;
}

struct linted_io_task_recv *
linted_io_task_recv_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_io_task_recv_to_asynch(struct linted_io_task_recv *task)
{
	return task->parent;
}

void *linted_io_task_recv_data(struct linted_io_task_recv *task)
{
	return task->data;
}

linted_ko linted_io_task_recv_ko(struct linted_io_task_recv *task)
{
	return task->ko;
}

size_t linted_io_task_recv_bytes_read(struct linted_io_task_recv *task)
{
	return task->bytes_read;
}

linted_error linted_io_task_sendto_create(struct linted_io_task_sendto **taskp,
                                          void *data)
{
	linted_error errnum;
	struct linted_io_task_sendto *task;
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
		                                   LINTED_ASYNCH_TASK_SENDTO);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;

free_parent:
	linted_asynch_task_destroy(parent);

free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_io_task_sendto_destroy(struct linted_io_task_sendto *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_sendto_prepare(struct linted_io_task_sendto *task,
                                   unsigned task_action, linted_ko ko,
                                   char const *buf, size_t size,
                                   struct sockaddr const *addr,
                                   size_t dest_addr_size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;

	memset(&task->dest_addr, 0, sizeof task->dest_addr);
	memcpy(&task->dest_addr, addr, dest_addr_size);
	task->dest_addr_size = dest_addr_size;
}

struct linted_io_task_sendto *
linted_io_task_sendto_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_io_task_sendto_to_asynch(struct linted_io_task_sendto *task)
{
	return task->parent;
}

void *linted_io_task_sendto_data(struct linted_io_task_sendto *task)
{
	return task->data;
}

linted_error linted_io_task_accept_create(struct linted_io_task_accept **taskp,
                                          void *data)
{
	linted_error errnum;
	struct linted_io_task_accept *task;
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
		                                   LINTED_ASYNCH_TASK_ACCEPT);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
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
	return errnum;
}

void linted_io_task_accept_destroy(struct linted_io_task_accept *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_io_task_accept_prepare(struct linted_io_task_accept *task,
                                   unsigned task_action, linted_ko ko)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
}

struct linted_io_task_accept *
linted_io_task_accept_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_io_task_accept_to_asynch(struct linted_io_task_accept *task)
{
	return task->parent;
}

void *linted_io_task_accept_data(struct linted_io_task_accept *task)
{
	return task->data;
}

linted_ko linted_io_task_accept_returned_ko(struct linted_io_task_accept *task)
{
	return task->returned_ko;
}

void linted_ko_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_io_task_poll *task_poll =
	    linted_io_task_poll_from_asynch(task);

	struct linted_asynch_waiter *waiter = task_poll->waiter;
	linted_ko ko = task_poll->ko;
	short events = task_poll->events;

	short revents = linted_asynch_waiter_revents(waiter);
	if (0 == revents) {
		linted_asynch_pool_wait_on_poll(pool, waiter, task, ko, events);
		return;
	}

	task_poll->revents = revents;
	linted_asynch_pool_complete(pool, task, 0);
}

void linted_ko_do_read(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_io_task_read *task_read =
	    linted_io_task_read_from_asynch(task);
	size_t bytes_read = task_read->current_position;
	size_t bytes_left = task_read->size - bytes_read;

	linted_ko ko = task_read->ko;
	char *buf = task_read->buf;

	struct linted_asynch_waiter *waiter = task_read->waiter;

	linted_error errnum = 0;

	size_t bytes_read_delta;
	{
		DWORD xx;
		if (!ReadFile(ko, buf + bytes_read, bytes_left, &xx, 0)) {
			errnum = GetLastError();
			LINTED_ASSUME(errnum != 0);

			if (ERROR_HANDLE_EOF == errnum) {
				errnum = 0;
				goto complete_task;
			}

			if (WSAEINTR == errnum)
				goto submit_retry;

			if (WSAEWOULDBLOCK == errnum)
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

	linted_asynch_pool_complete(pool, task, errnum);
	return;

submit_retry:
	task_read->bytes_read = bytes_read;
	task_read->current_position = bytes_read;
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, waiter, task, ko, POLLIN);
}

void linted_ko_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *task)
{
	struct linted_io_task_write *task_write =
	    linted_io_task_write_from_asynch(task);
	size_t bytes_wrote = task_write->current_position;
	size_t bytes_left = task_write->size - bytes_wrote;

	linted_error errnum = 0;

	linted_ko ko = task_write->ko;
	char const *buf = task_write->buf;

	size_t bytes_wrote_delta;
	{
		DWORD xx;
		if (!WriteFile(ko, buf + bytes_wrote, bytes_left, &xx, 0)) {
			errnum = GetLastError();
			LINTED_ASSUME(errnum != 0);

			if (WSAEINTR == errnum)
				goto submit_retry;

			if (WSAEWOULDBLOCK == errnum)
				goto wait_on_poll;

			if (errnum != 0)
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

	linted_asynch_pool_complete(pool, task, errnum);
	return;

submit_retry:
	task_write->bytes_wrote = bytes_wrote;
	task_write->current_position = bytes_wrote;

	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, task_write->waiter, task, ko,
	                                POLLOUT);
}

void linted_ko_do_recv(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_io_task_recv *task_recv =
	    linted_io_task_recv_from_asynch(task);
	size_t bytes_read = 0U;
	size_t size = task_recv->size;

	linted_ko ko = task_recv->ko;
	char *buf = task_recv->buf;

	struct linted_asynch_waiter *waiter = task_recv->waiter;

	linted_error errnum = 0;

	int result = recv((SOCKET)ko, buf, size, 0);
	if (SOCKET_ERROR == result) {
		errnum = WSAGetLastError();
		LINTED_ASSUME(errnum != 0);
	}

	if (WSAEINTR == errnum)
		goto submit_retry;

	if (WSAEWOULDBLOCK == errnum)
		goto wait_on_poll;

	if (errnum != 0)
		goto complete_task;

	bytes_read = result;

complete_task:
	task_recv->bytes_read = bytes_read;

	linted_asynch_pool_complete(pool, task, errnum);
	return;

submit_retry:
	task_recv->bytes_read = bytes_read;
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, waiter, task, ko, POLLIN);
}

void linted_ko_do_sendto(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task)
{
	struct linted_io_task_sendto *task_sendto =
	    linted_io_task_sendto_from_asynch(task);
	size_t bytes_wrote = 0U;
	size_t size = task_sendto->size;

	linted_error errnum = 0;

	linted_ko ko = task_sendto->ko;
	char const *buf = task_sendto->buf;

	int result =
	    sendto((SOCKET)ko, buf, size, 0, (void *)&task_sendto->dest_addr,
	           task_sendto->dest_addr_size);
	if (SOCKET_ERROR == result) {
		errnum = WSAGetLastError();
		LINTED_ASSUME(errnum != 0);
	}

	if (WSAEINTR == errnum)
		goto submit_retry;

	if (WSAEWOULDBLOCK == errnum)
		goto wait_on_poll;

	if (errnum != 0)
		goto complete_task;

	bytes_wrote = result;

complete_task:
	task_sendto->bytes_wrote = bytes_wrote;

	linted_asynch_pool_complete(pool, task, errnum);
	return;

submit_retry:
	task_sendto->bytes_wrote = bytes_wrote;
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, task_sendto->waiter, task, ko,
	                                POLLOUT);
}

void linted_ko_do_accept(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task)
{
	linted_asynch_pool_complete(pool, task, LINTED_ERROR_UNIMPLEMENTED);
}

static linted_error poll_one(linted_ko ko, short events, short *reventsp)
{
	linted_error errnum;

	short revents;
	{
		struct pollfd pollfd = {.fd = (SOCKET)ko, .events = events};
		int poll_status = WSAPoll(&pollfd, 1U, -1);
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

static linted_error check_for_poll_error(short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = LINTED_ERROR_INVALID_KO;

	return errnum;
}
