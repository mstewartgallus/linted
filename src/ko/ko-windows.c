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
#define _WIN32_WINNT 0x0600

#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/ko.h"

#include "linted/mem.h"
#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <winsock2.h>

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

struct linted_ko_task_read
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

struct linted_ko_task_write
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

struct linted_ko_task_recv
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t bytes_read;
	linted_ko ko;
};

struct linted_ko_task_sendto
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

	*kop = (linted_ko)total;
	return 0;
}

/**
 * @bug dirko is not respected.
 */
linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
{
	linted_error errnum;

	if ((flags & ~LINTED_KO_RDONLY & ~LINTED_KO_WRONLY & ~LINTED_KO_RDWR &
	     ~LINTED_KO_APPEND & ~LINTED_KO_SYNC & ~LINTED_KO_DIRECTORY) != 0U)
		return EINVAL;

	bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0U;
	bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0U;
	bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0U;

	bool ko_append = (flags & LINTED_KO_APPEND) != 0U;
	bool ko_sync = (flags & LINTED_KO_SYNC) != 0U;

	bool ko_directory = (flags & LINTED_KO_DIRECTORY) != 0U;

	if (ko_rdonly && ko_wronly)
		return EINVAL;

	if (ko_rdwr && ko_rdonly)
		return EINVAL;

	if (ko_rdwr && ko_wronly)
		return EINVAL;

	if (ko_append && !ko_wronly)
		return EINVAL;

	if ((ko_directory && ko_rdonly) || (ko_directory && ko_wronly) ||
	    (ko_directory && ko_rdwr) || (ko_directory && ko_sync))
		return EINVAL;

	DWORD desired_access = 0;

	if (ko_rdonly)
		desired_access |= GENERIC_READ;

	if (ko_wronly)
		desired_access |= GENERIC_WRITE;

	if (ko_rdwr)
		desired_access |= GENERIC_READ | GENERIC_WRITE;

	size_t buffer_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
	                                         pathname, -1, NULL, 0);
	if (0 == buffer_size) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	wchar_t *buffer;
	{
		void *xx;
		errnum =
		    linted_mem_alloc_array(&xx, buffer_size, sizeof buffer[0U]);
		if (errnum != 0)
			return errnum;
		buffer = xx;
	}

	if (0 == MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, pathname,
	                             -1, buffer, buffer_size)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		goto free_buffer;
	}

	linted_ko ko =
	    CreateFile(buffer, desired_access, 0, NULL, OPEN_EXISTING, 0, NULL);
	if (INVALID_HANDLE_VALUE == ko) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
	}

free_buffer:
	linted_mem_free(buffer);

	if (errnum != 0)
		return errnum;

	*kop = ko;

	return 0;
}

linted_error linted_ko_reopen(linted_ko *kooutp, linted_ko koin,
                              unsigned long flags)
{
	return ENOSYS;
}

linted_error linted_ko_close(linted_ko ko)
{
	linted_error errnum;

	if (SOCKET_ERROR == closesocket((uintptr_t)ko)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		if (errnum != WSAENOTSOCK)
			return errnum;
	}

	if (!CloseHandle(ko)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
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

void linted_ko_task_poll_destroy(struct linted_ko_task_poll *task)
{
	linted_asynch_waiter_destroy(task->waiter);
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

linted_error linted_ko_task_read_create(struct linted_ko_task_read **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_ko_task_read *task;
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

void linted_ko_task_read_destroy(struct linted_ko_task_read *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_ko_task_read_prepare(struct linted_ko_task_read *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;
}

struct linted_ko_task_read *
linted_ko_task_read_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_ko_task_read_to_asynch(struct linted_ko_task_read *task)
{
	return task->parent;
}

void *linted_ko_task_read_data(struct linted_ko_task_read *task)
{
	return task->data;
}

linted_ko linted_ko_task_read_ko(struct linted_ko_task_read *task)
{
	return task->ko;
}

size_t linted_ko_task_read_bytes_read(struct linted_ko_task_read *task)
{
	return task->bytes_read;
}

linted_error linted_ko_task_write_create(struct linted_ko_task_write **taskp,
                                         void *data)
{
	linted_error errnum;
	struct linted_ko_task_write *task;
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

void linted_ko_task_write_destroy(struct linted_ko_task_write *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_ko_task_write_prepare(struct linted_ko_task_write *task,
                                  unsigned task_action, linted_ko ko,
                                  char const *buf, size_t size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;
}

struct linted_ko_task_write *
linted_ko_task_write_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_ko_task_write_to_asynch(struct linted_ko_task_write *task)
{
	return task->parent;
}

void *linted_ko_task_write_data(struct linted_ko_task_write *task)
{
	return task->data;
}

linted_error linted_ko_task_recv_create(struct linted_ko_task_recv **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_ko_task_recv *task;
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

void linted_ko_task_recv_destroy(struct linted_ko_task_recv *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_ko_task_recv_prepare(struct linted_ko_task_recv *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t size)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = size;
}

struct linted_ko_task_recv *
linted_ko_task_recv_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_ko_task_recv_to_asynch(struct linted_ko_task_recv *task)
{
	return task->parent;
}

void *linted_ko_task_recv_data(struct linted_ko_task_recv *task)
{
	return task->data;
}

linted_ko linted_ko_task_recv_ko(struct linted_ko_task_recv *task)
{
	return task->ko;
}

size_t linted_ko_task_recv_bytes_read(struct linted_ko_task_recv *task)
{
	return task->bytes_read;
}

linted_error linted_ko_task_sendto_create(struct linted_ko_task_sendto **taskp,
                                          void *data)
{
	linted_error errnum;
	struct linted_ko_task_sendto *task;
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

void linted_ko_task_sendto_destroy(struct linted_ko_task_sendto *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_ko_task_sendto_prepare(struct linted_ko_task_sendto *task,
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

struct linted_ko_task_sendto *
linted_ko_task_sendto_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_ko_task_sendto_to_asynch(struct linted_ko_task_sendto *task)
{
	return task->parent;
}

void *linted_ko_task_sendto_data(struct linted_ko_task_sendto *task)
{
	return task->data;
}

linted_error linted_ko_task_accept_create(struct linted_ko_task_accept **taskp,
                                          void *data)
{
	linted_error errnum;
	struct linted_ko_task_accept *task;
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

void linted_ko_task_accept_destroy(struct linted_ko_task_accept *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_ko_task_accept_prepare(struct linted_ko_task_accept *task,
                                   unsigned task_action, linted_ko ko)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
}

struct linted_ko_task_accept *
linted_ko_task_accept_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_ko_task_accept_to_asynch(struct linted_ko_task_accept *task)
{
	return task->parent;
}

void *linted_ko_task_accept_data(struct linted_ko_task_accept *task)
{
	return task->data;
}

linted_ko linted_ko_task_accept_returned_ko(struct linted_ko_task_accept *task)
{
	return task->returned_ko;
}

void linted_ko_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_ko_task_poll *task_poll =
	    linted_ko_task_poll_from_asynch(task);

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
	struct linted_ko_task_read *task_read =
	    linted_ko_task_read_from_asynch(task);
	size_t bytes_read = task_read->current_position;
	size_t bytes_left = task_read->size - bytes_read;

	linted_ko ko = task_read->ko;
	char *buf = task_read->buf;

	struct linted_asynch_waiter *waiter = task_read->waiter;

	linted_error errnum = 0;

	size_t bytes_read_delta;
	{
		DWORD xx;
		if (!ReadFile(ko, buf + bytes_read, bytes_left, &xx, NULL)) {
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
	struct linted_ko_task_write *task_write =
	    linted_ko_task_write_from_asynch(task);
	size_t bytes_wrote = task_write->current_position;
	size_t bytes_left = task_write->size - bytes_wrote;

	linted_error errnum = 0;

	linted_ko ko = task_write->ko;
	char const *buf = task_write->buf;

	size_t bytes_wrote_delta;
	{
		DWORD xx;
		if (!WriteFile(ko, buf + bytes_wrote, bytes_left, &xx, NULL)) {
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
	struct linted_ko_task_recv *task_recv =
	    linted_ko_task_recv_from_asynch(task);
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
	struct linted_ko_task_sendto *task_sendto =
	    linted_ko_task_sendto_from_asynch(task);
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
	linted_asynch_pool_complete(pool, task, ENOSYS);
}
