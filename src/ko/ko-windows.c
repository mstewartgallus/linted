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
#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/ko.h"

#include "linted/error.h"
#include "linted/mem.h"
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

#define POLLIN 1U
#define POLLOUT (1U << 1U)
#define POLLNVAL (1U << 2U)

static void ko_to_str(char *buf, linted_ko fd);

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);

linted_error linted_ko_from_cstring(char const *str, linted_ko *kop)
{
	size_t length = strlen(str);
	uintptr_t position = 1U;

	if ('0' == str[0U] && length != 1U)
		return EINVAL;

	uintptr_t total = 0U;
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

		uintmax_t next_position = 10U * position;
		if (next_position > UINTPTR_MAX)
			return ERANGE;
		position = next_position;
	}

	*kop = (linted_ko)total;
	return 0;
}

linted_error linted_ko_dummy(linted_ko *kop)
{
	return ENOSYS;
}

/**
 * @bug dirko is not respected.
 */
linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
{
	linted_error errnum;

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

	if (ko_sync)
		return ENOSYS;

	if (ko_directory)
		return ENOSYS;

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
	do {
		short xx;
		errnum = poll_one(ko, events, &xx);
		if (0 == errnum)
			revents = xx;
	} while (EINTR == errnum);

	task_poll->revents = revents;
	task->errnum = errnum;

	linted_asynch_pool_complete(pool, task);
}

/**
 * @bug doesn't support sockets
 */
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
	for (;;) {
		size_t bytes_read_delta;
		{
			DWORD xx;
			if (!ReadFile(ko, buf + bytes_read, bytes_left, &xx,
			              NULL)) {
				errnum = GetLastError();
				LINTED_ASSUME(errnum != 0);

				if (ERROR_HANDLE_EOF == errnum) {
					errnum = 0;
					break;
				}

				break;
			}
			bytes_read_delta = xx;
		}

		bytes_read += bytes_read_delta;
		bytes_left -= bytes_read_delta;
		if (0U == bytes_left)
			break;
	}

	task->errnum = errnum;
	task_read->bytes_read = bytes_read;
	task_read->current_position = 0U;

	linted_asynch_pool_complete(pool, task);
}

/**
 * @bug doesn't support sockets
 */
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

	for (;;) {
		size_t bytes_wrote_delta;
		{
			DWORD xx;
			if (!WriteFile(ko, buf + bytes_wrote, bytes_left, &xx,
			               NULL)) {
				errnum = GetLastError();
				LINTED_ASSUME(errnum != 0);
				break;
			}
			bytes_wrote_delta = xx;
		}

		bytes_wrote += bytes_wrote_delta;
		bytes_left -= bytes_wrote_delta;
		if (0U == bytes_left)
			break;
	}

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

	linted_ko new_ko = INVALID_HANDLE_VALUE;
	linted_error errnum;
	linted_ko ko = task_accept->ko;

	for (;;) {
		new_ko = (linted_ko)WSAAccept((uintptr_t)ko, NULL, 0, NULL, 0);
		if ((linted_ko)INVALID_SOCKET == new_ko) {
			errnum = WSAGetLastError();
			LINTED_ASSUME(errnum != 0);
		} else {
			errnum = 0;
		}

		/* Retry on network error */
		switch (errnum) {
		case WSAEINTR:
		case WSAECONNREFUSED:
		case WSAECONNRESET:
		case WSAEINPROGRESS:
		case WSAENETDOWN:
		case WSAENOBUFS:
		case WSATRY_AGAIN:
			continue;
		}

		if (errnum != WSAEWOULDBLOCK)
			break;

		short revents = 0;
		do {
			short xx;
			errnum = poll_one(ko, POLLIN, &xx);
			if (0 == errnum)
				revents = xx;
		} while (WSAEINTR == errnum);
		if (errnum != 0)
			break;

		if ((errnum = check_for_poll_error(ko, revents)) != 0)
			break;
	}

	task->errnum = errnum;
	task_accept->returned_ko = new_ko;

	linted_asynch_pool_complete(pool, task);
}

static linted_error poll_one(linted_ko ko, short events, short *revents)
{
	return ENOSYS;
}

static linted_error check_for_poll_error(linted_ko ko, short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = EBADF;

	return errnum;
}

static void ko_to_str(char *buf, linted_ko ko)
{
	size_t strsize = 0U;
	uintptr_t ko_value = (uintptr_t)ko;

	for (;;) {
		memmove(buf + 1U, buf, strsize);

		uintptr_t digit = ko_value % 10;

		*buf = '0' + digit;

		ko_value /= 10;
		++strsize;

		if (0 == ko)
			break;
	}

	buf[strsize] = '\0';
}
