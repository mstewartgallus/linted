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
#include <pthread.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

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
	struct sockaddr_storage src_addr;
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t bytes_read;
	size_t src_addr_len;
	linted_ko ko;
};

struct linted_io_task_sendto
{
	struct sockaddr_storage dest_addr;
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t bytes_wrote;
	size_t dest_addr_size;
	linted_ko ko;
};

static void pipe_set_init(void);
static sigset_t const *get_pipe_set(void);
static linted_error eat_sigpipes(void);

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(short revents);

linted_error linted_io_read_all(linted_ko ko, size_t *bytes_read_out, void *buf,
                                size_t size)
{
	size_t bytes_read = 0U;
	size_t bytes_left = size;
	char *buf_offset = buf;

	linted_error errnum = 0;

restart_reading:
	;
	ssize_t result = read(ko, buf_offset, bytes_left);
	if (result < 0) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		if (EINTR == errnum)
			goto restart_reading;
		if (EAGAIN == errnum)
			goto poll_for_readability;
		if (EWOULDBLOCK == errnum)
			goto poll_for_readability;
		return errnum;
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
	return errnum;

poll_for_readability:
	;
	short revents;
	{
		short xx;
		errnum = poll_one(ko, POLLIN, &xx);
		if (EINTR == errnum)
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
	char const *buf_offset = buf;

	sigset_t oldset;
	/* Get EPIPEs */
	/* SIGPIPE may not be blocked already */
	/* Reuse oldset to save on stack space */
	sigemptyset(&oldset);
	sigaddset(&oldset, SIGPIPE);

	errnum = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (errnum != 0)
		goto write_bytes_wrote;

restart_writing:
	;
	ssize_t result = write(ko, buf_offset, bytes_left);
	if (-1 == result) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		if (EINTR == errnum)
			goto restart_writing;
		if (EAGAIN == errnum)
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
	sigset_t sigpipeset;

	sigemptyset(&sigpipeset);
	sigaddset(&sigpipeset, SIGPIPE);

	linted_error wait_errnum;
	do {
		struct timespec timeout = { 0 };

		if (-1 == sigtimedwait(&sigpipeset, 0, &timeout)) {
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
		    pthread_sigmask(SIG_SETMASK, &oldset, 0);
		if (0 == errnum)
			errnum = mask_errnum;
	}

write_bytes_wrote:
	if (bytes_wrote_out != 0)
		*bytes_wrote_out = bytes_wrote;
	return errnum;

poll_for_writeability:
	;
	short revents;
	{
		short xx;
		errnum = poll_one(ko, POLLIN, &xx);
		if (EINTR == errnum)
			goto poll_for_writeability;
		if (errnum != 0)
			goto get_sigpipe;
		revents = xx;
	}

	errnum = check_for_poll_error(revents);
	if (errnum != 0)
		goto get_sigpipe;

	if ((revents & POLLOUT) != 0)
		goto restart_writing;

	LINTED_ASSUME_UNREACHABLE();
}

linted_error linted_io_write_str(linted_ko ko, size_t *bytes_wrote,
                                 struct linted_str str)
{
	return linted_io_write_all(ko, bytes_wrote, str.bytes, str.size);
}

linted_error linted_io_write_string(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s)
{
	return linted_io_write_all(ko, bytes_wrote_out, s, strlen(s));
}

linted_error linted_io_write_format(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *format_str, ...)
{
	linted_error errnum = 0;

	va_list ap;
	va_start(ap, format_str);

	int bytes = vdprintf(ko, format_str, ap);
	if (bytes < 0) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_va_list;
	}

	if (bytes_wrote_out != 0)
		*bytes_wrote_out = bytes;

free_va_list:
	va_end(ap);

	return errnum;
}

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

static linted_error check_for_poll_error(short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = EBADF;

	return errnum;
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

void linted_io_task_recv_src_addr(struct linted_io_task_recv *task,
                                  struct sockaddr *addr, size_t *addr_len)
{
	memcpy(addr, &task->src_addr, *addr_len);
	*addr_len = task->src_addr_len;
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
		linted_asynch_pool_wait_on_poll(pool, waiter, task, ko, events);
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

	linted_error errnum = 0;

	ssize_t result = read(ko, buf + bytes_read, bytes_left);
	if (result < 0) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum)
		goto submit_retry;

	if (EAGAIN == errnum || EWOULDBLOCK == errnum)
		goto wait_on_poll;

	if (errnum != 0)
		goto complete_task;

	size_t bytes_read_delta = result;

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

void linted_io_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *task)
{
	struct linted_io_task_write *task_write =
	    linted_io_task_write_from_asynch(task);
	size_t bytes_wrote = task_write->current_position;
	size_t bytes_left = task_write->size - bytes_wrote;

	linted_error errnum = 0;

	linted_ko ko = task_write->ko;
	char const *buf = task_write->buf;

	char const *buf_offset = buf + bytes_wrote;

	ssize_t result;
	linted_error mask_errnum;
	{
		/* Get EPIPEs */
		/* SIGPIPE may not be blocked already */
		sigset_t oldset;
		errnum = pthread_sigmask(SIG_BLOCK, get_pipe_set(), &oldset);
		if (errnum != 0)
			goto complete_task;

		result = write(ko, buf_offset, bytes_left);
		if (-1 == result) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto reset_sigmask;
		}

		linted_error eat_errnum = eat_sigpipes();
		if (0 == errnum)
			errnum = eat_errnum;

	reset_sigmask:
		mask_errnum = pthread_sigmask(SIG_SETMASK, &oldset, 0);
	}
	if (0 == errnum)
		errnum = mask_errnum;

	if (EINTR == errnum)
		goto submit_retry;

	if (EAGAIN == errnum || EWOULDBLOCK == errnum)
		goto wait_on_poll;

	if (errnum != 0)
		goto complete_task;

	size_t bytes_wrote_delta = result;

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

static struct timespec const zero_timeout = { 0 };

static linted_error eat_sigpipes(void)
{
	linted_error errnum = 0;

	for (;;) {
		int wait_status =
		    sigtimedwait(get_pipe_set(), 0, &zero_timeout);
		if (wait_status != -1)
			continue;

		linted_error wait_errnum = errno;
		LINTED_ASSUME(wait_errnum != 0);

		if (EAGAIN == wait_errnum)
			break;

		if (wait_errnum != EINTR) {
			if (0 == errnum)
				errnum = wait_errnum;
			break;
		}
	}

	return errnum;
}

void linted_io_do_recv(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_io_task_recv *task_recv =
	    linted_io_task_recv_from_asynch(task);
	size_t bytes_read = 0U;
	size_t size = task_recv->size;

	linted_ko ko = task_recv->ko;
	char *buf = task_recv->buf;
	struct sockaddr_storage *src_addr = &task_recv->src_addr;
	memset(src_addr, 0, sizeof *src_addr);

	struct linted_asynch_waiter *waiter = task_recv->waiter;

	linted_error errnum = 0;

	size_t src_addr_len = 0U;
	ssize_t result;
	{
		socklen_t xx = sizeof *src_addr;
		result = recvfrom(ko, buf, size, MSG_DONTWAIT, (void *)src_addr,
		                  &xx);
		if (-1 == result) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
		src_addr_len = xx;
	}

	if (EINTR == errnum)
		goto submit_retry;

	if (EAGAIN == errnum || EWOULDBLOCK == errnum)
		goto wait_on_poll;

	if (errnum != 0)
		goto complete_task;

	bytes_read = result;

complete_task:
	task_recv->bytes_read = bytes_read;
	task_recv->src_addr_len = src_addr_len;

	linted_asynch_pool_complete(pool, task, errnum);
	return;

submit_retry:
	task_recv->bytes_read = bytes_read;
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, waiter, task, ko, POLLIN);
}

void linted_io_do_sendto(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task)
{
	struct linted_io_task_sendto *task_sendto =
	    linted_io_task_sendto_from_asynch(task);
	size_t bytes_wrote = 0U;
	size_t size = task_sendto->size;

	linted_error errnum = 0;

	linted_ko ko = task_sendto->ko;
	char const *buf = task_sendto->buf;

	ssize_t result = sendto(ko, buf, size, MSG_NOSIGNAL | MSG_DONTWAIT,
	                        (void *)&task_sendto->dest_addr,
	                        task_sendto->dest_addr_size);
	if (result < 0) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum)
		goto submit_retry;

	if (EAGAIN == errnum || EWOULDBLOCK == errnum)
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

void linted_io_do_accept(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task)
{
	struct linted_io_task_accept *task_accept =
	    linted_io_task_accept_from_asynch(task);

	linted_error errnum = 0;
	linted_ko ko = task_accept->ko;

	int new_fd = accept4(ko, 0, 0, SOCK_NONBLOCK | SOCK_CLOEXEC);
	if (-1 == new_fd) {
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
		goto submit_retry;
	}

	if (EAGAIN == errnum || EWOULDBLOCK == errnum)
		goto wait_on_poll;

	task_accept->returned_ko = new_fd;

	linted_asynch_pool_complete(pool, task, errnum);
	return;

submit_retry:
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, task_accept->waiter, task, ko,
	                                POLLIN);
}

static sigset_t pipeset;
static pthread_once_t pipe_set_once_control = PTHREAD_ONCE_INIT;

static sigset_t const *get_pipe_set(void)
{
	linted_error errnum;

	errnum = pthread_once(&pipe_set_once_control, pipe_set_init);
	assert(0 == errnum);

	return &pipeset;
}

static void pipe_set_init(void)
{
	sigemptyset(&pipeset);
	sigaddset(&pipeset, SIGPIPE);
}
