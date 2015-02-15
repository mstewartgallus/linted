/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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

#include "linted/signal.h"

#include "linted/asynch.h"
#include "linted/io.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <signal.h>
#include <unistd.h>

#include <windows.h>
#include <winsock2.h>

/**
 * @bug Either sockets or pipes need to be used
 */

static linted_ko sigint_pipe_reader = (linted_ko)-1;
static linted_ko sigint_pipe_writer = (linted_ko)-1;

static linted_ko sigterm_pipe_reader = (linted_ko)-1;
static linted_ko sigterm_pipe_writer = (linted_ko)-1;

struct linted_signal_task_wait
{
	struct linted_asynch_task *parent;
	void *data;
	int signo;
};

static void report_sigint(int signo);
static void report_sigterm(int signo);

linted_error
linted_signal_task_wait_create(struct linted_signal_task_wait **taskp,
                               void *data)
{
	linted_error errnum;
	struct linted_signal_task_wait *task;
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
		errnum = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_SIGNAL_WAIT);
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

void linted_signal_task_wait_destroy(struct linted_signal_task_wait *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_signal_task_wait_data(struct linted_signal_task_wait *task)
{
	return task->data;
}

int linted_signal_task_wait_signo(struct linted_signal_task_wait *task)
{
	return task->signo;
}

void linted_signal_task_wait_prepare(struct linted_signal_task_wait *task,
                                     unsigned task_action)
{
	linted_asynch_task_prepare(task->parent, task_action);
}

struct linted_asynch_task *
linted_signal_task_wait_to_asynch(struct linted_signal_task_wait *task)
{
	return task->parent;
}

struct linted_signal_task_wait *
linted_signal_task_wait_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

void linted_signal_do_wait(struct linted_asynch_pool *pool,
                           struct linted_asynch_task *task)
{
	struct linted_signal_task_wait *task_wait =
	    linted_asynch_task_data(task);
	linted_error errnum = 0;

	struct pollfd pollfds[] = {
	    {.fd = (SOCKET)sigint_pipe_reader, .events = POLLIN},
	    {.fd = (SOCKET)sigterm_pipe_reader, .events = POLLIN}};

	int results = WSAPoll(pollfds, LINTED_ARRAY_SIZE(pollfds), -1);
	if (-1 == results) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (WSAEINTR == errnum)
		goto resubmit;

	if (errnum != 0)
		goto complete;

	int signo = -1;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(pollfds); ++ii) {
		static int const signals[] = {SIGINT, SIGTERM};

		int maybe_signo = signals[ii];

		char dummy;
		if ((pollfds[ii].revents & POLLIN) != 0) {
			if (0 == linted_io_read_all((linted_ko)pollfds[ii].fd,
			                            0, &dummy, sizeof dummy)) {
				signo = maybe_signo;
				break;
			}
		}
	}

	if (-1 == signo)
		goto resubmit;

	task_wait->signo = signo;

complete:
	linted_asynch_pool_complete(pool, task, errnum);
	return;

resubmit:
	linted_asynch_pool_resubmit(pool, task);
}

char const *linted_signal_string(int signo)
{
	extern const char *const sys_siglist[];

	if (signo < 1)
		return 0;

	if (signo >= NSIG)
		return 0;

	return sys_siglist[signo];
}

static linted_error listen_to_signal(int signo);

linted_error linted_signal_listen_to_sighup(void)
{
	return 0;
}

linted_error linted_signal_listen_to_sigint(void)
{
	return listen_to_signal(SIGINT);
}

linted_error linted_signal_listen_to_sigquit(void)
{
	return 0;
}

linted_error linted_signal_listen_to_sigterm(void)
{
	return listen_to_signal(SIGTERM);
}

static linted_error listen_to_signal(int signo)
{
	linted_error errnum;

	linted_ko reader;
	linted_ko writer;
	{
		int xx[2U];
		if (-1 == _pipe(xx, 4096U, 0)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		reader = (linted_ko)_get_osfhandle(xx[0U]);
		writer = (linted_ko)_get_osfhandle(xx[1U]);
	}

	void (*handler)(int);
	switch (signo) {
	case SIGINT:
		sigint_pipe_reader = reader;
		sigint_pipe_writer = writer;
		handler = report_sigint;
		break;

	case SIGTERM:
		sigterm_pipe_reader = reader;
		sigterm_pipe_writer = writer;
		handler = report_sigterm;
		break;

	default:
		assert(false);
	}

	if (SIG_ERR == signal(signo, handler)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(false);
	}

	return 0;
}

static char const dummy;

static void report_sigint(int signo)
{
	linted_error errnum = errno;
	signal(SIGINT, report_sigint);
	linted_io_write_all(sigint_pipe_writer, 0, &dummy, sizeof dummy);
	errno = errnum;
}

static void report_sigterm(int signo)
{
	linted_error errnum = errno;
	signal(SIGTERM, report_sigterm);
	linted_io_write_all(sigterm_pipe_writer, 0, &dummy, sizeof dummy);
	errno = errnum;
}
