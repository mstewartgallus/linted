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

enum { LINTED_SIGNAL_HUP,
       LINTED_SIGNAL_INT,
       LINTED_SIGNAL_TERM,
       LINTED_SIGNAL_QUIT,
       NUM_SIGS };

static linted_ko sigpipe_reader = (linted_ko)-1;
static linted_ko sigpipe_writer = (linted_ko)-1;

struct linted_signal_task_wait
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	int signo;
};

static void report_sigint(int signo);
static void report_sigterm(int signo);

static int volatile sigint_signalled;
static int volatile sigterm_signalled;

static int const signals[NUM_SIGS] = {[LINTED_SIGNAL_INT] = SIGINT,
                                      [LINTED_SIGNAL_TERM] = SIGTERM};

static void (*const sighandlers[NUM_SIGS])(
    int) = {[LINTED_SIGNAL_INT] = report_sigint,
            [LINTED_SIGNAL_TERM] = report_sigterm};

linted_error linted_signal_init(void)
{
	linted_error errnum = 0;

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

	sigpipe_reader = reader;
	sigpipe_writer = writer;

	return 0;
}

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
	struct linted_asynch_waiter *waiter;
	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
			goto free_parent;
		waiter = xx;
	}
	task->waiter = waiter;
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

void linted_signal_task_wait_destroy(struct linted_signal_task_wait *task)
{
	linted_asynch_waiter_destroy(task->waiter);
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
	struct linted_asynch_waiter *waiter = task_wait->waiter;

	int signo = -1;

	for (;;) {
		char dummy;
		DWORD xx;
		if (!ReadFile(sigpipe_reader, &dummy, sizeof dummy, &xx, 0)) {
			errnum = GetLastError();
			LINTED_ASSUME(errnum != 0);

			if (WSAEWOULDBLOCK == errnum)
				break;

			if (WSAEINTR == errnum)
				goto resubmit;

			goto complete;
		}
	}
	errnum = 0;

	if (__atomic_fetch_and(&sigint_signalled, 0, __ATOMIC_SEQ_CST)) {
		signo = SIGINT;
		goto complete;
	}

	if (__atomic_fetch_and(&sigterm_signalled, 0, __ATOMIC_SEQ_CST)) {
		signo = SIGTERM;
		goto complete;
	}

	goto wait_on_poll;

complete:
	task_wait->signo = signo;

	if (0 == errnum) {
		char dummy = 0;
		linted_io_write_all(sigpipe_writer, 0, &dummy, sizeof dummy);
	}

	linted_asynch_pool_complete(pool, task, errnum);
	return;

resubmit:
	linted_asynch_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, waiter, task, sigpipe_reader,
	                                POLLIN);
}

char const *linted_signal_string(int signo)
{
	switch (signo) {
	case SIGABRT:
		return "sigabrt";

	case SIGFPE:
		return "sigfpe";

	case SIGILL:
		return "sigill";

	case SIGINT:
		return "sigint";

	case SIGSEGV:
		return "sigsegv";

	case SIGTERM:
		return "sigterm";

	default:
		return 0;
	}
}

static void listen_to_signal(size_t ii);

void linted_signal_listen_to_sighup(void)
{
}

void linted_signal_listen_to_sigint(void)
{
	listen_to_signal(LINTED_SIGNAL_INT);
}

void linted_signal_listen_to_sigquit(void)
{
}

void linted_signal_listen_to_sigterm(void)
{
	listen_to_signal(LINTED_SIGNAL_TERM);
}

static void listen_to_signal(size_t ii)
{
	linted_error errnum;
	int signo = signals[ii];
	void (*handler)(int) = sighandlers[ii];
	if (SIG_ERR == signal(signo, handler)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(false);
	}
}

static char const dummy;

static void report_sigint(int signo)
{
	linted_error errnum = errno;
	signal(SIGINT, report_sigint);
	__atomic_store_n(&sigint_signalled, 1, __ATOMIC_SEQ_CST);
	linted_io_write_all(sigpipe_writer, 0, &dummy, sizeof dummy);
	errno = errnum;
}

static void report_sigterm(int signo)
{
	linted_error errnum = errno;
	signal(SIGTERM, report_sigterm);
	__atomic_store_n(&sigterm_signalled, 1, __ATOMIC_SEQ_CST);
	linted_io_write_all(sigpipe_writer, 0, &dummy, sizeof dummy);
	errno = errnum;
}
