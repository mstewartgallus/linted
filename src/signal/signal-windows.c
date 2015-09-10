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

#include "linted/signal.h"

#include "linted/async.h"
#include "linted/mem.h"
#include "linted/fifo.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <signal.h>

#include <windows.h>

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

struct linted_signal_task_wait {
	struct linted_async_task *parent;
	void *data;
	int signo;
};

static void write_one(linted_ko ko);

static int volatile sigint_signalled;
static int volatile sigterm_signalled;

static BOOL WINAPI sigint_handler_routine(DWORD dwCtrlType);
static BOOL WINAPI sigterm_handler_routine(DWORD dwCtrlType);

linted_error linted_signal_init(void)
{
	linted_error err = 0;

	linted_ko reader;
	linted_ko writer;
	{
		linted_ko xx;
		linted_ko yy;
		err = linted_fifo_pair(&xx, &yy, 0);
		if (err != 0)
			return err;
		reader = xx;
		writer = yy;
	}
	sigpipe_reader = reader;
	sigpipe_writer = writer;

	return 0;
}

linted_error
linted_signal_task_wait_create(struct linted_signal_task_wait **taskp,
                               void *data)
{
	linted_error err;
	struct linted_signal_task_wait *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_async_task *parent;
	{
		struct linted_async_task *xx;
		err = linted_async_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_SIGNAL_WAIT);
		if (err != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;

free_task:
	linted_mem_free(task);
	return err;
}

void linted_signal_task_wait_destroy(
    struct linted_signal_task_wait *task)
{
	linted_async_task_destroy(task->parent);
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

void linted_signal_task_wait_prepare(
    struct linted_signal_task_wait *task, union linted_async_ck task_ck)
{
	linted_async_task_prepare(task->parent, task_ck);
}

struct linted_async_task *
linted_signal_task_wait_to_async(struct linted_signal_task_wait *task)
{
	return task->parent;
}

struct linted_signal_task_wait *
linted_signal_task_wait_from_async(struct linted_async_task *task)
{
	return linted_async_task_data(task);
}

void linted_signal_do_wait(struct linted_async_pool *pool,
                           struct linted_async_task *task)
{
	struct linted_signal_task_wait *task_wait =
	    linted_async_task_data(task);
	linted_error err = 0;

	int signo = -1;

retry:
	for (;;) {
		char dummy;
		DWORD xx;
		if (!ReadFile(sigpipe_reader, &dummy, sizeof dummy, &xx,
		              0)) {
			err = GetLastError();
			LINTED_ASSUME(err != 0);

			goto complete;
		}
	}
	err = 0;

	if (__atomic_fetch_and(&sigint_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGINT;
		goto complete;
	}

	if (__atomic_fetch_and(&sigterm_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGTERM;
		goto complete;
	}

	goto retry;

complete:
	task_wait->signo = signo;

	if (0 == err) {
		write_one(sigpipe_writer);
	}

	linted_async_pool_complete(pool, task, err);
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

void linted_signal_listen_to_sighup(void)
{
}

void linted_signal_listen_to_sigint(void)
{
	SetConsoleCtrlHandler(sigint_handler_routine, true);
}

void linted_signal_listen_to_sigquit(void)
{
}

void linted_signal_listen_to_sigterm(void)
{
	SetConsoleCtrlHandler(sigterm_handler_routine, true);
}

static BOOL WINAPI sigint_handler_routine(DWORD dwCtrlType)
{
	switch (dwCtrlType) {
	case CTRL_C_EVENT:
		__atomic_store_n(&sigint_signalled, 1,
		                 __ATOMIC_SEQ_CST);
		write_one(sigpipe_writer);
		return true;

	default:
		return false;
	}
}

static BOOL WINAPI sigterm_handler_routine(DWORD dwCtrlType)
{
	switch (dwCtrlType) {
	case CTRL_BREAK_EVENT:
	case CTRL_CLOSE_EVENT:
	case CTRL_LOGOFF_EVENT:
	case CTRL_SHUTDOWN_EVENT:
		__atomic_store_n(&sigterm_signalled, 1,
		                 __ATOMIC_SEQ_CST);
		write_one(sigpipe_writer);
		return true;

	default:
		return false;
	}
}

static void write_one(linted_ko ko)
{
	static char const dummy;

	linted_error err = 0;

	for (;;) {
		size_t result;
		{
			DWORD xx = 0;
			if (!WriteFile(ko, &dummy, 1U, &xx, 0)) {
				err = GetLastError();
				LINTED_ASSUME(err != 0);
			}
			result = xx;
		}
		if (0 == result)
			continue;

		if (err != WSAEINTR)
			break;
	}
	LINTED_ASSERT(err != ERROR_INVALID_USER_BUFFER);
	LINTED_ASSERT(err != ERROR_NOT_ENOUGH_MEMORY);
	LINTED_ASSERT(err != ERROR_NOT_ENOUGH_QUOTA);
	LINTED_ASSERT(err != ERROR_BROKEN_PIPE);
	LINTED_ASSERT(0 == err);
}
