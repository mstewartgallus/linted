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

#include "config.h"

#include "lntd/signal.h"

#include "lntd/async.h"
#include "lntd/mem.h"
#include "lntd/fifo.h"
#include "lntd/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <signal.h>

#include <windows.h>

/**
 * @bug Either sockets or pipes need to be used
 */

enum { LNTD_SIGNAL_HUP,
       LNTD_SIGNAL_INT,
       LNTD_SIGNAL_TERM,
       LNTD_SIGNAL_QUIT,
       NUM_SIGS };

static lntd_ko sigpipe_reader = (lntd_ko)-1;
static lntd_ko sigpipe_writer = (lntd_ko)-1;

struct lntd_signal_task_wait {
	struct lntd_async_task *parent;
	void *data;
	int signo;
};

static void write_one(lntd_ko ko);

static atomic_int sigint_signalled;
static atomic_int sigterm_signalled;

static BOOL WINAPI sigint_handler_routine(DWORD dwCtrlType);
static BOOL WINAPI sigterm_handler_routine(DWORD dwCtrlType);

lntd_error lntd_signal_init(void)
{
	lntd_error err = 0;

	lntd_ko reader;
	lntd_ko writer;
	{
		lntd_ko xx;
		lntd_ko yy;
		err = lntd_fifo_pair(&xx, &yy, 0);
		if (err != 0)
			return err;
		reader = xx;
		writer = yy;
	}
	sigpipe_reader = reader;
	sigpipe_writer = writer;

	return 0;
}

lntd_error
lntd_signal_task_wait_create(struct lntd_signal_task_wait **taskp,
                             void *data)
{
	lntd_error err;
	struct lntd_signal_task_wait *task;
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
		err = lntd_async_task_create(
		    &xx, task, LNTD_ASYNCH_TASK_SIGNAL_WAIT);
		if (err != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;

free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_signal_task_wait_destroy(struct lntd_signal_task_wait *task)
{
	lntd_async_task_destroy(task->parent);
	lntd_mem_free(task);
}

void *lntd_signal_task_wait_data(struct lntd_signal_task_wait *task)
{
	return task->data;
}

int lntd_signal_task_wait_signo(struct lntd_signal_task_wait *task)
{
	return task->signo;
}

struct lntd_async_task *
lntd_signal_task_wait_prepare(struct lntd_signal_task_wait *task,
                              union lntd_async_ck task_ck,
                              void *userstate)
{
	return lntd_async_task_prepare(task->parent, task_ck,
	                               userstate);
}

struct lntd_async_task *
lntd_signal_task_wait_to_async(struct lntd_signal_task_wait *task)
{
	return task->parent;
}

struct lntd_signal_task_wait *
lntd_signal_task_wait_from_async(struct lntd_async_task *task)
{
	return lntd_async_task_data(task);
}

void lntd_signal_do_wait(struct lntd_async_pool *pool,
                         struct lntd_async_task *task)
{
	struct lntd_signal_task_wait *task_wait =
	    lntd_async_task_data(task);
	lntd_error err = 0;

	int signo = -1;

retry:
	for (;;) {
		char dummy;
		DWORD xx;
		if (!ReadFile(sigpipe_reader, &dummy, sizeof dummy, &xx,
		              0)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);

			goto complete;
		}
	}
	err = 0;

	if (atomic_fetch_and_explicit(&sigint_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGINT;
		goto complete;
	}

	if (atomic_fetch_and_explicit(&sigterm_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGTERM;
		goto complete;
	}

	goto retry;

complete:
	task_wait->signo = signo;

	if (0 == err) {
		write_one(sigpipe_writer);
	}

	lntd_async_pool_complete(pool, task, err);
}

char const *lntd_signal_string(int signo)
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

void lntd_signal_listen_to_sighup(void)
{
}

void lntd_signal_listen_to_sigint(void)
{
	SetConsoleCtrlHandler(sigint_handler_routine, true);
}

void lntd_signal_listen_to_sigquit(void)
{
}

void lntd_signal_listen_to_sigterm(void)
{
	SetConsoleCtrlHandler(sigterm_handler_routine, true);
}

static BOOL WINAPI sigint_handler_routine(DWORD dwCtrlType)
{
	switch (dwCtrlType) {
	case CTRL_C_EVENT:
		atomic_store_explicit(&sigint_signalled, 1,
		                      memory_order_seq_cst);
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
		atomic_store_explicit(&sigterm_signalled, 1,
		                      memory_order_seq_cst);
		write_one(sigpipe_writer);
		return true;

	default:
		return false;
	}
}

static void write_one(lntd_ko ko)
{
	static char const dummy;

	lntd_error err = 0;

	for (;;) {
		size_t result;
		{
			DWORD xx = 0;
			if (!WriteFile(ko, &dummy, 1U, &xx, 0)) {
				err =
				    HRESULT_FROM_WIN32(GetLastError());
				LNTD_ASSUME(err != 0);
			}
			result = xx;
		}
		if (0 == result)
			continue;
	}
	LNTD_ASSERT(err !=
	            HRESULT_FROM_WIN32(ERROR_INVALID_USER_BUFFER));
	LNTD_ASSERT(err != LNTD_ERROR_OUT_OF_MEMORY);
	LNTD_ASSERT(err != HRESULT_FROM_WIN32(ERROR_NOT_ENOUGH_QUOTA));
	LNTD_ASSERT(err != HRESULT_FROM_WIN32(ERROR_BROKEN_PIPE));
	LNTD_ASSERT(0 == err);
}
