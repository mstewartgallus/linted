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
#define _GNU_SOURCE

#include "config.h"

#include "linted/signal.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/fifo.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

enum { LINTED_SIGNAL_HUP,
       LINTED_SIGNAL_CHLD,
       LINTED_SIGNAL_INT,
       LINTED_SIGNAL_TERM,
       LINTED_SIGNAL_QUIT,
       NUM_SIGS };

static linted_ko sigpipe_reader = (linted_ko)-1;
static linted_ko sigpipe_writer = (linted_ko)-1;

struct linted_signal_task_wait {
	struct linted_async_task *parent;
	struct linted_async_waiter *waiter;
	void *data;
	int signo;
};

static void write_one(linted_ko ko);

static void report_sigchld(int signo);
static void report_sighup(int signo);
static void report_sigint(int signo);
static void report_sigquit(int signo);
static void report_sigterm(int signo);

static int volatile sigchld_signalled;
static int volatile sighup_signalled;
static int volatile sigint_signalled;
static int volatile sigquit_signalled;
static int volatile sigterm_signalled;

static int const signals[NUM_SIGS] = {[LINTED_SIGNAL_HUP] = SIGHUP,
                                      [LINTED_SIGNAL_CHLD] = SIGCHLD,
                                      [LINTED_SIGNAL_INT] = SIGINT,
                                      [LINTED_SIGNAL_QUIT] = SIGQUIT,
                                      [LINTED_SIGNAL_TERM] = SIGTERM};

static void (*const sighandlers[NUM_SIGS])(
    int) = {[LINTED_SIGNAL_HUP] = report_sighup,
            [LINTED_SIGNAL_CHLD] = report_sigchld,
            [LINTED_SIGNAL_INT] = report_sigint,
            [LINTED_SIGNAL_QUIT] = report_sigquit,
            [LINTED_SIGNAL_TERM] = report_sigterm};

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
	struct linted_async_waiter *waiter;
	{
		struct linted_async_waiter *xx;
		err = linted_async_waiter_create(&xx);
		if (err != 0)
			goto free_parent;
		waiter = xx;
	}
	task->waiter = waiter;
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_parent:
	linted_async_task_destroy(parent);
free_task:
	linted_mem_free(task);
	return err;
}

void linted_signal_task_wait_destroy(
    struct linted_signal_task_wait *task)
{
	linted_async_waiter_destroy(task->waiter);
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

struct linted_async_task *
linted_signal_task_wait_prepare(struct linted_signal_task_wait *task,
                                union linted_async_ck task_ck)
{
	return linted_async_task_prepare(task->parent, task_ck);
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
	struct linted_async_waiter *waiter = task_wait->waiter;

	int signo = -1;

	for (;;) {
		char dummy;
		if (-1 == read(sigpipe_reader, &dummy, sizeof dummy)) {
			err = errno;
			LINTED_ASSUME(err != 0);

			if (EAGAIN == err)
				break;

			if (EINTR == err)
				goto resubmit;

			goto complete;
		}
	}
	err = 0;

	if (__atomic_fetch_and(&sighup_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGHUP;
		goto complete;
	}

	if (__atomic_fetch_and(&sigchld_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGCHLD;
		goto complete;
	}

	if (__atomic_fetch_and(&sigint_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGINT;
		goto complete;
	}

	if (__atomic_fetch_and(&sigquit_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGQUIT;
		goto complete;
	}

	if (__atomic_fetch_and(&sigterm_signalled, 0,
	                       __ATOMIC_SEQ_CST)) {
		signo = SIGTERM;
		goto complete;
	}

	goto wait_on_poll;

complete:
	task_wait->signo = signo;

	if (0 == err) {
		write_one(sigpipe_writer);
	}

	linted_async_pool_complete(pool, task, err);
	return;

resubmit:
	linted_async_pool_resubmit(pool, task);
	return;

wait_on_poll:
	linted_async_pool_wait_on_poll(pool, waiter, task,
	                               sigpipe_reader, POLLIN);
}

#if defined __GLIBC__
char const *linted_signal_string(int signo)
{
	/* GLibc's strsignal can allocate memory on unknown values
	 * which is wrong and bad. */
	static char const unknown_signal[] = "Unknown signal";

	if (signo >= NSIG)
		return unknown_signal;
	if (signo <= 0)
		return unknown_signal;

	return strsignal(signo);
}
#else
char const *linted_signal_string(int signo)
{
	return strsignal(signo);
}
#endif
static void listen_to_signal(size_t ii);

void linted_signal_listen_to_sigchld(void)
{
	listen_to_signal(LINTED_SIGNAL_CHLD);
}

void linted_signal_listen_to_sighup(void)
{
	listen_to_signal(LINTED_SIGNAL_HUP);
}

void linted_signal_listen_to_sigint(void)
{
	listen_to_signal(LINTED_SIGNAL_INT);
}

void linted_signal_listen_to_sigquit(void)
{
	listen_to_signal(LINTED_SIGNAL_QUIT);
}

void linted_signal_listen_to_sigterm(void)
{
	listen_to_signal(LINTED_SIGNAL_TERM);
}

static void listen_to_signal(size_t ii)
{
	linted_error err;
	int signo = signals[ii];
	void (*handler)(int) = sighandlers[ii];

	struct sigaction action = {0};

	action.sa_handler = handler;
	action.sa_flags = 0;

	/*
	 * We cannot block all signals as that may lead to bad
	 * situations when things like stack overflows happen.
	 */
	sigemptyset(&action.sa_mask);
	/* Block SIGPIPEs to get EPIPEs */
	sigaddset(&action.sa_mask, SIGPIPE);

	if (-1 == sigaction(signo, &action, 0)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		LINTED_ASSERT(false);
	}
}

static void report_sigchld(int signo)
{
	linted_error err = errno;
	__atomic_store_n(&sigchld_signalled, 1, __ATOMIC_SEQ_CST);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sighup(int signo)
{
	linted_error err = errno;
	__atomic_store_n(&sighup_signalled, 1, __ATOMIC_SEQ_CST);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sigint(int signo)
{
	linted_error err = errno;
	__atomic_store_n(&sigint_signalled, 1, __ATOMIC_SEQ_CST);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sigquit(int signo)
{
	linted_error err = errno;
	__atomic_store_n(&sigquit_signalled, 1, __ATOMIC_SEQ_CST);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sigterm(int signo)
{
	linted_error err = errno;
	__atomic_store_n(&sigterm_signalled, 1, __ATOMIC_SEQ_CST);
	write_one(sigpipe_writer);
	errno = err;
}

static void write_one(linted_ko ko)
{
	linted_error err = 0;

	static char const dummy;

	for (;;) {
		ssize_t result = write(ko, &dummy, 1U);
		if (-1 == result) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}

		if (0 == result)
			continue;

		if (err != EINTR)
			break;
	}
	/* EAGAIN and EWOULDBLOCK are okay, they mean the pipe is full
	 * and that's fine.
	 */
	if (EAGAIN == err)
		err = 0;
	if (EWOULDBLOCK == err)
		err = 0;

	LINTED_ASSERT(err != EBADF);
	LINTED_ASSERT(err != EDESTADDRREQ);
	LINTED_ASSERT(err != EDQUOT);
	LINTED_ASSERT(err != EFAULT);
	LINTED_ASSERT(err != EFBIG);
	LINTED_ASSERT(err != EINVAL);
	LINTED_ASSERT(err != EIO);
	LINTED_ASSERT(err != ENOSPC);

	/* EPIPE should never happen because SIGPIPE is blocked */
	LINTED_ASSERT(err != EPIPE);

	LINTED_ASSERT(0 == err);
}
