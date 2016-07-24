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

#include "lntd/signal.h"

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/fifo.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/util.h"

#include <errno.h>
#include <poll.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

enum { LNTD_SIGNAL_HUP,
       LNTD_SIGNAL_CHLD,
       LNTD_SIGNAL_INT,
       LNTD_SIGNAL_TERM,
       LNTD_SIGNAL_QUIT,
       NUM_SIGS };

static lntd_ko sigpipe_reader = (lntd_ko)-1;
static lntd_ko sigpipe_writer = (lntd_ko)-1;

struct lntd_signal_task_wait {
	struct lntd_async_task *parent;
	struct lntd_async_waiter *waiter;
	void *data;
	int signo;
};

static void write_one(lntd_ko ko);

static void report_sigchld(int signo);
static void report_sighup(int signo);
static void report_sigint(int signo);
static void report_sigquit(int signo);
static void report_sigterm(int signo);

static atomic_bool sigchld_signalled;
static atomic_bool sighup_signalled;
static atomic_bool sigint_signalled;
static atomic_bool sigquit_signalled;
static atomic_bool sigterm_signalled;

static int const signals[NUM_SIGS] = {[LNTD_SIGNAL_HUP] = SIGHUP,
                                      [LNTD_SIGNAL_CHLD] = SIGCHLD,
                                      [LNTD_SIGNAL_INT] = SIGINT,
                                      [LNTD_SIGNAL_QUIT] = SIGQUIT,
                                      [LNTD_SIGNAL_TERM] = SIGTERM};

static void (*const sighandlers[NUM_SIGS])(
    int) = {[LNTD_SIGNAL_HUP] = report_sighup,
            [LNTD_SIGNAL_CHLD] = report_sigchld,
            [LNTD_SIGNAL_INT] = report_sigint,
            [LNTD_SIGNAL_QUIT] = report_sigquit,
            [LNTD_SIGNAL_TERM] = report_sigterm};

static void *signal_routine(void *arg);

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

	{
		sigset_t set;
		sigemptyset(&set);
		for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(signals);
		     ++ii) {
			sigaddset(&set, signals[ii]);
		}

		err = pthread_sigmask(SIG_BLOCK, &set, 0);
		if (err != 0)
			goto close_fifos;
	}

	pthread_t xx;
	err = pthread_create(&xx, 0, signal_routine, 0);
	if (err != 0)
		goto close_fifos;

	return 0;

close_fifos:
	lntd_ko_close(reader);
	lntd_ko_close(writer);

	return err;
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
	struct lntd_async_waiter *waiter;
	{
		struct lntd_async_waiter *xx;
		err = lntd_async_waiter_create(&xx);
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
	lntd_async_task_destroy(parent);
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_signal_task_wait_destroy(struct lntd_signal_task_wait *task)
{
	lntd_async_waiter_destroy(task->waiter);
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

void lntd_signal_task_wait_cancel(struct lntd_signal_task_wait *task)
{
	lntd_async_task_cancel(task->parent);
}

void lntd_signal_task_wait_submit(struct lntd_async_pool *pool,
                                  struct lntd_signal_task_wait *task,
                                  union lntd_async_ck task_ck,
                                  void *userstate)
{
	lntd_async_task_submit(pool, task->parent, task_ck, userstate);
}

void lntd_signal_do_wait(struct lntd_async_pool *pool,
                         struct lntd_async_task *task)
{
	struct lntd_signal_task_wait *task_wait =
	    lntd_async_task_data(task);
	lntd_error err = 0;
	struct lntd_async_waiter *waiter = task_wait->waiter;

	int signo = -1;

	for (;;) {
		static char dummy;
		if (-1 == read(sigpipe_reader, &dummy, sizeof dummy)) {
			err = errno;
			LNTD_ASSUME(err != 0);

			if (EAGAIN == err)
				break;

			if (EINTR == err)
				goto resubmit;

			goto complete;
		}
	}
	err = 0;

	if (atomic_fetch_and_explicit(&sighup_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGHUP;
		goto complete;
	}

	if (atomic_fetch_and_explicit(&sigchld_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGCHLD;
		goto complete;
	}

	if (atomic_fetch_and_explicit(&sigint_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGINT;
		goto complete;
	}

	if (atomic_fetch_and_explicit(&sigquit_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGQUIT;
		goto complete;
	}

	if (atomic_fetch_and_explicit(&sigterm_signalled, 0,
	                              memory_order_seq_cst)) {
		signo = SIGTERM;
		goto complete;
	}

	goto wait_on_poll;

complete:
	task_wait->signo = signo;

	if (0 == err) {
		write_one(sigpipe_writer);
	}

	lntd_async_pool_complete(pool, task, err);
	return;

resubmit:
	lntd_async_pool_resubmit(pool, task);
	return;

wait_on_poll:
	lntd_async_pool_wait_on_poll(pool, waiter, task, sigpipe_reader,
	                             POLLIN);
}

#if defined __GLIBC__
char const *lntd_signal_string(int signo)
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
char const *lntd_signal_string(int signo)
{
	return strsignal(signo);
}
#endif
static void listen_to_signal(size_t ii);

void lntd_signal_listen_to_sigchld(void)
{
	listen_to_signal(LNTD_SIGNAL_CHLD);
}

void lntd_signal_listen_to_sighup(void)
{
	listen_to_signal(LNTD_SIGNAL_HUP);
}

void lntd_signal_listen_to_sigint(void)
{
	listen_to_signal(LNTD_SIGNAL_INT);
}

void lntd_signal_listen_to_sigquit(void)
{
	listen_to_signal(LNTD_SIGNAL_QUIT);
}

void lntd_signal_listen_to_sigterm(void)
{
	listen_to_signal(LNTD_SIGNAL_TERM);
}

static void *signal_routine(void *arg)
{
	{
		sigset_t set;
		sigemptyset(&set);
		for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(signals);
		     ++ii) {
			sigaddset(&set, signals[ii]);
		}

		lntd_error err = pthread_sigmask(SIG_UNBLOCK, &set, 0);
		LNTD_ASSERT(0 == err);
	}

	for (;;)
		pause();
}

static void listen_to_signal(size_t n)
{
	lntd_error err;
	int signo = signals[n];
	void (*handler)(int) = sighandlers[n];

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
	/* Block the other kill signals for less unpredictability */
	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(signals); ++ii) {
		sigaddset(&action.sa_mask, signals[ii]);
	}

	if (-1 == sigaction(signo, &action, 0)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		LNTD_ASSERT(false);
	}
}

static void report_sigchld(int signo)
{
	lntd_error err = errno;
	atomic_store_explicit(&sigchld_signalled, 1,
	                      memory_order_seq_cst);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sighup(int signo)
{
	lntd_error err = errno;
	atomic_store_explicit(&sighup_signalled, 1,
	                      memory_order_seq_cst);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sigint(int signo)
{
	lntd_error err = errno;
	atomic_store_explicit(&sigint_signalled, 1,
	                      memory_order_seq_cst);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sigquit(int signo)
{
	lntd_error err = errno;
	atomic_store_explicit(&sigquit_signalled, 1,
	                      memory_order_seq_cst);
	write_one(sigpipe_writer);
	errno = err;
}

static void report_sigterm(int signo)
{
	lntd_error err = errno;
	atomic_store_explicit(&sigterm_signalled, 1,
	                      memory_order_seq_cst);
	write_one(sigpipe_writer);
	errno = err;
}

static void write_one(lntd_ko ko)
{
	lntd_error err = 0;

	static char const dummy;

	for (;;) {
		ssize_t result = write(ko, &dummy, 1U);
		if (-1 == result) {
			err = errno;
			LNTD_ASSUME(err != 0);
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

	LNTD_ASSERT(err != EBADF);
	LNTD_ASSERT(err != EDESTADDRREQ);
	LNTD_ASSERT(err != EDQUOT);
	LNTD_ASSERT(err != EFAULT);
	LNTD_ASSERT(err != EFBIG);
	LNTD_ASSERT(err != EINVAL);
	LNTD_ASSERT(err != EIO);
	LNTD_ASSERT(err != ENOSPC);

	/* EPIPE should never happen because SIGPIPE is blocked */
	LNTD_ASSERT(err != EPIPE);

	LNTD_ASSERT(0 == err);
}
