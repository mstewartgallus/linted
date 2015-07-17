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

#include "linted/signal.h"

#include "linted/asynch.h"
#include "linted/io.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdbool.h>
#include <signal.h>
#include <unistd.h>

#include <stdio.h>

enum { LINTED_SIGNAL_HUP,
       LINTED_SIGNAL_INT,
       LINTED_SIGNAL_TERM,
       LINTED_SIGNAL_QUIT,
       NUM_SIGS };

struct linted_signal_task_wait
{
	struct linted_asynch_task *parent;
	void *data;
	int signo;
};

static void *sigaction_thread_routine(void *);

static pthread_t sigaction_thread;
static int signal_pipe_reader;
static int signal_pipe_writer;

linted_error linted_signal_init(void)
{
	linted_error err = 0;

	linted_ko reader;
	linted_ko writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			assert(err != EFAULT);
			assert(err != EINVAL);
			return err;
		}
		reader = xx[0U];
		writer = xx[1U];
	}
	signal_pipe_reader = reader;
	signal_pipe_writer = writer;

	{
		sigset_t signals;

		sigemptyset(&signals);
		sigaddset(&signals, SIGINT);
		sigaddset(&signals, SIGHUP);
		sigaddset(&signals, SIGTERM);
		sigaddset(&signals, SIGQUIT);

		err = pthread_sigmask(SIG_BLOCK, &signals, 0);
		assert(err != EFAULT);
		assert(err != EINVAL);
		assert(0 == err);
	}

	pthread_attr_t attr;

	err = pthread_attr_init(&attr);
	if (err != 0)
		return err;

	err =
	    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	if (err != 0)
		goto destroy_attr;

	err = pthread_create(&sigaction_thread, 0,
	                     sigaction_thread_routine, 0);
	assert(err != EINVAL);
	assert(err != EFAULT);

destroy_attr:
	pthread_attr_destroy(&attr);

	return err;
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
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		err = linted_asynch_task_create(
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

void
linted_signal_task_wait_destroy(struct linted_signal_task_wait *task)
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

void
linted_signal_task_wait_prepare(struct linted_signal_task_wait *task,
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
	linted_error err = 0;

	int signo = -1;
	{
		sigset_t signals;

		sigemptyset(&signals);
		sigaddset(&signals, SIGINT);
		sigaddset(&signals, SIGHUP);
		sigaddset(&signals, SIGTERM);
		sigaddset(&signals, SIGQUIT);

		siginfo_t xx;
		signo = sigwaitinfo(&signals, &xx);
	}
	if (signo < 0) {
		err = errno;
		LINTED_ASSUME(err != 0);
		assert(err != EFAULT);
		if (EINTR == err)
			goto resubmit;
		assert(0 == err);
		goto complete;
	}

complete:
	task_wait->signo = signo;

	linted_asynch_pool_complete(pool, task, err);
	return;

resubmit:
	linted_asynch_pool_resubmit(pool, task);
}

char const *linted_signal_string(int signo)
{
	if (signo < 1)
		return 0;

	if (signo >= NSIG)
		return 0;

	return sys_siglist[signo];
}

static void listen_to_signal(int signo);

void linted_signal_listen_to_sighup(void)
{
	listen_to_signal(SIGHUP);
}

void linted_signal_listen_to_sigint(void)
{
	listen_to_signal(SIGINT);
}

void linted_signal_listen_to_sigquit(void)
{
	listen_to_signal(SIGQUIT);
}

void linted_signal_listen_to_sigterm(void)
{
	listen_to_signal(SIGTERM);
}

static void *sigaction_thread_routine(void *arg)
{
	linted_error err;

	{
		sigset_t signal_set;
		sigemptyset(&signal_set);
		sigaddset(&signal_set, SIGHUP);
		sigaddset(&signal_set, SIGINT);
		sigaddset(&signal_set, SIGTERM);
		sigaddset(&signal_set, SIGQUIT);

		err = pthread_sigmask(SIG_UNBLOCK, &signal_set, 0);
		assert(err != EFAULT);
		assert(err != EINVAL);
		assert(0 == err);
	}

	for (;;) {
		int signo;
		{
			int xx;
			err = linted_io_read_all(signal_pipe_reader, 0,
			                         &xx, sizeof xx);
			assert(err != EBADF);
			assert(err != EFAULT);
			assert(err != EINVAL);
			assert(err != EIO);
			assert(err != EISDIR);
			assert(0 == err);

			signo = xx;
		}

		{
			sigset_t signal_set;
			sigemptyset(&signal_set);
			sigaddset(&signal_set, signo);

			err =
			    pthread_sigmask(SIG_BLOCK, &signal_set, 0);
			assert(err != EFAULT);
			assert(err != EINVAL);
			assert(0 == err);
		}
	}

	return 0;
}

static void listen_to_signal(int signo)
{
	linted_error err;

	{
		int xx = signo;
		err = linted_io_write_all(signal_pipe_writer, 0, &xx,
		                          sizeof xx);
	}
	assert(err != EBADF);
	assert(err != EDESTADDRREQ);
	assert(err != EDQUOT);
	assert(err != EFAULT);
	assert(err != EFBIG);
	assert(err != EINTR);
	assert(err != EINVAL);
	assert(err != EIO);
	assert(err != ENOSPC);
	assert(err != EPIPE);
	assert(0 == err);
}
