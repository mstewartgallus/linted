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

#include "linted/asynch.h"

#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/queue.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/util.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#include <windows.h>
#include <winsock2.h>

/**
 * @file
 *
 * Asynchronous IO implementation.
 */

/**
 * A one reader to many writers queue. Should be able to retrieve
 * many values at once. As all writes are a direct result of
 * submitted commands there is no need to worry about it growing
 * too large.
 */
struct completion_queue;
static linted_error completion_queue_create(struct completion_queue **queuep);
static void complete_task(struct completion_queue *queue,
                          struct linted_asynch_task *task);
static linted_error completion_recv(struct completion_queue *queue,
                                    struct linted_asynch_task **taskp);
static linted_error completion_try_recv(struct completion_queue *queue,
                                        struct linted_asynch_task **taskp);
static void completion_queue_destroy(struct completion_queue *queue);

/**
 * A one writer to many readers queue.
 */
struct job_queue;
static linted_error job_queue_create(struct job_queue **queuep);
static void job_submit(struct job_queue *queue,
                       struct linted_asynch_task *task);
static linted_error job_recv(struct job_queue *queue,
                             struct linted_asynch_task **taskp);
static void job_queue_destroy(struct job_queue *queue);

struct waiter_queue;
static linted_error waiter_queue_create(struct waiter_queue **queuep);
static void waiter_submit(struct waiter_queue *queue,
                          struct linted_asynch_waiter *waiter);
static linted_error waiter_recv(struct waiter_queue *queue,
                                struct linted_asynch_waiter **waiterp);
static void waiter_queue_destroy(struct waiter_queue *queue);

struct worker_pool;
static linted_error worker_pool_create(struct worker_pool **poolp,
                                       struct job_queue *worker_queue,
                                       struct linted_asynch_pool *pool,
                                       unsigned max_tasks);
static void worker_pool_stop(struct worker_pool *pool);
static void worker_pool_destroy(struct worker_pool *pool);

struct wait_manager;
static linted_error wait_manager_create(struct wait_manager **managerp,
                                        struct waiter_queue *waiter_queue,
                                        struct linted_asynch_pool *pool,
                                        unsigned max_pollers);
static void wait_manager_stop(struct wait_manager *manager);
static void wait_manager_destroy(struct wait_manager *manager);

struct linted_asynch_pool
{
	struct wait_manager *wait_manager;
	struct worker_pool *worker_pool;
	struct job_queue *worker_queue;

	struct waiter_queue *waiter_queue;
	struct completion_queue *completion_queue;
};

struct linted_asynch_task
{
	struct linted_queue_node parent;

	CRITICAL_SECTION owner_lock;
	linted_error errnum;

	linted_ko owner;

	bool *cancel_replier;
	void *data;

	linted_asynch_type type;
	unsigned task_action;

	bool owned : 1U;
	bool in_flight : 1U;
};

struct linted_asynch_waiter
{
	struct linted_queue_node parent;
	struct linted_asynch_task *task;
	linted_ko ko;
	short flags;
	short revents;
};

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks)
{
	linted_error errnum;

	struct linted_asynch_pool *pool;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *pool);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	struct waiter_queue *waiter_queue;
	{
		struct waiter_queue *xx;
		errnum = waiter_queue_create(&xx);
		if (errnum != 0)
			goto free_pool;
		waiter_queue = xx;
	}

	struct job_queue *worker_queue;
	{
		struct job_queue *xx;
		errnum = job_queue_create(&xx);
		if (errnum != 0)
			goto destroy_waiter_queue;
		worker_queue = xx;
	}

	struct completion_queue *completion_queue;
	{
		struct completion_queue *xx;
		errnum = completion_queue_create(&xx);
		if (errnum != 0)
			goto destroy_worker_queue;
		completion_queue = xx;
	}

	struct wait_manager *wait_manager;
	{
		struct wait_manager *xx;
		errnum =
		    wait_manager_create(&xx, waiter_queue, pool, max_tasks);
		if (errnum != 0)
			goto destroy_completion_queue;
		wait_manager = xx;
	}

	struct worker_pool *worker_pool;
	{
		struct worker_pool *xx;
		errnum = worker_pool_create(&xx, worker_queue, pool, max_tasks);
		if (errnum != 0)
			goto destroy_wait_manager;
		worker_pool = xx;
	}

	pool->worker_pool = worker_pool;
	pool->wait_manager = wait_manager;
	pool->waiter_queue = waiter_queue;
	pool->worker_queue = worker_queue;
	pool->completion_queue = completion_queue;

	*poolp = pool;

	return 0;

destroy_wait_manager:
	wait_manager_destroy(wait_manager);

destroy_completion_queue:
	completion_queue_destroy(pool->completion_queue);

destroy_worker_queue:
	job_queue_destroy(pool->worker_queue);

destroy_waiter_queue:
	waiter_queue_destroy(pool->waiter_queue);

free_pool:
	linted_mem_free(pool);

	return errnum;
}

linted_error linted_asynch_pool_stop(struct linted_asynch_pool *pool)
{
	worker_pool_stop(pool->worker_pool);
	wait_manager_stop(pool->wait_manager);

	return 0;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{

	worker_pool_destroy(pool->worker_pool);
	wait_manager_destroy(pool->wait_manager);

	completion_queue_destroy(pool->completion_queue);
	job_queue_destroy(pool->worker_queue);
	waiter_queue_destroy(pool->waiter_queue);

	linted_mem_free(pool);

	return 0;
}

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task)
{
	assert(pool != 0);

	EnterCriticalSection(&task->owner_lock);

	assert(!task->in_flight);
	assert(!task->owned);

	task->in_flight = true;
	task->owned = false;

	LeaveCriticalSection(&task->owner_lock);

	job_submit(pool->worker_queue, task);
}

void linted_asynch_pool_resubmit(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	bool cancelled;

	assert(pool != 0);

	EnterCriticalSection(&task->owner_lock);

	assert(task->in_flight);
	assert(task->owned);

	task->owned = false;
	{
		bool *cancel_replier = task->cancel_replier;
		cancelled = cancel_replier != 0;
		if (cancelled)
			*cancel_replier = true;
		task->cancel_replier = 0;
	}

	LeaveCriticalSection(&task->owner_lock);

	if (cancelled) {
		task->errnum = LINTED_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
	} else {
		job_submit(pool->worker_queue, task);
	}
}

void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task,
                                 linted_error task_errnum)
{
	/* Aren't a cancellation point so always works */

	EnterCriticalSection(&task->owner_lock);

	assert(task->owned);
	assert(task->in_flight);

	{
		bool *cancel_replier = task->cancel_replier;
		bool cancelled = cancel_replier != 0;
		if (cancelled)
			*cancel_replier = true;
		task->cancel_replier = 0;
	}

	task->in_flight = false;
	task->owned = false;

	task->errnum = task_errnum;

	LeaveCriticalSection(&task->owner_lock);

	complete_task(pool->completion_queue, task);
}

void linted_asynch_pool_wait_on_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_waiter *waiter,
                                     struct linted_asynch_task *task,
                                     linted_ko ko, short flags)
{
	assert(pool != 0);

	bool cancelled;

	EnterCriticalSection(&task->owner_lock);

	task->in_flight = true;
	task->owned = false;
	{
		bool *cancel_replier = task->cancel_replier;
		cancelled = cancel_replier != 0;
		if (cancelled)
			*cancel_replier = true;
		task->cancel_replier = 0;
	}

	LeaveCriticalSection(&task->owner_lock);

	if (cancelled) {
		task->errnum = LINTED_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	waiter->task = task;
	waiter->ko = ko;
	waiter->flags = flags;

	waiter_submit(pool->waiter_queue, waiter);
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
	return completion_recv(pool->completion_queue, completionp);
}

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
	return completion_try_recv(pool->completion_queue, completionp);
}

linted_error linted_asynch_waiter_create(struct linted_asynch_waiter **waiterp)
{
	linted_error errnum;
	struct linted_asynch_waiter *waiter;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *waiter);
		if (errnum != 0)
			return errnum;
		waiter = xx;
	}
	linted_queue_node(&waiter->parent);

	waiter->revents = 0;

	*waiterp = waiter;
	return 0;
}

void linted_asynch_waiter_destroy(struct linted_asynch_waiter *waiter)
{
	linted_mem_free(waiter);
}

short linted_asynch_waiter_revents(struct linted_asynch_waiter *waiter)
{
	short ev = waiter->revents;
	waiter->revents = 0;
	return ev;
}

linted_error linted_asynch_task_create(struct linted_asynch_task **taskp,
                                       void *data, linted_asynch_type type)
{
	linted_error errnum;
	struct linted_asynch_task *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	linted_queue_node(&task->parent);

	InitializeCriticalSection(&task->owner_lock);

	task->in_flight = false;
	task->owned = false;
	task->cancel_replier = 0;

	task->data = data;
	task->type = type;
	task->errnum = LINTED_ERROR_INVALID_PARAMETER;

	*taskp = task;
	return 0;
}

void linted_asynch_task_destroy(struct linted_asynch_task *task)
{
	DeleteCriticalSection(&task->owner_lock);
	linted_mem_free(task);
}

void linted_asynch_task_cancel(struct linted_asynch_task *task)
{
	bool cancel_reply = false;
	bool in_flight;

	/* This can't be a POSIX real-time signal as those queue up so
	 * we can end up queuing a barrage of signals that trap the
	 * thread were waiting in signal handling.
	 */

	{
		EnterCriticalSection(&task->owner_lock);

		assert(0 == task->cancel_replier);

		in_flight = task->in_flight;
		if (in_flight) {
			task->cancel_replier = &cancel_reply;

			bool owned = task->owned;
			if (owned)
				CancelSynchronousIo(task->owner);
		}

		LeaveCriticalSection(&task->owner_lock);
	}

	if (!in_flight)
		return;

	/* Yes, really, we do have to busy wait to prevent race
	 * conditions unfortunately */
	bool cancel_replied;
	do {
		SwitchToThread();

		EnterCriticalSection(&task->owner_lock);

		cancel_replied = cancel_reply;
		if (!cancel_replied) {
			bool owned = task->owned;
			if (owned)
				CancelSynchronousIo(task->owner);
		}

		LeaveCriticalSection(&task->owner_lock);
	} while (!cancel_replied);
}

void linted_asynch_task_prepare(struct linted_asynch_task *task,
                                unsigned task_action)
{
	task->task_action = task_action;
}

unsigned linted_asynch_task_action(struct linted_asynch_task *task)
{
	return task->task_action;
}

linted_error linted_asynch_task_errnum(struct linted_asynch_task *task)
{
	return task->errnum;
}

void linted_asynch_task_seterrnum(struct linted_asynch_task *task,
                                  linted_error errnum)
{
	task->errnum = errnum;
}

void *linted_asynch_task_data(struct linted_asynch_task *task)
{
	return task->data;
}
static DWORD WINAPI worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task);

struct worker_pool
{
	struct linted_asynch_pool *asynch_pool;
	struct job_queue *worker_queue;

	size_t worker_count;

	bool stopped : 1U;

	linted_ko workers[];
};

static linted_error worker_pool_create(struct worker_pool **poolp,
                                       struct job_queue *worker_queue,
                                       struct linted_asynch_pool *asynch_pool,
                                       unsigned max_tasks)
{
	linted_error errnum = 0;

	struct worker_pool *pool;
	size_t workers_count = max_tasks;
	size_t workers_size = workers_count * sizeof pool->workers[0U];

	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *pool + workers_size);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	pool->stopped = false;
	pool->worker_count = workers_count;
	pool->worker_queue = worker_queue;
	pool->asynch_pool = asynch_pool;

	size_t created_threads = 0U;

	for (; created_threads < max_tasks; ++created_threads) {
		linted_ko thread =
		    CreateThread(0, 0, worker_routine, pool, 0, 0);
		if (INVALID_HANDLE_VALUE == thread) {
			errnum = GetLastError();
			goto destroy_threads;
		}
	}

	if (errnum != 0)
		goto destroy_threads;

	*poolp = pool;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < created_threads; ++ii)
		TerminateThread(pool->workers[ii], 0);

	for (size_t ii = 0U; ii < created_threads; ++ii)
		CloseHandle(pool->workers[ii]);

	linted_mem_free(pool);

	return errnum;
}

static void worker_pool_stop(struct worker_pool *pool)
{
	assert(!pool->stopped);

	pool->stopped = true;

	size_t worker_count = pool->worker_count;

	for (size_t ii = 0U; ii < worker_count; ++ii)
		TerminateThread(pool->workers[ii], 0);

	/* After this point a few workers can still be left hanging
	 * waiting to return completed tasks and users should clean
	 * those up by polling after here. */
}

static void worker_pool_destroy(struct worker_pool *pool)
{
	assert(pool->stopped);

	/* All workers should have had their results retrieved by
	 * now. */
	size_t worker_count = pool->worker_count;

	for (size_t ii = 0U; ii < worker_count; ++ii)
		CloseHandle(pool->workers[ii]);
}

static DWORD WINAPI worker_routine(void *arg)
{
	struct worker_pool *pool = arg;
	struct linted_asynch_pool *asynch_pool = pool->asynch_pool;

	linted_ko self;
	{
		HANDLE xx;
		DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
		                GetCurrentProcess(), &xx, 0, FALSE,
		                DUPLICATE_SAME_ACCESS);
		self = xx;
	}

	for (;;) {
		bool cancelled;

		struct linted_asynch_task *task;
		{
			struct linted_asynch_task *xx;
			job_recv(pool->worker_queue, &xx);
			task = xx;
		}

		EnterCriticalSection(&task->owner_lock);

		{
			cancelled = task->cancel_replier != 0;

			/* Don't actually complete the cancellation if
			 * cancelled and let the completion do that.
			 */
			task->owner = self;
			task->owned = true;
		}

		LeaveCriticalSection(&task->owner_lock);

		if (cancelled) {
			linted_asynch_pool_complete(asynch_pool, task,
			                            LINTED_ERROR_CANCELLED);
		} else {
			run_task(asynch_pool, task);
		}
	}

	LINTED_ASSUME_UNREACHABLE();
}

#pragma weak linted_sched_do_idle
#pragma weak linted_io_do_poll
#pragma weak linted_io_do_read
#pragma weak linted_io_do_write
#pragma weak linted_pid_do_waitid
#pragma weak linted_signal_do_sigwaitinfo
#pragma weak linted_sched_do_sleep_until
#pragma weak linted_io_do_accept
#pragma weak linted_io_do_recv
#pragma weak linted_io_do_sendto

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task)
{
	switch (task->type) {
	case LINTED_ASYNCH_TASK_IDLE:
		linted_sched_do_idle(pool, task);
		break;

	case LINTED_ASYNCH_TASK_POLL:
		linted_io_do_poll(pool, task);
		break;

	case LINTED_ASYNCH_TASK_READ:
		linted_io_do_read(pool, task);
		break;

	case LINTED_ASYNCH_TASK_WRITE:
		linted_io_do_write(pool, task);
		break;

	case LINTED_ASYNCH_TASK_WAITID:
		linted_pid_do_waitid(pool, task);
		break;

	case LINTED_ASYNCH_TASK_SIGWAITINFO:
		linted_signal_do_sigwaitinfo(pool, task);
		break;

	case LINTED_ASYNCH_TASK_SLEEP_UNTIL:
		linted_sched_do_sleep_until(pool, task);
		break;

	case LINTED_ASYNCH_TASK_ACCEPT:
		linted_io_do_accept(pool, task);
		break;

	case LINTED_ASYNCH_TASK_RECV:
		linted_io_do_recv(pool, task);
		break;

	case LINTED_ASYNCH_TASK_SENDTO:
		linted_io_do_sendto(pool, task);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

struct wait_manager
{
	struct linted_asynch_pool *asynch_pool;
	struct waiter_queue *waiter_queue;

	size_t poller_count;

	bool stopped : 1U;

	linted_ko pollers[];
};

static DWORD WINAPI poller_routine(void *arg);

static linted_error poll_one(linted_ko ko, short events, short *revents);

static linted_error wait_manager_create(struct wait_manager **managerp,
                                        struct waiter_queue *waiter_queue,
                                        struct linted_asynch_pool *asynch_pool,
                                        unsigned max_pollers)
{
	linted_error errnum;
	size_t created_threads = 0U;
	struct wait_manager *manager;

	size_t pollers_count = max_pollers;
	size_t pollers_size = pollers_count * sizeof manager->pollers[0U];

	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *manager + pollers_size);
		if (errnum != 0)
			return errnum;
		manager = xx;
	}

	manager->stopped = false;
	manager->poller_count = pollers_count;
	manager->waiter_queue = waiter_queue;
	manager->asynch_pool = asynch_pool;

	for (; created_threads < max_pollers; ++created_threads) {
		linted_ko thread =
		    CreateThread(0, 0, poller_routine, manager, 0, 0);
		if (INVALID_HANDLE_VALUE == thread) {
			errnum = GetLastError();
			goto destroy_threads;
		}
	}

	if (errnum != 0)
		goto destroy_threads;

	*managerp = manager;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < created_threads; ++ii)
		TerminateThread(manager->pollers[ii], 0);

	for (size_t ii = 0U; ii < created_threads; ++ii)
		CloseHandle(manager->pollers[ii]);

	linted_mem_free(manager);

	return errnum;
}

static void wait_manager_stop(struct wait_manager *manager)
{
	assert(!manager->stopped);

	manager->stopped = true;

	size_t poller_count = manager->poller_count;

	for (size_t ii = 0U; ii < poller_count; ++ii)
		TerminateThread(manager->pollers[ii], 0);

	/* After this point a few workers can still be left hanging
	 * waiting to return completed tasks and users should clean
	 * those up by polling after here. */
}

static void wait_manager_destroy(struct wait_manager *manager)
{
	assert(manager->stopped);

	/* All workers should have had their results retrieved by
	 * now. */
	size_t poller_count = manager->poller_count;

	for (size_t ii = 0U; ii < poller_count; ++ii)
		CloseHandle(manager->pollers[ii]);

	linted_mem_free(manager);
}

static DWORD WINAPI poller_routine(void *arg)
{
	struct wait_manager *pool = arg;
	struct linted_asynch_pool *asynch_pool = pool->asynch_pool;
	linted_error errnum;

	linted_ko self;
	{
		HANDLE xx;
		DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
		                GetCurrentProcess(), &xx, 0, FALSE,
		                DUPLICATE_SAME_ACCESS);
		self = xx;
	}

	for (;;) {
		bool cancelled;

		struct linted_asynch_waiter *waiter;
		{
			struct linted_asynch_waiter *xx;
			waiter_recv(pool->waiter_queue, &xx);
			waiter = xx;
		}

		struct linted_asynch_task *task = waiter->task;
		linted_ko ko = waiter->ko;
		unsigned short flags = waiter->flags;

		EnterCriticalSection(&task->owner_lock);

		{
			cancelled = task->cancel_replier != 0;

			/* Don't actually complete the cancellation if
			 * cancelled and let the completion do that.
			 */
			task->owner = self;
			task->owned = true;
		}

		LeaveCriticalSection(&task->owner_lock);

		if (cancelled) {
			errnum = LINTED_ERROR_CANCELLED;
			goto complete_task;
		}

		short revents;
		{
			short xx;
			errnum = poll_one(ko, flags, &xx);
			if (WSAEINTR == errnum)
				goto wait_on_poll;
			if (errnum != 0)
				goto complete_task;
			revents = xx;
		}

		if ((revents & POLLNVAL) != 0) {
			errnum = LINTED_ERROR_INVALID_KO;
			goto complete_task;
		}

		if ((revents & POLLERR) != 0) {
			int xx;
			int yy = sizeof xx;
			if (-1 == getsockopt((SOCKET)ko, SOL_SOCKET, SO_ERROR,
			                     (char *)&xx, &yy)) {
				errnum = errno;
				goto complete_task;
			}
			errnum = xx;
			/* If another poller got the error then we
			 * could get zero instead so just resubmit in
			 * that case.
			 */
			if (errnum != 0)
				goto complete_task;
		}

		waiter->revents = revents;

		linted_asynch_pool_resubmit(asynch_pool, task);
		continue;

	complete_task:
		linted_asynch_pool_complete(asynch_pool, task, errnum);
		continue;

	wait_on_poll:
		linted_asynch_pool_wait_on_poll(asynch_pool, waiter, task, ko,
		                                flags);
	}

	LINTED_ASSUME_UNREACHABLE();
}

static linted_error poll_one(linted_ko ko, short events, short *reventsp)
{
	linted_error errnum;

	short revents;
	{
		struct pollfd pollfd = {.fd = (SOCKET)ko, .events = events};
		int poll_status = WSAPoll(&pollfd, 1U, -1);
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

/* struct complete_queue is just a fake */
static linted_error completion_queue_create(struct completion_queue **queuep)
{
	struct linted_queue *xx;
	linted_error errnum = linted_queue_create(&xx);
	if (errnum != 0)
		return errnum;
	*queuep = (struct completion_queue *)xx;
	return 0;
}

static void complete_task(struct completion_queue *queue,
                          struct linted_asynch_task *task)
{
	linted_queue_send((struct linted_queue *)queue, LINTED_UPCAST(task));
}

static linted_error completion_recv(struct completion_queue *queue,
                                    struct linted_asynch_task **taskp)
{
	struct linted_queue_node *node;

	linted_queue_recv((struct linted_queue *)queue, &node);

	*taskp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0;
}

static linted_error completion_try_recv(struct completion_queue *queue,
                                        struct linted_asynch_task **taskp)
{
	linted_error errnum;
	struct linted_queue_node *node;

	errnum = linted_queue_try_recv((struct linted_queue *)queue, &node);
	if (errnum != 0)
		return errnum;

	*taskp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0;
}

static void completion_queue_destroy(struct completion_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}

/* struct job_queue is just a fake */
static linted_error job_queue_create(struct job_queue **queuep)
{
	struct linted_queue *xx;
	linted_error errnum = linted_queue_create(&xx);
	if (errnum != 0)
		return errnum;
	*queuep = (struct job_queue *)xx;
	return 0;
}

static void job_submit(struct job_queue *queue, struct linted_asynch_task *task)
{
	linted_queue_send((struct linted_queue *)queue, LINTED_UPCAST(task));
}

static linted_error job_recv(struct job_queue *queue,
                             struct linted_asynch_task **taskp)
{
	struct linted_queue_node *node;

	linted_queue_recv((struct linted_queue *)queue, &node);

	*taskp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0;
}

static void job_queue_destroy(struct job_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}

/* struct waiter_queue is just a fake */
static linted_error waiter_queue_create(struct waiter_queue **queuep)
{
	struct linted_queue *xx;
	linted_error errnum = linted_queue_create(&xx);
	if (errnum != 0)
		return errnum;
	*queuep = (struct waiter_queue *)xx;
	return 0;
}

static void waiter_submit(struct waiter_queue *queue,
                          struct linted_asynch_waiter *waiter)
{
	linted_queue_send((struct linted_queue *)queue, LINTED_UPCAST(waiter));
}

static linted_error waiter_recv(struct waiter_queue *queue,
                                struct linted_asynch_waiter **waiterp)
{
	struct linted_queue_node *node;

	linted_queue_recv((struct linted_queue *)queue, &node);

	*waiterp = LINTED_DOWNCAST(struct linted_asynch_waiter, node);

	return 0;
}

static void waiter_queue_destroy(struct waiter_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}
