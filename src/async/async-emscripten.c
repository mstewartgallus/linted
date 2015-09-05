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
#include "linted/async.h"

#include "linted/io.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/queue.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <poll.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <unistd.h>

static linted_error poll_one(linted_ko ko, short events,
                             short *reventsp);
static void run_task(struct linted_async_pool *pool,
                     struct linted_async_task *task);

struct completion_queue;
static linted_error
completion_queue_create(struct completion_queue **queuep);
static void complete_task(struct completion_queue *queue,
                          struct linted_async_task *task);
static linted_error
completion_try_recv(struct completion_queue *queue,
                    struct linted_async_task **taskp);
static void completion_queue_destroy(struct completion_queue *queue);

/**
 * A one writer to many readers queue.
 */
struct job_queue;
static linted_error job_queue_create(struct job_queue **queuep);
static void job_submit(struct job_queue *queue,
                       struct linted_async_task *task);
static linted_error job_try_recv(struct job_queue *queue,
                                 struct linted_async_task **taskp);
static void job_queue_destroy(struct job_queue *queue);

struct waiter_queue;
static linted_error waiter_queue_create(struct waiter_queue **queuep);
static void waiter_submit(struct waiter_queue *queue,
                          struct linted_async_waiter *waiter);
static linted_error
waiter_try_recv(struct waiter_queue *queue,
                struct linted_async_waiter **waiterp);
static void waiter_queue_destroy(struct waiter_queue *queue);

struct canceller {
	bool cancelled;
	bool in_flight : 1U;
};

static void canceller_init(struct canceller *canceller);
static void canceller_start(struct canceller *canceller);
static void canceller_stop(struct canceller *canceller);
static void canceller_cancel(struct canceller *canceller);
static bool canceller_check(struct canceller *canceller);

struct linted_async_pool {
	struct job_queue *worker_queue;

	struct waiter_queue *waiter_queue;
	struct completion_queue *completion_queue;
};

struct linted_async_task {
	struct linted_queue_node parent;
	struct canceller canceller;
	void *data;
	linted_error err;
	unsigned task_ck;
	linted_async_type type;
};

struct linted_async_waiter {
	struct linted_queue_node parent;
	struct linted_async_task *task;
	linted_ko ko;
	short flags;
	short revents;
};

linted_error linted_async_pool_create(struct linted_async_pool **poolp,
                                      unsigned max_tasks)
{
	linted_error err;

	struct linted_async_pool *pool;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *pool);
		if (err != 0)
			return err;
		pool = xx;
	}

	struct waiter_queue *waiter_queue;
	{
		struct waiter_queue *xx;
		err = waiter_queue_create(&xx);
		if (err != 0)
			goto free_pool;
		waiter_queue = xx;
	}

	struct job_queue *worker_queue;
	{
		struct job_queue *xx;
		err = job_queue_create(&xx);
		if (err != 0)
			goto destroy_waiter_queue;
		worker_queue = xx;
	}

	struct completion_queue *completion_queue;
	{
		struct completion_queue *xx;
		err = completion_queue_create(&xx);
		if (err != 0)
			goto destroy_worker_queue;
		completion_queue = xx;
	}

	pool->waiter_queue = waiter_queue;
	pool->worker_queue = worker_queue;
	pool->completion_queue = completion_queue;

	*poolp = pool;

destroy_completion_queue:
	completion_queue_destroy(pool->completion_queue);

destroy_worker_queue:
	job_queue_destroy(pool->worker_queue);

destroy_waiter_queue:
	waiter_queue_destroy(pool->waiter_queue);

free_pool:
	linted_mem_free(pool);

	return err;
}

linted_error linted_async_pool_destroy(struct linted_async_pool *pool)
{
	completion_queue_destroy(pool->completion_queue);
	job_queue_destroy(pool->worker_queue);
	waiter_queue_destroy(pool->waiter_queue);

	linted_mem_free(pool);

	return 0;
}

void linted_async_pool_submit(struct linted_async_pool *pool,
                              struct linted_async_task *task)
{
	assert(pool != 0);

	canceller_start(&task->canceller);

	job_submit(pool->worker_queue, task);
}

void linted_async_pool_resubmit(struct linted_async_pool *pool,
                                struct linted_async_task *task)
{
	assert(pool != 0);
	assert(task != 0);

	if (canceller_check(&task->canceller)) {
		task->err = LINTED_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	job_submit(pool->worker_queue, task);
}

void linted_async_pool_complete(struct linted_async_pool *pool,
                                struct linted_async_task *task,
                                linted_error task_err)
{
	assert(pool != 0);
	assert(task != 0);

	canceller_stop(&task->canceller);

	task->err = task_err;
	complete_task(pool->completion_queue, task);
}

void linted_async_pool_wait_on_poll(struct linted_async_pool *pool,
                                    struct linted_async_waiter *waiter,
                                    struct linted_async_task *task,
                                    linted_ko ko, short flags)
{
	assert(pool != 0);

	if (canceller_check(&task->canceller)) {
		task->err = LINTED_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	waiter->task = task;
	waiter->ko = ko;
	waiter->flags = flags;

	waiter_submit(pool->waiter_queue, waiter);
}

linted_error
linted_async_pool_wait(struct linted_async_pool *pool,
                       struct linted_async_task **completionp)
{
	linted_error err = 0;

	for (;;) {
		/* Poll for completions */
		err = completion_try_recv(pool->completion_queue,
		                          completionp);
		if (EAGAIN == err)
			goto poll_for_jobs;
		return err;

	poll_for_jobs : {
		struct linted_async_task *task;
		{
			struct linted_async_task *xx;
			err = job_try_recv(pool->worker_queue, &xx);
			if (EAGAIN == err)
				goto poll_for_pollers;
			task = xx;
		}

		if (canceller_check(&task->canceller)) {
			linted_async_pool_complete(
			    pool, task, LINTED_ERROR_CANCELLED);
			continue;
		}

		run_task(pool, task);

		/* Try polling for completions again */
		continue;
	}

	poll_for_pollers : {
		struct linted_async_waiter *waiter;
		{
			struct linted_async_waiter *xx;
			err = waiter_try_recv(pool->waiter_queue, &xx);
			assert(err != EAGAIN);
			waiter = xx;
		}

		struct linted_async_task *task = waiter->task;
		linted_ko ko = waiter->ko;
		unsigned short flags = waiter->flags;

		if (canceller_check(&task->canceller)) {
			err = LINTED_ERROR_CANCELLED;
			goto complete_task;
		}

		short revents;
		{
			short xx;
			err = poll_one(ko, flags, &xx);
			if (EINTR == err)
				goto wait_on_poll;
			if (err != 0)
				goto complete_task;
			revents = xx;
		}

		if ((revents & POLLNVAL) != 0) {
			err = LINTED_ERROR_INVALID_KO;
			goto complete_task;
		}

		if ((revents & POLLERR) != 0) {
			int xx;
			socklen_t yy = sizeof xx;
			if (-1 == getsockopt(ko, SOL_SOCKET, SO_ERROR,
			                     &xx, &yy)) {
				err = errno;
				goto complete_task;
			}
			err = xx;
			/* If another poller got the error then we
			 * could get zero instead so just resubmit in
			 * that case.
			 */
			if (err != 0)
				goto complete_task;
		}

		waiter->revents = revents;

		linted_async_pool_resubmit(pool, task);
		continue;

	complete_task:
		linted_async_pool_complete(pool, task, err);
		continue;

	wait_on_poll:
		linted_async_pool_wait_on_poll(pool, waiter, task, ko,
		                               flags);
		continue;
	}
	}
}

linted_error
linted_async_pool_poll(struct linted_async_pool *pool,
                       struct linted_async_task **completionp)
{
	linted_error err = 0;

	for (;;) {
		/* Poll for completions */
		err = completion_try_recv(pool->completion_queue,
		                          completionp);
		if (EAGAIN == err)
			goto poll_for_jobs;
		return err;

	poll_for_jobs : {
		struct linted_async_task *task;
		{
			struct linted_async_task *xx;
			err = job_try_recv(pool->worker_queue, &xx);
			if (EAGAIN == err)
				goto poll_for_pollers;
			task = xx;
		}

		if (canceller_check(&task->canceller)) {
			linted_async_pool_complete(
			    pool, task, LINTED_ERROR_CANCELLED);
			continue;
		}

		run_task(pool, task);

		/* Try polling for completions again */
		continue;
	}

	poll_for_pollers : {
		struct linted_async_waiter *waiter;
		{
			struct linted_async_waiter *xx;
			err = waiter_try_recv(pool->waiter_queue, &xx);
			if (err != 0)
				return err;
			waiter = xx;
		}

		struct linted_async_task *task = waiter->task;
		linted_ko ko = waiter->ko;
		unsigned short flags = waiter->flags;

		if (canceller_check(&task->canceller)) {
			err = LINTED_ERROR_CANCELLED;
			goto complete_task;
		}

		short revents;
		{
			short xx;
			err = poll_one(ko, flags, &xx);
			if (EINTR == err)
				goto wait_on_poll;
			if (err != 0)
				goto complete_task;
			revents = xx;
		}

		if ((revents & POLLNVAL) != 0) {
			err = LINTED_ERROR_INVALID_KO;
			goto complete_task;
		}

		if ((revents & POLLERR) != 0) {
			int xx;
			socklen_t yy = sizeof xx;
			if (-1 == getsockopt(ko, SOL_SOCKET, SO_ERROR,
			                     &xx, &yy)) {
				err = errno;
				goto complete_task;
			}
			err = xx;
			/* If another poller got the error then we
			 * could get zero instead so just resubmit in
			 * that case.
			 */
			if (err != 0)
				goto complete_task;
		}

		waiter->revents = revents;

		linted_async_pool_resubmit(pool, task);
		continue;

	complete_task:
		linted_async_pool_complete(pool, task, err);
		continue;

	wait_on_poll:
		linted_async_pool_wait_on_poll(pool, waiter, task, ko,
		                               flags);
		continue;
	}
	}
}

linted_error
linted_async_waiter_create(struct linted_async_waiter **waiterp)
{
	linted_error err;
	struct linted_async_waiter *waiter;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *waiter);
		if (err != 0)
			return err;
		waiter = xx;
	}
	linted_queue_node(&waiter->parent);

	waiter->revents = 0;

	*waiterp = waiter;
	return 0;
}

void linted_async_waiter_destroy(struct linted_async_waiter *waiter)
{
	linted_mem_free(waiter);
}

short linted_async_waiter_revents(struct linted_async_waiter *waiter)
{
	short ev = waiter->revents;
	waiter->revents = 0;
	return ev;
}

linted_error linted_async_task_create(struct linted_async_task **taskp,
                                      void *data,
                                      linted_async_type type)
{
	linted_error err;
	struct linted_async_task *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	linted_queue_node(&task->parent);

	canceller_init(&task->canceller);

	task->data = data;
	task->type = type;
	task->err = LINTED_ERROR_INVALID_PARAMETER;

	*taskp = task;
	return 0;
}

void linted_async_task_destroy(struct linted_async_task *task)
{
	linted_mem_free(task);
}

void linted_async_task_cancel(struct linted_async_task *task)
{
	canceller_cancel(&task->canceller);
}

void linted_async_task_prepare(struct linted_async_task *task,
                               unsigned task_ck)
{
	task->task_ck = task_ck;
}

unsigned linted_async_task_ck(struct linted_async_task *task)
{
	return task->task_ck;
}

linted_error linted_async_task_err(struct linted_async_task *task)
{
	return task->err;
}

void *linted_async_task_data(struct linted_async_task *task)
{
	return task->data;
}

#pragma weak linted_sched_do_idle
#pragma weak linted_io_do_poll
#pragma weak linted_io_do_read
#pragma weak linted_io_do_write
#pragma weak linted_pid_do_waitid
#pragma weak linted_signal_do_sigwaitinfo
#pragma weak linted_sched_do_sleep_until

static void run_task(struct linted_async_pool *pool,
                     struct linted_async_task *task)
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

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error poll_one(linted_ko ko, short events,
                             short *reventsp)
{
	linted_error err;

	short revents;
	{
		struct pollfd pollfd = {.fd = ko, .events = events};
		int poll_status = poll(&pollfd, 1U, -1);
		if (-1 == poll_status)
			goto poll_failed;

		revents = pollfd.revents;
		goto poll_succeeded;
	}

poll_failed:
	err = errno;
	LINTED_ASSUME(err != 0);
	return err;

poll_succeeded:
	*reventsp = revents;
	return 0;
}

/* struct complete_queue is just a fake */
static linted_error
completion_queue_create(struct completion_queue **queuep)
{
	struct linted_queue *xx;
	linted_error err = linted_queue_create(&xx);
	if (err != 0)
		return err;
	*queuep = (struct completion_queue *)xx;
	return 0;
}

static void complete_task(struct completion_queue *queue,
                          struct linted_async_task *task)
{
	assert(queue != 0);
	linted_queue_send((struct linted_queue *)queue,
	                  LINTED_UPCAST(task));
}

static linted_error
completion_try_recv(struct completion_queue *queue,
                    struct linted_async_task **taskp)
{
	linted_error err;
	struct linted_queue_node *node;

	err =
	    linted_queue_try_recv((struct linted_queue *)queue, &node);
	if (err != 0)
		return err;

	*taskp = LINTED_DOWNCAST(struct linted_async_task, node);

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
	linted_error err = linted_queue_create(&xx);
	if (err != 0)
		return err;
	*queuep = (struct job_queue *)xx;
	return 0;
}

static void job_submit(struct job_queue *queue,
                       struct linted_async_task *task)
{
	assert(queue != 0);
	assert(task != 0);
	linted_queue_send((struct linted_queue *)queue,
	                  LINTED_UPCAST(task));
}

static linted_error job_try_recv(struct job_queue *queue,
                                 struct linted_async_task **taskp)
{
	struct linted_queue_node *node;

	linted_error err =
	    linted_queue_try_recv((struct linted_queue *)queue, &node);
	if (err != 0)
		return err;

	*taskp = LINTED_DOWNCAST(struct linted_async_task, node);

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
	linted_error err = linted_queue_create(&xx);
	if (err != 0)
		return err;
	*queuep = (struct waiter_queue *)xx;
	return 0;
}

static void waiter_submit(struct waiter_queue *queue,
                          struct linted_async_waiter *waiter)
{
	linted_queue_send((struct linted_queue *)queue,
	                  LINTED_UPCAST(waiter));
}

static linted_error
waiter_try_recv(struct waiter_queue *queue,
                struct linted_async_waiter **waiterp)
{
	struct linted_queue_node *node;

	linted_error err =
	    linted_queue_try_recv((struct linted_queue *)queue, &node);
	if (err != 0)
		return err;

	*waiterp = LINTED_DOWNCAST(struct linted_async_waiter, node);

	return 0;
}

static void waiter_queue_destroy(struct waiter_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}

static void canceller_init(struct canceller *canceller)
{
	canceller->in_flight = false;
}

static void canceller_start(struct canceller *canceller)
{
	linted_error err;

	assert(!canceller->in_flight);

	canceller->in_flight = true;
	canceller->cancelled = false;
}

static void canceller_stop(struct canceller *canceller)
{
	linted_error err;

	assert(canceller->in_flight);

	canceller->in_flight = false;
}

static void canceller_cancel(struct canceller *canceller)
{
	bool in_flight = canceller->in_flight;
	if (in_flight)
		canceller->cancelled = true;
}

static bool canceller_check(struct canceller *canceller)
{
	return canceller->cancelled;
}
