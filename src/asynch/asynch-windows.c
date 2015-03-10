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
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/channel.h"
#include "linted/queue.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/util.h"

#include <assert.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>

#include <windows.h>
#include <winsock2.h>

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

struct job_queue;
static linted_error job_queue_create(struct job_queue **queuep);
static void job_submit(struct job_queue *queue,
                       struct linted_asynch_task *task);
static linted_error job_recv(struct job_queue *queue,
                             struct linted_asynch_task **taskp);
static void job_queue_destroy(struct job_queue *queue);

struct worker_queue;
static linted_error worker_queue_create(struct worker_queue **queuep);
static linted_error worker_try_submit(struct worker_queue *queue,
                                      struct linted_asynch_task *task);
static linted_error worker_recv(struct worker_queue *queue,
                                struct linted_asynch_task **taskp);
static void worker_queue_destroy(struct worker_queue *queue);

struct waiter_queue;
static linted_error waiter_queue_create(struct waiter_queue **queuep);
static void waiter_submit(struct waiter_queue *queue,
                          struct linted_asynch_waiter *waiter);
static linted_error waiter_recv(struct waiter_queue *queue,
                                struct linted_asynch_waiter **waiterp);
static void waiter_queue_destroy(struct waiter_queue *queue);

struct worker_pool;
static linted_error worker_pool_create(struct worker_pool **poolp,
                                       struct job_queue *job_queue,
                                       struct linted_asynch_pool *pool,
                                       unsigned max_tasks);
static void worker_pool_destroy(struct worker_pool *pool);

struct wait_manager;
static linted_error wait_manager_create(struct wait_manager **managerp,
                                        struct waiter_queue *waiter_queue,
                                        struct linted_asynch_pool *pool,
                                        unsigned max_pollers);
static void wait_manager_destroy(struct wait_manager *manager);

struct canceller
{
	linted_ko owner;
	CRITICAL_SECTION lock;
	bool *cancel_replier;
	bool owned : 1U;
	bool in_flight : 1U;
};

static void canceller_init(struct canceller *canceller);
static void canceller_start(struct canceller *canceller);
static void canceller_stop(struct canceller *canceller);
static void canceller_cancel(struct canceller *canceller);
static bool canceller_check_or_register(struct canceller *canceller,
                                        linted_ko self);
static bool canceller_check_and_unregister(struct canceller *canceller);

struct linted_asynch_pool
{
	struct wait_manager *wait_manager;
	struct worker_pool *worker_pool;

	struct job_queue *job_queue;
	struct waiter_queue *waiter_queue;
	struct completion_queue *completion_queue;
};

struct linted_asynch_task
{
	struct linted_queue_node parent;
	struct canceller canceller;
	void *data;
	linted_error errnum;
	unsigned task_action;
	linted_asynch_type type;
	bool thread_canceller : 1U;
};

struct linted_asynch_waiter
{
	struct linted_queue_node parent;
	struct linted_asynch_task *task;
	linted_ko ko;
	short flags;
	short revents;
	bool thread_canceller : 1U;
};

static void set_thread_name(char const *name);

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

	struct job_queue *job_queue;
	{
		struct job_queue *xx;
		errnum = job_queue_create(&xx);
		if (errnum != 0)
			goto destroy_waiter_queue;
		job_queue = xx;
	}

	struct completion_queue *completion_queue;
	{
		struct completion_queue *xx;
		errnum = completion_queue_create(&xx);
		if (errnum != 0)
			goto destroy_job_queue;
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
		errnum = worker_pool_create(&xx, job_queue, pool, max_tasks);
		if (errnum != 0)
			goto destroy_wait_manager;
		worker_pool = xx;
	}

	pool->worker_pool = worker_pool;
	pool->wait_manager = wait_manager;
	pool->waiter_queue = waiter_queue;
	pool->job_queue = job_queue;
	pool->completion_queue = completion_queue;

	*poolp = pool;

	return 0;

destroy_wait_manager:
	wait_manager_destroy(wait_manager);

destroy_completion_queue:
	completion_queue_destroy(completion_queue);

destroy_job_queue:
	job_queue_destroy(job_queue);

destroy_waiter_queue:
	waiter_queue_destroy(waiter_queue);

free_pool:
	linted_mem_free(pool);

	return errnum;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
	struct wait_manager *wait_manager = pool->wait_manager;
	struct worker_pool *worker_pool = pool->worker_pool;

	struct job_queue *job_queue = pool->job_queue;
	struct waiter_queue *waiter_queue = pool->waiter_queue;
	struct completion_queue *completion_queue = pool->completion_queue;

	worker_pool_destroy(worker_pool);
	wait_manager_destroy(wait_manager);

	job_queue_destroy(job_queue);
	waiter_queue_destroy(waiter_queue);
	completion_queue_destroy(completion_queue);

	linted_mem_free(pool);

	return 0;
}

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task)
{
	assert(pool != 0);

	canceller_start(&task->canceller);

	job_submit(pool->job_queue, task);
}

void linted_asynch_pool_resubmit(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	assert(pool != 0);

	if (canceller_check_and_unregister(&task->canceller)) {
		task->errnum = LINTED_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	job_submit(pool->job_queue, task);
}

void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task,
                                 linted_error task_errnum)
{
	canceller_stop(&task->canceller);

	task->errnum = task_errnum;
	complete_task(pool->completion_queue, task);
}

void linted_asynch_pool_wait_on_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_waiter *waiter,
                                     struct linted_asynch_task *task,
                                     linted_ko ko, short flags)
{
	assert(pool != 0);

	if (canceller_check_and_unregister(&task->canceller)) {
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
	waiter->thread_canceller = false;

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

	canceller_init(&task->canceller);

	task->data = data;
	task->type = type;
	task->errnum = LINTED_ERROR_INVALID_PARAMETER;

	task->thread_canceller = false;

	*taskp = task;
	return 0;
}

void linted_asynch_task_destroy(struct linted_asynch_task *task)
{
	linted_mem_free(task);
}

void linted_asynch_task_cancel(struct linted_asynch_task *task)
{
	canceller_cancel(&task->canceller);
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

void *linted_asynch_task_data(struct linted_asynch_task *task)
{
	return task->data;
}

static DWORD WINAPI master_worker_routine(void *arg);
static DWORD WINAPI worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task);

struct worker_pool;

struct worker
{
	struct linted_asynch_pool *pool;
	struct worker_queue *queue;
	linted_ko thread;
};

struct worker_pool
{
	struct linted_asynch_pool *asynch_pool;
	struct job_queue *job_queue;
	struct worker_queue **worker_queues;

	size_t worker_count;

	linted_ko master_thread;

	struct worker workers[];
};

static linted_error worker_pool_create(struct worker_pool **poolp,
                                       struct job_queue *job_queue,
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

	size_t worker_queues_created = 0U;
	for (; worker_queues_created < max_tasks; ++worker_queues_created) {
		errnum = worker_queue_create(
		    &pool->workers[worker_queues_created].queue);
		if (errnum != 0)
			goto destroy_worker_queues;
	}

	pool->worker_count = workers_count;
	pool->job_queue = job_queue;
	pool->asynch_pool = asynch_pool;

	size_t created_threads = 0U;
	for (; created_threads < max_tasks; ++created_threads) {
		struct worker *worker = &pool->workers[created_threads];

		worker->pool = asynch_pool;
		linted_ko thread =
		    CreateThread(0, 0, worker_routine, worker, 0, 0);
		if (INVALID_HANDLE_VALUE == thread) {
			errnum = GetLastError();
			break;
		}
		worker->thread = thread;
	}

	if (errnum != 0)
		goto destroy_threads;

	{
		linted_ko thread =
		    CreateThread(0, 0, master_worker_routine, pool, 0, 0);
		if (INVALID_HANDLE_VALUE == thread) {
			errnum = GetLastError();
			goto destroy_threads;
		}
		pool->master_thread = thread;
	}

	*poolp = pool;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < created_threads; ++ii) {
		struct worker const *worker = &pool->workers[ii];
		struct worker_queue *worker_queue = worker->queue;
		linted_ko thread = worker->thread;

		for (;;) {
			struct linted_asynch_task task;
			linted_queue_node(&task.parent);
			task.thread_canceller = true;

			linted_error try_errnum =
			    worker_try_submit(worker_queue, &task);
			if (0 == try_errnum) {

				switch (WaitForSingleObject(thread, INFINITE)) {
				case WAIT_OBJECT_0:
					break;

				case WAIT_FAILED:
				default:
					assert(false);
				}
				break;
			}

			CancelSynchronousIo(thread);

			SwitchToThread();
		}
	}

destroy_worker_queues:
	for (size_t ii = 0U; ii < worker_queues_created; ++ii)
		worker_queue_destroy(pool->workers[ii].queue);

	linted_mem_free(pool);

	return errnum;
}

static void worker_pool_destroy(struct worker_pool *pool)
{
	struct job_queue *job_queue = pool->job_queue;
	linted_ko master_thread = pool->master_thread;
	struct worker const *workers = pool->workers;
	size_t worker_count = pool->worker_count;

	{
		struct linted_asynch_task task;
		linted_queue_node(&task.parent);
		task.thread_canceller = true;

		job_submit(job_queue, &task);

		switch (WaitForSingleObject(master_thread, INFINITE)) {
		case WAIT_OBJECT_0:
			break;

		case WAIT_FAILED:
		default:
			assert(false);
		}
	}

	for (size_t ii = 0U; ii < worker_count; ++ii)
		worker_queue_destroy(workers[ii].queue);

	linted_mem_free(pool);
}

static DWORD WINAPI master_worker_routine(void *arg)
{
	set_thread_name("asynch-worker-master");

	struct worker_pool *pool = arg;

	struct job_queue *job_queue = pool->job_queue;
	struct worker const *workers = pool->workers;
	size_t max_tasks = pool->worker_count;

	for (;;) {
		struct linted_asynch_task *task;
		{
			struct linted_asynch_task *xx;
			job_recv(job_queue, &xx);
			task = xx;
		}

		if (task->thread_canceller)
			break;

		for (size_t ii = 0U;; ii = (ii + 1U) % max_tasks) {
			linted_error errnum =
			    worker_try_submit(workers[ii].queue, task);
			if (LINTED_ERROR_AGAIN == errnum)
				continue;
			assert(0 == errnum);
			break;
		}
	}

	for (size_t ii = 0U; ii < max_tasks; ++ii) {
		struct worker *worker = &pool->workers[ii];
		struct worker_queue *worker_queue = worker->queue;
		linted_ko thread = worker->thread;
		for (;;) {
			struct linted_asynch_task task;
			linted_queue_node(&task.parent);
			task.thread_canceller = true;

			linted_error errnum =
			    worker_try_submit(worker_queue, &task);
			if (0 == errnum) {
				switch (WaitForSingleObject(thread, INFINITE)) {
				case WAIT_OBJECT_0:
					break;

				case WAIT_FAILED:
				default:
					assert(false);
				}
				break;
			}

			CancelSynchronousIo(thread);

			SwitchToThread();
		}
	}

	return 0;
}

static DWORD WINAPI worker_routine(void *arg)
{
	set_thread_name("asynch-worker");

	struct worker *worker = arg;

	struct linted_asynch_pool *asynch_pool = worker->pool;
	struct worker_queue *worker_queue = worker->queue;

	for (;;) {
		struct linted_asynch_task *task;
		{
			struct linted_asynch_task *xx;
			worker_recv(worker_queue, &xx);
			task = xx;
		}

		if (task->thread_canceller)
			break;

		linted_ko self = worker->thread;

		if (canceller_check_or_register(&task->canceller, self)) {
			linted_asynch_pool_complete(asynch_pool, task,
			                            LINTED_ERROR_CANCELLED);
			continue;
		}

		run_task(asynch_pool, task);
	}

	return 0;
}

#pragma weak linted_sched_do_idle
#pragma weak linted_io_do_poll
#pragma weak linted_io_do_read
#pragma weak linted_io_do_write
#pragma weak linted_pid_do_waitid
#pragma weak linted_signal_do_wait
#pragma weak linted_sched_do_sleep_until

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

	case LINTED_ASYNCH_TASK_SIGNAL_WAIT:
		linted_signal_do_wait(pool, task);
		break;

	case LINTED_ASYNCH_TASK_SLEEP_UNTIL:
		linted_sched_do_sleep_until(pool, task);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

struct poller_queue;
static linted_error poller_queue_create(struct poller_queue **queuep);
static linted_error poller_try_submit(struct poller_queue *queue,
                                      struct linted_asynch_waiter *waiter);
static linted_error poller_recv(struct poller_queue *queue,
                                struct linted_asynch_waiter **waiterp);
static void poller_queue_destroy(struct poller_queue *queue);

struct wait_manager;

struct poller
{
	struct linted_asynch_pool *pool;
	struct poller_queue *queue;
	linted_ko thread;
};

struct wait_manager
{
	struct linted_asynch_pool *asynch_pool;
	struct waiter_queue *waiter_queue;

	size_t poller_count;

	bool stopped : 1U;

	linted_ko master_thread;

	struct poller pollers[];
};

static DWORD WINAPI master_poller_routine(void *arg);
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

	size_t poller_queues_created = 0U;
	for (; poller_queues_created < max_pollers; ++poller_queues_created) {
		errnum = poller_queue_create(
		    &manager->pollers[poller_queues_created].queue);
		if (errnum != 0)
			goto free_manager;
	}

	manager->stopped = false;
	manager->poller_count = pollers_count;
	manager->waiter_queue = waiter_queue;
	manager->asynch_pool = asynch_pool;

	for (; created_threads < max_pollers; ++created_threads) {
		struct poller *poller = &manager->pollers[created_threads];

		poller->pool = asynch_pool;

		linted_ko thread =
		    CreateThread(0, 0, poller_routine, poller, 0, 0);
		if (INVALID_HANDLE_VALUE == thread) {
			errnum = GetLastError();
			break;
		}
		poller->thread = thread;
	}

	if (errnum != 0)
		goto destroy_threads;

	{
		linted_ko thread =
		    CreateThread(0, 0, master_poller_routine, manager, 0, 0);
		if (INVALID_HANDLE_VALUE == thread) {
			errnum = GetLastError();
			goto destroy_threads;
		}
		manager->master_thread = thread;
	}

	*managerp = manager;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < created_threads; ++ii) {
		struct poller const *poller = &manager->pollers[ii];
		struct poller_queue *poller_queue = poller->queue;
		linted_ko thread = poller->thread;

		for (;;) {
			struct linted_asynch_waiter waiter;
			linted_queue_node(&waiter.parent);
			waiter.thread_canceller = true;

			linted_error try_errnum =
			    poller_try_submit(poller_queue, &waiter);
			if (0 == try_errnum) {
				switch (WaitForSingleObject(thread, INFINITE)) {
				case WAIT_OBJECT_0:
					break;

				case WAIT_FAILED:
				default:
					assert(false);
				}
				break;
			}

			CancelSynchronousIo(thread);

			SwitchToThread();
		}
	}

	for (size_t ii = 0U; ii < poller_queues_created; ++ii)
		poller_queue_destroy(manager->pollers[ii].queue);

free_manager:
	linted_mem_free(manager);

	return errnum;
}

static void wait_manager_destroy(struct wait_manager *manager)
{
	struct waiter_queue *waiter_queue = manager->waiter_queue;
	linted_ko master_thread = manager->master_thread;
	size_t poller_count = manager->poller_count;
	struct poller const *pollers = manager->pollers;

	{
		struct linted_asynch_waiter waiter;
		linted_queue_node(&waiter.parent);
		waiter.thread_canceller = true;

		waiter_submit(waiter_queue, &waiter);

		switch (WaitForSingleObject(master_thread, INFINITE)) {
		case WAIT_OBJECT_0:
			break;

		case WAIT_FAILED:
		default:
			assert(false);
		}
	}

	for (size_t ii = 0U; ii < poller_count; ++ii)
		poller_queue_destroy(pollers[ii].queue);

	linted_mem_free(manager);
}

static DWORD WINAPI master_poller_routine(void *arg)
{
	set_thread_name("asynch-poller-master");

	struct wait_manager *pool = arg;

	struct waiter_queue *waiter_queue = pool->waiter_queue;
	struct poller *pollers = pool->pollers;
	size_t max_tasks = pool->poller_count;

	for (;;) {
		struct linted_asynch_waiter *waiter;
		{
			struct linted_asynch_waiter *xx;
			waiter_recv(waiter_queue, &xx);
			waiter = xx;
		}

		if (waiter->thread_canceller)
			break;

		for (size_t ii = 0U;; ii = (ii + 1U) % max_tasks) {
			linted_error errnum =
			    poller_try_submit(pollers[ii].queue, waiter);
			if (LINTED_ERROR_AGAIN == errnum)
				continue;
			assert(0 == errnum);
			break;
		}
	}

	for (size_t ii = 0U; ii < max_tasks; ++ii) {
		struct poller_queue *poller_queue = pollers[ii].queue;
		linted_ko thread = pollers[ii].thread;

		for (;;) {
			struct linted_asynch_waiter waiter;
			linted_queue_node(&waiter.parent);
			waiter.thread_canceller = true;

			linted_error errnum =
			    poller_try_submit(poller_queue, &waiter);
			if (0 == errnum) {
				switch (WaitForSingleObject(thread, INFINITE)) {
				case WAIT_OBJECT_0:
					break;

				case WAIT_FAILED:
				default:
					assert(false);
				}

				break;
			}

			CancelSynchronousIo(thread);

			SwitchToThread();
		}
	}

	return 0;
}

static DWORD WINAPI poller_routine(void *arg)
{
	set_thread_name("asynch-poller");

	struct poller *poller = arg;

	struct linted_asynch_pool *asynch_pool = poller->pool;
	struct poller_queue *poller_queue = poller->queue;

	linted_error errnum = 0;
	for (;;) {
		struct linted_asynch_waiter *waiter;
		{
			struct linted_asynch_waiter *xx;
			poller_recv(poller_queue, &xx);
			waiter = xx;
		}

		if (waiter->thread_canceller)
			break;

		linted_ko self = poller->thread;

		struct linted_asynch_task *task = waiter->task;
		linted_ko ko = waiter->ko;
		unsigned short flags = waiter->flags;

		if (canceller_check_or_register(&task->canceller, self)) {
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
			                     (void *)&xx, &yy)) {
				errnum = WSAGetLastError();
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

	return 0;
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
	errnum = WSAGetLastError();
	LINTED_ASSUME(errnum != 0);
	return errnum;

poll_succeeded:
	*reventsp = revents;
	return 0;
}

/* struct complete_queue is just a fake */
static linted_error completion_queue_create(struct completion_queue **queuep)
{
	linted_error errnum;

	struct linted_queue *queue;
	{
		struct linted_queue *xx;
		errnum = linted_queue_create(&xx);
		if (errnum != 0)
			return errnum;
		queue = xx;
	}

	*queuep = (struct completion_queue *)queue;
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
	{
		struct linted_queue_node *xx;
		errnum =
		    linted_queue_try_recv((struct linted_queue *)queue, &xx);
		if (errnum != 0)
			return errnum;
		node = xx;
	}

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
	linted_error errnum;

	struct linted_queue *queue;
	{
		struct linted_queue *xx;
		errnum = linted_queue_create(&xx);
		if (errnum != 0)
			return errnum;
		queue = xx;
	}

	*queuep = (struct job_queue *)queue;
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

	{
		struct linted_queue_node *xx;
		linted_queue_recv((struct linted_queue *)queue, &xx);
		node = xx;
	}

	*taskp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0;
}

static void job_queue_destroy(struct job_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}

/* struct worker_queue is just a fake */
static linted_error worker_queue_create(struct worker_queue **queuep)
{
	linted_error errnum;

	struct linted_channel *channel;
	{
		struct linted_channel *xx;
		errnum = linted_channel_create(&xx);
		if (errnum != 0)
			return errnum;
		channel = xx;
	}
	*queuep = (struct worker_queue *)channel;
	return 0;
}

static linted_error worker_try_submit(struct worker_queue *queue,
                                      struct linted_asynch_task *task)
{
	assert(queue != 0);
	assert(task != 0);
	return linted_channel_try_send((struct linted_channel *)queue, task);
}

static linted_error worker_recv(struct worker_queue *queue,
                                struct linted_asynch_task **taskp)
{
	assert(queue != 0);
	assert(taskp != 0);

	struct linted_asynch_task *task;
	{
		void *xx;
		linted_channel_recv((struct linted_channel *)queue, &xx);
		task = xx;
	}

	assert(task != 0);
	*taskp = task;

	return 0;
}

static void worker_queue_destroy(struct worker_queue *queue)
{
	linted_channel_destroy((struct linted_channel *)queue);
}

/* struct waiter_queue is just a fake */
static linted_error waiter_queue_create(struct waiter_queue **queuep)
{
	linted_error errnum;

	struct linted_queue *raw_queue;
	{
		struct linted_queue *xx;
		errnum = linted_queue_create(&xx);
		if (errnum != 0)
			return errnum;
		raw_queue = xx;
	}
	*queuep = (struct waiter_queue *)raw_queue;
	return 0;
}

static void waiter_queue_destroy(struct waiter_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}

static void waiter_submit(struct waiter_queue *queue,
                          struct linted_asynch_waiter *waiter)
{
	assert(queue != 0);
	assert(waiter != 0);

	linted_queue_send((struct linted_queue *)queue, LINTED_UPCAST(waiter));
}

static linted_error waiter_recv(struct waiter_queue *queue,
                                struct linted_asynch_waiter **waiterp)
{
	assert(queue != 0);
	assert(waiterp != 0);

	struct linted_queue_node *node;
	{
		struct linted_queue_node *xx;
		linted_queue_recv((struct linted_queue *)queue, &xx);
		node = xx;
	}

	*waiterp = LINTED_DOWNCAST(struct linted_asynch_waiter, node);

	return 0;
}

/* struct poller_queue is just a fake */
static linted_error poller_queue_create(struct poller_queue **queuep)
{
	linted_error errnum;

	struct linted_channel *channel;
	{
		struct linted_channel *xx;
		errnum = linted_channel_create(&xx);
		if (errnum != 0)
			return errnum;
		channel = xx;
	}

	*queuep = (struct poller_queue *)channel;
	return 0;
}

static linted_error poller_try_submit(struct poller_queue *queue,
                                      struct linted_asynch_waiter *waiter)
{
	assert(queue != 0);
	assert(waiter != 0);
	return linted_channel_try_send((struct linted_channel *)queue, waiter);
}

static linted_error poller_recv(struct poller_queue *queue,
                                struct linted_asynch_waiter **waiterp)
{
	struct linted_asynch_waiter *waiter;
	{
		void *xx;
		linted_channel_recv((struct linted_channel *)queue, &xx);
		waiter = xx;
	}

	assert(waiter != 0);
	*waiterp = waiter;

	return 0;
}

static void poller_queue_destroy(struct poller_queue *queue)
{
	linted_channel_destroy((struct linted_channel *)queue);
}

static void canceller_init(struct canceller *canceller)
{
	InitializeCriticalSection(&canceller->lock);

	canceller->in_flight = false;

	canceller->owned = false;
	canceller->cancel_replier = 0;
}

static void canceller_start(struct canceller *canceller)
{
	EnterCriticalSection(&canceller->lock);

	assert(!canceller->in_flight);
	assert(!canceller->owned);

	canceller->in_flight = true;
	canceller->owned = false;

	LeaveCriticalSection(&canceller->lock);
}

static void canceller_stop(struct canceller *canceller)
{
	EnterCriticalSection(&canceller->lock);

	assert(canceller->owned);
	assert(canceller->in_flight);

	{
		bool *cancel_replier = canceller->cancel_replier;
		bool cancelled = cancel_replier != 0;
		if (cancelled)
			*cancel_replier = true;
		canceller->cancel_replier = 0;
	}

	canceller->in_flight = false;
	canceller->owned = false;

	LeaveCriticalSection(&canceller->lock);
}

static void canceller_cancel(struct canceller *canceller)
{
	bool cancel_reply = false;
	bool in_flight;

	{
		EnterCriticalSection(&canceller->lock);

		assert(0 == canceller->cancel_replier);

		in_flight = canceller->in_flight;
		if (in_flight) {
			canceller->cancel_replier = &cancel_reply;

			bool owned = canceller->owned;
			if (owned)
				CancelSynchronousIo(canceller->owner);
		}

		LeaveCriticalSection(&canceller->lock);
	}

	if (!in_flight)
		return;

	/* Yes, really, we do have to busy wait to prevent race
	 * conditions unfortunately */
	bool cancel_replied;
	do {
		SwitchToThread();

		EnterCriticalSection(&canceller->lock);

		cancel_replied = cancel_reply;
		if (!cancel_replied) {
			bool owned = canceller->owned;
			if (owned)
				CancelSynchronousIo(canceller->owner);
		}

		LeaveCriticalSection(&canceller->lock);
	} while (!cancel_replied);
}

static bool canceller_check_or_register(struct canceller *canceller,
                                        linted_ko self)
{
	bool cancelled;

	EnterCriticalSection(&canceller->lock);

	{
		cancelled = canceller->cancel_replier != 0;

		/* Don't actually complete the cancellation if
		 * cancelled and let the completion do that.
		 */
		canceller->owner = self;
		canceller->owned = true;
	}

	LeaveCriticalSection(&canceller->lock);

	return cancelled;
}

static bool canceller_check_and_unregister(struct canceller *canceller)
{
	bool cancelled;

	EnterCriticalSection(&canceller->lock);

	assert(canceller->in_flight);
	assert(canceller->owned);

	canceller->owned = false;
	{
		bool *cancel_replier = canceller->cancel_replier;
		cancelled = cancel_replier != 0;
		if (cancelled)
			*cancel_replier = true;
		canceller->cancel_replier = 0;
	}

	LeaveCriticalSection(&canceller->lock);

	return cancelled;
}

/* MSVC's way of setting thread names is really weird and hacky */

#define MS_VC_EXCEPTION 0x406D1388

#pragma pack(push, 8)
typedef struct tagTHREADNAME_INFO
{
	DWORD dwType;
	LPCSTR szName;
	DWORD dwThreadID;
	DWORD dwFlags;
} THREADNAME_INFO;
#pragma pack(pop)

static void set_thread_name(char const *name)
{

	THREADNAME_INFO info = {0};

	/* Must be 0x1000 */
	info.dwType = 0x1000;
	info.szName = name;

	/* Thread ID (-1=caller thread) */
	info.dwThreadID = -1;

	/* Reserved for the future */
	info.dwFlags = 0;

/* TODO: Find some way of getting this working with GCC which
 * doesn't support SEH. */
#if !defined __GNUC__
	__try
	{
#endif
		RaiseException(MS_VC_EXCEPTION, 0,
		               sizeof info / sizeof(ULONG_PTR),
		               (ULONG_PTR *)&info);
#if !defined __GNUC__
	}
	__except(EXCEPTION_EXECUTE_HANDLER)
	{
		/* Do nothing */
	}
#endif
}
