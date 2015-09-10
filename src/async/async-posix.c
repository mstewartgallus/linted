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

#include "linted/async.h"

#include "linted/io.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/channel.h"
#include "linted/queue.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/util.h"

#include <errno.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <unistd.h>

#if !defined HAVE_PTHREAD_SETNAME_NP && defined HAVE_SYS_PRCTL_H
#include <sys/prctl.h>
#endif

#if defined _POSIX_SPIN_LOCKS
typedef pthread_spinlock_t spinlock;
#else
typedef pthread_mutex_t spinlock;
#endif

static inline void spinlock_init(spinlock *lock);
static inline void spinlock_destroy(spinlock *lock);
static inline linted_error spinlock_lock(spinlock *lock);
static inline linted_error spinlock_unlock(spinlock *lock);

static size_t small_stack_size(void);

/**
 * A one reader to many writers queue. Should be able to retrieve
 * many values at once. As all writes are a direct result of
 * submitted commands there is no need to worry about it growing
 * too large.
 */
struct completion_queue;
static linted_error
completion_queue_create(struct completion_queue **queuep);
static void complete_task(struct completion_queue *queue,
                          struct linted_async_task *task);
static linted_error completion_recv(struct completion_queue *queue,
                                    struct linted_async_task **taskp);
static linted_error
completion_try_recv(struct completion_queue *queue,
                    struct linted_async_task **taskp);
static void completion_queue_destroy(struct completion_queue *queue);

struct job_queue;
static linted_error job_queue_create(struct job_queue **queuep);
static void job_submit(struct job_queue *queue,
                       struct linted_async_task *task);
static linted_error job_recv(struct job_queue *queue,
                             struct linted_async_task **taskp);
static void job_queue_destroy(struct job_queue *queue);

struct worker_queue;
static linted_error worker_queue_create(struct worker_queue **queuep);
static linted_error worker_try_submit(struct worker_queue *queue,
                                      struct linted_async_task *task);
static linted_error worker_recv(struct worker_queue *queue,
                                struct linted_async_task **taskp);
static void worker_queue_destroy(struct worker_queue *queue);

struct waiter_queue;
static linted_error waiter_queue_create(struct waiter_queue **queuep);
static void waiter_queue_destroy(struct waiter_queue *queue);
static linted_ko waiter_ko(struct waiter_queue *queue);
static void waiter_submit(struct waiter_queue *queue,
                          struct linted_async_waiter *waiter);
static linted_error
waiter_try_recv(struct waiter_queue *queue,
                struct linted_async_waiter **waiterp);

struct worker_pool;
static linted_error worker_pool_create(struct worker_pool **poolp,
                                       struct job_queue *job_queue,
                                       struct linted_async_pool *pool,
                                       unsigned max_tasks);
static void worker_pool_destroy(struct worker_pool *pool);

struct wait_manager;
static linted_error wait_manager_create(
    struct wait_manager **managerp, struct waiter_queue *waiter_queue,
    struct linted_async_pool *pool, unsigned max_pollers);
static void wait_manager_destroy(struct wait_manager *manager);

struct canceller;

static void canceller_init(struct canceller *canceller);
static void canceller_start(struct canceller *canceller);
static void canceller_stop(struct canceller *canceller);
static void canceller_cancel(struct canceller *canceller);
static bool canceller_check_or_register(struct canceller *canceller,
                                        pthread_t self);
static bool canceller_check_and_unregister(struct canceller *canceller);

struct canceller {
	pthread_t owner;
	spinlock lock;
	bool cancelled : 1U;
	bool cancel_reply : 1U;
	bool owned : 1U;
	bool in_flight : 1U;
};

struct linted_async_pool {
	struct wait_manager *wait_manager;
	struct worker_pool *worker_pool;

	struct job_queue *job_queue;
	struct waiter_queue *waiter_queue;
	struct completion_queue *completion_queue;
};

struct linted_async_task {
	struct linted_queue_node parent;
	struct canceller canceller;
	void *data;
	union linted_async_ck task_ck;
	linted_error err;
	linted_async_type type;
	bool thread_canceller : 1U;
};

struct linted_async_waiter {
	struct linted_queue_node parent;
	struct linted_async_task *task;
	linted_ko ko;
	short flags;
	short revents;
	bool thread_canceller : 1U;
};

static void set_thread_name(char const *name);

linted_error linted_async_pool_create(struct linted_async_pool **poolp,
                                      unsigned max_tasks)
{
	LINTED_ASSERT(poolp != 0);

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

	struct job_queue *job_queue;
	{
		struct job_queue *xx;
		err = job_queue_create(&xx);
		if (err != 0)
			goto destroy_waiter_queue;
		job_queue = xx;
	}

	struct completion_queue *completion_queue;
	{
		struct completion_queue *xx;
		err = completion_queue_create(&xx);
		if (err != 0)
			goto destroy_job_queue;
		completion_queue = xx;
	}

	struct wait_manager *wait_manager;
	{
		struct wait_manager *xx;
		err = wait_manager_create(&xx, waiter_queue, pool,
		                          max_tasks);
		if (err != 0)
			goto destroy_completion_queue;
		wait_manager = xx;
	}

	struct worker_pool *worker_pool;
	{
		struct worker_pool *xx;
		err =
		    worker_pool_create(&xx, job_queue, pool, max_tasks);
		if (err != 0)
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

	return err;
}

linted_error linted_async_pool_destroy(struct linted_async_pool *pool)
{
	LINTED_ASSERT(pool != 0);

	struct wait_manager *wait_manager = pool->wait_manager;
	struct worker_pool *worker_pool = pool->worker_pool;

	struct job_queue *job_queue = pool->job_queue;
	struct waiter_queue *waiter_queue = pool->waiter_queue;
	struct completion_queue *completion_queue =
	    pool->completion_queue;

	worker_pool_destroy(worker_pool);
	wait_manager_destroy(wait_manager);

	job_queue_destroy(job_queue);
	waiter_queue_destroy(waiter_queue);
	completion_queue_destroy(completion_queue);

	linted_mem_free(pool);

	return 0;
}

void linted_async_pool_submit(struct linted_async_pool *pool,
                              struct linted_async_task *task)
{
	LINTED_ASSERT(pool != 0);
	LINTED_ASSERT(task != 0);

	canceller_start(&task->canceller);

	job_submit(pool->job_queue, task);
}

void linted_async_pool_resubmit(struct linted_async_pool *pool,
                                struct linted_async_task *task)
{
	LINTED_ASSERT(pool != 0);
	LINTED_ASSERT(task != 0);

	if (canceller_check_and_unregister(&task->canceller)) {
		task->err = LINTED_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	job_submit(pool->job_queue, task);
}

void linted_async_pool_complete(struct linted_async_pool *pool,
                                struct linted_async_task *task,
                                linted_error task_err)
{
	LINTED_ASSERT(pool != 0);
	LINTED_ASSERT(task != 0);

	canceller_stop(&task->canceller);

	task->err = task_err;
	complete_task(pool->completion_queue, task);
}

void linted_async_pool_wait_on_poll(struct linted_async_pool *pool,
                                    struct linted_async_waiter *waiter,
                                    struct linted_async_task *task,
                                    linted_ko ko, short flags)
{
	LINTED_ASSERT(pool != 0);
	LINTED_ASSERT(waiter != 0);
	LINTED_ASSERT(task != 0);

	if (canceller_check_and_unregister(&task->canceller)) {
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
	return completion_recv(pool->completion_queue, completionp);
}

linted_error
linted_async_pool_poll(struct linted_async_pool *pool,
                       struct linted_async_task **completionp)
{
	LINTED_ASSERT(pool != 0);
	return completion_try_recv(pool->completion_queue, completionp);
}

linted_error
linted_async_waiter_create(struct linted_async_waiter **waiterp)
{
	LINTED_ASSERT(waiterp != 0);

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
	waiter->thread_canceller = false;

	*waiterp = waiter;
	return 0;
}

void linted_async_waiter_destroy(struct linted_async_waiter *waiter)
{
	linted_mem_free(waiter);
}

short linted_async_waiter_revents(struct linted_async_waiter *waiter)
{
	LINTED_ASSERT(waiter != 0);

	short ev = waiter->revents;
	waiter->revents = 0;
	return ev;
}

linted_error linted_async_task_create(struct linted_async_task **taskp,
                                      void *data,
                                      linted_async_type type)
{
	LINTED_ASSERT(taskp != 0);

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

	task->thread_canceller = false;

	*taskp = task;
	return 0;
}

void linted_async_task_destroy(struct linted_async_task *task)
{
	linted_mem_free(task);
}

void linted_async_task_cancel(struct linted_async_task *task)
{
	LINTED_ASSERT(task != 0);
	canceller_cancel(&task->canceller);
}

void linted_async_task_prepare(struct linted_async_task *task,
                               union linted_async_ck task_ck)
{
	LINTED_ASSERT(task != 0);
	task->task_ck = task_ck;
}

union linted_async_ck
linted_async_task_ck(struct linted_async_task *task)
{
	LINTED_ASSERT(task != 0);
	return task->task_ck;
}

linted_error linted_async_task_err(struct linted_async_task *task)
{
	LINTED_ASSERT(task != 0);
	return task->err;
}

void *linted_async_task_data(struct linted_async_task *task)
{
	LINTED_ASSERT(task != 0);
	return task->data;
}

static void *master_worker_routine(void *arg);
static void *worker_routine(void *arg);

static void run_task(struct linted_async_pool *pool,
                     struct linted_async_task *task);

struct worker_pool;

struct worker {
	struct linted_async_pool *pool;
	struct worker_queue *queue;
	pthread_t thread;
};

struct worker_pool {
	struct linted_async_pool *async_pool;
	struct job_queue *job_queue;
	struct worker_queue **worker_queues;

	size_t worker_stacks_size;
	void *worker_stacks;

	size_t worker_count;

	pthread_t master_thread;

	struct worker workers[];
};

static linted_error worker_pool_create(
    struct worker_pool **poolp, struct job_queue *job_queue,
    struct linted_async_pool *async_pool, unsigned max_tasks)
{
	LINTED_ASSERT(poolp != 0);
	LINTED_ASSERT(job_queue != 0);
	LINTED_ASSERT(async_pool != 0);

	linted_error err = 0;

	size_t workers_count = max_tasks;

	struct worker_pool *pool;
	size_t workers_size = workers_count * sizeof pool->workers[0U];
	size_t worker_pool_size = sizeof *pool + workers_size;
	{
		void *xx;
		err = linted_mem_alloc(&xx, worker_pool_size);
		if (err != 0)
			return err;
		pool = xx;
	}

	size_t queues_created = 0U;
	for (; queues_created < max_tasks; ++queues_created) {
		struct worker_queue **queuep =
		    &pool->workers[queues_created].queue;
		err = worker_queue_create(queuep);
		if (err != 0)
			goto destroy_worker_queues;
	}

	size_t stack_size = small_stack_size();

	long maybe_page_size = sysconf(_SC_PAGE_SIZE);
	LINTED_ASSERT(maybe_page_size >= 0);
	size_t page_size = maybe_page_size;

	size_t stack_and_guard_size = stack_size + page_size;
	size_t worker_stacks_size =
	    page_size + stack_and_guard_size * workers_count;
	void *worker_stacks = mmap(
	    0, worker_stacks_size, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK, -1,
	    0);
	if (0 == worker_stacks) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto destroy_worker_queues;
	}

	/* Guard pages are shared between the stacks */
	if (-1 ==
	    mprotect((char *)worker_stacks, page_size, PROT_NONE)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto destroy_stacks;
	}

	char *tail_guard_page =
	    (char *)worker_stacks + page_size + stack_size;
	for (size_t ii = 0U; ii < workers_count; ++ii) {
		if (-1 ==
		    mprotect(tail_guard_page, page_size, PROT_NONE)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto destroy_stacks;
		}
		tail_guard_page += page_size + stack_size;
	}

	pool->worker_count = workers_count;
	pool->worker_stacks = worker_stacks;
	pool->worker_stacks_size = worker_stacks_size;
	pool->job_queue = job_queue;
	pool->async_pool = async_pool;

	size_t created_threads = 0U;
	{
		pthread_attr_t attr;

		err = pthread_attr_init(&attr);
		if (err != 0)
			goto destroy_stacks;

		char *last_stack = (char *)worker_stacks + page_size;
		for (; created_threads < max_tasks; ++created_threads) {
			err = pthread_attr_setstack(&attr, last_stack,
			                            stack_size);
			if (err != 0) {
				LINTED_ASSERT(err != EINVAL);
				LINTED_ASSERT(false);
			}
			last_stack += stack_size + page_size;

			struct worker *worker =
			    &pool->workers[created_threads];

			worker->pool = async_pool;

			/* Get EPIPEs */
			/* SIGPIPE may not be blocked already */
			sigset_t oldset;
			{
				sigset_t pipe_set;
				sigemptyset(&pipe_set);
				sigaddset(&pipe_set, SIGPIPE);
				err = pthread_sigmask(
				    SIG_BLOCK, &pipe_set, &oldset);
				if (err != 0)
					break;
			}

			err = pthread_create(&worker->thread, &attr,
			                     worker_routine, worker);

			linted_error mask_err =
			    pthread_sigmask(SIG_SETMASK, &oldset, 0);
			if (0 == err)
				err = mask_err;

			if (err != 0)
				break;
		}

		linted_error destroy_err = pthread_attr_destroy(&attr);
		if (0 == err)
			err = destroy_err;
	}

	if (err != 0)
		goto destroy_threads;

	err = pthread_create(&pool->master_thread, 0,
	                     master_worker_routine, pool);
	if (err != 0)
		goto destroy_threads;

	*poolp = pool;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < created_threads; ++ii) {
		struct worker const *worker = &pool->workers[ii];
		struct worker_queue *worker_queue = worker->queue;
		pthread_t thread = worker->thread;

		for (;;) {
			struct linted_async_task task;
			linted_queue_node(&task.parent);
			task.thread_canceller = true;

			linted_error try_err =
			    worker_try_submit(worker_queue, &task);
			if (0 == try_err) {
				pthread_join(thread, 0);
				break;
			}

			linted_error kill_err =
			    pthread_kill(thread, LINTED_ASYNCH_SIGNO);
			if (kill_err != 0 && kill_err != EAGAIN) {
				LINTED_ASSERT(kill_err != ESRCH);
				LINTED_ASSERT(kill_err != EINVAL);
				LINTED_ASSERT(false);
			}

			sched_yield();
		}
	}

destroy_stacks:
	munmap(worker_stacks, worker_stacks_size);

destroy_worker_queues:
	for (size_t ii = 0U; ii < queues_created; ++ii)
		worker_queue_destroy(pool->workers[ii].queue);

	linted_mem_free(pool);

	return err;
}

static void worker_pool_destroy(struct worker_pool *pool)
{
	LINTED_ASSERT(pool != 0);

	struct job_queue *job_queue = pool->job_queue;
	pthread_t master_thread = pool->master_thread;
	struct worker const *workers = pool->workers;
	size_t worker_count = pool->worker_count;

	{
		struct linted_async_task task;
		linted_queue_node(&task.parent);
		task.thread_canceller = true;

		job_submit(job_queue, &task);

		pthread_join(master_thread, 0);
	}

	munmap(pool->worker_stacks, pool->worker_stacks_size);

	for (size_t ii = 0U; ii < worker_count; ++ii)
		worker_queue_destroy(workers[ii].queue);

	linted_mem_free(pool);
}

static void *master_worker_routine(void *arg)
{
	LINTED_ASSERT(arg != 0);

	set_thread_name("async-worker-master");

	struct worker_pool *pool = arg;

	struct job_queue *job_queue = pool->job_queue;
	struct worker const *workers = pool->workers;
	size_t max_tasks = pool->worker_count;

	for (;;) {
		struct linted_async_task *task;
		{
			struct linted_async_task *xx;
			job_recv(job_queue, &xx);
			task = xx;
		}

		if (task->thread_canceller)
			break;

		for (size_t ii = 0U;; ii = (ii + 1U) % max_tasks) {
			linted_error err =
			    worker_try_submit(workers[ii].queue, task);
			if (LINTED_ERROR_AGAIN == err)
				continue;
			LINTED_ASSERT(0 == err);
			break;
		}
	}

	for (size_t ii = 0U; ii < max_tasks; ++ii) {
		struct worker *worker = &pool->workers[ii];
		struct worker_queue *worker_queue = worker->queue;
		pthread_t thread = worker->thread;
		for (;;) {
			struct linted_async_task task;
			linted_queue_node(&task.parent);
			task.thread_canceller = true;

			linted_error err =
			    worker_try_submit(worker_queue, &task);
			if (0 == err) {
				pthread_join(thread, 0);
				break;
			}

			err = pthread_kill(thread, LINTED_ASYNCH_SIGNO);
			if (err != 0 && err != EAGAIN) {
				LINTED_ASSERT(err != ESRCH);
				LINTED_ASSERT(err != EINVAL);
				LINTED_ASSERT(false);
			}

			sched_yield();
		}
	}

	return 0;
}

static void *worker_routine(void *arg)
{
	LINTED_ASSERT(arg != 0);

	set_thread_name("async-worker");

	struct worker *worker = arg;

	struct linted_async_pool *async_pool = worker->pool;
	struct worker_queue *worker_queue = worker->queue;

	pthread_t self = pthread_self();

	for (;;) {
		struct linted_async_task *task;
		{
			struct linted_async_task *xx;
			worker_recv(worker_queue, &xx);
			task = xx;
		}

		if (task->thread_canceller)
			break;

		if (canceller_check_or_register(&task->canceller,
		                                self)) {
			linted_async_pool_complete(
			    async_pool, task, LINTED_ERROR_CANCELLED);
			continue;
		}

		run_task(async_pool, task);
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

struct wait_manager {
	struct linted_async_pool *async_pool;
	struct waiter_queue *waiter_queue;

	size_t poller_count;

	pthread_t master_thread;

	struct epoll_event *epoll_events;
	struct linted_async_waiter **waiters;

	linted_ko epoll_ko;

	bool stopped : 1U;
};

static void *master_poller_routine(void *arg);
static linted_error
do_task_for_event(struct linted_async_pool *async_pool,
                  struct linted_async_waiter *waiter, uint32_t revents);
static linted_error recv_waiters(struct linted_async_pool *async_pool,
                                 linted_ko epoll_ko,
                                 struct linted_async_waiter **waiters,
                                 struct waiter_queue *waiter_queue,
                                 size_t max_tasks, uint32_t revents);

static linted_error wait_manager_create(
    struct wait_manager **managerp, struct waiter_queue *waiter_queue,
    struct linted_async_pool *async_pool, unsigned max_pollers)
{
	LINTED_ASSERT(managerp != 0);
	LINTED_ASSERT(waiter_queue != 0);
	LINTED_ASSERT(async_pool != 0);

	linted_error err;

	struct wait_manager *manager;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *manager);
		if (err != 0)
			return err;
		manager = xx;
	}

	struct linted_async_waiter **waiters;
	{
		void *xx;
		err = linted_mem_alloc_array(&xx, max_pollers,
		                             sizeof waiters[0U]);
		if (err != 0)
			goto free_manager;
		waiters = xx;
	}

	struct epoll_event *epoll_events;
	{
		void *xx;
		err = linted_mem_alloc_array(&xx, max_pollers + 1U,
		                             sizeof epoll_events[0U]);
		if (err != 0)
			goto free_waiters;
		epoll_events = xx;
	}

	int epoll_fd = epoll_create1(EPOLL_CLOEXEC);
	if (-1 == epoll_fd) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto free_events;
	}

	/* We need an extra page for signals */
	manager->stopped = false;
	manager->waiters = waiters;
	manager->epoll_ko = epoll_fd;
	manager->epoll_events = epoll_events;
	manager->poller_count = max_pollers;
	manager->waiter_queue = waiter_queue;
	manager->async_pool = async_pool;

	err = pthread_create(&manager->master_thread, 0,
	                     master_poller_routine, manager);
	if (err != 0)
		goto close_epoll_fd;

	*managerp = manager;

	return 0;

close_epoll_fd:
	linted_ko_close(epoll_fd);

free_events:
	linted_mem_free(epoll_events);

free_waiters:
	linted_mem_free(waiters);

free_manager:
	linted_mem_free(manager);

	return err;
}

static void wait_manager_destroy(struct wait_manager *manager)
{
	LINTED_ASSERT(manager != 0);

	struct waiter_queue *waiter_queue = manager->waiter_queue;
	pthread_t master_thread = manager->master_thread;
	linted_ko epoll_ko = manager->epoll_ko;
	struct epoll_event *epoll_events = manager->epoll_events;
	struct linted_async_waiter **waiters = manager->waiters;

	{
		struct linted_async_waiter waiter;
		linted_queue_node(&waiter.parent);
		waiter.thread_canceller = true;

		waiter_submit(waiter_queue, &waiter);

		pthread_join(master_thread, 0);
	}

	linted_ko_close(epoll_ko);

	linted_mem_free(epoll_events);
	linted_mem_free(waiters);

	linted_mem_free(manager);
}

static void *master_poller_routine(void *arg)
{
	LINTED_ASSERT(arg != 0);

	set_thread_name("async-poller-master");

	struct wait_manager *pool = arg;

	struct waiter_queue *waiter_queue = pool->waiter_queue;
	struct linted_async_waiter **waiters = pool->waiters;
	linted_ko epoll_ko = pool->epoll_ko;
	struct epoll_event *epoll_events = pool->epoll_events;
	size_t max_tasks = pool->poller_count;
	struct linted_async_pool *async_pool = pool->async_pool;

	for (size_t ii = 0U; ii < max_tasks; ++ii)
		waiters[ii] = 0;

	linted_error err = 0;
	{
		struct epoll_event xx = {0};
		xx.events = EPOLLIN;
		xx.data.u64 = 0U;
		if (-1 == epoll_ctl(epoll_ko, EPOLL_CTL_ADD,
		                    waiter_ko(waiter_queue), &xx)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto exit_mainloop;
		}
	}

	for (;;) {
		int numpolled;
		for (;;) {
			numpolled = epoll_wait(epoll_ko, epoll_events,
			                       1U + max_tasks, -1);
			if (-1 == numpolled) {
				err = errno;
				LINTED_ASSUME(err != 0);

				if (EINTR == err)
					goto check_cancellers;
				if (EAGAIN == err)
					continue;
				if (ENOMEM == err)
					continue;

				LINTED_ASSERT(err != EFAULT);
				LINTED_ASSERT(err != EINVAL);

				LINTED_ASSERT(0);
			}
			break;
		}
		for (size_t ii = 0U; ii < (unsigned)numpolled; ++ii) {
			size_t index = epoll_events[ii].data.u64;
			uint32_t revents = epoll_events[ii].events;

			if (index != 0U) {
				LINTED_ASSERT(waiters[index - 1U] != 0);
				err = do_task_for_event(
				    async_pool, waiters[index - 1U],
				    revents);
				if (err != 0)
					goto exit_mainloop;
				int fd = waiters[index - 1U]->ko;
				if (-1 == epoll_ctl(epoll_ko,
				                    EPOLL_CTL_DEL, fd,
				                    0)) {
					err = errno;
					LINTED_ASSUME(err != 0);
					goto exit_mainloop;
				}

				waiters[index - 1U] = 0;
				continue;
			}

			err = recv_waiters(async_pool, epoll_ko,
			                   waiters, waiter_queue,
			                   max_tasks, revents);
			if (ECANCELED == err) {
				goto exit_mainloop;
			}
		}

	check_cancellers:
		for (size_t ii = 0U; ii < max_tasks; ++ii) {
			struct linted_async_waiter *waiter =
			    waiters[ii];
			if (0 == waiter)
				continue;

			struct linted_async_task *task = waiter->task;

			pthread_t self = pthread_self();

			if (!canceller_check_or_register(
			        &task->canceller, self))
				continue;

			if (-1 == epoll_ctl(epoll_ko, EPOLL_CTL_DEL,
			                    waiter->ko, 0)) {
				LINTED_ASSERT(0);
			}
			waiters[ii] = 0;

			linted_async_pool_complete(
			    async_pool, task, LINTED_ERROR_CANCELLED);
		}
	}
exit_mainloop:
	return 0;
}

static linted_error recv_waiters(struct linted_async_pool *async_pool,
                                 linted_ko epoll_ko,
                                 struct linted_async_waiter **waiters,
                                 struct waiter_queue *waiter_queue,
                                 size_t max_tasks, uint32_t revents)
{
	linted_error err = 0;

	bool has_pollerr = (revents & EPOLLERR) != 0;
	bool has_pollhup = (revents & EPOLLHUP) != 0;
	bool has_pollin = (revents & EPOLLIN) != 0;

	LINTED_ASSERT(!has_pollerr);
	LINTED_ASSERT(!has_pollhup);

	LINTED_ASSERT(has_pollin);

	for (;;) {
		uint64_t xx;
		if (-1 ==
		    read(waiter_ko(waiter_queue), &xx, sizeof xx)) {
			err = errno;
			LINTED_ASSERT(err != 0);
			if (EINTR == err)
				continue;
			if (EAGAIN == err)
				break;
			LINTED_ASSERT(0);
		}
		break;
	}

	for (;;) {
		struct linted_async_waiter *waiter;
		{
			struct linted_async_waiter *xx;
			err = waiter_try_recv(waiter_queue, &xx);
			if (EAGAIN == err)
				break;
			waiter = xx;
		}

		if (waiter->thread_canceller)
			return ECANCELED;

		linted_ko ko = waiter->ko;
		struct linted_async_task *task = waiter->task;
		uint32_t flags = waiter->flags;

		pthread_t self = pthread_self();
		if (canceller_check_or_register(&task->canceller,
		                                self)) {
			err = LINTED_ERROR_CANCELLED;
			goto complete_task;
		}

		size_t jj = 0U;
		for (; jj < max_tasks; ++jj) {
			if (0 == waiters[jj])
				goto finish;
		}
		LINTED_ASSERT(0);

	finish:
		waiters[jj] = waiter;

		{
			struct epoll_event xx = {0};
			xx.events = flags | EPOLLONESHOT | EPOLLET;
			xx.data.u64 = 1U + jj;
			if (-1 == epoll_ctl(epoll_ko, EPOLL_CTL_ADD, ko,
			                    &xx)) {
				err = errno;
				LINTED_ASSUME(err != 0);
				return err;
			}
		}
		continue;

	complete_task:
		linted_async_pool_complete(async_pool, task, err);
		continue;
	}

	return 0;
}

static linted_error
do_task_for_event(struct linted_async_pool *async_pool,
                  struct linted_async_waiter *waiter, uint32_t revents)
{
	struct linted_async_task *task = waiter->task;
	linted_ko ko = waiter->ko;

	bool has_pollerr = (revents & POLLERR) != 0;
	bool has_pollnval = (revents & POLLNVAL) != 0;

	linted_error err = 0;

	if (has_pollnval) {
		err = LINTED_ERROR_INVALID_KO;
		goto complete_task;
	}

	if (has_pollerr) {
		int xx;
		socklen_t yy = sizeof xx;
		if (-1 ==
		    getsockopt(ko, SOL_SOCKET, SO_ERROR, &xx, &yy)) {
			err = errno;
			goto complete_task;
		}
		err = xx;
		/* If another poller got the error then we could get
		 * zero instead so just resubmit in that case.
		 */
		if (err != 0)
			goto complete_task;
	}

	waiter->revents = revents;
	linted_async_pool_resubmit(async_pool, task);
	return 0;

complete_task:
	linted_async_pool_complete(async_pool, task, err);
	return 0;
}

/* struct complete_queue is just a fake */
static linted_error
completion_queue_create(struct completion_queue **queuep)
{
	LINTED_ASSERT(queuep != 0);

	linted_error err;

	struct linted_queue *queue;
	{
		struct linted_queue *xx;
		err = linted_queue_create(&xx);
		if (err != 0)
			return err;
		queue = xx;
	}

	*queuep = (struct completion_queue *)queue;
	return 0;
}

static void complete_task(struct completion_queue *queue,
                          struct linted_async_task *task)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(task != 0);

	linted_queue_send((struct linted_queue *)queue,
	                  LINTED_UPCAST(task));
}

static linted_error completion_recv(struct completion_queue *queue,
                                    struct linted_async_task **taskp)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(taskp != 0);

	struct linted_queue_node *node;

	linted_queue_recv((struct linted_queue *)queue, &node);

	*taskp = LINTED_DOWNCAST(struct linted_async_task, node);

	return 0;
}

static linted_error
completion_try_recv(struct completion_queue *queue,
                    struct linted_async_task **taskp)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(taskp != 0);

	linted_error err;

	struct linted_queue_node *node;
	{
		struct linted_queue_node *xx;
		err = linted_queue_try_recv(
		    (struct linted_queue *)queue, &xx);
		if (err != 0)
			return err;
		node = xx;
	}

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
	LINTED_ASSERT(queuep != 0);

	linted_error err;

	struct linted_queue *queue;
	{
		struct linted_queue *xx;
		err = linted_queue_create(&xx);
		if (err != 0)
			return err;
		queue = xx;
	}

	*queuep = (struct job_queue *)queue;
	return 0;
}

static void job_submit(struct job_queue *queue,
                       struct linted_async_task *task)
{
	linted_queue_send((struct linted_queue *)queue,
	                  LINTED_UPCAST(task));
}

static linted_error job_recv(struct job_queue *queue,
                             struct linted_async_task **taskp)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(taskp != 0);

	struct linted_queue_node *node;

	{
		struct linted_queue_node *xx;
		linted_queue_recv((struct linted_queue *)queue, &xx);
		node = xx;
	}

	*taskp = LINTED_DOWNCAST(struct linted_async_task, node);

	return 0;
}

static void job_queue_destroy(struct job_queue *queue)
{
	linted_queue_destroy((struct linted_queue *)queue);
}

/* struct worker_queue is just a fake */
static linted_error worker_queue_create(struct worker_queue **queuep)
{
	LINTED_ASSERT(queuep != 0);

	linted_error err;

	struct linted_channel *channel;
	{
		struct linted_channel *xx;
		err = linted_channel_create(&xx);
		if (err != 0)
			return err;
		channel = xx;
	}
	*queuep = (struct worker_queue *)channel;
	return 0;
}

static linted_error worker_try_submit(struct worker_queue *queue,
                                      struct linted_async_task *task)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(task != 0);
	return linted_channel_try_send((struct linted_channel *)queue,
	                               task);
}

static linted_error worker_recv(struct worker_queue *queue,
                                struct linted_async_task **taskp)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(taskp != 0);

	struct linted_async_task *task;
	{
		void *xx;
		linted_channel_recv((struct linted_channel *)queue,
		                    &xx);
		task = xx;
	}

	LINTED_ASSERT(task != 0);
	*taskp = task;

	return 0;
}

static void worker_queue_destroy(struct worker_queue *queue)
{
	linted_channel_destroy((struct linted_channel *)queue);
}

struct waiter_queue {
	struct linted_queue *queue;
	int waiter_fd;
};

static linted_error waiter_queue_create(struct waiter_queue **queuep)
{
	LINTED_ASSERT(queuep != 0);

	linted_error err;

	struct linted_queue *raw_queue;
	{
		struct linted_queue *xx;
		err = linted_queue_create(&xx);
		if (err != 0)
			return err;
		raw_queue = xx;
	}

	int waiter_fd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
	if (-1 == waiter_fd) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto free_queue;
	}

	struct waiter_queue *queue;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *queue);
		if (err != 0)
			goto close_waiter_fd;
		queue = xx;
	}

	queue->waiter_fd = waiter_fd;
	queue->queue = raw_queue;
	*queuep = queue;
	return 0;

close_waiter_fd:
	linted_ko_close(waiter_fd);

free_queue:
	linted_queue_destroy(raw_queue);

	return err;
}

static void waiter_queue_destroy(struct waiter_queue *queue)
{
	linted_queue_destroy(queue->queue);
	linted_ko_close(queue->waiter_fd);
	linted_mem_free(queue);
}

static linted_ko waiter_ko(struct waiter_queue *queue)
{
	return queue->waiter_fd;
}

static void waiter_submit(struct waiter_queue *queue,
                          struct linted_async_waiter *waiter)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(waiter != 0);

	linted_error err = 0;

	linted_queue_send(queue->queue, LINTED_UPCAST(waiter));
	for (;;) {
		uint64_t xx = 0xFF;
		if (-1 == write(queue->waiter_fd, &xx, sizeof xx)) {
			err = errno;
			LINTED_ASSERT(err != 0);
			if (EINTR == err)
				continue;

			LINTED_ASSERT(false);
		}
		break;
	}
}

static linted_error
waiter_try_recv(struct waiter_queue *queue,
                struct linted_async_waiter **waiterp)
{
	LINTED_ASSERT(queue != 0);
	LINTED_ASSERT(waiterp != 0);

	linted_error err = 0;

	struct linted_queue_node *node;
	{
		struct linted_queue_node *xx;
		err = linted_queue_try_recv(queue->queue, &xx);
		if (err != 0)
			return err;
		node = xx;
	}

	*waiterp = LINTED_DOWNCAST(struct linted_async_waiter, node);

	return 0;
}

static void canceller_init(struct canceller *canceller)
{
	LINTED_ASSERT(canceller != 0);

	spinlock_init(&canceller->lock);

	canceller->in_flight = false;

	canceller->owned = false;
	canceller->cancelled = false;
	canceller->cancel_reply = false;
}

static void canceller_start(struct canceller *canceller)
{
	LINTED_ASSERT(canceller != 0);

	linted_error err;

	spinlock *lock = &canceller->lock;

	err = spinlock_lock(lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	LINTED_ASSERT(!canceller->in_flight);
	LINTED_ASSERT(!canceller->owned);

	canceller->cancelled = false;
	canceller->cancel_reply = false;
	canceller->in_flight = true;
	canceller->owned = false;

	err = spinlock_unlock(lock);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}
}

static void canceller_stop(struct canceller *canceller)
{
	LINTED_ASSERT(canceller != 0);

	linted_error err;

	spinlock *lock = &canceller->lock;

	err = spinlock_lock(lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	LINTED_ASSERT(canceller->owned);
	LINTED_ASSERT(canceller->in_flight);

	{
		bool cancelled = canceller->cancelled;
		if (cancelled)
			canceller->cancel_reply = true;
		canceller->cancelled = false;
	}

	canceller->in_flight = false;
	canceller->owned = false;

	err = spinlock_unlock(lock);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}
}

static void canceller_cancel(struct canceller *canceller)
{
	LINTED_ASSERT(canceller != 0);

	linted_error err;

	spinlock *lock = &canceller->lock;

	bool in_flight;

	{
		err = spinlock_lock(lock);
		if (err != 0) {
			LINTED_ASSERT(err != EDEADLK);
			LINTED_ASSERT(false);
		}

		LINTED_ASSERT(false == canceller->cancel_reply);

		in_flight = canceller->in_flight;
		if (in_flight) {
			canceller->cancel_reply = false;

			bool owned = canceller->owned;
			if (owned) {
				err = pthread_kill(canceller->owner,
				                   LINTED_ASYNCH_SIGNO);
				if (err != 0 && err != EAGAIN) {
					LINTED_ASSERT(err != ESRCH);
					LINTED_ASSERT(err != EINVAL);
					LINTED_ASSERT(false);
				}
			}
		}

		canceller->cancelled = true;

		err = spinlock_unlock(lock);
		if (err != 0) {
			LINTED_ASSERT(err != EPERM);
			LINTED_ASSERT(false);
		}
	}

	if (!in_flight)
		return;

	/* Yes, really, we do have to busy wait to prevent race
	 * conditions unfortunately */
	bool cancel_replied;
	do {
		sched_yield();

		err = spinlock_lock(lock);
		if (err != 0) {
			LINTED_ASSERT(err != EDEADLK);
			LINTED_ASSERT(false);
		}

		cancel_replied = canceller->cancel_reply;
		if (!cancel_replied) {
			bool owned = canceller->owned;
			if (owned) {
				err = pthread_kill(canceller->owner,
				                   LINTED_ASYNCH_SIGNO);
				if (err != 0 && err != EAGAIN) {
					LINTED_ASSERT(err != ESRCH);
					LINTED_ASSERT(err != EINVAL);
					LINTED_ASSERT(false);
				}
			}
		}

		err = spinlock_unlock(lock);
		if (err != 0) {
			LINTED_ASSERT(err != EPERM);
			LINTED_ASSERT(false);
		}
	} while (!cancel_replied);
}

static bool canceller_check_or_register(struct canceller *canceller,
                                        pthread_t self)
{
	LINTED_ASSERT(canceller != 0);

	linted_error err;

	spinlock *lock = &canceller->lock;

	err = spinlock_lock(lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	bool cancelled = canceller->cancelled;

	/* Don't actually complete the cancellation if cancelled and
	 * let the completion do that.
	 */
	canceller->owner = self;
	canceller->owned = true;

	err = spinlock_unlock(lock);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}

	return cancelled;
}

static bool canceller_check_and_unregister(struct canceller *canceller)
{
	linted_error err;

	err = spinlock_lock(&canceller->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EDEADLK);
		LINTED_ASSERT(false);
	}

	LINTED_ASSERT(canceller->in_flight);
	LINTED_ASSERT(canceller->owned);

	canceller->owned = false;

	bool cancelled = canceller->cancelled;
	if (cancelled)
		canceller->cancel_reply = true;

	err = spinlock_unlock(&canceller->lock);
	if (err != 0) {
		LINTED_ASSERT(err != EPERM);
		LINTED_ASSERT(false);
	}

	return cancelled;
}

#if defined _POSIX_SPIN_LOCKS
static inline void spinlock_init(spinlock *lock)
{
	pthread_spin_init(lock, false);
}

static inline void spinlock_destroy(spinlock *lock)
{
	pthread_spin_destroy(lock);
}

static inline linted_error spinlock_lock(spinlock *lock)
{
	return pthread_spin_lock(lock);
}

static inline linted_error spinlock_unlock(spinlock *lock)
{
	return pthread_spin_unlock(lock);
}
#else
static inline void spinlock_init(spinlock *lock)
{
	pthread_mutex_init(lock, 0);
}

static inline void spinlock_destroy(spinlock *lock)
{
	pthread_mutex_destroy(lock);
}

static inline linted_error spinlock_lock(spinlock *lock)
{
	return pthread_mutex_lock(lock);
}

static inline linted_error spinlock_unlock(spinlock *lock)
{
	return pthread_mutex_unlock(lock);
}
#endif

static size_t small_stack_size(void)
{
	/*
	 * Our tasks are only I/O tasks and have extremely tiny stacks.
	 */
	long maybe_page_size = sysconf(_SC_PAGE_SIZE);
	LINTED_ASSERT(maybe_page_size >= 0);

	long maybe_stack_min_size = sysconf(_SC_THREAD_STACK_MIN);
	LINTED_ASSERT(maybe_stack_min_size >= 0);

	size_t page_size = maybe_page_size;
	size_t stack_min_size = maybe_stack_min_size;

	return stack_min_size + page_size;
}

#if defined HAVE_PTHREAD_SETNAME_NP
static void set_thread_name(char const *name)
{
	pthread_setname_np(pthread_self(), name);
}
#elif defined HAVE_SYS_PRCTL_H
static void set_thread_name(char const *name)
{
	prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL);
}
#else
static void set_thread_name(char const *name)
{
	/* Do nothing */
}
#endif
