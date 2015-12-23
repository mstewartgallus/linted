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

#include "lntd/async.h"

#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/channel.h"
#include "lntd/ko-stack.h"
#include "lntd/node.h"
#include "lntd/proc.h"
#include "lntd/sched.h"
#include "lntd/signal.h"
#include "lntd/stack.h"
#include "lntd/util.h"

#include <errno.h>
#include <poll.h>
#include <pthread.h>
#include <sched.h>
#include <stddef.h>
#include <stdint.h>
#include <signal.h>
#include <stdbool.h>
#include <sys/epoll.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <unistd.h>

#define UPCAST(X) ((void *)(X))
#define DOWNCAST(T, X) ((T *)(X))

#if defined _POSIX_SPIN_LOCKS
typedef pthread_spinlock_t spinlock;
#else
typedef pthread_mutex_t spinlock;
#endif

static inline void spinlock_init(spinlock *lock);
static inline lntd_error spinlock_lock(spinlock *lock);
static inline lntd_error spinlock_unlock(spinlock *lock);

static size_t small_stack_size(void);

/**
 * A one reader to many writers queue. Should be able to retrieve
 * many values at once. As all writes are a direct result of
 * submitted commands there is no need to worry about it growing
 * too large.
 */
struct completion_queue;
static lntd_error
completion_queue_create(struct completion_queue **queuep);
static void complete_task(struct completion_queue *queue,
                          struct lntd_async_task *task);
static lntd_error completion_recv(struct completion_queue *queue,
                                  struct lntd_async_task **taskp);
static lntd_error completion_try_recv(struct completion_queue *queue,
                                      struct lntd_async_task **taskp);
static void completion_queue_destroy(struct completion_queue *queue);

struct job_queue;
static lntd_error job_queue_create(struct job_queue **queuep);
static void job_submit(struct job_queue *queue,
                       struct lntd_async_task *task);
static lntd_error job_recv(struct job_queue *queue,
                           struct lntd_async_task **taskp);
static void job_queue_destroy(struct job_queue *queue);

struct worker_queue;
static lntd_error worker_queue_create(struct worker_queue **queuep);
static lntd_error worker_try_submit(struct worker_queue *queue,
                                    struct lntd_async_task *task);
static lntd_error worker_recv(struct worker_queue *queue,
                              struct lntd_async_task **taskp);
static void worker_queue_destroy(struct worker_queue *queue);

struct waiter_queue;
static lntd_error waiter_queue_create(struct waiter_queue **queuep);
static void waiter_queue_destroy(struct waiter_queue *queue);
static lntd_ko waiter_ko(struct waiter_queue *queue);
static void waiter_submit(struct waiter_queue *queue,
                          struct lntd_async_waiter *waiter);
static lntd_error waiter_try_recv(struct waiter_queue *queue,
                                  struct lntd_async_waiter **waiterp);

struct worker_pool;
static lntd_error worker_pool_create(struct worker_pool **poolp,
                                     struct job_queue *job_queue,
                                     struct lntd_async_pool *pool,
                                     unsigned max_tasks);
static void worker_pool_destroy(struct worker_pool *pool);

struct wait_manager;
static lntd_error wait_manager_create(struct wait_manager **managerp,
                                      struct waiter_queue *waiter_queue,
                                      struct lntd_async_pool *pool,
                                      unsigned max_pollers);
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

struct lntd_async_pool {
	struct wait_manager *wait_manager;
	struct worker_pool *worker_pool;

	struct job_queue *job_queue;
	struct waiter_queue *waiter_queue;
	struct completion_queue *completion_queue;
};

struct lntd_async_task {
	struct lntd_node parent;
	struct canceller canceller;
	lntd_error err;
	bool thread_canceller : 1U;

	char __padding1[64U -
	                (sizeof(struct lntd_node) +
	                 sizeof(struct canceller) + sizeof(lntd_error) +
	                 sizeof(bool)) %
	                    64U];

	/* Stuff below should only be written by one thread on the fast
	 * path */
	void *data;
	void *userstate;
	union lntd_async_ck task_ck;
	lntd_async_type type;
};

struct lntd_async_waiter {
	struct lntd_node parent;
	short revents;
	bool thread_canceller : 1U;

	char
	    __padding1[64U -
	               (sizeof(struct lntd_node) + sizeof(bool)) % 64U];

	/* Stuff below should only be written by one thread on the fast
	 * path */
	struct lntd_async_task *task;
	lntd_ko ko;
	short flags;
};

lntd_error lntd_async_pool_create(struct lntd_async_pool **poolp,
                                  unsigned max_tasks)
{
	LNTD_ASSERT_NOT_NULL(poolp);

	lntd_error err;

	struct lntd_async_pool *pool;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *pool);
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
	lntd_mem_free(pool);

	LNTD_ASSERT(err != 0);

	return err;
}

lntd_error lntd_async_pool_destroy(struct lntd_async_pool *pool)
{
	LNTD_ASSERT_NOT_NULL(pool);

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

	lntd_mem_free(pool);

	return 0;
}

void lntd_async_pool_submit(struct lntd_async_pool *pool,
                            struct lntd_async_task *task)
{
	LNTD_ASSERT_NOT_NULL(pool);
	LNTD_ASSERT_NOT_NULL(task);

	canceller_start(&task->canceller);

	job_submit(pool->job_queue, task);
}

void lntd_async_pool_resubmit(struct lntd_async_pool *pool,
                              struct lntd_async_task *task)
{
	LNTD_ASSERT_NOT_NULL(pool);
	LNTD_ASSERT_NOT_NULL(task);

	if (canceller_check_and_unregister(&task->canceller)) {
		task->err = LNTD_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	job_submit(pool->job_queue, task);
}

void lntd_async_pool_complete(struct lntd_async_pool *pool,
                              struct lntd_async_task *task,
                              lntd_error task_err)
{
	LNTD_ASSERT_NOT_NULL(pool);
	LNTD_ASSERT_NOT_NULL(task);

	canceller_stop(&task->canceller);

	task->err = task_err;
	complete_task(pool->completion_queue, task);
}

void lntd_async_pool_wait_on_poll(struct lntd_async_pool *pool,
                                  struct lntd_async_waiter *waiter,
                                  struct lntd_async_task *task,
                                  lntd_ko ko, short flags)
{
	LNTD_ASSERT_NOT_NULL(pool);
	LNTD_ASSERT_NOT_NULL(waiter);
	LNTD_ASSERT_NOT_NULL(task);

	if (canceller_check_and_unregister(&task->canceller)) {
		task->err = LNTD_ERROR_CANCELLED;
		complete_task(pool->completion_queue, task);
		return;
	}

	waiter->task = task;
	waiter->ko = ko;
	waiter->flags = flags;

	waiter_submit(pool->waiter_queue, waiter);
}

lntd_error lntd_async_pool_wait(struct lntd_async_pool *pool,
                                struct lntd_async_result *resultp)
{
	lntd_error err = 0;

	struct completion_queue *queue = pool->completion_queue;

	struct lntd_async_task *task;
	{
		struct lntd_async_task *xx;
		err = completion_recv(queue, &xx);
		if (err != 0)
			return err;
		task = xx;
	}
	resultp->task_ck = task->task_ck;
	resultp->err = task->err;
	resultp->userstate = task->userstate;
	return 0;
}

lntd_error lntd_async_pool_poll(struct lntd_async_pool *pool,
                                struct lntd_async_result *resultp)
{
	LNTD_ASSERT_NOT_NULL(pool);

	lntd_error err = 0;

	struct completion_queue *queue = pool->completion_queue;

	struct lntd_async_task *task;
	{
		struct lntd_async_task *xx;
		err = completion_try_recv(queue, &xx);
		if (err != 0)
			return err;
		task = xx;
	}
	resultp->task_ck = task->task_ck;
	resultp->err = task->err;
	resultp->userstate = task->userstate;
	return 0;
}

lntd_error lntd_async_waiter_create(struct lntd_async_waiter **waiterp)
{
	LNTD_ASSERT_NOT_NULL(waiterp);

	lntd_error err;
	struct lntd_async_waiter *waiter;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *waiter);
		if (err != 0)
			return err;
		waiter = xx;
	}

	waiter->revents = 0;
	waiter->thread_canceller = false;

	*waiterp = waiter;
	return 0;
}

void lntd_async_waiter_destroy(struct lntd_async_waiter *waiter)
{
	lntd_mem_free(waiter);
}

short lntd_async_waiter_revents(struct lntd_async_waiter *waiter)
{
	LNTD_ASSERT_NOT_NULL(waiter);

	short ev = waiter->revents;
	waiter->revents = 0;
	return ev;
}

lntd_error lntd_async_task_create(struct lntd_async_task **taskp,
                                  void *data, lntd_async_type type)
{
	LNTD_ASSERT_NOT_NULL(taskp);

	lntd_error err;
	struct lntd_async_task *task;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}

	canceller_init(&task->canceller);

	task->data = data;
	task->type = type;
	task->err = LNTD_ERROR_INVALID_PARAMETER;

	task->thread_canceller = false;

	*taskp = task;
	return 0;
}

void lntd_async_task_destroy(struct lntd_async_task *task)
{
	lntd_mem_free(task);
}

void lntd_async_task_cancel(struct lntd_async_task *task)
{
	LNTD_ASSERT_NOT_NULL(task);
	canceller_cancel(&task->canceller);
}

struct lntd_async_task *
lntd_async_task_prepare(struct lntd_async_task *task,
                        union lntd_async_ck task_ck, void *userstate)
{
	LNTD_ASSERT_NOT_NULL(task);
	task->task_ck = task_ck;
	task->userstate = userstate;
	return task;
}

void *lntd_async_task_data(struct lntd_async_task *task)
{
	LNTD_ASSERT_NOT_NULL(task);
	return task->data;
}

static void *master_worker_routine(void *arg);
static void *worker_routine(void *arg);

static void run_task(struct lntd_async_pool *pool,
                     struct lntd_async_task *task);

struct worker_pool;

struct worker {
	struct lntd_async_pool *pool;
	struct worker_queue *queue;
	pthread_t thread;
};

struct worker_pool {
	struct lntd_async_pool *async_pool;
	struct job_queue *job_queue;
	struct worker_queue **worker_queues;

	size_t worker_stacks_size;
	void *worker_stacks;

	size_t worker_count;

	pthread_t master_thread;

	struct worker workers[];
};

static lntd_error worker_pool_create(struct worker_pool **poolp,
                                     struct job_queue *job_queue,
                                     struct lntd_async_pool *async_pool,
                                     unsigned max_tasks)
{
	LNTD_ASSERT_NOT_NULL(poolp);
	LNTD_ASSERT_NOT_NULL(job_queue);
	LNTD_ASSERT_NOT_NULL(async_pool);

	lntd_error err = 0;

	size_t workers_count = max_tasks;

	struct worker_pool *pool;
	size_t workers_size = workers_count * sizeof pool->workers[0U];
	size_t worker_pool_size = sizeof *pool + workers_size;
	LNTD_ASSERT(worker_pool_size > 0U);
	{
		void *xx;
		err = lntd_mem_alloc(&xx, worker_pool_size);
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
	LNTD_ASSERT(maybe_page_size >= 0);
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
		LNTD_ASSUME(err != 0);
		goto destroy_worker_queues;
	}

	/* Guard pages are shared between the stacks */
	if (-1 ==
	    mprotect((char *)worker_stacks, page_size, PROT_NONE)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		goto destroy_stacks;
	}

	char *tail_guard_page =
	    (char *)worker_stacks + page_size + stack_size;
	for (size_t ii = 0U; ii < workers_count; ++ii) {
		if (-1 ==
		    mprotect(tail_guard_page, page_size, PROT_NONE)) {
			err = errno;
			LNTD_ASSUME(err != 0);
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
				LNTD_ASSERT(
				    err !=
				    LNTD_ERROR_INVALID_PARAMETER);
				LNTD_ASSERT(false);
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

			lntd_error mask_err =
			    pthread_sigmask(SIG_SETMASK, &oldset, 0);
			if (0 == err)
				err = mask_err;

			if (err != 0)
				break;
		}

		lntd_error destroy_err = pthread_attr_destroy(&attr);
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
			struct lntd_async_task task;
			task.thread_canceller = true;

			lntd_error try_err =
			    worker_try_submit(worker_queue, &task);
			if (0 == try_err) {
				pthread_join(thread, 0);
				break;
			}

			lntd_error kill_err =
			    pthread_kill(thread, LNTD_ASYNCH_SIGNO);
			if (kill_err != 0 && kill_err != EAGAIN) {
				LNTD_ASSERT(kill_err != ESRCH);
				LNTD_ASSERT(
				    kill_err !=
				    LNTD_ERROR_INVALID_PARAMETER);
				LNTD_ASSERT(false);
			}

			sched_yield();
		}
	}

destroy_stacks:
	munmap(worker_stacks, worker_stacks_size);

destroy_worker_queues:
	for (size_t ii = 0U; ii < queues_created; ++ii)
		worker_queue_destroy(pool->workers[ii].queue);

	lntd_mem_free(pool);

	return err;
}

static void worker_pool_destroy(struct worker_pool *pool)
{
	LNTD_ASSERT_NOT_NULL(pool);

	struct job_queue *job_queue = pool->job_queue;
	pthread_t master_thread = pool->master_thread;
	struct worker const *workers = pool->workers;
	size_t worker_count = pool->worker_count;

	{
		struct lntd_async_task task;
		task.thread_canceller = true;

		job_submit(job_queue, &task);

		pthread_join(master_thread, 0);
	}

	munmap(pool->worker_stacks, pool->worker_stacks_size);

	for (size_t ii = 0U; ii < worker_count; ++ii)
		worker_queue_destroy(workers[ii].queue);

	lntd_mem_free(pool);
}

static void *master_worker_routine(void *arg)
{
	LNTD_ASSERT_NOT_NULL(arg);

	lntd_proc_name("async-worker-master");

	struct worker_pool *pool = arg;

	struct job_queue *job_queue = pool->job_queue;
	struct worker const *workers = pool->workers;
	size_t max_tasks = pool->worker_count;

	for (;;) {
		struct lntd_async_task *task;
		{
			struct lntd_async_task *xx;
			job_recv(job_queue, &xx);
			task = xx;
		}

		if (task->thread_canceller)
			break;

		for (;;) {
			for (size_t ii = 0U; ii < max_tasks; ++ii) {
				lntd_error err = worker_try_submit(
				    workers[ii].queue, task);
				if (LNTD_ERROR_AGAIN == err)
					continue;
				if (0 == err)
					goto exit_loop;
			}
			sched_yield();
		}
	exit_loop:
		;
	}

	for (size_t ii = 0U; ii < max_tasks; ++ii) {
		struct worker *worker = &pool->workers[ii];
		struct worker_queue *worker_queue = worker->queue;
		pthread_t thread = worker->thread;
		for (;;) {
			struct lntd_async_task task;
			task.thread_canceller = true;

			lntd_error err =
			    worker_try_submit(worker_queue, &task);
			if (0 == err) {
				pthread_join(thread, 0);
				break;
			}

			err = pthread_kill(thread, LNTD_ASYNCH_SIGNO);
			if (err != 0 && err != EAGAIN) {
				LNTD_ASSERT(err != ESRCH);
				LNTD_ASSERT(
				    err !=
				    LNTD_ERROR_INVALID_PARAMETER);
				LNTD_ASSERT(false);
			}

			sched_yield();
		}
	}

	return 0;
}

static void *worker_routine(void *arg)
{
	LNTD_ASSERT_NOT_NULL(arg);

	lntd_proc_name("async-worker");

	struct worker *worker = arg;

	struct lntd_async_pool *async_pool = worker->pool;
	struct worker_queue *worker_queue = worker->queue;

	pthread_t self = pthread_self();

	for (;;) {
		struct lntd_async_task *task;
		{
			struct lntd_async_task *xx;
			worker_recv(worker_queue, &xx);
			task = xx;
		}

		if (task->thread_canceller)
			break;

		if (canceller_check_or_register(&task->canceller,
		                                self)) {
			lntd_async_pool_complete(async_pool, task,
			                         LNTD_ERROR_CANCELLED);
			continue;
		}

		run_task(async_pool, task);
	}

	return 0;
}

#pragma weak lntd_sched_do_idle
#pragma weak lntd_io_do_poll
#pragma weak lntd_io_do_read
#pragma weak lntd_io_do_write
#pragma weak lntd_signal_do_wait
#pragma weak lntd_sched_do_sleep_until

static void run_task(struct lntd_async_pool *pool,
                     struct lntd_async_task *task)
{
	switch (task->type) {
	case LNTD_ASYNCH_TASK_IDLE:
		lntd_sched_do_idle(pool, task);
		return;

	case LNTD_ASYNCH_TASK_POLL:
		lntd_io_do_poll(pool, task);
		return;

	case LNTD_ASYNCH_TASK_READ:
		lntd_io_do_read(pool, task);
		return;

	case LNTD_ASYNCH_TASK_WRITE:
		lntd_io_do_write(pool, task);
		return;

	case LNTD_ASYNCH_TASK_SIGNAL_WAIT:
		lntd_signal_do_wait(pool, task);
		return;

	case LNTD_ASYNCH_TASK_SLEEP_UNTIL:
		lntd_sched_do_sleep_until(pool, task);
		return;

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

struct wait_manager {
	struct lntd_async_pool *async_pool;
	struct waiter_queue *waiter_queue;

	size_t poller_count;

	pthread_t master_thread;

	struct epoll_event *epoll_events;
	struct lntd_async_waiter **waiters;

	lntd_ko epoll_ko;

	bool stopped : 1U;
};

static void *master_poller_routine(void *arg);
static lntd_error do_task_for_event(struct lntd_async_pool *async_pool,
                                    struct lntd_async_waiter *waiter,
                                    uint32_t revents);
static lntd_error recv_waiters(struct lntd_async_pool *async_pool,
                               pthread_t self, lntd_ko epoll_ko,
                               struct lntd_async_waiter **waiters,
                               struct waiter_queue *waiter_queue,
                               size_t max_tasks, uint32_t revents);

static lntd_error wait_manager_create(
    struct wait_manager **managerp, struct waiter_queue *waiter_queue,
    struct lntd_async_pool *async_pool, unsigned max_pollers)
{
	LNTD_ASSERT_NOT_NULL(managerp);
	LNTD_ASSERT_NOT_NULL(waiter_queue);
	LNTD_ASSERT_NOT_NULL(async_pool);

	lntd_error err;

	struct wait_manager *manager;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *manager);
		if (err != 0)
			return err;
		manager = xx;
	}

	struct lntd_async_waiter **waiters;
	{
		void *xx;
		err = lntd_mem_alloc_array(&xx, max_pollers,
		                           sizeof waiters[0U]);
		if (err != 0)
			goto free_manager;
		waiters = xx;
	}

	struct epoll_event *epoll_events;
	{
		void *xx;
		err = lntd_mem_alloc_array(&xx, max_pollers + 1U,
		                           sizeof epoll_events[0U]);
		if (err != 0)
			goto free_waiters;
		epoll_events = xx;
	}

	int epoll_fd = epoll_create1(EPOLL_CLOEXEC);
	if (-1 == epoll_fd) {
		err = errno;
		LNTD_ASSUME(err != 0);
		goto free_events;
	}

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
	lntd_ko_close(epoll_fd);

free_events:
	lntd_mem_free(epoll_events);

free_waiters:
	lntd_mem_free(waiters);

free_manager:
	lntd_mem_free(manager);

	return err;
}

static void wait_manager_destroy(struct wait_manager *manager)
{
	LNTD_ASSERT_NOT_NULL(manager);

	struct waiter_queue *waiter_queue = manager->waiter_queue;
	pthread_t master_thread = manager->master_thread;
	lntd_ko epoll_ko = manager->epoll_ko;
	struct epoll_event *epoll_events = manager->epoll_events;
	struct lntd_async_waiter **waiters = manager->waiters;

	{
		struct lntd_async_waiter waiter;
		waiter.thread_canceller = true;

		waiter_submit(waiter_queue, &waiter);
		pthread_join(master_thread, 0);
	}

	lntd_ko_close(epoll_ko);

	lntd_mem_free(epoll_events);
	lntd_mem_free(waiters);

	lntd_mem_free(manager);
}

static void *master_poller_routine(void *arg)
{
	LNTD_ASSERT_NOT_NULL(arg);

	pthread_t self = pthread_self();

	lntd_proc_name("async-poller");

	struct wait_manager *pool = arg;

	struct waiter_queue *waiter_queue = pool->waiter_queue;
	struct lntd_async_waiter **waiters = pool->waiters;
	lntd_ko epoll_ko = pool->epoll_ko;
	struct epoll_event *epoll_events = pool->epoll_events;
	size_t max_tasks = pool->poller_count;
	struct lntd_async_pool *async_pool = pool->async_pool;

	for (size_t ii = 0U; ii < max_tasks; ++ii)
		waiters[ii] = 0;

	lntd_ko wait_ko = waiter_ko(waiter_queue);

	lntd_error err = 0;
	{
		struct epoll_event xx = {0};
		xx.events = EPOLLIN;
		xx.data.u64 = 0U;
		if (-1 ==
		    epoll_ctl(epoll_ko, EPOLL_CTL_ADD, wait_ko, &xx)) {
			err = errno;
			LNTD_ASSUME(err != 0);
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
				LNTD_ASSUME(err != 0);

				if (EINTR == err)
					goto check_cancellers;
				if (EAGAIN == err)
					continue;
				if (ENOMEM == err)
					continue;

				LNTD_ASSERT(err != EFAULT);
				LNTD_ASSERT(
				    err !=
				    LNTD_ERROR_INVALID_PARAMETER);

				LNTD_ASSERT(0);
			}
			break;
		}
		for (size_t ii = 0U; ii < (unsigned)numpolled; ++ii) {
			size_t index = epoll_events[ii].data.u64;
			uint32_t revents = epoll_events[ii].events;

			if (0U == index) {
				err = recv_waiters(
				    async_pool, self, epoll_ko, waiters,
				    waiter_queue, max_tasks, revents);
				if (ECANCELED == err) {
					goto exit_mainloop;
				}
				continue;
			}

			struct lntd_async_waiter **waiterp =
			    &waiters[index - 1U];
			struct lntd_async_waiter *waiter = *waiterp;

			LNTD_ASSERT(waiter != 0);

			err = do_task_for_event(async_pool, waiter,
			                        revents);
			if (err != 0)
				goto exit_mainloop;

			int fd = waiter->ko;
			if (-1 ==
			    epoll_ctl(epoll_ko, EPOLL_CTL_DEL, fd, 0)) {
				err = errno;
				LNTD_ASSUME(err != 0);
				goto exit_mainloop;
			}

			*waiterp = 0;
		}

	check_cancellers:
		for (size_t ii = 0U; ii < max_tasks; ++ii) {
			struct lntd_async_waiter *waiter = waiters[ii];
			if (0 == waiter)
				continue;

			struct lntd_async_task *task = waiter->task;

			if (!canceller_check_or_register(
			        &task->canceller, self))
				continue;

			if (-1 == epoll_ctl(epoll_ko, EPOLL_CTL_DEL,
			                    waiter->ko, 0)) {
				LNTD_ASSERT(0);
			}
			waiters[ii] = 0;

			lntd_async_pool_complete(async_pool, task,
			                         LNTD_ERROR_CANCELLED);
		}
	}
exit_mainloop:
	return 0;
}

static lntd_error recv_waiters(struct lntd_async_pool *async_pool,
                               pthread_t self, lntd_ko epoll_ko,
                               struct lntd_async_waiter **waiters,
                               struct waiter_queue *waiter_queue,
                               size_t max_tasks, uint32_t revents)
{
	lntd_error err = 0;

	bool has_pollerr = (revents & EPOLLERR) != 0;
	bool has_pollhup = (revents & EPOLLHUP) != 0;
	bool has_pollin = (revents & EPOLLIN) != 0;

	LNTD_ASSERT(!has_pollerr);
	LNTD_ASSERT(!has_pollhup);

	LNTD_ASSERT(has_pollin);

	lntd_ko wait_ko = waiter_ko(waiter_queue);
	for (;;) {
		uint64_t xx;
		if (-1 == read(wait_ko, &xx, sizeof xx)) {
			err = errno;
			LNTD_ASSERT(err != 0);
			if (EINTR == err)
				continue;
			if (EAGAIN == err)
				break;
			LNTD_ASSERT(0);
		}
		break;
	}

	for (;;) {
		struct lntd_async_waiter *waiter;
		{
			struct lntd_async_waiter *xx;
			err = waiter_try_recv(waiter_queue, &xx);
			if (EAGAIN == err)
				break;
			if (err != 0)
				return err;
			waiter = xx;
		}

		if (waiter->thread_canceller)
			return ECANCELED;

		lntd_ko ko = waiter->ko;
		struct lntd_async_task *task = waiter->task;
		uint32_t flags = waiter->flags;

		if (canceller_check_or_register(&task->canceller,
		                                self)) {
			err = LNTD_ERROR_CANCELLED;
			goto complete_task;
		}

		size_t jj = 0U;
		for (; jj < max_tasks; ++jj) {
			if (0 == waiters[jj])
				goto finish;
		}
		LNTD_ASSERT(0);

	finish:
		waiters[jj] = waiter;

		{
			struct epoll_event xx = {0};
			xx.events = flags | EPOLLONESHOT | EPOLLET;
			xx.data.u64 = 1U + jj;
			if (-1 == epoll_ctl(epoll_ko, EPOLL_CTL_ADD, ko,
			                    &xx)) {
				err = errno;
				LNTD_ASSUME(err != 0);
				return err;
			}
		}
		continue;

	complete_task:
		lntd_async_pool_complete(async_pool, task, err);
		continue;
	}

	return 0;
}

static lntd_error do_task_for_event(struct lntd_async_pool *async_pool,
                                    struct lntd_async_waiter *waiter,
                                    uint32_t revents)
{
	struct lntd_async_task *task = waiter->task;
	lntd_ko ko = waiter->ko;

	bool has_pollerr = (revents & POLLERR) != 0;
	bool has_pollnval = (revents & POLLNVAL) != 0;

	lntd_error err = 0;

	if (has_pollnval) {
		err = LNTD_ERROR_INVALID_KO;
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
	lntd_async_pool_resubmit(async_pool, task);
	return 0;

complete_task:
	lntd_async_pool_complete(async_pool, task, err);
	return 0;
}

/* struct complete_queue is just a fake */
static lntd_error
completion_queue_create(struct completion_queue **queuep)
{
	LNTD_ASSERT_NOT_NULL(queuep);

	lntd_error err;

	struct lntd_stack *queue;
	{
		struct lntd_stack *xx;
		err = lntd_stack_create(&xx);
		if (err != 0)
			return err;
		queue = xx;
	}

	*queuep = (struct completion_queue *)queue;
	return 0;
}

static void complete_task(struct completion_queue *queue,
                          struct lntd_async_task *task)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(task);

	lntd_stack_send((void *)queue, UPCAST(task));
}

static lntd_error completion_recv(struct completion_queue *queue,
                                  struct lntd_async_task **taskp)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(taskp);

	struct lntd_node *node;
	{
		struct lntd_node *xx;
		lntd_stack_recv((void *)queue, &xx);
		node = xx;
	}

	*taskp = DOWNCAST(struct lntd_async_task, node);

	return 0;
}

static lntd_error completion_try_recv(struct completion_queue *queue,
                                      struct lntd_async_task **taskp)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(taskp);

	lntd_error err;

	struct lntd_node *node;
	{
		struct lntd_node *xx;
		err = lntd_stack_try_recv((void *)queue, &xx);
		if (err != 0)
			return err;
		node = xx;
	}

	*taskp = DOWNCAST(struct lntd_async_task, node);

	return 0;
}

static void completion_queue_destroy(struct completion_queue *queue)
{
	lntd_stack_destroy((void *)queue);
}

/* struct job_queue is just a fake */
static lntd_error job_queue_create(struct job_queue **queuep)
{
	LNTD_ASSERT_NOT_NULL(queuep);

	lntd_error err;

	struct lntd_stack *stack;
	{
		struct lntd_stack *xx;
		err = lntd_stack_create(&xx);
		if (err != 0)
			return err;
		stack = xx;
	}

	*queuep = (struct job_queue *)stack;
	return 0;
}

static void job_submit(struct job_queue *queue,
                       struct lntd_async_task *task)
{
	lntd_stack_send((void *)queue, UPCAST(task));
}

static lntd_error job_recv(struct job_queue *queue,
                           struct lntd_async_task **taskp)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(taskp);

	struct lntd_node *node;
	{
		struct lntd_node *xx;
		lntd_stack_recv((void *)queue, &xx);
		node = xx;
	}

	*taskp = DOWNCAST(struct lntd_async_task, node);

	return 0;
}

static void job_queue_destroy(struct job_queue *queue)
{
	lntd_stack_destroy((void *)queue);
}

/* struct worker_queue is just a fake */
static lntd_error worker_queue_create(struct worker_queue **queuep)
{
	LNTD_ASSERT_NOT_NULL(queuep);

	lntd_error err;

	struct lntd_channel *channel;
	{
		struct lntd_channel *xx;
		err = lntd_channel_create(&xx);
		if (err != 0)
			return err;
		channel = xx;
	}
	*queuep = (struct worker_queue *)channel;
	return 0;
}

static lntd_error worker_try_submit(struct worker_queue *queue,
                                    struct lntd_async_task *task)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(task);
	return lntd_channel_try_send((struct lntd_channel *)queue,
	                             task);
}

static lntd_error worker_recv(struct worker_queue *queue,
                              struct lntd_async_task **taskp)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(taskp);

	struct lntd_async_task *task;
	{
		void *xx;
		lntd_channel_recv((struct lntd_channel *)queue, &xx);
		task = xx;
	}

	LNTD_ASSERT_NOT_NULL(task);
	*taskp = task;

	return 0;
}

static void worker_queue_destroy(struct worker_queue *queue)
{
	lntd_channel_destroy((struct lntd_channel *)queue);
}

static lntd_error waiter_queue_create(struct waiter_queue **queuep)
{
	LNTD_ASSERT_NOT_NULL(queuep);

	lntd_error err = 0;

	struct waiter_queue *queue;
	{
		struct lntd_ko_stack *xx;
		err = lntd_ko_stack_create(&xx);
		if (err != 0)
			return err;
		queue = (void *)xx;
	}
	*queuep = queue;
	return 0;
}

static void waiter_queue_destroy(struct waiter_queue *queue)
{
	lntd_ko_stack_destroy((void *)queue);
}

static lntd_ko waiter_ko(struct waiter_queue *queue)
{
	return lntd_ko_stack_ko((void *)queue);
}

static void waiter_submit(struct waiter_queue *queue,
                          struct lntd_async_waiter *waiter)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(waiter);

	lntd_ko_stack_send((void *)queue, UPCAST(waiter));
}

static lntd_error waiter_try_recv(struct waiter_queue *queue,
                                  struct lntd_async_waiter **waiterp)
{
	LNTD_ASSERT_NOT_NULL(queue);
	LNTD_ASSERT_NOT_NULL(waiterp);

	lntd_error err = 0;

	struct lntd_async_waiter *waiter;
	{
		struct lntd_node *node;
		err = lntd_ko_stack_try_recv((void *)queue, &node);
		if (err != 0)
			return err;
		waiter = (void *)node;
	}

	*waiterp = waiter;

	return 0;
}

static void canceller_init(struct canceller *canceller)
{
	LNTD_ASSERT_NOT_NULL(canceller);

	spinlock_init(&canceller->lock);

	canceller->in_flight = false;

	canceller->owned = false;
	canceller->cancelled = false;
	canceller->cancel_reply = false;
}

static void canceller_start(struct canceller *canceller)
{
	LNTD_ASSERT_NOT_NULL(canceller);

	lntd_error err;

	spinlock *lock = &canceller->lock;

	err = spinlock_lock(lock);
	if (err != 0) {
		LNTD_ASSERT(err != EDEADLK);
		LNTD_ASSERT(false);
	}

	LNTD_ASSERT(!canceller->in_flight);
	LNTD_ASSERT(!canceller->owned);

	canceller->cancelled = false;
	canceller->cancel_reply = false;
	canceller->in_flight = true;
	canceller->owned = false;

	err = spinlock_unlock(lock);
	if (err != 0) {
		LNTD_ASSERT(err != EPERM);
		LNTD_ASSERT(false);
	}
}

static void canceller_stop(struct canceller *canceller)
{
	LNTD_ASSERT_NOT_NULL(canceller);

	lntd_error err;

	spinlock *lock = &canceller->lock;

	err = spinlock_lock(lock);
	if (err != 0) {
		LNTD_ASSERT(err != EDEADLK);
		LNTD_ASSERT(false);
	}

	LNTD_ASSERT(canceller->owned);
	LNTD_ASSERT(canceller->in_flight);

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
		LNTD_ASSERT(err != EPERM);
		LNTD_ASSERT(false);
	}
}

static void canceller_cancel(struct canceller *canceller)
{
	LNTD_ASSERT_NOT_NULL(canceller);

	lntd_error err;

	spinlock *lock = &canceller->lock;

	bool in_flight;

	{
		err = spinlock_lock(lock);
		if (err != 0) {
			LNTD_ASSERT(err != EDEADLK);
			LNTD_ASSERT(false);
		}

		LNTD_ASSERT(false == canceller->cancel_reply);

		in_flight = canceller->in_flight;
		if (in_flight) {
			canceller->cancel_reply = false;

			bool owned = canceller->owned;
			if (owned) {
				err = pthread_kill(canceller->owner,
				                   LNTD_ASYNCH_SIGNO);
				if (err != 0 && err != EAGAIN) {
					LNTD_ASSERT(err != ESRCH);
					LNTD_ASSERT(
					    err !=
					    LNTD_ERROR_INVALID_PARAMETER);
					LNTD_ASSERT(false);
				}
			}
		}

		canceller->cancelled = true;

		err = spinlock_unlock(lock);
		if (err != 0) {
			LNTD_ASSERT(err != EPERM);
			LNTD_ASSERT(false);
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
			LNTD_ASSERT(err != EDEADLK);
			LNTD_ASSERT(false);
		}

		cancel_replied = canceller->cancel_reply;
		if (!cancel_replied) {
			bool owned = canceller->owned;
			if (owned) {
				err = pthread_kill(canceller->owner,
				                   LNTD_ASYNCH_SIGNO);
				if (err != 0 && err != EAGAIN) {
					LNTD_ASSERT(err != ESRCH);
					LNTD_ASSERT(
					    err !=
					    LNTD_ERROR_INVALID_PARAMETER);
					LNTD_ASSERT(false);
				}
			}
		}

		err = spinlock_unlock(lock);
		if (err != 0) {
			LNTD_ASSERT(err != EPERM);
			LNTD_ASSERT(false);
		}
	} while (!cancel_replied);
}

static bool canceller_check_or_register(struct canceller *canceller,
                                        pthread_t self)
{
	LNTD_ASSERT_NOT_NULL(canceller);

	lntd_error err;

	spinlock *lock = &canceller->lock;

	err = spinlock_lock(lock);
	if (err != 0) {
		LNTD_ASSERT(err != EDEADLK);
		LNTD_ASSERT(false);
	}

	bool cancelled = canceller->cancelled;

	/* Don't actually complete the cancellation if cancelled and
	 * let the completion do that.
	 */
	canceller->owner = self;
	canceller->owned = true;

	err = spinlock_unlock(lock);
	if (err != 0) {
		LNTD_ASSERT(err != EPERM);
		LNTD_ASSERT(false);
	}

	return cancelled;
}

static bool canceller_check_and_unregister(struct canceller *canceller)
{
	lntd_error err;

	err = spinlock_lock(&canceller->lock);
	if (err != 0) {
		LNTD_ASSERT(err != EDEADLK);
		LNTD_ASSERT(false);
	}

	LNTD_ASSERT(canceller->in_flight);
	LNTD_ASSERT(canceller->owned);

	canceller->owned = false;

	bool cancelled = canceller->cancelled;
	if (cancelled)
		canceller->cancel_reply = true;

	err = spinlock_unlock(&canceller->lock);
	if (err != 0) {
		LNTD_ASSERT(err != EPERM);
		LNTD_ASSERT(false);
	}

	return cancelled;
}

#if defined _POSIX_SPIN_LOCKS
static inline void spinlock_init(spinlock *lock)
{
	pthread_spin_init(lock, false);
}

static inline lntd_error spinlock_lock(spinlock *lock)
{
	return pthread_spin_lock(lock);
}

static inline lntd_error spinlock_unlock(spinlock *lock)
{
	return pthread_spin_unlock(lock);
}
#else
static inline void spinlock_init(spinlock *lock)
{
	pthread_mutex_init(lock, 0);
}

static inline lntd_error spinlock_lock(spinlock *lock)
{
	return pthread_mutex_lock(lock);
}

static inline lntd_error spinlock_unlock(spinlock *lock)
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
	LNTD_ASSERT(maybe_page_size >= 0);

	long maybe_stack_min_size = sysconf(_SC_THREAD_STACK_MIN);
	LNTD_ASSERT(maybe_stack_min_size >= 0);

	size_t page_size = maybe_page_size;
	size_t stack_min_size = maybe_stack_min_size;

	return stack_min_size + page_size;
}
