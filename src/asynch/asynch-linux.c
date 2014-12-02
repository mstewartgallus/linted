/*
 * Copyright 2014 Steven Stewart-Gallus
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
#define _GNU_SOURCE

#include "config.h"

#include "linted/asynch.h"

#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

struct linted_asynch_pool
{
	/**
	 * A one writer to many readers queue.
	 */
	struct linted_queue *worker_command_queue;

	/**
	 * A one reader to many writers queue. Should be able to retrieve
	 * many values at once. As all writes are a direct result of
	 * submitted commands there is no need to worry about it growing
	 * too large.
	 */
	struct linted_queue *event_queue;

	size_t worker_stacks_size;
	void *worker_stacks;

	size_t worker_count;

	bool stopped : 1U;

	pthread_t workers[];
};

struct linted_asynch_task_waitid
{
	struct linted_asynch_task *parent;
	void *data;
	siginfo_t info;
	idtype_t idtype;
	id_t id;
	int options;
};

struct linted_asynch_task_sigwaitinfo
{
	struct linted_asynch_task *parent;
	void *data;
	siginfo_t info;
	sigset_t set;
	int signo;
};

struct linted_asynch_task_sleep_until
{
	struct linted_asynch_task *parent;
	void *data;
	struct timespec request;
	int flags;
};

struct linted_asynch_task
{
	struct linted_queue_node parent;

	pthread_spinlock_t owner_lock;
	pthread_t owner;
	bool owned : 1U;
	bool *cancel_replier;

	void *data;
	linted_error errnum;
	unsigned type;
	unsigned task_action;
};

static void *worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task);

static void run_task_waitid(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *task);
static void run_task_sigwaitinfo(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);
static void run_task_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks)
{
	linted_error errnum;
	size_t created_threads = 0U;
	struct linted_asynch_pool *pool;

	size_t workers_size = max_tasks * sizeof pool->workers[0U];

	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *pool + workers_size);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	pool->stopped = false;

	errnum = linted_queue_create(&pool->worker_command_queue);
	if (errnum != 0)
		goto free_pool;

	errnum = linted_queue_create(&pool->event_queue);
	if (errnum != 0)
		goto destroy_worker_command_queue;

	pool->worker_count = max_tasks;

	/*
	 * Our tasks are only I/O tasks and have extremely tiny stacks.
	 */
	long page_size = sysconf(_SC_PAGE_SIZE);
	assert(page_size != -1);

	/* We need an extra page for signals */
	long stack_size = sysconf(_SC_THREAD_STACK_MIN) + page_size;
	assert(stack_size != -1);

	size_t stack_and_guard_size = stack_size + page_size;
	size_t worker_stacks_size =
	    page_size + stack_and_guard_size * max_tasks;
	void *worker_stacks = mmap(
	    NULL, worker_stacks_size, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK, -1, 0);
	if (NULL == worker_stacks) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto destroy_event_queue;
	}

	/* Guard pages are shared between the stacks */
	if (-1 == mprotect((char *)worker_stacks, page_size, PROT_NONE)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto destroy_stacks;
	}

	for (size_t ii = 0U; ii < max_tasks; ++ii) {
		if (-1 == mprotect((char *)worker_stacks + page_size +
		                       ii * stack_and_guard_size + stack_size,
		                   page_size, PROT_NONE)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto destroy_stacks;
		}
	}

	pool->worker_stacks = worker_stacks;
	pool->worker_stacks_size = worker_stacks_size;

	{
		pthread_attr_t attr;

		errnum = pthread_attr_init(&attr);
		if (errnum != 0)
			goto destroy_stacks;

		for (; created_threads < max_tasks; ++created_threads) {
			errnum = pthread_attr_setstack(
			    &attr, (char *)worker_stacks + page_size +
			               created_threads * stack_and_guard_size,
			    stack_size);
			if (errnum != 0) {
				assert(errnum != EINVAL);
				assert(false);
			}

			errnum = pthread_create(&pool->workers[created_threads],
			                        &attr, worker_routine, pool);
			if (errnum != 0)
				break;
		}

		linted_error destroy_errnum = pthread_attr_destroy(&attr);
		if (0 == errnum)
			errnum = destroy_errnum;
	}

	if (errnum != 0)
		goto destroy_threads;

	*poolp = pool;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < created_threads; ++ii)
		pthread_cancel(pool->workers[ii]);

	for (size_t ii = 0U; ii < created_threads; ++ii)
		pthread_join(pool->workers[ii], NULL);

destroy_stacks:
	munmap(worker_stacks, worker_stacks_size);

destroy_event_queue:
	linted_queue_destroy(pool->event_queue);

destroy_worker_command_queue:
	linted_queue_destroy(pool->worker_command_queue);

free_pool:
	linted_mem_free(pool);

	return errnum;
}

linted_error linted_asynch_pool_stop(struct linted_asynch_pool *pool)
{
	linted_error errnum = 0;

	if (pool->stopped)
		return EINVAL;

	pool->stopped = true;

	size_t worker_count = pool->worker_count;

	for (size_t ii = 0U; ii < worker_count; ++ii) {
		errnum = pthread_cancel(pool->workers[ii]);
		if (errnum != 0) {
			assert(errnum != ESRCH);
			assert(false);
		}
	}

	/* After this point a few workers can still be left hanging
	 * waiting to return completed tasks and users should clean
	 * those up by polling after here. */

	return errnum;
}

linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
	linted_error errnum = 0;

	if (!pool->stopped)
		return EBUSY;

	/* All workers should have had their results retrieved by
	 * now. */
	size_t worker_count = pool->worker_count;

	for (size_t ii = 0U; ii < worker_count; ++ii) {
		pthread_t worker = pool->workers[ii];

		void *retval;
		errnum = pthread_join(worker, &retval);
		if (errnum != 0) {
			assert(errnum != EDEADLK);
			assert(errnum != EINVAL);
			assert(errnum != ESRCH);
		}
		assert(PTHREAD_CANCELED == retval);
	}

	munmap(pool->worker_stacks, pool->worker_stacks_size);

	linted_queue_destroy(pool->worker_command_queue);

	linted_queue_destroy(pool->event_queue);

	linted_mem_free(pool);

	return errnum;
}

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task)
{
	bool cancelled;
	linted_error errnum;

	assert(pool != NULL);
	assert(!pool->stopped);

	errnum = pthread_spin_lock(&task->owner_lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	task->owned = false;
	{
		bool *cancel_replier = task->cancel_replier;
		cancelled = cancel_replier != NULL;
		if (cancelled)
			*cancel_replier = true;
		task->cancel_replier = NULL;
	}

	errnum = pthread_spin_unlock(&task->owner_lock);
	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}

	if (cancelled) {
		task->errnum = ECANCELED;
		linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
	} else {
		linted_queue_send(pool->worker_command_queue,
		                  LINTED_UPCAST(task));
	}
}

void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	linted_error errnum;

	assert(pool != NULL);

	/* Aren't a cancellation point so always works */

	errnum = pthread_spin_lock(&task->owner_lock);
	if (errnum != 0) {
		assert(errnum != EDEADLK);
		assert(false);
	}

	assert(task->owned);

	{
		bool *cancel_replier = task->cancel_replier;
		bool cancelled = cancel_replier != NULL;
		if (cancelled)
			*cancel_replier = true;
		task->cancel_replier = NULL;
	}

	task->owned = false;
	errnum = pthread_spin_unlock(&task->owner_lock);

	if (errnum != 0) {
		assert(errnum != EPERM);
		assert(false);
	}

	linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
	struct linted_queue_node *node;

	linted_queue_recv(pool->event_queue, &node);

	*completionp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0;
}

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
	linted_error errnum;
	struct linted_queue_node *node;

	errnum = linted_queue_try_recv(pool->event_queue, &node);
	if (errnum != 0)
		return errnum;

	*completionp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0;
}

linted_error linted_asynch_task_create(struct linted_asynch_task **taskp,
                                       void *data, unsigned type)
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

	errnum = pthread_spin_init(&task->owner_lock, false);
	if (errnum != 0)
		goto free_task;

	task->owned = false;
	task->cancel_replier = NULL;

	task->data = data;
	task->type = type;
	task->errnum = EINVAL;

	*taskp = task;
	return 0;

free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_asynch_task_destroy(struct linted_asynch_task *task)
{
	linted_error errnum;
	errnum = pthread_spin_destroy(&task->owner_lock);
	if (errnum != 0) {
		assert(errnum != EBUSY);
		assert(false);
	}
	linted_mem_free(task);
}

void linted_asynch_task_cancel(struct linted_asynch_task *task)
{
	linted_error errnum;

	bool cancel_reply = false;

	{
		errnum = pthread_spin_lock(&task->owner_lock);
		if (errnum != 0) {
			assert(errnum != EDEADLK);
			assert(false);
		}

		assert(NULL == task->cancel_replier);
		task->cancel_replier = &cancel_reply;

		bool owned = task->owned;
		if (owned) {
			errnum = pthread_kill(task->owner, SIGRTMIN);
			if (errnum != 0 && errnum != EAGAIN) {
				assert(errnum != ESRCH);
				assert(errnum != EINVAL);
				assert(false);
			}
		}

		errnum = pthread_spin_unlock(&task->owner_lock);
		if (errnum != 0) {
			assert(errnum != EPERM);
			assert(false);
		}
	}

	/* Yes, really, we do have to busy wait to prevent race
	 * conditions unfortunately */

	/**
	 * Signals are REALLY, REALLY, REALLY slow so we have to yield
	 * a lot or else we just keep the sendee trapped in a barrage
	 * of signals. Yes, this is a horrible hack.
	 *
	 * @todo Make signals less racy.
	 */

	bool cancel_replied;
	do {
		for (size_t ii = 0U; ii < 4U; ++ii)
			pthread_yield();

		errnum = pthread_spin_lock(&task->owner_lock);
		if (errnum != 0) {
			assert(errnum != EDEADLK);
			assert(false);
		}

		cancel_replied = cancel_reply;
		if (!cancel_replied) {
			bool owned = task->owned;
			if (owned) {
				errnum = pthread_kill(task->owner, SIGRTMIN);
				if (errnum != 0 && errnum != EAGAIN) {
					assert(errnum != ESRCH);
					assert(errnum != EINVAL);
					assert(false);
				}
			}
		}

		errnum = pthread_spin_unlock(&task->owner_lock);
		if (errnum != 0) {
			assert(errnum != EPERM);
			assert(false);
		}
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

linted_error
linted_asynch_task_waitid_create(struct linted_asynch_task_waitid **taskp,
                                 void *data)
{
	linted_error errnum;
	struct linted_asynch_task_waitid *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(&xx, task,
		                                   LINTED_ASYNCH_TASK_WAITID);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_asynch_task_waitid_destroy(struct linted_asynch_task_waitid *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_asynch_task_waitid_data(struct linted_asynch_task_waitid *task)
{
	return task->data;
}

struct linted_asynch_task *
linted_asynch_task_waitid_to_asynch(struct linted_asynch_task_waitid *task)
{
	return task->parent;
}

struct linted_asynch_task_waitid *
linted_asynch_task_waitid_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

void linted_asynch_task_waitid_info(struct linted_asynch_task_waitid *task,
                                    siginfo_t *info)
{
	*info = task->info;
}

void linted_asynch_task_waitid_prepare(struct linted_asynch_task_waitid *task,
                                       unsigned task_action, idtype_t type,
                                       id_t id, int options)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->idtype = type;
	task->id = id;
	task->options = options;
}

linted_error linted_asynch_task_sigwaitinfo_create(
    struct linted_asynch_task_sigwaitinfo **taskp, void *data)
{
	linted_error errnum;
	struct linted_asynch_task_sigwaitinfo *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_SIGWAITINFO);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_asynch_task_sigwaitinfo_destroy(
    struct linted_asynch_task_sigwaitinfo *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_asynch_task_sigwaitinfo_data(struct linted_asynch_task_sigwaitinfo *task)
{
	return task->data;
}

int linted_asynch_task_sigwaitinfo_signo(
    struct linted_asynch_task_sigwaitinfo *task)
{
	return task->signo;
}

void linted_asynch_task_sigwaitinfo_prepare(
    struct linted_asynch_task_sigwaitinfo *task, unsigned task_action,
    sigset_t const *set)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->set = *set;
}

struct linted_asynch_task *linted_asynch_task_sigwaitinfo_to_asynch(
    struct linted_asynch_task_sigwaitinfo *task)
{
	return task->parent;
}

struct linted_asynch_task_sigwaitinfo *
linted_asynch_task_sigwaitinfo_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

linted_error linted_asynch_task_sleep_until_create(
    struct linted_asynch_task_sleep_until **taskp, void *data)
{
	linted_error errnum;
	struct linted_asynch_task_sleep_until *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_SLEEP_UNTIL);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_asynch_task_sleep_until_destroy(
    struct linted_asynch_task_sleep_until *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_asynch_task_sleep_until_data(struct linted_asynch_task_sleep_until *task)
{
	return task->data;
}

void linted_asynch_task_sleep_until_request(
    struct linted_asynch_task_sleep_until *task, struct timespec *req)
{
	*req = task->request;
}

void linted_asynch_task_sleep_until_prepare(
    struct linted_asynch_task_sleep_until *task, unsigned task_action,
    int flags, struct timespec const *req)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->flags = flags;
	task->request = *req;
}

struct linted_asynch_task *linted_asynch_task_sleep_until_to_asynch(
    struct linted_asynch_task_sleep_until *task)
{
	return task->parent;
}

struct linted_asynch_task_sleep_until *
linted_asynch_task_sleep_until_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

static void *worker_routine(void *arg)
{
	struct linted_asynch_pool *pool = arg;
	linted_error errnum;

	pthread_t self = pthread_self();

	for (;;) {
		bool cancelled;

		struct linted_asynch_task *task;
		{
			struct linted_queue_node *node;
			linted_queue_recv(pool->worker_command_queue, &node);
			task = LINTED_DOWNCAST(struct linted_asynch_task, node);
		}

		errnum = pthread_spin_lock(&task->owner_lock);
		if (errnum != 0) {
			assert(errnum != EDEADLK);
			assert(false);
		}

		{
			bool *cancel_replier = task->cancel_replier;
			cancelled = cancel_replier != NULL;
			if (cancelled) {
				*cancel_replier = true;
				task->cancel_replier = NULL;
			} else {
				task->owner = self;
				task->owned = true;
			}
		}

		errnum = pthread_spin_unlock(&task->owner_lock);
		if (errnum != 0) {
			assert(errnum != EPERM);
			assert(false);
		}

		if (cancelled) {
			task->errnum = ECANCELED;
			linted_asynch_pool_complete(pool, task);
		} else {
			run_task(pool, task);
		}
	}

	LINTED_ASSUME_UNREACHABLE();
}

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task)
{
	switch (task->type) {
	case LINTED_ASYNCH_TASK_POLL:
		linted_ko_do_poll(pool, task);
		break;

	case LINTED_ASYNCH_TASK_READ:
		linted_ko_do_read(pool, task);
		break;

	case LINTED_ASYNCH_TASK_WRITE:
		linted_ko_do_write(pool, task);
		break;

	case LINTED_ASYNCH_TASK_MQ_RECEIVE:
		linted_mq_do_receive(pool, task);
		break;

	case LINTED_ASYNCH_TASK_MQ_SEND:
		linted_mq_do_send(pool, task);
		break;

	case LINTED_ASYNCH_TASK_WAITID:
		run_task_waitid(pool, task);
		break;

	case LINTED_ASYNCH_TASK_SIGWAITINFO:
		run_task_sigwaitinfo(pool, task);
		break;

	case LINTED_ASYNCH_TASK_ACCEPT:
		linted_ko_do_accept(pool, task);
		break;

	case LINTED_ASYNCH_TASK_SLEEP_UNTIL:
		run_task_sleep_until(pool, task);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static void run_task_waitid(struct linted_asynch_pool *pool,
                            struct linted_asynch_task *task)
{
	struct linted_asynch_task_waitid *task_wait = task->data;

	linted_error errnum = 0;

	idtype_t idtype = task_wait->idtype;
	id_t id = task_wait->id;
	int options = task_wait->options;

	if (-1 == waitid(idtype, id, &task_wait->info, options)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum) {
		linted_asynch_pool_submit(pool, task);
		return;
	}

	task->errnum = errnum;

	linted_asynch_pool_complete(pool, task);
}

static void run_task_sigwaitinfo(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	struct linted_asynch_task_sigwaitinfo *task_wait = task->data;
	linted_error errnum = 0;

	int signo = sigwaitinfo(&task_wait->set, &task_wait->info);
	if (-1 == signo) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum) {
		linted_asynch_pool_submit(pool, task);
		return;
	}

	task_wait->signo = signo;
	task->errnum = errnum;

	linted_asynch_pool_complete(pool, task);
}

static void run_task_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	struct linted_asynch_task_sleep_until *restrict task_sleep = task->data;
	linted_error errnum = 0;

	int flags = task_sleep->flags;

	if (-1 == clock_nanosleep(CLOCK_MONOTONIC, flags, &task_sleep->request,
	                          &task_sleep->request)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum) {
		linted_asynch_pool_submit(pool, task);
		return;
	}

	task->errnum = errnum;

	linted_asynch_pool_complete(pool, task);
}
