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
#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/asynch.h"

#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/queue.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <process.h>
#include <stdbool.h>
#include <string.h>
#include <windows.h>

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

	size_t worker_count;

	linted_ko workers[];
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

	CRITICAL_SECTION owner_lock;
	linted_ko owner;
	bool owned : 1U;
	bool in_flight : 1U;
	bool *cancel_replier;

	void *data;
	linted_error errnum;
	unsigned type;
	unsigned task_action;
};

static DWORD WINAPI worker_routine(void *arg);

static void run_task(struct linted_asynch_pool *pool,
                     struct linted_asynch_task *task);

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks)
{
	linted_error errnum;
	size_t worker_count = 0;
	struct linted_asynch_pool *pool;

	size_t workers_size = max_tasks * sizeof pool->workers[0U];
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *pool + workers_size);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	errnum = linted_queue_create(&pool->worker_command_queue);
	if (errnum != 0)
		goto free_pool;

	errnum = linted_queue_create(&pool->event_queue);
	if (errnum != 0)
		goto destroy_worker_command_queue;

	pool->worker_count = max_tasks;

	for (; worker_count < max_tasks; ++worker_count) {
		linted_ko thread =
		    CreateThread(NULL, 0, worker_routine, pool, 0, NULL);
		if (INVALID_HANDLE_VALUE == thread) {
			DWORD error = GetLastError();
			errnum = HRESULT_FROM_WIN32(error);
			goto destroy_threads;
		}
	}

	*poolp = pool;

	return 0;

destroy_threads:
	for (size_t ii = 0U; ii < worker_count; ++ii)
		TerminateThread(pool->workers[ii], 0);

	for (size_t ii = 0U; ii < worker_count; ++ii)
		CloseHandle(pool->workers[ii]);

	linted_queue_destroy(pool->event_queue);

destroy_worker_command_queue:
	linted_queue_destroy(pool->worker_command_queue);

free_pool:
	linted_mem_free(pool);

	return errnum;
}

/**
 * @bug The queue isn't safe to terminate. Use an OS queue or list
 *      that isn't corruptible.
 */
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool)
{
	linted_error errnum = 0;

	size_t worker_count = pool->worker_count;

	for (size_t ii = 0U; ii < worker_count; ++ii)
		TerminateThread(pool->workers[ii], 0);

	for (size_t ii = 0U; ii < worker_count; ++ii)
		CloseHandle(pool->workers[ii]);

	linted_queue_destroy(pool->worker_command_queue);
	linted_queue_destroy(pool->event_queue);

	linted_mem_free(pool);

	return errnum;
}

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task)
{
	if (NULL == pool) {
		run_task(NULL, task);
	} else {
		linted_queue_send(pool->worker_command_queue,
		                  LINTED_UPCAST(task));
	}
}

void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	if (NULL == pool)
		return;

	linted_queue_send(pool->event_queue, LINTED_UPCAST(task));
}

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp)
{
	struct linted_queue_node *node;

	linted_queue_recv(pool->event_queue, &node);

	*completionp = LINTED_DOWNCAST(struct linted_asynch_task, node);

	return 0U;
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

void linted_asynch_task(struct linted_asynch_task *task, unsigned type,
                        unsigned task_action)
{
	linted_queue_node(LINTED_UPCAST(task));

	task->type = type;
	task->errnum = 0;
	task->task_action = task_action;
}

static DWORD WINAPI worker_routine(void *arg)
{
	struct linted_asynch_pool *pool = arg;

	for (;;) {
		struct linted_asynch_task *task;
		{
			struct linted_queue_node *node;
			linted_queue_recv(pool->worker_command_queue, &node);
			task = LINTED_DOWNCAST(struct linted_asynch_task, node);
		}

		run_task(pool, task);
	}
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

	case LINTED_ASYNCH_TASK_ACCEPT:
		linted_ko_do_accept(pool, task);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}
