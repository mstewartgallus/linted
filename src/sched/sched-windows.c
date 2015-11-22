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
#include "config.h"

#include "linted/sched.h"

#include "linted/async.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <sys/time.h>
#include <time.h>

struct linted_sched_task_sleep_until {
	struct linted_async_task *parent;
	void *data;
	struct timespec time;
};

linted_error linted_sched_time(struct timespec *now)
{
	return LINTED_ERROR_UNIMPLEMENTED;
}

/* task_idle is just a fake */
linted_error
linted_sched_task_idle_create(struct linted_sched_task_idle **taskp,
                              void *data)
{
	struct linted_async_task *xx;
	linted_error err = linted_async_task_create(
	    &xx, data, LINTED_ASYNCH_TASK_IDLE);
	if (err != 0)
		return err;
	*taskp = (struct linted_sched_task_idle *)xx;

	return 0;
}

void linted_sched_task_idle_destroy(struct linted_sched_task_idle *task)
{
	linted_async_task_destroy((void *)task);
}

void *linted_sched_task_idle_data(struct linted_sched_task_idle *task)
{
	return linted_async_task_data((void *)task);
}

struct linted_async_task *
linted_sched_task_idle_to_async(struct linted_sched_task_idle *task)
{
	return (void *)task;
}

struct linted_sched_task_idle *
linted_sched_task_idle_from_async(struct linted_async_task *task)
{
	return (void *)task;
}

struct linted_async_task *
linted_sched_task_idle_prepare(struct linted_sched_task_idle *task,
                               union linted_async_ck task_ck,
                               void *userstate)
{
	return linted_async_task_prepare((void *)task, task_ck,
	                                 userstate);
}

linted_error linted_sched_task_sleep_until_create(
    struct linted_sched_task_sleep_until **taskp, void *data)
{
	linted_error err;
	struct linted_sched_task_sleep_until *task;
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
		    &xx, task, LINTED_ASYNCH_TASK_SLEEP_UNTIL);
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

void linted_sched_task_sleep_until_destroy(
    struct linted_sched_task_sleep_until *task)
{
	linted_async_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_sched_task_sleep_until_data(
    struct linted_sched_task_sleep_until *task)
{
	return task->data;
}

void linted_sched_task_sleep_until_time(
    struct linted_sched_task_sleep_until *task, struct timespec *xx)
{
	*xx = task->time;
}

struct linted_async_task *linted_sched_task_sleep_until_prepare(
    struct linted_sched_task_sleep_until *task,
    union linted_async_ck task_ck, void *userstate,
    struct timespec const *req)
{
	task->time = *req;
	return linted_async_task_prepare(task->parent, task_ck,
	                                 userstate);
}

struct linted_async_task *linted_sched_task_sleep_until_to_async(
    struct linted_sched_task_sleep_until *task)
{
	return task->parent;
}

struct linted_sched_task_sleep_until *
linted_sched_task_sleep_until_from_async(struct linted_async_task *task)
{
	return linted_async_task_data(task);
}

void linted_sched_do_idle(struct linted_async_pool *pool,
                          struct linted_async_task *task)
{
	linted_async_pool_complete(pool, task, 0);
}

void linted_sched_do_sleep_until(struct linted_async_pool *pool,
                                 struct linted_async_task *task)
{
	linted_async_pool_complete(pool, task,
	                           LINTED_ERROR_UNIMPLEMENTED);
}
