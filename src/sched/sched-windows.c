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

#include "lntd/sched.h"

#include "lntd/async.h"
#include "lntd/mem.h"
#include "lntd/util.h"

#include <sys/time.h>
#include <time.h>

struct lntd_sched_task_sleep_until {
	struct lntd_async_task *parent;
	void *data;
	struct timespec time;
};

lntd_error lntd_sched_time(struct timespec *now)
{
	return LNTD_ERROR_UNIMPLEMENTED;
}

/* task_idle is just a fake */
lntd_error
lntd_sched_task_idle_create(struct lntd_sched_task_idle **taskp,
                            void *data)
{
	struct lntd_async_task *xx;
	lntd_error err =
	    lntd_async_task_create(&xx, data, LNTD_ASYNCH_TASK_IDLE);
	if (err != 0)
		return err;
	*taskp = (struct lntd_sched_task_idle *)xx;

	return 0;
}

void lntd_sched_task_idle_destroy(struct lntd_sched_task_idle *task)
{
	lntd_async_task_destroy((void *)task);
}

void *lntd_sched_task_idle_data(struct lntd_sched_task_idle *task)
{
	return lntd_async_task_data((void *)task);
}

struct lntd_async_task *
lntd_sched_task_idle_to_async(struct lntd_sched_task_idle *task)
{
	return (void *)task;
}

struct lntd_sched_task_idle *
lntd_sched_task_idle_from_async(struct lntd_async_task *task)
{
	return (void *)task;
}

struct lntd_async_task *
lntd_sched_task_idle_prepare(struct lntd_sched_task_idle *task,
                             union lntd_async_ck task_ck,
                             void *userstate)
{
	return lntd_async_task_prepare((void *)task, task_ck,
	                               userstate);
}

lntd_error lntd_sched_task_sleep_until_create(
    struct lntd_sched_task_sleep_until **taskp, void *data)
{
	lntd_error err;
	struct lntd_sched_task_sleep_until *task;
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
		    &xx, task, LNTD_ASYNCH_TASK_SLEEP_UNTIL);
		if (err != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_sched_task_sleep_until_destroy(
    struct lntd_sched_task_sleep_until *task)
{
	lntd_async_task_destroy(task->parent);
	lntd_mem_free(task);
}

void *lntd_sched_task_sleep_until_data(
    struct lntd_sched_task_sleep_until *task)
{
	return task->data;
}

void lntd_sched_task_sleep_until_time(
    struct lntd_sched_task_sleep_until *task, struct timespec *xx)
{
	*xx = task->time;
}

struct lntd_async_task *lntd_sched_task_sleep_until_prepare(
    struct lntd_sched_task_sleep_until *task,
    union lntd_async_ck task_ck, void *userstate,
    struct timespec const *req)
{
	task->time = *req;
	return lntd_async_task_prepare(task->parent, task_ck,
	                               userstate);
}

struct lntd_async_task *lntd_sched_task_sleep_until_to_async(
    struct lntd_sched_task_sleep_until *task)
{
	return task->parent;
}

struct lntd_sched_task_sleep_until *
lntd_sched_task_sleep_until_from_async(struct lntd_async_task *task)
{
	return lntd_async_task_data(task);
}

void lntd_sched_do_idle(struct lntd_async_pool *pool,
                        struct lntd_async_task *task)
{
	lntd_async_pool_complete(pool, task, 0);
}

void lntd_sched_do_sleep_until(struct lntd_async_pool *pool,
                               struct lntd_async_task *task)
{
	lntd_async_pool_complete(pool, task, LNTD_ERROR_UNIMPLEMENTED);
}
