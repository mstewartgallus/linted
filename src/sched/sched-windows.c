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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "config.h"

#include "linted/sched.h"

#include "linted/asynch.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <sys/time.h>
#include <time.h>

struct linted_sched_task_sleep_until
{
	struct linted_asynch_task *parent;
	void *data;
	struct timespec request;
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
	struct linted_asynch_task *xx;
	linted_error errnum = linted_asynch_task_create(
	    &xx, data, LINTED_ASYNCH_TASK_IDLE);
	if (errnum != 0)
		return errnum;
	*taskp = (struct linted_sched_task_idle *)xx;

	return 0;
}

void linted_sched_task_idle_destroy(struct linted_sched_task_idle *task)
{
	linted_asynch_task_destroy((void *)task);
}

void *linted_sched_task_idle_data(struct linted_sched_task_idle *task)
{
	return linted_asynch_task_data((void *)task);
}

struct linted_asynch_task *
linted_sched_task_idle_to_asynch(struct linted_sched_task_idle *task)
{
	return (void *)task;
}

struct linted_sched_task_idle *
linted_sched_task_idle_from_asynch(struct linted_asynch_task *task)
{
	return (void *)task;
}

void linted_sched_task_idle_prepare(struct linted_sched_task_idle *task,
                                    unsigned task_action)
{
	linted_asynch_task_prepare((void *)task, task_action);
}

linted_error linted_sched_task_sleep_until_create(
    struct linted_sched_task_sleep_until **taskp, void *data)
{
	linted_error errnum;
	struct linted_sched_task_sleep_until *task;
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

void linted_sched_task_sleep_until_destroy(
    struct linted_sched_task_sleep_until *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_sched_task_sleep_until_data(
    struct linted_sched_task_sleep_until *task)
{
	return task->data;
}

void linted_sched_task_sleep_until_request(
    struct linted_sched_task_sleep_until *task, struct timespec *req)
{
	*req = task->request;
}

void linted_sched_task_sleep_until_prepare(
    struct linted_sched_task_sleep_until *task, unsigned task_action,
    struct timespec const *req)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->request = *req;
}

struct linted_asynch_task *linted_sched_task_sleep_until_to_asynch(
    struct linted_sched_task_sleep_until *task)
{
	return task->parent;
}

struct linted_sched_task_sleep_until *
linted_sched_task_sleep_until_from_asynch(
    struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

void linted_sched_do_idle(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
	linted_asynch_pool_complete(pool, task, 0);
}

void linted_sched_do_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task)
{
	linted_asynch_pool_complete(pool, task,
	                            LINTED_ERROR_UNIMPLEMENTED);
}
