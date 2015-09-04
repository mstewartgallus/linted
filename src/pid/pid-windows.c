/*
 * Copyright 2015 Steven Stewart-Gallus
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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/pid.h"

#include "linted/async.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <signal.h>
#include <sys/types.h>

#include <windows.h>

struct linted_pid_task_waitid {
	struct linted_async_task *parent;
	void *data;
	int options;
};

linted_error
linted_pid_task_waitid_create(struct linted_pid_task_waitid **taskp,
                              void *data)
{
	linted_error err;
	struct linted_pid_task_waitid *task;
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
		    &xx, task, LINTED_ASYNCH_TASK_WAITID);
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

void linted_pid_task_waitid_destroy(struct linted_pid_task_waitid *task)
{
	linted_async_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_pid_task_waitid_data(struct linted_pid_task_waitid *task)
{
	return task->data;
}

struct linted_async_task *
linted_pid_task_waitid_to_async(struct linted_pid_task_waitid *task)
{
	return task->parent;
}

struct linted_pid_task_waitid *
linted_pid_task_waitid_from_async(struct linted_async_task *task)
{
	return linted_async_task_data(task);
}

void linted_pid_do_waitid(struct linted_async_pool *pool,
                          struct linted_async_task *task)
{
	linted_async_pool_complete(pool, task,
	                           LINTED_ERROR_UNIMPLEMENTED);
}

linted_pid linted_pid_get_pid(void)
{
	return GetCurrentProcessId();
}
