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
#include "linted/signal.h"

#include "linted/asynch.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <signal.h>

struct linted_signal_task_sigwaitinfo
{
	struct linted_asynch_task *parent;
	void *data;
	int signo;
};

linted_error linted_signal_task_sigwaitinfo_create(
    struct linted_signal_task_sigwaitinfo **taskp, void *data)
{
	linted_error errnum;
	struct linted_signal_task_sigwaitinfo *task;
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

void linted_signal_task_sigwaitinfo_destroy(
    struct linted_signal_task_sigwaitinfo *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_signal_task_sigwaitinfo_data(struct linted_signal_task_sigwaitinfo *task)
{
	return task->data;
}

int linted_signal_task_sigwaitinfo_signo(
    struct linted_signal_task_sigwaitinfo *task)
{
	return task->signo;
}

struct linted_asynch_task *linted_signal_task_sigwaitinfo_to_asynch(
    struct linted_signal_task_sigwaitinfo *task)
{
	return task->parent;
}

struct linted_signal_task_sigwaitinfo *
linted_signal_task_sigwaitinfo_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

void linted_signal_do_sigwaitinfo(struct linted_asynch_pool *pool,
                                  struct linted_asynch_task *task)
{
	linted_asynch_pool_complete(pool, task, LINTED_ERROR_UNIMPLEMENTED);
}
