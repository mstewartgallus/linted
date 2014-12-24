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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/pid.h"

#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>

struct linted_pid_task_waitid
{
	struct linted_asynch_task *parent;
	void *data;
	siginfo_t info;
	idtype_t idtype;
	id_t id;
	int options;
};

linted_error
linted_pid_task_waitid_create(struct linted_pid_task_waitid **taskp, void *data)
{
	linted_error errnum;
	struct linted_pid_task_waitid *task;
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

void linted_pid_task_waitid_destroy(struct linted_pid_task_waitid *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_pid_task_waitid_data(struct linted_pid_task_waitid *task)
{
	return task->data;
}

struct linted_asynch_task *
linted_pid_task_waitid_to_asynch(struct linted_pid_task_waitid *task)
{
	return task->parent;
}

struct linted_pid_task_waitid *
linted_pid_task_waitid_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

void linted_pid_task_waitid_info(struct linted_pid_task_waitid *task,
                                 siginfo_t *info)
{
	*info = task->info;
}

void linted_pid_task_waitid_prepare(struct linted_pid_task_waitid *task,
                                    unsigned task_action, idtype_t type,
                                    id_t id, int options)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->idtype = type;
	task->id = id;
	task->options = options;
}

void linted_pid_do_waitid(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
	struct linted_pid_task_waitid *task_wait =
	    linted_asynch_task_data(task);

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

	linted_asynch_task_seterrnum(task, errnum);
	linted_asynch_pool_complete(pool, task);
}
