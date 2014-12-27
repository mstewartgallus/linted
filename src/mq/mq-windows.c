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

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

struct linted_mq_task_receive
{
	struct linted_asynch_task *parent;
	void *data;
	char *buf;
	size_t size;
	size_t bytes_read;
	linted_ko ko;
};

struct linted_mq_task_send
{
	struct linted_asynch_task *parent;
	void *data;
	char const *buf;
	size_t size;
	size_t bytes_wrote;
	linted_ko ko;
};

linted_error linted_mq_create(linted_mq *mqp, char const *debugpath,
                              size_t maxmsg, size_t msgsize,
                              unsigned long flags)
{
	return ENOSYS;
}

linted_error
linted_mq_task_receive_create(struct linted_mq_task_receive **taskp, void *data)
{
	linted_error errnum;
	struct linted_mq_task_receive *task;
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
		    &xx, task, LINTED_ASYNCH_TASK_MQ_RECEIVE);
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

void linted_mq_task_receive_destroy(struct linted_mq_task_receive *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_mq_task_receive_prepare(struct linted_mq_task_receive *task,
                                    unsigned task_action, linted_ko ko,
                                    char *buf, size_t msglen)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = msglen;
	task->bytes_read = 0;
}

struct linted_mq_task_receive *
linted_mq_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_mq_task_receive_to_asynch(struct linted_mq_task_receive *task)
{
	return task->parent;
}

void *linted_mq_task_receive_data(struct linted_mq_task_receive *task)
{
	return task->data;
}

size_t linted_mq_task_receive_bytes_read(struct linted_mq_task_receive *task)
{
	return task->bytes_read;
}

linted_error linted_mq_task_send_create(struct linted_mq_task_send **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_mq_task_send *task;
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
		                                   LINTED_ASYNCH_TASK_MQ_SEND);
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

void linted_mq_task_send_destroy(struct linted_mq_task_send *task)
{
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_mq_task_send_prepare(struct linted_mq_task_send *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t msglen)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = msglen;
	task->bytes_wrote = 0;
}

struct linted_mq_task_send *
linted_mq_task_send_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_mq_task_send_to_asynch(struct linted_mq_task_send *task)
{
	return task->parent;
}

void *linted_mq_task_send_data(struct linted_mq_task_send *task)
{
	return task->data;
}

void linted_mq_do_receive(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
	struct linted_mq_task_receive *task_receive =
	    LINTED_DOWNCAST(struct linted_mq_task_receive, task);

	task_receive->bytes_read = 0U;
	linted_asynch_pool_complete(pool, task, ENOSYS);
}

void linted_mq_do_send(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_mq_task_send *task_send =
	    LINTED_DOWNCAST(struct linted_mq_task_send, task);

	task_send->bytes_wrote = 0U;
	linted_asynch_pool_complete(pool, task, ENOSYS);
}
