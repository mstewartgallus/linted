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

linted_error linted_mq_create(linted_mq *mqp, char const *debugpath,
                              size_t maxmsg, size_t msgsize,
                              unsigned long flags)
{
	return ENOSYS;
}

void linted_mq_task_receive(struct linted_mq_task_receive *task,
                            unsigned task_action, linted_ko ko, char *buf,
                            size_t size)
{
	linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_MQ_RECEIVE,
	                   task_action);

	task->ko = ko;
	task->buf = buf;
	task->size = size;
	task->bytes_read = 0U;
}

void linted_mq_task_send(struct linted_mq_task_send *task, unsigned task_action,
                         linted_ko ko, char const *buf, size_t size)
{
	linted_asynch_task(LINTED_UPCAST(task), LINTED_ASYNCH_TASK_MQ_SEND,
	                   task_action);

	task->ko = ko;
	task->buf = buf;
	task->size = size;
	task->bytes_wrote = 0U;
}

void linted_mq_do_receive(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
	struct linted_mq_task_receive *task_receive =
	    LINTED_DOWNCAST(struct linted_mq_task_receive, task);
	size_t bytes_read = 0U;

	task->errnum = ENOSYS;
	task_receive->bytes_read = bytes_read;

	linted_asynch_pool_complete(pool, task);
}

void linted_mq_do_send(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_mq_task_send *task_send =
	    LINTED_DOWNCAST(struct linted_mq_task_send, task);
	size_t bytes_wrote = 0U;

	task->errnum = ENOSYS;
	task_send->bytes_wrote = bytes_wrote;

	linted_asynch_pool_complete(pool, task);
}
