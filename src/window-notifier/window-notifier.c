/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#include "config.h"

#include "linted/window-notifier.h"

#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <string.h>

struct linted_window_notifier_task_send
{
	struct linted_mq_task_send *parent;
	void *data;
	char message[LINTED_RPC_UINT32_SIZE];
};

struct linted_window_notifier_task_receive
{
	struct linted_mq_task_receive *parent;
	void *data;
	char message[LINTED_RPC_UINT32_SIZE];
};

linted_error linted_window_notifier_task_receive_create(
    struct linted_window_notifier_task_receive **taskp, void *data)
{
	linted_error errnum;
	struct linted_window_notifier_task_receive *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_mq_task_receive *parent;
	{
		struct linted_mq_task_receive *xx;
		errnum = linted_mq_task_receive_create(&xx, task);
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

void linted_window_notifier_task_receive_destroy(
    struct linted_window_notifier_task_receive *task)
{
	linted_mq_task_receive_destroy(task->parent);
	linted_mem_free(task);
}

void linted_window_notifier_task_receive_prepare(
    struct linted_window_notifier_task_receive *task, unsigned task_action,
    linted_ko notifier)
{
	linted_mq_task_receive_prepare(task->parent, task_action, notifier,
	                               task->message, sizeof task->message);
}

struct linted_window_notifier_task_receive *
linted_window_notifier_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_mq_task_receive_data(
	    linted_mq_task_receive_from_asynch(task));
}

struct linted_asynch_task *linted_window_notifier_task_receive_to_asynch(
    struct linted_window_notifier_task_receive *task)
{
	return linted_mq_task_receive_to_asynch(task->parent);
}

void *linted_window_notifier_task_receive_data(
    struct linted_window_notifier_task_receive *task)
{
	return task->data;
}

linted_error linted_window_notifier_task_send_create(
    struct linted_window_notifier_task_send **taskp, void *data)
{
	linted_error errnum;
	struct linted_window_notifier_task_send *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_mq_task_send *parent;
	{
		struct linted_mq_task_send *xx;
		errnum = linted_mq_task_send_create(&xx, task);
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

void linted_window_notifier_task_send_destroy(
    struct linted_window_notifier_task_send *task)
{
	linted_mq_task_send_destroy(task->parent);
	linted_mem_free(task);
}

void linted_window_notifier_task_send_prepare(
    struct linted_window_notifier_task_send *task, unsigned task_action,
    linted_ko notifier, uint_fast32_t window)
{
	linted_mq_task_send_prepare(task->parent, task_action, notifier,
	                            task->message, sizeof task->message);

	char *tip = task->message;

	linted_rpc_pack_uint32(window, tip);
}

struct linted_window_notifier_task_send *
linted_window_notifier_task_send_from_asynch(struct linted_asynch_task *task)
{
	return linted_mq_task_send_data(linted_mq_task_send_from_asynch(task));
}

struct linted_asynch_task *linted_window_notifier_task_send_to_asynch(
    struct linted_window_notifier_task_send *task)
{
	return linted_mq_task_send_to_asynch(task->parent);
}

void *linted_window_notifier_task_send_data(
    struct linted_window_notifier_task_send *task)
{
	return task->data;
}

uint_fast32_t linted_window_notifier_decode(
    struct linted_window_notifier_task_receive const *task)
{
	char const *tip = task->message;

	return linted_rpc_unpack_uint32(tip);
}
