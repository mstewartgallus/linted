/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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

#include "linted/updater.h"

#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/rpc.h"

struct linted_updater_task_send
{
	struct linted_io_task_write *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE +
	             LINTED_RPC_INT32_SIZE + LINTED_RPC_UINT32_SIZE +
	             LINTED_RPC_UINT32_SIZE];
};

struct linted_updater_task_receive
{
	struct linted_io_task_read *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE +
	             LINTED_RPC_INT32_SIZE + LINTED_RPC_UINT32_SIZE +
	             LINTED_RPC_UINT32_SIZE];
};

linted_error
linted_updater_task_receive_create(struct linted_updater_task_receive **taskp,
                                   void *data)
{
	linted_error errnum;
	struct linted_updater_task_receive *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_io_task_read *parent;
	{
		struct linted_io_task_read *xx;
		errnum = linted_io_task_read_create(&xx, task);
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

void
linted_updater_task_receive_destroy(struct linted_updater_task_receive *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void
linted_updater_task_receive_prepare(struct linted_updater_task_receive *task,
                                    unsigned task_action, linted_ko updater)
{
	linted_io_task_read_prepare(task->parent, task_action, updater,
	                            task->message, sizeof task->message);
}

struct linted_updater_task_receive *
linted_updater_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_read_data(linted_io_task_read_from_asynch(task));
}

struct linted_asynch_task *
linted_updater_task_receive_to_asynch(struct linted_updater_task_receive *task)
{
	return linted_io_task_read_to_asynch(task->parent);
}

void *linted_updater_task_receive_data(struct linted_updater_task_receive *task)
{
	return task->data;
}

linted_error
linted_updater_task_send_create(struct linted_updater_task_send **taskp,
                                void *data)
{
	linted_error errnum;
	struct linted_updater_task_send *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_io_task_write *parent;
	{
		struct linted_io_task_write *xx;
		errnum = linted_io_task_write_create(&xx, task);
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

void linted_updater_task_send_destroy(struct linted_updater_task_send *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void
linted_updater_task_send_prepare(struct linted_updater_task_send *task,
                                 unsigned task_action, linted_ko updater,
                                 struct linted_updater_update const *update)
{
	linted_io_task_write_prepare(task->parent, task_action, updater,
	                             task->message, sizeof task->message);

	char *tip = task->message;

	linted_rpc_pack(update->x_position, tip);
	tip += LINTED_RPC_INT32_SIZE;

	linted_rpc_pack(update->y_position, tip);
	tip += LINTED_RPC_INT32_SIZE;

	linted_rpc_pack(update->z_position, tip);
	tip += LINTED_RPC_INT32_SIZE;

	linted_rpc_pack_uint32(update->x_rotation._value, tip);
	tip += LINTED_RPC_UINT32_SIZE;

	linted_rpc_pack_uint32(update->y_rotation._value, tip);
}

struct linted_updater_task_send *
linted_updater_task_send_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_asynch(task));
}

struct linted_asynch_task *
linted_updater_task_send_to_asynch(struct linted_updater_task_send *task)
{
	return linted_io_task_write_to_asynch(task->parent);
}

void *linted_updater_task_send_data(struct linted_updater_task_send *task)
{
	return task->data;
}

void linted_updater_decode(struct linted_updater_task_receive const *task,
                           struct linted_updater_update *update)
{
	char const *tip = task->message;

	update->x_position = linted_rpc_unpack(tip);
	tip += LINTED_RPC_INT32_SIZE;

	update->y_position = linted_rpc_unpack(tip);
	tip += LINTED_RPC_INT32_SIZE;

	update->z_position = linted_rpc_unpack(tip);
	tip += LINTED_RPC_INT32_SIZE;

	update->x_rotation._value = linted_rpc_unpack_uint32(tip);
	tip += LINTED_RPC_UINT32_SIZE;

	update->y_rotation._value = linted_rpc_unpack_uint32(tip);
}
