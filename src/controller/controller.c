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

#include "linted/controller.h"

#include "linted/io.h"
#include "linted/mem.h"
#include "linted/rpc.h"

#include <errno.h>
#include <stdint.h>
#include <string.h>

struct linted_controller_task_send
{
	struct linted_io_task_write *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE + 1U];
};

struct linted_controller_task_receive
{
	struct linted_io_task_read *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE + 1U];
};

linted_error
linted_controller_task_send_create(struct linted_controller_task_send **taskp,
                                   void *data)
{
	linted_error errnum;
	struct linted_controller_task_send *task;
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

void
linted_controller_task_send_destroy(struct linted_controller_task_send *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

struct linted_asynch_task *
linted_controller_task_send_to_asynch(struct linted_controller_task_send *task)
{
	return linted_io_task_write_to_asynch(task->parent);
}

struct linted_controller_task_send *
linted_controller_task_send_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_asynch(task));
}

void *linted_controller_task_send_data(struct linted_controller_task_send *task)
{
	return task->data;
}

void linted_controller_task_send_prepare(
    struct linted_controller_task_send *task, unsigned task_action,
    linted_controller controller,
    struct linted_controller_message const *message)
{
	linted_io_task_write_prepare(task->parent, task_action, controller,
	                             task->message, sizeof task->message);

	char *tip = task->message;

	linted_rpc_pack(message->x_tilt, tip);
	tip += LINTED_RPC_INT32_SIZE;

	linted_rpc_pack(message->y_tilt, tip);
	tip += LINTED_RPC_INT32_SIZE;

	unsigned char bitfield = ((uintmax_t)message->forward) |
	                         ((uintmax_t)message->back) << 1U |
	                         ((uintmax_t)message->right) << 2U |
	                         ((uintmax_t)message->left) << 3U |
	                         ((uintmax_t)message->jumping) << 4U;
	memcpy(tip, &bitfield, sizeof bitfield);
}

linted_error linted_controller_task_receive_create(
    struct linted_controller_task_receive **taskp, void *data)
{
	linted_error errnum;
	struct linted_controller_task_receive *task;
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

void linted_controller_task_receive_destroy(
    struct linted_controller_task_receive *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

struct linted_asynch_task *linted_controller_task_receive_to_asynch(
    struct linted_controller_task_receive *task)
{
	return linted_io_task_read_to_asynch(task->parent);
}

struct linted_controller_task_receive *
linted_controller_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_read_data(linted_io_task_read_from_asynch(task));
}

void *
linted_controller_task_receive_data(struct linted_controller_task_receive *task)
{
	return task->data;
}

void linted_controller_task_receive_prepare(
    struct linted_controller_task_receive *task, unsigned task_action,
    linted_controller controller)
{
	linted_io_task_read_prepare(task->parent, task_action, controller,
	                            task->message, sizeof task->message);
}

linted_error
linted_controller_decode(struct linted_controller_task_receive const *task,
                         struct linted_controller_message *message)
{
	char const *tip = task->message;

	message->x_tilt = linted_rpc_unpack(tip);
	tip += LINTED_RPC_INT32_SIZE;

	message->y_tilt = linted_rpc_unpack(tip);
	tip += LINTED_RPC_INT32_SIZE;

	unsigned char bitfield;
	memcpy(&bitfield, tip, sizeof bitfield);

	if ((bitfield & ~(1U | 1U << 1U | 1U << 2U | 1U << 3U | 1U << 4U)) !=
	    0U)
		return EPROTO;

	message->forward = bitfield & 1U;
	message->back = (bitfield & (1U << 1U)) != 0U;
	message->right = (bitfield & (1U << 2U)) != 0U;
	message->left = (bitfield & (1U << 3U)) != 0U;

	message->jumping = (bitfield & (1U << 4U)) != 0U;

	return 0;
}
