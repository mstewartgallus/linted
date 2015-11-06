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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#include "config.h"

#include "updater.h"

#include "linted/updater.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/rpc.h"

#include <assert.h>

struct linted_updater_task_send {
	struct linted_io_task_write *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE +
	             LINTED_RPC_INT32_SIZE + LINTED_RPC_UINT32_SIZE +
	             LINTED_RPC_UINT32_SIZE];
};

struct linted_update_task_recv {
	struct linted_io_task_read *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE +
	             LINTED_RPC_INT32_SIZE + LINTED_RPC_UINT32_SIZE +
	             LINTED_RPC_UINT32_SIZE];
};

linted_error
linted_update_task_recv_create(struct linted_update_task_recv **taskp,
                               void *data)
{
	linted_error err;
	struct linted_update_task_recv *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_io_task_read *parent;
	{
		struct linted_io_task_read *xx;
		err = linted_io_task_read_create(&xx, task);
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

void linted_update_task_recv_destroy(
    struct linted_update_task_recv *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void linted_update_task_recv_prepare(
    struct linted_update_task_recv *task, union linted_async_ck task_ck,
    linted_ko updater)
{
	linted_io_task_read_prepare(task->parent, task_ck, updater,
	                            task->message,
	                            sizeof task->message);
}

struct linted_update_task_recv *
linted_update_task_recv_from_async(struct linted_async_task *task)
{
	return linted_io_task_read_data(
	    linted_io_task_read_from_async(task));
}

struct linted_async_task *
linted_update_task_recv_to_async(struct linted_update_task_recv *task)
{
	return linted_io_task_read_to_async(task->parent);
}

void *linted_update_task_recv_data(struct linted_update_task_recv *task)
{
	return task->data;
}

linted_error
linted_updater_task_send_create(struct linted_updater_task_send **taskp,
                                void *data)
{
	linted_error err;
	struct linted_updater_task_send *task;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct linted_io_task_write *parent;
	{
		struct linted_io_task_write *xx;
		err = linted_io_task_write_create(&xx, task);
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

void linted_updater_task_send_destroy(
    struct linted_updater_task_send *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void linted_updater_task_send_prepare(
    struct linted_updater_task_send *task,
    union linted_async_ck task_ck, linted_ko updater,
    struct linted_updater_update const *update)
{
	linted_io_task_write_prepare(task->parent, task_ck, updater,
	                             task->message,
	                             sizeof task->message);

	XDR xdr;
	xdrmem_create(&xdr, task->message, sizeof task->message,
	              XDR_ENCODE);

	struct linted_updater_code code = {
	    .x_position = update->x_position,
	    .y_position = update->y_position,
	    .z_position = update->z_position,

	    .z_rotation = update->z_rotation._value,
	    .x_rotation = update->x_rotation._value,
	};

	if (!xdr_linted_updater_code(&xdr, &code))
		assert(0);
}

struct linted_updater_task_send *
linted_updater_task_send_from_async(struct linted_async_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_async(task));
}

struct linted_async_task *
linted_updater_task_send_to_async(struct linted_updater_task_send *task)
{
	return linted_io_task_write_to_async(task->parent);
}

void *
linted_updater_task_send_data(struct linted_updater_task_send *task)
{
	return task->data;
}

void linted_updater_decode(struct linted_update_task_recv const *task,
                           struct linted_updater_update *update)
{
	XDR xdr;
	xdrmem_create(&xdr, (void *)task->message, sizeof task->message,
	              XDR_DECODE);

	struct linted_updater_code code;
	if (!xdr_linted_updater_code(&xdr, &code))
		assert(0);

	update->x_position = code.x_position;
	update->y_position = code.y_position;
	update->z_position = code.z_position;

	update->z_rotation._value = code.z_rotation;
	update->x_rotation._value = code.x_rotation;
}
