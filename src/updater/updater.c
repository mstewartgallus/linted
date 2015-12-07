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

#include "lntd/updater.h"

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/rpc.h"
#include "lntd/util.h"

#include <rpc/xdr.h>

struct lntd_updater_task_send {
	struct lntd_io_task_write *parent;
	void *data;
	char message[LNTD_RPC_INT32_SIZE + LNTD_RPC_INT32_SIZE +
	             LNTD_RPC_INT32_SIZE + LNTD_RPC_UINT32_SIZE +
	             LNTD_RPC_UINT32_SIZE];
};

struct lntd_updater_task_recv {
	struct lntd_io_task_read *parent;
	void *data;
	char message[LNTD_RPC_INT32_SIZE + LNTD_RPC_INT32_SIZE +
	             LNTD_RPC_INT32_SIZE + LNTD_RPC_UINT32_SIZE +
	             LNTD_RPC_UINT32_SIZE];
};

lntd_error
lntd_updater_task_recv_create(struct lntd_updater_task_recv **taskp,
                              void *data)
{
	lntd_error err;
	struct lntd_updater_task_recv *task;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct lntd_io_task_read *parent;
	{
		struct lntd_io_task_read *xx;
		err = lntd_io_task_read_create(&xx, task);
		if (err != 0)
			goto free_task;
		parent = xx;
	}

	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_updater_task_recv_destroy(struct lntd_updater_task_recv *task)
{
	lntd_io_task_read_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *
lntd_updater_task_recv_prepare(struct lntd_updater_task_recv *task,
                               union lntd_async_ck task_ck,
                               void *userstate, lntd_ko updater)
{
	return lntd_io_task_read_prepare(
	    task->parent, task_ck, userstate, updater, task->message,
	    sizeof task->message);
}

struct lntd_async_task *
lntd_updater_task_recv_to_async(struct lntd_updater_task_recv *task)
{
	return lntd_io_task_read_to_async(task->parent);
}

void *lntd_updater_task_recv_data(struct lntd_updater_task_recv *task)
{
	return task->data;
}

lntd_error
lntd_updater_task_send_create(struct lntd_updater_task_send **taskp,
                              void *data)
{
	lntd_error err;
	struct lntd_updater_task_send *task;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *task);
		if (err != 0)
			return err;
		task = xx;
	}
	struct lntd_io_task_write *parent;
	{
		struct lntd_io_task_write *xx;
		err = lntd_io_task_write_create(&xx, task);
		if (err != 0)
			goto free_task;
		parent = xx;
	}
	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_updater_task_send_destroy(struct lntd_updater_task_send *task)
{
	lntd_io_task_write_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *
lntd_updater_task_send_prepare(struct lntd_updater_task_send *task,
                               union lntd_async_ck task_ck,
                               void *userstate, lntd_ko updater,
                               struct lntd_updater_update const *update)
{
	XDR xdr = {0};
	xdrmem_create(&xdr, task->message, sizeof task->message,
	              XDR_ENCODE);

	struct lntd_updater_code code = {
	    .x_position = update->x_position,
	    .y_position = update->y_position,
	    .z_position = update->z_position,

	    .z_rotation = update->z_rotation._value,
	    .x_rotation = update->x_rotation._value,
	};

	if (!xdr_lntd_updater_code(&xdr, &code))
		LNTD_ASSERT(0);

	xdr_destroy(&xdr);

	return lntd_io_task_write_prepare(
	    task->parent, task_ck, userstate, updater, task->message,
	    sizeof task->message);
}

struct lntd_async_task *
lntd_updater_task_send_to_async(struct lntd_updater_task_send *task)
{
	return lntd_io_task_write_to_async(task->parent);
}

void *lntd_updater_task_send_data(struct lntd_updater_task_send *task)
{
	return task->data;
}

void lntd_updater_decode(struct lntd_updater_task_recv const *task,
                         struct lntd_updater_update *update)
{
	XDR xdr = {0};
	xdrmem_create(&xdr, (void *)task->message, sizeof task->message,
	              XDR_DECODE);

	struct lntd_updater_code code = {0};
	if (!xdr_lntd_updater_code(&xdr, &code))
		LNTD_ASSERT(0);

	update->x_position = code.x_position;
	update->y_position = code.y_position;
	update->z_position = code.z_position;

	update->z_rotation._value = code.z_rotation;
	update->x_rotation._value = code.x_rotation;

	xdr_destroy(&xdr);
}
