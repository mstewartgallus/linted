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

#include "controller.h"

#include "linted/controller.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/mem.h"
#include "linted/rpc.h"
#include "linted/util.h"

#include <errno.h>
#include <stdint.h>
#include <string.h>

struct linted_controller_task_send {
	struct linted_io_task_write *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE +
	             LINTED_RPC_INT32_SIZE * 5U];
};

struct linted_controller_task_recv {
	struct linted_io_task_read *parent;
	void *data;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE +
	             LINTED_RPC_INT32_SIZE * 5U];
};

linted_error linted_controller_task_send_create(
    struct linted_controller_task_send **taskp, void *data)
{
	linted_error err;
	struct linted_controller_task_send *task;
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

void linted_controller_task_send_destroy(
    struct linted_controller_task_send *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

struct linted_async_task *linted_controller_task_send_to_async(
    struct linted_controller_task_send *task)
{
	return linted_io_task_write_to_async(task->parent);
}

struct linted_controller_task_send *
linted_controller_task_send_from_async(struct linted_async_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_async(task));
}

void *linted_controller_task_send_data(
    struct linted_controller_task_send *task)
{
	return task->data;
}

void linted_controller_task_send_prepare(
    struct linted_controller_task_send *task,
    union linted_async_ck task_ck, linted_controller controller,
    struct linted_controller_message const *message)
{
	{
		XDR xdr = {0};
		xdrmem_create(&xdr, task->message, sizeof task->message,
		              XDR_ENCODE);

		struct linted_controller_code code = {
		    .z_tilt = message->z_tilt,
		    .x_tilt = message->x_tilt,

		    .left = message->left,
		    .right = message->right,
		    .forward = message->forward,
		    .back = message->back,

		    .jumping = message->jumping};

		if (!xdr_linted_controller_code(&xdr, &code))
			LINTED_ASSERT(0);

		xdr_destroy(&xdr);
	}

	linted_io_task_write_prepare(task->parent, task_ck, controller,
	                             task->message,
	                             sizeof task->message);
}

linted_error linted_controller_task_recv_create(
    struct linted_controller_task_recv **taskp, void *data)
{
	linted_error err;
	struct linted_controller_task_recv *task;
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

void linted_controller_task_recv_destroy(
    struct linted_controller_task_recv *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

struct linted_async_task *linted_controller_task_recv_to_async(
    struct linted_controller_task_recv *task)
{
	return linted_io_task_read_to_async(task->parent);
}

struct linted_controller_task_recv *
linted_controller_task_recv_from_async(struct linted_async_task *task)
{
	return linted_io_task_read_data(
	    linted_io_task_read_from_async(task));
}

void *linted_controller_task_recv_data(
    struct linted_controller_task_recv *task)
{
	return task->data;
}

void linted_controller_task_recv_prepare(
    struct linted_controller_task_recv *task,
    union linted_async_ck task_ck, linted_controller controller)
{
	linted_io_task_read_prepare(task->parent, task_ck, controller,
	                            task->message,
	                            sizeof task->message);
}

linted_error
linted_controller_decode(struct linted_controller_task_recv const *task,
                         struct linted_controller_message *message)
{
	XDR xdr = {0};
	xdrmem_create(&xdr, (void *)task->message, sizeof task->message,
	              XDR_DECODE);

	struct linted_controller_code code = {0};
	if (!xdr_linted_controller_code(&xdr, &code))
		return EPROTO;

	xdr_destroy(&xdr);

	message->z_tilt = code.z_tilt;
	message->x_tilt = code.x_tilt;

	message->left = code.left;
	message->right = code.right;
	message->forward = code.forward;
	message->back = code.back;

	message->jumping = code.jumping;

	return 0;
}
