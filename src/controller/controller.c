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

#include "lntd/controller.h"

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/mem.h"
#include "lntd/rpc.h"
#include "lntd/util.h"

#include <errno.h>
#include <rpc/xdr.h>

struct lntd_controller_task_send {
	struct lntd_io_task_write *parent;
	void *data;
	char message[LNTD_RPC_INT32_SIZE + LNTD_RPC_INT32_SIZE +
	             LNTD_RPC_INT32_SIZE * 5U];
};

struct lntd_controller_task_recv {
	struct lntd_io_task_read *parent;
	void *data;
	char message[LNTD_RPC_INT32_SIZE + LNTD_RPC_INT32_SIZE +
	             LNTD_RPC_INT32_SIZE * 5U];
};

lntd_error lntd_controller_task_send_create(
    struct lntd_controller_task_send **taskp, void *data)
{
	lntd_error err;
	struct lntd_controller_task_send *task;
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

void lntd_controller_task_send_destroy(
    struct lntd_controller_task_send *task)
{
	lntd_io_task_write_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *lntd_controller_task_send_to_async(
    struct lntd_controller_task_send *task)
{
	return lntd_io_task_write_to_async(task->parent);
}

void *
lntd_controller_task_send_data(struct lntd_controller_task_send *task)
{
	return task->data;
}

struct lntd_async_task *lntd_controller_task_send_prepare(
    struct lntd_controller_task_send *task, union lntd_async_ck task_ck,
    void *userstate, lntd_controller controller,
    struct lntd_controller_message const *message)
{
	{
		XDR xdr = {0};
		xdrmem_create(&xdr, task->message, sizeof task->message,
		              XDR_ENCODE);

		struct lntd_controller_code code = {
		    .z_tilt = message->z_tilt,
		    .x_tilt = message->x_tilt,

		    .left = message->left,
		    .right = message->right,
		    .forward = message->forward,
		    .back = message->back,

		    .jumping = message->jumping};

		if (!xdr_lntd_controller_code(&xdr, &code))
			LNTD_ASSERT(0);

		xdr_destroy(&xdr);
	}

	return lntd_io_task_write_prepare(
	    task->parent, task_ck, userstate, controller, task->message,
	    sizeof task->message);
}

lntd_error lntd_controller_task_recv_create(
    struct lntd_controller_task_recv **taskp, void *data)
{
	lntd_error err;
	struct lntd_controller_task_recv *task;
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

void lntd_controller_task_recv_destroy(
    struct lntd_controller_task_recv *task)
{
	lntd_io_task_read_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *lntd_controller_task_recv_to_async(
    struct lntd_controller_task_recv *task)
{
	return lntd_io_task_read_to_async(task->parent);
}

void *
lntd_controller_task_recv_data(struct lntd_controller_task_recv *task)
{
	return task->data;
}

struct lntd_async_task *lntd_controller_task_recv_prepare(
    struct lntd_controller_task_recv *task, union lntd_async_ck task_ck,
    void *userstate, lntd_controller controller)
{
	return lntd_io_task_read_prepare(
	    task->parent, task_ck, userstate, controller, task->message,
	    sizeof task->message);
}

lntd_error
lntd_controller_decode(struct lntd_controller_task_recv const *task,
                       struct lntd_controller_message *message)
{
	XDR xdr = {0};
	xdrmem_create(&xdr, (void *)task->message, sizeof task->message,
	              XDR_DECODE);

	struct lntd_controller_code code = {0};
	if (!xdr_lntd_controller_code(&xdr, &code))
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
