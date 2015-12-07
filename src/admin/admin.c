/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "admin.h"

#include "lntd/admin.h"

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/util.h"

#include <errno.h>
#include <rpc/xdr.h>
#include <stddef.h>
#include <string.h>

LNTD_STATIC_ASSERT(sizeof(struct lntd_admin_request) ==
                   sizeof(struct lntd_admin_proto_request));

LNTD_STATIC_ASSERT(sizeof(struct lntd_admin_reply) ==
                   sizeof(struct lntd_admin_proto_reply));

#define CHUNK_SIZE 4096U

#define ALIGN(X)                                                       \
	(sizeof(struct {                                               \
		char _a;                                               \
		X _b;                                                  \
	}) -                                                           \
	 sizeof(X))

struct lntd_admin_in_task_recv {
	struct lntd_io_task_read *parent;
	void *data;
	char request[CHUNK_SIZE];
};

struct lntd_admin_out_task_send {
	struct lntd_io_task_write *data;
	void *parent;
	char reply[CHUNK_SIZE];
};

lntd_error
lntd_admin_in_task_recv_create(struct lntd_admin_in_task_recv **taskp,
                               void *data)
{
	lntd_error err;
	struct lntd_admin_in_task_recv *task;
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

	memset(task->request, 0, CHUNK_SIZE);

	*taskp = task;
	return 0;
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_admin_in_task_recv_destroy(
    struct lntd_admin_in_task_recv *task)
{
	lntd_io_task_read_destroy(task->parent);
	lntd_mem_free(task);
}

void *lntd_admin_in_task_recv_data(struct lntd_admin_in_task_recv *task)
{
	return task->data;
}

lntd_error
lntd_admin_in_task_recv_request(struct lntd_admin_request **outp,
                                struct lntd_admin_in_task_recv *task)
{
	lntd_error err = 0;

	struct lntd_admin_request *request;
	{
		void *xx;
		err = lntd_mem_alloc_zeroed(&xx, sizeof *request);
		if (err != 0)
			return err;
		request = xx;
	}

	char *raw = task->request;

	XDR xdr = {0};
	xdrmem_create(&xdr, raw, CHUNK_SIZE, XDR_DECODE);

	if (!xdr_lntd_admin_proto_request(&xdr, (void *)request))
		LNTD_ASSERT(0);

	xdr_destroy(&xdr);

	*outp = request;
	return 0;
}

void lntd_admin_request_free(struct lntd_admin_request *request)
{
	xdr_free((xdrproc_t)xdr_lntd_admin_proto_request,
	         (char *)request);
	lntd_mem_free(request);
}

lntd_error lntd_admin_in_send(lntd_admin_in admin,
                              struct lntd_admin_request const *request)
{
	lntd_error err = 0;

	char *raw;
	{
		void *xx;
		err = lntd_mem_alloc_zeroed(&xx, CHUNK_SIZE);
		if (err != 0)
			return err;
		raw = xx;
	}

	XDR xdr = {0};
	xdrmem_create(&xdr, raw, CHUNK_SIZE, XDR_ENCODE);

	if (!xdr_lntd_admin_proto_request(&xdr, (void *)request))
		LNTD_ASSERT(0);

	err = lntd_io_write_all(admin, 0, raw, CHUNK_SIZE);

	xdr_destroy(&xdr);

	lntd_mem_free(raw);

	return err;
}

struct lntd_async_task *
lntd_admin_in_task_recv_prepare(struct lntd_admin_in_task_recv *task,
                                union lntd_async_ck task_ck,
                                void *userstate, lntd_ko ko)
{
	return lntd_io_task_read_prepare(task->parent, task_ck,
	                                 userstate, ko, task->request,
	                                 sizeof task->request);
}

struct lntd_async_task *
lntd_admin_in_task_recv_to_async(struct lntd_admin_in_task_recv *task)
{
	return lntd_io_task_read_to_async(task->parent);
}

lntd_error
lntd_admin_out_task_send_create(struct lntd_admin_out_task_send **taskp,
                                void *data)
{
	lntd_error err;
	struct lntd_admin_out_task_send *task;
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

	memset(task->reply, 0, CHUNK_SIZE);

	*taskp = task;
	return 0;
free_task:
	lntd_mem_free(task);
	return err;
}

void lntd_admin_out_task_send_destroy(
    struct lntd_admin_out_task_send *task)
{
	lntd_io_task_write_destroy(task->parent);
	lntd_mem_free(task);
}

void *
lntd_admin_out_task_send_data(struct lntd_admin_out_task_send *task)
{
	return task->data;
}

struct lntd_async_task *lntd_admin_out_task_send_prepare(
    struct lntd_admin_out_task_send *task, union lntd_async_ck task_ck,
    void *userstate, lntd_ko ko, struct lntd_admin_reply const *reply)
{
	char *tip = task->reply;
	memset(tip, 0, CHUNK_SIZE);

	XDR xdr = {0};
	xdrmem_create(&xdr, tip, CHUNK_SIZE, XDR_ENCODE);

	if (!xdr_lntd_admin_proto_reply(&xdr, (void *)reply))
		LNTD_ASSERT(0);

	xdr_destroy(&xdr);

	return lntd_io_task_write_prepare(task->parent, task_ck,
	                                  userstate, ko, task->reply,
	                                  sizeof task->reply);
}

struct lntd_async_task *
lntd_admin_out_task_send_to_async(struct lntd_admin_out_task_send *task)
{
	return lntd_io_task_write_to_async(task->parent);
}

lntd_error lntd_admin_out_recv(lntd_admin_out admin,
                               struct lntd_admin_reply *reply)
{
	lntd_error err = 0;

	char *chunk;
	{
		void *xx;
		err = lntd_mem_alloc_zeroed(&xx, CHUNK_SIZE);
		if (err != 0)
			return err;
		chunk = xx;
	}

	size_t size;
	{
		size_t xx;
		err = lntd_io_read_all(admin, &xx, chunk, CHUNK_SIZE);
		if (err != 0)
			goto free_chunk;
		size = xx;
	}

	/* Sent malformed input */
	if (size != CHUNK_SIZE) {
		err = EPROTO;
		goto free_chunk;
	}

	XDR xdr = {0};
	xdrmem_create(&xdr, chunk, CHUNK_SIZE, XDR_DECODE);

	if (!xdr_lntd_admin_proto_reply(&xdr, (void *)reply))
		LNTD_ASSERT(0);

	xdr_destroy(&xdr);

free_chunk:
	lntd_mem_free(chunk);
	return err;
}
