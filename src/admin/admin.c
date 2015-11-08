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

#include "linted/admin.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

LINTED_STATIC_ASSERT(sizeof(struct linted_admin_request) ==
                     sizeof(struct linted_admin_proto_request));

LINTED_STATIC_ASSERT(sizeof(struct linted_admin_reply) ==
                     sizeof(struct linted_admin_proto_reply));

#define CHUNK_SIZE 4096U

#define ALIGN(X)                                                       \
	(sizeof(struct {                                               \
		char _a;                                               \
		X _b;                                                  \
	}) -                                                           \
	 sizeof(X))

struct linted_admin_in_task_recv {
	struct linted_io_task_read *parent;
	void *data;
	char request[CHUNK_SIZE];
};

struct linted_admin_out_task_send {
	struct linted_io_task_write *data;
	void *parent;
	char reply[CHUNK_SIZE];
};

struct mem_field {
	size_t size;
	size_t align;
	size_t offset;
};

linted_error linted_admin_in_task_recv_create(
    struct linted_admin_in_task_recv **taskp, void *data)
{
	linted_error err;
	struct linted_admin_in_task_recv *task;
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

	memset(task->request, 0, CHUNK_SIZE);

	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return err;
}

void linted_admin_in_task_recv_destroy(
    struct linted_admin_in_task_recv *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_admin_in_task_recv_data(struct linted_admin_in_task_recv *task)
{
	return task->data;
}

linted_error linted_admin_in_task_recv_request(
    struct linted_admin_request **outp,
    struct linted_admin_in_task_recv *task)
{
	linted_error err = 0;

	struct linted_admin_request *request;
	{
		void *xx;
		err = linted_mem_alloc_zeroed(&xx, sizeof *request);
		if (err != 0)
			return err;
		request = xx;
	}

	char *raw = task->request;

	XDR xdr = {0};
	xdrmem_create(&xdr, raw, CHUNK_SIZE, XDR_DECODE);

	if (!xdr_linted_admin_proto_request(&xdr, (void *)request))
		assert(0);

	xdr_destroy(&xdr);

	*outp = request;
	return 0;
}

void linted_admin_request_free(struct linted_admin_request *request)
{
	xdr_free((xdrproc_t)xdr_linted_admin_proto_request,
	         (char *)request);
	linted_mem_free(request);
}

linted_error
linted_admin_in_send(linted_admin_in admin,
                     struct linted_admin_request const *request)
{
	linted_admin_type type = request->type;

	linted_error err = 0;

	char *raw;
	{
		void *xx;
		err = linted_mem_alloc_zeroed(&xx, CHUNK_SIZE);
		if (err != 0)
			return err;
		raw = xx;
	}

	XDR xdr = {0};
	xdrmem_create(&xdr, raw, CHUNK_SIZE, XDR_ENCODE);

	struct linted_admin_proto_request code = {0};
	code.type = type;
	switch (type) {
	case LINTED_ADMIN_PROTO_ADD_UNIT: {
		struct linted_admin_request_add_unit *status =
		    (void *)&request->linted_admin_request_u.add_unit;

		int_least64_t *priority = status->priority;
		int_least64_t *limit_no_file = status->limit_no_file;
		int_least64_t *limit_msgqueue = status->limit_msgqueue;
		int_least64_t *limit_locks = status->limit_locks;

		linted_admin_bool clone_newuser = status->clone_newuser;
		linted_admin_bool clone_newpid = status->clone_newpid;
		linted_admin_bool clone_newipc = status->clone_newipc;
		linted_admin_bool clone_newnet = status->clone_newnet;
		linted_admin_bool clone_newns = status->clone_newns;
		linted_admin_bool clone_newuts = status->clone_newuts;

		linted_admin_bool no_new_privs = status->no_new_privs;

		char *name = status->name;
		char *fstab = status->fstab;
		char *chdir_path = status->chdir_path;

		code.linted_admin_proto_request_u.add_unit.name = name;
		code.linted_admin_proto_request_u.add_unit.fstab =
		    fstab;
		code.linted_admin_proto_request_u.add_unit.chdir_path =
		    chdir_path;

		code.linted_admin_proto_request_u.add_unit.command
		    .command_len = status->command.command_len;

		code.linted_admin_proto_request_u.add_unit.command
		    .command_val = status->command.command_val;

		code.linted_admin_proto_request_u.add_unit.env_whitelist
		    .env_whitelist_len =
		    status->env_whitelist.env_whitelist_len;
		code.linted_admin_proto_request_u.add_unit.env_whitelist
		    .env_whitelist_val =
		    status->env_whitelist.env_whitelist_val;

		code.linted_admin_proto_request_u.add_unit.priority =
		    priority;
		code.linted_admin_proto_request_u.add_unit
		    .limit_no_file = limit_no_file;
		code.linted_admin_proto_request_u.add_unit
		    .limit_msgqueue = limit_msgqueue;
		code.linted_admin_proto_request_u.add_unit.limit_locks =
		    limit_locks;

		code.linted_admin_proto_request_u.add_unit
		    .clone_newuser = clone_newuser;
		code.linted_admin_proto_request_u.add_unit
		    .clone_newpid = clone_newpid;
		code.linted_admin_proto_request_u.add_unit
		    .clone_newipc = clone_newipc;
		code.linted_admin_proto_request_u.add_unit
		    .clone_newnet = clone_newnet;
		code.linted_admin_proto_request_u.add_unit.clone_newns =
		    clone_newns;
		code.linted_admin_proto_request_u.add_unit
		    .clone_newuts = clone_newuts;

		code.linted_admin_proto_request_u.add_unit
		    .no_new_privs = no_new_privs;
		break;
	}

	case LINTED_ADMIN_PROTO_ADD_SOCKET: {
		struct linted_admin_request_add_socket const *
		    add_socket =
		        &request->linted_admin_request_u.add_socket;

		char *name = add_socket->name;
		char *path = add_socket->path;
		int32_t fifo_size = add_socket->fifo_size;
		linted_unit_socket_type sock_type =
		    add_socket->sock_type;

		code.linted_admin_proto_request_u.add_socket.name =
		    name;
		code.linted_admin_proto_request_u.add_socket.path =
		    path;
		code.linted_admin_proto_request_u.add_socket.fifo_size =
		    fifo_size;
		code.linted_admin_proto_request_u.add_socket.sock_type =
		    sock_type;
		break;
	}

	case LINTED_ADMIN_PROTO_STATUS:
		code.linted_admin_proto_request_u.status.name =
		    (char *)request->linted_admin_request_u.status.name;
		break;

	case LINTED_ADMIN_PROTO_STOP:
		code.linted_admin_proto_request_u.stop.name =
		    (char *)request->linted_admin_request_u.stop.name;
		break;

	default:
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto free_raw;
	}

	if (!xdr_linted_admin_proto_request(&xdr, &code))
		assert(0);

	err = linted_io_write_all(admin, 0, raw, CHUNK_SIZE);

	xdr_destroy(&xdr);

free_raw:
	linted_mem_free(raw);

	return err;
}

void linted_admin_in_task_recv_prepare(
    struct linted_admin_in_task_recv *task,
    union linted_async_ck task_ck, linted_ko ko)
{
	linted_io_task_read_prepare(task->parent, task_ck, ko,
	                            task->request,
	                            sizeof task->request);
}

struct linted_async_task *linted_admin_in_task_recv_to_async(
    struct linted_admin_in_task_recv *task)
{
	return linted_io_task_read_to_async(task->parent);
}

struct linted_admin_in_task_recv *
linted_admin_in_task_recv_from_async(struct linted_async_task *task)
{
	return linted_io_task_read_data(
	    linted_io_task_read_from_async(task));
}

linted_error linted_admin_out_task_send_create(
    struct linted_admin_out_task_send **taskp, void *data)
{
	linted_error err;
	struct linted_admin_out_task_send *task;
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

	memset(task->reply, 0, CHUNK_SIZE);

	*taskp = task;
	return 0;
free_task:
	linted_mem_free(task);
	return err;
}

void linted_admin_out_task_send_destroy(
    struct linted_admin_out_task_send *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_admin_out_task_send_data(struct linted_admin_out_task_send *task)
{
	return task->data;
}

void linted_admin_out_task_send_prepare(
    struct linted_admin_out_task_send *task,
    union linted_async_ck task_ck, linted_ko ko,
    struct linted_admin_reply const *reply)
{
	char *tip = task->reply;
	memset(tip, 0, CHUNK_SIZE);

	XDR xdr = {0};
	xdrmem_create(&xdr, tip, CHUNK_SIZE, XDR_ENCODE);

	if (!xdr_linted_admin_proto_reply(&xdr, (void *)reply))
		assert(0);

	xdr_destroy(&xdr);

	linted_io_task_write_prepare(task->parent, task_ck, ko,
	                             task->reply, sizeof task->reply);
}

struct linted_async_task *linted_admin_out_task_send_to_async(
    struct linted_admin_out_task_send *task)
{
	return linted_io_task_write_to_async(task->parent);
}

struct linted_admin_out_task_send *
linted_admin_out_task_send_from_async(struct linted_async_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_async(task));
}

linted_error linted_admin_out_recv(linted_admin_out admin,
                                   struct linted_admin_reply *reply)
{
	linted_error err = 0;

	char *chunk;
	{
		void *xx;
		err = linted_mem_alloc_zeroed(&xx, CHUNK_SIZE);
		if (err != 0)
			return err;
		chunk = xx;
	}

	size_t size;
	{
		size_t xx;
		err = linted_io_read_all(admin, &xx, chunk, CHUNK_SIZE);
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

	if (!xdr_linted_admin_proto_reply(&xdr, (void *)reply))
		assert(0);

	xdr_destroy(&xdr);

free_chunk:
	linted_mem_free(chunk);
	return err;
}
