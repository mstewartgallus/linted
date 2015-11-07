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

/**
 * @file
 *
 * @todo Use proper marshalling between structures and byte arrays
 *       for `linted_admin` instead of just copying.
 */

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

static size_t null_list_size(char const *const *list);

static size_t process_mem_fields(struct mem_field *mem_fields,
                                 size_t fields_size);

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
		err = linted_mem_alloc(&xx, sizeof *request);
		if (err != 0)
			return err;
		request = xx;
	}

	char *raw = task->request;

	XDR xdr = {0};
	xdrmem_create(&xdr, raw, CHUNK_SIZE, XDR_DECODE);

	struct linted_admin_proto_request code = {0};
	if (!xdr_linted_admin_proto_request(&xdr, &code))
		assert(0);

	linted_admin_type type = code.type;
	switch (type) {
	case LINTED_ADMIN_ADD_UNIT: {
		struct linted_admin_add_unit_request *status =
		    &request->x.add_unit;
		struct linted_admin_proto_request_add_unit *codes =
		    &code.linted_admin_proto_request_u.add_unit;

		bool has_priority = codes->priority != 0;
		bool has_limit_no_file = codes->limit_no_file != 0;
		bool has_limit_msgqueue = codes->limit_msgqueue != 0;
		bool has_limit_locks = codes->limit_locks != 0;

		bool clone_newuser = codes->clone_newuser;
		bool clone_newpid = codes->clone_newpid;
		bool clone_newipc = codes->clone_newipc;
		bool clone_newnet = codes->clone_newnet;
		bool clone_newns = codes->clone_newns;
		bool clone_newuts = codes->clone_newuts;

		bool no_new_privs = codes->no_new_privs;

		rlim_t priority = has_priority ? *codes->priority : 0;
		rlim_t limit_no_file =
		    has_limit_no_file ? *codes->limit_no_file : 0;
		rlim_t limit_msgqueue =
		    has_limit_msgqueue ? *codes->limit_msgqueue : 0;
		rlim_t limit_locks =
		    has_limit_locks ? *codes->limit_locks : 0;

		size_t name_size = strlen(codes->name);
		size_t fstab_size = strlen(codes->fstab);
		size_t chdir_size = strlen(codes->chdir_path);

		size_t command_count = codes->command.command_len;
		size_t total_command_size = 0U;
		for (size_t ii = 0U; ii < command_count; ++ii) {
			total_command_size +=
			    strlen(codes->command.command_val[ii]) + 1U;
		}

		size_t env_whitelist_count =
		    codes->env_whitelist.env_whitelist_len;

		size_t total_env_whitelist_size = 0U;
		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			total_env_whitelist_size +=
			    strlen(codes->env_whitelist
			               .env_whitelist_val[ii]) +
			    1U;
		}

		enum { COMMAND,
		       ENV_WHITELIST,
		       NAME,
		       FSTAB,
		       CHDIR_PATH,
		       COMMAND_STORAGE,
		       ENV_WHITELIST_STORAGE };
		struct mem_field mem_sizes[] =
		    {[COMMAND] = {.size = (command_count + 1U) *
		                          sizeof(char *),
		                  .align = ALIGN(char *)},
		     [ENV_WHITELIST] = {.size =
		                            (env_whitelist_count + 1U) *
		                            sizeof(char *),
		                        .align = ALIGN(char *)},
		     [NAME] = {.size = name_size + 1U,
		               .align = ALIGN(char)},
		     [FSTAB] = {.size = fstab_size + 1U,
		                .align = ALIGN(char)},
		     [CHDIR_PATH] = {.size = chdir_size + 1U,
		                     .align = ALIGN(char)},
		     [COMMAND_STORAGE] = {.size = total_command_size,
		                          .align = ALIGN(char)},
		     [ENV_WHITELIST_STORAGE] = {
		         .size = total_env_whitelist_size,
		         .align = ALIGN(char)}};

		size_t total_size = process_mem_fields(
		    mem_sizes, LINTED_ARRAY_SIZE(mem_sizes));
		char *mem;
		{
			void *xx;
			err = linted_mem_alloc(&xx, total_size);
			if (err != 0)
				goto free_request;
			mem = xx;
		}

		char *name = &mem[mem_sizes[NAME].offset];
		char *fstab = &mem[mem_sizes[FSTAB].offset];
		char *chdir_path = &mem[mem_sizes[CHDIR_PATH].offset];
		char **command =
		    (void *)&mem[mem_sizes[COMMAND].offset];
		char **env_whitelist =
		    (void *)&mem[mem_sizes[ENV_WHITELIST].offset];

		char *command_storage =
		    &mem[mem_sizes[COMMAND_STORAGE].offset];
		for (size_t ii = 0U; ii < command_count; ++ii) {
			command[ii] = command_storage;
			command_storage +=
			    strlen(codes->command.command_val[ii]) + 1U;
		}

		char *env_whitelist_storage =
		    &mem[mem_sizes[ENV_WHITELIST_STORAGE].offset];
		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			env_whitelist[ii] = env_whitelist_storage;
			env_whitelist_storage +=
			    strlen(codes->env_whitelist
			               .env_whitelist_val[ii]) +
			    1U;
		}

		memcpy(name, codes->name, name_size);
		name[name_size] = '\0';

		memcpy(fstab, codes->fstab, fstab_size);
		fstab[fstab_size] = '\0';

		memcpy(chdir_path, codes->chdir_path, chdir_size);
		chdir_path[chdir_size] = '\0';

		for (size_t ii = 0U; ii < command_count; ++ii) {
			size_t arg_size =
			    strlen(codes->command.command_val[ii]);

			char *arg = command[ii];

			memcpy(arg, codes->command.command_val[ii],
			       arg_size);

			arg[arg_size] = '\0';
		}
		command[command_count] = 0;

		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			size_t arg_size = strlen(
			    codes->env_whitelist.env_whitelist_val[ii]);

			char *arg = env_whitelist[ii];

			memcpy(
			    arg,
			    codes->env_whitelist.env_whitelist_val[ii],
			    arg_size);

			arg[arg_size] = '\0';
		}
		env_whitelist[env_whitelist_count] = 0;

		status->type = type;

		status->priority = priority;
		status->limit_no_file = limit_no_file;
		status->limit_msgqueue = limit_msgqueue;
		status->limit_locks = limit_locks;

		status->has_priority = has_priority;
		status->has_limit_no_file = has_limit_no_file;
		status->has_limit_msgqueue = has_limit_msgqueue;
		status->has_limit_locks = has_limit_locks;

		status->clone_newuser = clone_newuser;
		status->clone_newpid = clone_newpid;
		status->clone_newipc = clone_newipc;
		status->clone_newnet = clone_newnet;
		status->clone_newns = clone_newns;
		status->clone_newuts = clone_newuts;

		status->no_new_privs = no_new_privs;

		status->name = name;
		status->fstab = fstab;
		status->chdir_path = chdir_path;
		status->command = (char const *const *)command;
		status->env_whitelist =
		    (char const *const *)env_whitelist;

		request->private_data = mem;
		break;
	}

	case LINTED_ADMIN_ADD_SOCKET: {
		struct linted_admin_add_socket_request *add_socket =
		    &request->x.add_socket;

		struct linted_admin_proto_request_add_socket *codes =
		    &code.linted_admin_proto_request_u.add_socket;

		int32_t fifo_size = codes->fifo_size;
		int32_t sock_type = codes->sock_type;
		size_t name_size = strlen(codes->name);
		size_t path_size = strlen(codes->path);

		enum { NAME, PATH };
		struct mem_field mem_sizes[] =
		    {[NAME] = {.size = name_size + 1U,
		               .align = ALIGN(char)},
		     [PATH] = {.size = path_size + 1U,
		               .align = ALIGN(char)}};
		size_t total_size = process_mem_fields(
		    mem_sizes, LINTED_ARRAY_SIZE(mem_sizes));
		char *mem;
		{
			void *xx;
			err = linted_mem_alloc(&xx, total_size);
			if (err != 0)
				goto free_request;
			mem = xx;
		}

		char *name = &mem[mem_sizes[NAME].offset];
		char *path = &mem[mem_sizes[PATH].offset];

		memcpy(name, codes->name, name_size);
		name[name_size] = '\0';

		memcpy(path, codes->path, path_size);
		path[path_size] = '\0';

		add_socket->type = type;

		add_socket->name = name;
		add_socket->path = path;
		add_socket->fifo_size = fifo_size;
		add_socket->sock_type = sock_type;

		request->private_data = mem;
		break;
	}

	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_request *status =
		    &request->x.status;
		struct linted_admin_proto_request_status *codes =
		    &code.linted_admin_proto_request_u.status;

		size_t size = strlen(codes->name);

		char *name;
		err = linted_str_dup_len(&name, codes->name, size);
		if (err != 0)
			goto free_request;

		status->type = type;
		status->name = name;
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_request *stop =
		    &request->x.stop;
		struct linted_admin_proto_request_stop *codes =
		    &code.linted_admin_proto_request_u.stop;

		size_t size = strlen(codes->name);

		char *name;
		err = linted_str_dup_len(&name, codes->name, size);
		if (err != 0)
			goto free_request;

		stop->type = type;
		stop->name = name;
		break;
	}

	default:
		LINTED_ASSERT(0);
	}

	xdr_free((xdrproc_t)xdr_linted_admin_proto_request,
	         (char *)&code);
	xdr_destroy(&xdr);

free_request:
	if (err != 0) {
		linted_mem_free(request);
		return err;
	}

	*outp = request;
	return 0;
}

void linted_admin_request_free(struct linted_admin_request *request)
{
	switch (request->x.type) {
	case LINTED_ADMIN_ADD_UNIT:
	case LINTED_ADMIN_ADD_SOCKET:
		linted_mem_free(request->private_data);
		break;

	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_request *status =
		    &request->x.status;
		linted_mem_free((void *)status->name);
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_request *stop =
		    &request->x.stop;
		linted_mem_free((void *)stop->name);
		break;
	}
	}

	linted_mem_free(request);
}

linted_error
linted_admin_in_send(linted_admin_in admin,
                     struct linted_admin_request const *request)
{
	linted_admin_type type = request->x.type;

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
		struct linted_admin_add_unit_request *status =
		    (void *)&request->x.add_unit;

		int64_t priority = status->priority;
		int64_t limit_no_file = status->limit_no_file;
		int64_t limit_msgqueue = status->limit_msgqueue;
		int64_t limit_locks = status->limit_locks;

		bool has_priority = status->has_priority;
		bool has_limit_no_file = status->has_limit_no_file;
		bool has_limit_msgqueue = status->has_limit_msgqueue;
		bool has_limit_locks = status->has_limit_locks;

		bool clone_newuser = status->clone_newuser;
		bool clone_newpid = status->clone_newpid;
		bool clone_newipc = status->clone_newipc;
		bool clone_newnet = status->clone_newnet;
		bool clone_newns = status->clone_newns;
		bool clone_newuts = status->clone_newuts;

		bool no_new_privs = status->no_new_privs;

		char const *name = status->name;

		code.linted_admin_proto_request_u.add_unit.name =
		    (char *)name;
		code.linted_admin_proto_request_u.add_unit.fstab =
		    (char *)status->fstab;
		code.linted_admin_proto_request_u.add_unit.chdir_path =
		    (char *)status->chdir_path;

		code.linted_admin_proto_request_u.add_unit.command
		    .command_len = null_list_size(status->command);
		code.linted_admin_proto_request_u.add_unit.command
		    .command_val = (char **)status->command;

		code.linted_admin_proto_request_u.add_unit.env_whitelist
		    .env_whitelist_len =
		    null_list_size(status->env_whitelist);
		code.linted_admin_proto_request_u.add_unit.env_whitelist
		    .env_whitelist_val = (char **)status->env_whitelist;

		if (has_priority)
			code.linted_admin_proto_request_u.add_unit
			    .priority = &priority;

		if (has_limit_no_file)
			code.linted_admin_proto_request_u.add_unit
			    .limit_no_file = &limit_no_file;

		if (has_limit_msgqueue)
			code.linted_admin_proto_request_u.add_unit
			    .limit_msgqueue = &limit_msgqueue;

		if (has_limit_locks)
			code.linted_admin_proto_request_u.add_unit
			    .limit_locks = &limit_locks;

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

		if (!xdr_linted_admin_proto_request(&xdr, &code))
			assert(0);
		break;
	}

	case LINTED_ADMIN_PROTO_ADD_SOCKET: {
		struct linted_admin_add_socket_request const *
		    add_socket = &request->x.add_socket;

		char const *name = add_socket->name;
		char const *path = add_socket->path;
		int32_t fifo_size = add_socket->fifo_size;
		linted_unit_socket_type sock_type =
		    add_socket->sock_type;

		code.linted_admin_proto_request_u.add_socket.name =
		    (char *)name;
		code.linted_admin_proto_request_u.add_socket.path =
		    (char *)path;
		code.linted_admin_proto_request_u.add_socket.fifo_size =
		    fifo_size;
		code.linted_admin_proto_request_u.add_socket.sock_type =
		    sock_type;

		if (!xdr_linted_admin_proto_request(&xdr, &code))
			assert(0);
		break;
	}

	case LINTED_ADMIN_PROTO_STATUS:
		code.linted_admin_proto_request_u.status.name =
		    (char *)request->x.status.name;
		if (!xdr_linted_admin_proto_request(&xdr, &code))
			assert(0);
		break;

	case LINTED_ADMIN_PROTO_STOP:
		code.linted_admin_proto_request_u.stop.name =
		    (char *)request->x.stop.name;

		if (!xdr_linted_admin_proto_request(&xdr, &code))
			assert(0);
		break;

	default:
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto free_raw;
	}

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
    union linted_admin_reply const *reply)
{
	linted_admin_type type = reply->type;

	char *tip = task->reply;
	memset(tip, 0, CHUNK_SIZE);

	XDR xdr = {0};
	xdrmem_create(&xdr, tip, CHUNK_SIZE, XDR_ENCODE);

	struct linted_admin_proto_reply code = {0};
	code.type = type;
	switch (type) {
	case LINTED_ADMIN_PROTO_ADD_UNIT:
	case LINTED_ADMIN_PROTO_ADD_SOCKET:
		break;

	case LINTED_ADMIN_PROTO_STATUS:
		code.linted_admin_proto_reply_u.status.is_up =
		    reply->status.is_up;
		break;

	case LINTED_ADMIN_PROTO_STOP:
		code.linted_admin_proto_reply_u.stop.was_up =
		    reply->stop.was_up;
		break;
	}

	if (!xdr_linted_admin_proto_reply(&xdr, &code))
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
                                   union linted_admin_reply *reply)
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

	struct linted_admin_proto_reply code = {0};
	if (!xdr_linted_admin_proto_reply(&xdr, &code))
		assert(0);

	reply->type = code.type;
	switch (code.type) {
	case LINTED_ADMIN_PROTO_ADD_UNIT:
	case LINTED_ADMIN_PROTO_ADD_SOCKET:
		break;

	case LINTED_ADMIN_PROTO_STATUS:
		reply->status.is_up =
		    code.linted_admin_proto_reply_u.status.is_up;
		break;

	case LINTED_ADMIN_PROTO_STOP:
		reply->stop.was_up =
		    code.linted_admin_proto_reply_u.stop.was_up;
		break;
	}

	xdr_free((xdrproc_t)xdr_linted_admin_proto_reply,
	         (char *)&code);
	xdr_destroy(&xdr);

free_chunk:
	linted_mem_free(chunk);
	return err;
}

static size_t null_list_size(char const *const *list)
{
	for (size_t ii = 0U;; ++ii)
		if (0 == list[ii])
			return ii;
}

static size_t process_mem_fields(struct mem_field *mem_fields,
                                 size_t fields_size)
{
	size_t total_size = 0U;
	for (size_t ii = 0U; ii < fields_size; ++ii) {
		size_t size = mem_fields[ii].size;
		size_t align = mem_fields[ii].align;
		total_size += 0U == total_size % align
		                  ? 0U
		                  : align - total_size % align;
		mem_fields[ii].offset = total_size;
		total_size += size;
	}
	return total_size;
}
