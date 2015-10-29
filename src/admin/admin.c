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

#include "linted/admin.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/log.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
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

size_t process_mem_fields(struct mem_field *mem_fields,
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

static size_t bytes_to_size(char const *bytes)
{
	size_t size;
	memcpy(&size, bytes, sizeof size);
	return size;
}

static size_t nth_size(char const *bytes, size_t ii)
{
	size_t size;
	memcpy(&size, bytes + ii * sizeof(size_t), sizeof size);
	return size;
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

	char *tip = task->request;

	linted_admin_type type;
	memcpy(&type, tip, sizeof type);
	tip += sizeof type;

	switch (type) {
	case LINTED_ADMIN_ADD_UNIT: {
		struct linted_admin_add_unit_request *status =
		    &request->x.add_unit;

		unsigned char bitfield[2U];
		memcpy(bitfield, tip, sizeof bitfield);
		tip += sizeof bitfield;

		bool has_priority = (bitfield[0U] & 1U) != 0U;
		bool has_limit_no_file =
		    (bitfield[0U] & (1U << 2U)) != 0;
		bool has_limit_msgqueue =
		    (bitfield[0U] & (1U << 3U)) != 0;
		bool has_limit_locks = (bitfield[0U] & (1U << 4U)) != 0;

		bool clone_newuser = (bitfield[0U] & (1U << 5U)) != 0;
		bool clone_newpid = (bitfield[0U] & (1U << 6U)) != 0;
		bool clone_newipc = (bitfield[0U] & (1U << 7U)) != 0;
		bool clone_newnet = (bitfield[0U] & (1U << 7U)) != 0;
		bool clone_newns = (bitfield[1U] & 1U) != 0;
		bool clone_newuts = (bitfield[1U] & (1U << 1U)) != 0;

		bool no_new_privs = (bitfield[1U] & (1U << 2U)) != 0U;

		size_t name_size;
		memcpy(&name_size, tip, sizeof name_size);
		tip += sizeof name_size;

		size_t fstab_size;
		memcpy(&fstab_size, tip, sizeof fstab_size);
		tip += sizeof fstab_size;

		size_t chdir_size;
		memcpy(&chdir_size, tip, sizeof chdir_size);
		tip += sizeof chdir_size;

		size_t command_count = bytes_to_size(tip);
		tip += sizeof command_count;

		char const *command_sizes_start = tip;
		size_t total_command_size = 0U;
		for (size_t ii = 0U; ii < command_count; ++ii) {
			total_command_size +=
			    nth_size(command_sizes_start, ii) + 1U;
			tip += sizeof(size_t);
		}

		size_t env_whitelist_count = bytes_to_size(tip);
		tip += sizeof env_whitelist_count;

		char const *env_whitelist_sizes_start = tip;
		size_t total_env_whitelist_size = 0U;
		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			total_env_whitelist_size +=
			    nth_size(env_whitelist_sizes_start, ii) +
			    1U;
			tip += sizeof(size_t);
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
			    nth_size(command_sizes_start, ii) + 1U;
		}

		char *env_whitelist_storage =
		    &mem[mem_sizes[ENV_WHITELIST_STORAGE].offset];
		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			env_whitelist[ii] = env_whitelist_storage;
			env_whitelist_storage +=
			    nth_size(env_whitelist_sizes_start, ii) +
			    1U;
		}

		memcpy(name, tip, name_size);
		name[name_size] = '\0';
		tip += name_size;

		memcpy(fstab, tip, fstab_size);
		fstab[fstab_size] = '\0';
		tip += fstab_size;

		memcpy(chdir_path, tip, chdir_size);
		chdir_path[chdir_size] = '\0';
		tip += chdir_size;

		for (size_t ii = 0U; ii < command_count; ++ii) {
			size_t arg_size =
			    nth_size(command_sizes_start, ii);

			char *arg = command[ii];

			memcpy(arg, tip, arg_size);
			tip += arg_size;

			arg[arg_size] = '\0';
		}
		command[command_count] = 0;

		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			size_t arg_size =
			    nth_size(env_whitelist_sizes_start, ii);

			char *arg = env_whitelist[ii];

			memcpy(arg, tip, arg_size);
			tip += arg_size;

			arg[arg_size] = '\0';
		}
		env_whitelist[env_whitelist_count] = 0;

		status->type = type;

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

		int32_t fifo_size;
		memcpy(&fifo_size, tip, sizeof fifo_size);
		tip += sizeof fifo_size;

		linted_unit_socket_type sock_type;
		memcpy(&sock_type, tip, sizeof sock_type);
		tip += sizeof sock_type;

		size_t name_size;
		memcpy(&name_size, tip, sizeof name_size);
		tip += sizeof name_size;

		size_t path_size;
		memcpy(&path_size, tip, sizeof path_size);
		tip += sizeof path_size;

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

		memcpy(name, tip, name_size);
		name[name_size] = '\0';
		tip += name_size;

		memcpy(path, tip, path_size);
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

		size_t size;
		memcpy(&size, tip, sizeof size);
		tip += sizeof size;

		char *name;
		err = linted_str_dup_len(&name, tip, size);
		if (err != 0)
			goto free_request;

		status->type = type;
		status->name = name;
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_request *stop =
		    &request->x.stop;

		size_t size;
		memcpy(&size, tip, sizeof size);
		tip += sizeof size;

		char *name;
		err = linted_str_dup_len(&name, tip, size);
		if (err != 0)
			goto free_request;

		stop->type = type;
		stop->name = name;
		break;
	}

	default:
		LINTED_ASSERT(0);
	}

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
	char *tip = raw;
	switch (type) {
	case LINTED_ADMIN_ADD_UNIT: {
		struct linted_admin_add_unit_request const *status =
		    &request->x.add_unit;

		memcpy(tip, &type, sizeof type);
		tip += sizeof type;

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

		unsigned char bitfield[2U] = {
		    has_priority | has_limit_no_file << 1U |
		        has_limit_msgqueue << 2U |
		        has_limit_locks << 3U |

		        clone_newuser << 4U | clone_newpid << 5U |
		        clone_newipc << 6U | clone_newnet << 7U,

		    clone_newns | clone_newuts << 1U |

		        no_new_privs << 2U};

		memcpy(tip, bitfield, sizeof bitfield);
		tip += sizeof bitfield;

		char const *namep = status->name;
		size_t name_size = strlen(namep);

		if (name_size > LINTED_UNIT_NAME_MAX) {
			err = LINTED_ERROR_INVALID_PARAMETER;
			goto free_raw;
		}

		memcpy(tip, &name_size, sizeof name_size);
		tip += sizeof name_size;

		char const *fstabp = status->fstab;
		size_t fstab_size = strlen(fstabp);

		memcpy(tip, &fstab_size, sizeof fstab_size);
		tip += sizeof fstab_size;

		char const *chdir_pathp = status->chdir_path;
		size_t chdir_path_size = strlen(chdir_pathp);

		memcpy(tip, &chdir_path_size, sizeof chdir_path_size);
		tip += sizeof chdir_path_size;

		char const *const *command = status->command;

		size_t command_count = 0U;
		for (; command[command_count] != 0U; ++command_count) {
		}

		memcpy(tip, &command_count, sizeof command_count);
		tip += sizeof command_count;

		for (size_t ii = 0U; ii < command_count; ++ii) {
			size_t arg_size = strlen(command[ii]);

			memcpy(tip, &arg_size, sizeof arg_size);
			tip += sizeof arg_size;
		}

		char const *const *env_whitelist =
		    status->env_whitelist;

		size_t env_whitelist_count = 0U;
		for (; env_whitelist[env_whitelist_count] != 0;
		     ++env_whitelist_count) {
		}

		memcpy(tip, &env_whitelist_count,
		       sizeof env_whitelist_count);
		tip += sizeof env_whitelist_count;

		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			size_t arg_size = strlen(env_whitelist[ii]);

			memcpy(tip, &arg_size, sizeof arg_size);
			tip += sizeof arg_size;
		}

		/* Now do actual stuff */

		memcpy(tip, namep, name_size);
		tip += name_size;

		memcpy(tip, fstabp, fstab_size);
		tip += fstab_size;

		memcpy(tip, chdir_pathp, chdir_path_size);
		tip += chdir_path_size;

		for (size_t ii = 0U; ii < command_count; ++ii) {
			char const *arg = command[ii];
			size_t arg_size = strlen(arg);

			memcpy(tip, arg, arg_size);
			tip += arg_size;
		}

		for (size_t ii = 0U; ii < env_whitelist_count; ++ii) {
			char const *arg = env_whitelist[ii];
			size_t arg_size = strlen(arg);

			memcpy(tip, arg, arg_size);
			tip += arg_size;
		}
		break;
	}

	case LINTED_ADMIN_ADD_SOCKET: {
		struct linted_admin_add_socket_request const *
		    add_socket = &request->x.add_socket;

		memcpy(tip, &type, sizeof type);
		tip += sizeof type;

		char const *namep = add_socket->name;
		char const *path = add_socket->path;
		int32_t fifo_size = add_socket->fifo_size;
		linted_unit_socket_type sock_type =
		    add_socket->sock_type;

		memcpy(tip, &fifo_size, sizeof fifo_size);
		tip += sizeof fifo_size;

		memcpy(tip, &sock_type, sizeof sock_type);
		tip += sizeof sock_type;

		size_t name_size = strlen(namep);
		size_t path_size = strlen(path);

		if (name_size > LINTED_UNIT_NAME_MAX) {
			err = LINTED_ERROR_INVALID_PARAMETER;
			goto free_raw;
		}

		memcpy(tip, &name_size, sizeof name_size);
		tip += sizeof name_size;

		memcpy(tip, &path_size, sizeof path_size);
		tip += sizeof path_size;

		/* Now do actual stuff */

		memcpy(tip, namep, name_size);
		tip += name_size;

		memcpy(tip, path, path_size);
		break;
	}

	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_request const *status =
		    (void *)request;
		char const *namep = status->name;

		size_t size = strlen(namep);
		if (size > LINTED_UNIT_NAME_MAX)
			return LINTED_ERROR_INVALID_PARAMETER;

		memcpy(tip, &type, sizeof type);
		tip += sizeof type;

		memcpy(tip, &size, sizeof size);
		tip += sizeof size;

		memcpy(tip, namep, size);
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_request const *stop =
		    (void *)request;
		char const *namep = stop->name;

		size_t size = strlen(namep);
		if (size > LINTED_UNIT_NAME_MAX) {
			err = LINTED_ERROR_INVALID_PARAMETER;
			goto free_raw;
		}

		memcpy(tip, &type, sizeof type);
		tip += sizeof type;

		memcpy(tip, &size, sizeof size);
		tip += sizeof size;

		memcpy(tip, namep, size);
		break;
	}

	default:
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto free_raw;
	}

	err = linted_io_write_all(admin, 0, raw, CHUNK_SIZE);

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

	memcpy(tip, &type, sizeof type);
	tip += sizeof type;

	switch (type) {
	case LINTED_ADMIN_ADD_UNIT:
	case LINTED_ADMIN_ADD_SOCKET:
		break;

	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_reply const *status =
		    (void *)reply;
		bool is_up = status->is_up;

		memcpy(tip, &is_up, sizeof is_up);
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_reply const *stop =
		    (void *)reply;
		bool was_up = stop->was_up;

		memcpy(tip, &was_up, sizeof was_up);
		break;
	}

	default:
		LINTED_ASSERT(0);
	}

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
	linted_error err;

	char chunk[CHUNK_SIZE] = {0};

	size_t size;
	{
		size_t xx;
		err =
		    linted_io_read_all(admin, &xx, chunk, sizeof chunk);
		if (err != 0)
			return err;
		size = xx;
	}

	/* Sent malformed input */
	if (size != sizeof chunk)
		return EPROTO;

	memcpy(reply, chunk, sizeof *reply);

	return 0;
}
