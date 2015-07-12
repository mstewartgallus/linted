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
#include "config.h"

#include "linted/admin.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"

#include <errno.h>
#include <stddef.h>
#include <string.h>

/**
 * @todo Use proper marshalliing between structures and byte arrays
 *       for linted_admin instead of just copying.
 */

#define CHUNK_SIZE 512U

LINTED_STATIC_ASSERT(
    LINTED_FIELD_SIZEOF(struct linted_admin_status_request, type) +
        LINTED_FIELD_SIZEOF(struct linted_admin_status_request, size) +
        LINTED_FIELD_SIZEOF(struct linted_admin_status_request, name) <
    CHUNK_SIZE);

LINTED_STATIC_ASSERT(
    LINTED_FIELD_SIZEOF(struct linted_admin_status_request, type) +
        LINTED_FIELD_SIZEOF(struct linted_admin_status_request, size) +
        LINTED_FIELD_SIZEOF(struct linted_admin_status_request, name) <
    CHUNK_SIZE);

struct linted_admin_in_task_read
{
	struct linted_io_task_read *parent;
	void *data;
	char request[CHUNK_SIZE];
};

struct linted_admin_out_task_write
{
	struct linted_io_task_write *data;
	void *parent;
	char reply[sizeof(union linted_admin_reply)];
};

linted_error linted_admin_in_task_read_create(
    struct linted_admin_in_task_read **taskp, void *data)
{
	linted_error errnum;
	struct linted_admin_in_task_read *task;
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

void linted_admin_in_task_read_destroy(
    struct linted_admin_in_task_read *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_admin_in_task_read_data(struct linted_admin_in_task_read *task)
{
	return task->data;
}

linted_admin_in
linted_admin_in_task_read_ko(struct linted_admin_in_task_read *task)
{
	return linted_io_task_read_ko(task->parent);
}

void linted_admin_in_task_read_request(
    struct linted_admin_in_task_read *task,
    union linted_admin_request *outp)
{
	char raw[CHUNK_SIZE] = {0};
	memcpy(raw, task->request, sizeof raw);

	char *tip = raw;

	linted_admin_type type;
	memcpy(&type, tip, sizeof type);
	tip += sizeof type;

	switch (type) {
	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_request *status =
		    (void *)outp;
		char *namep = status->name;

		size_t size;

		memcpy(&size, tip, sizeof size);
		tip += sizeof size;

		memcpy(namep, tip, size);

		status->type = type;
		status->size = size;
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_request *stop = (void *)outp;
		char *namep = stop->name;

		size_t size;

		memcpy(&size, tip, sizeof size);
		tip += sizeof size;

		memcpy(namep, tip, size);

		stop->type = type;
		stop->size = size;
		break;
	}

	default:
		assert(0);
	}
}

linted_error
linted_admin_in_write(linted_admin_in admin,
                      union linted_admin_request const *request)
{
	linted_admin_type type = request->type;

	char raw[CHUNK_SIZE] = {0};
	switch (type) {
	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_request const *status =
		    (void *)request;
		char const *namep = status->name;

		size_t size;

		size = status->size;
		if (size > LINTED_UNIT_NAME_MAX)
			return EINVAL;

		char *tip = raw;

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

		size_t size;

		size = stop->size;
		if (size > LINTED_UNIT_NAME_MAX)
			return EINVAL;

		char *tip = raw;

		memcpy(tip, &type, sizeof type);
		tip += sizeof type;

		memcpy(tip, &size, sizeof size);
		tip += sizeof size;

		memcpy(tip, namep, size);
		break;
	}

	default:
		return EINVAL;
	}

	return linted_io_write_all(admin, 0, raw, sizeof raw);
}

void linted_admin_in_task_read_prepare(
    struct linted_admin_in_task_read *task, unsigned task_action,
    linted_ko ko)
{
	linted_io_task_read_prepare(task->parent, task_action, ko,
	                            task->request,
	                            sizeof task->request);
}

struct linted_asynch_task *linted_admin_in_task_read_to_asynch(
    struct linted_admin_in_task_read *task)
{
	return linted_io_task_read_to_asynch(task->parent);
}

struct linted_admin_in_task_read *
linted_admin_in_task_read_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_read_data(
	    linted_io_task_read_from_asynch(task));
}

linted_error linted_admin_out_task_write_create(
    struct linted_admin_out_task_write **taskp, void *data)
{
	linted_error errnum;
	struct linted_admin_out_task_write *task;
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

void linted_admin_out_task_write_destroy(
    struct linted_admin_out_task_write *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_admin_out_task_write_data(
    struct linted_admin_out_task_write *task)
{
	return task->data;
}

void linted_admin_out_task_write_prepare(
    struct linted_admin_out_task_write *task, unsigned task_action,
    linted_ko ko, union linted_admin_reply const *reply)
{
	linted_io_task_write_prepare(task->parent, task_action, ko,
	                             (char const *)&task->reply,
	                             sizeof task->reply);
	memcpy(task->reply, reply, sizeof *reply);
}

struct linted_asynch_task *linted_admin_out_task_write_to_asynch(
    struct linted_admin_out_task_write *task)
{
	return linted_io_task_write_to_asynch(task->parent);
}

struct linted_admin_out_task_write *
linted_admin_out_task_write_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_asynch(task));
}

linted_error linted_admin_out_read(linted_admin_out admin,
                                   union linted_admin_reply *reply)
{
	linted_error errnum;

	size_t size;
	{
		size_t xx;
		errnum = linted_io_read_all(admin, &xx, reply,
		                            sizeof *reply);
		if (errnum != 0)
			return errnum;
		size = xx;
	}

	/* Sent malformed input */
	if (size != sizeof *reply)
		return EPROTO;

	return 0;
}
