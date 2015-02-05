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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "config.h"

#include "linted/admin.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <string.h>

struct linted_admin_in_task_read
{
	struct linted_io_task_read *parent;
	void *data;
	union linted_admin_request request;
};

struct linted_admin_out_task_write
{
	struct linted_io_task_write *data;
	void *parent;
	union linted_admin_reply reply;
};

linted_error
linted_admin_in_task_read_create(struct linted_admin_in_task_read **taskp,
                                 void *data)
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

void linted_admin_in_task_read_destroy(struct linted_admin_in_task_read *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_admin_in_task_read_data(struct linted_admin_in_task_read *task)
{
	return task->data;
}

linted_admin_in
linted_admin_in_task_read_ko(struct linted_admin_in_task_read *task)
{
	return linted_io_task_read_ko(task->parent);
}

union linted_admin_request const *
linted_admin_in_task_read_request(struct linted_admin_in_task_read *task)
{
	return &task->request;
}

void linted_admin_in_task_read_prepare(struct linted_admin_in_task_read *task,
                                       unsigned task_action, linted_ko ko)
{
	linted_io_task_read_prepare(task->parent, task_action, ko,
	                            (char *)&task->request,
	                            sizeof task->request);
}

struct linted_asynch_task *
linted_admin_in_task_read_to_asynch(struct linted_admin_in_task_read *task)
{
	return linted_io_task_read_to_asynch(task->parent);
}

struct linted_admin_in_task_read *
linted_admin_in_task_read_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_read_data(linted_io_task_read_from_asynch(task));
}

linted_error
linted_admin_out_task_write_create(struct linted_admin_out_task_write **taskp,
                                   void *data)
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

void
linted_admin_out_task_write_destroy(struct linted_admin_out_task_write *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void *linted_admin_out_task_write_data(struct linted_admin_out_task_write *task)
{
	return task->data;
}

void
linted_admin_out_task_write_prepare(struct linted_admin_out_task_write *task,
                                    unsigned task_action, linted_ko ko,
                                    union linted_admin_reply const *reply)
{
	linted_io_task_write_prepare(task->parent, task_action, ko,
	                             (char const *)&task->reply,
	                             sizeof task->reply);
	task->reply = *reply;
}

struct linted_asynch_task *
linted_admin_out_task_write_to_asynch(struct linted_admin_out_task_write *task)
{
	return linted_io_task_write_to_asynch(task->parent);
}

struct linted_admin_out_task_write *
linted_admin_out_task_write_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_asynch(task));
}

linted_error linted_admin_in_write(linted_admin_in admin,
                                   union linted_admin_request const *request)
{
	return linted_io_write_all(admin, 0, request, sizeof *request);
}

linted_error linted_admin_out_read(linted_admin_out admin,
                                   union linted_admin_reply *reply)
{
	linted_error errnum;

	size_t size;
	{
		size_t xx;
		errnum = linted_io_read_all(admin, &xx, reply, sizeof *reply);
		if (errnum != 0)
			return errnum;
		size = xx;
	}

	/* Sent malformed input */
	if (size != sizeof *reply)
		return EPROTO;

	return 0;
}
