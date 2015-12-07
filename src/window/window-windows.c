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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "lntd/window.h"

#include "lntd/async.h"
#include "lntd/io.h"
#include "lntd/mem.h"
#include "lntd/rpc.h"
#include "lntd/util.h"

#include <errno.h>
#include <stdint.h>

#include <windows.h>

lntd_error lntd_window_write(lntd_window window, uint_fast32_t in)
{
	char buf[LNTD_RPC_UINT32_SIZE];
	lntd_rpc_pack_uint32(in, buf);

	OVERLAPPED overlapped = {0};
	overlapped.Offset = 0;
	overlapped.OffsetHigh = 0;

	if (!WriteFile(window, buf, sizeof buf, 0, &overlapped)) {
		lntd_error err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}

lntd_error lntd_window_read(lntd_window window, uint_fast32_t *outp)
{
	char buf[LNTD_RPC_UINT32_SIZE];

	size_t bytes_read;
	{
		DWORD xx;
		OVERLAPPED overlapped = {0};
		overlapped.Offset = 0;
		overlapped.OffsetHigh = 0;

		if (!ReadFile(window, buf, sizeof buf, &xx,
		              &overlapped)) {
			lntd_error err =
			    HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);
			return err;
		}
		bytes_read = xx;
	}

	if (bytes_read != sizeof buf)
		return EPROTO;

	*outp = lntd_rpc_unpack_uint32(buf);
	return 0;
}

struct lntd_window_task_notify {
	struct lntd_io_task_write *parent;
	void *data;
};

struct lntd_window_task_watch {
	struct lntd_io_task_read *parent;
	void *data;
	char dummy[1U];
};

lntd_error
lntd_window_task_watch_create(struct lntd_window_task_watch **taskp,
                              void *data)
{
	lntd_error err;
	struct lntd_window_task_watch *task;
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

void lntd_window_task_watch_destroy(struct lntd_window_task_watch *task)
{
	lntd_io_task_read_destroy(task->parent);
	lntd_mem_free(task);
}

struct lntd_async_task *
lntd_window_task_watch_prepare(struct lntd_window_task_watch *task,
                               union lntd_async_ck task_ck,
                               void *userstate, lntd_ko notifier)
{
	return lntd_io_task_read_prepare(
	    task->parent, task_ck, userstate, notifier, task->dummy,
	    sizeof task->dummy);
}

struct lntd_async_task *
lntd_window_task_watch_to_async(struct lntd_window_task_watch *task)
{
	return lntd_io_task_read_to_async(task->parent);
}

void *lntd_window_task_watch_data(struct lntd_window_task_watch *task)
{
	return task->data;
}

lntd_error
lntd_window_task_notify_create(struct lntd_window_task_notify **taskp,
                               void *data)
{
	lntd_error err;
	struct lntd_window_task_notify *task;
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

void lntd_window_task_notify_destroy(
    struct lntd_window_task_notify *task)
{
	lntd_io_task_write_destroy(task->parent);
	lntd_mem_free(task);
}

static const char dummy[1U];

struct lntd_async_task *
lntd_window_task_notify_prepare(struct lntd_window_task_notify *task,
                                union lntd_async_ck task_ck,
                                void *userstate, lntd_ko notifier)
{
	return lntd_io_task_write_prepare(task->parent, task_ck,
	                                  userstate, notifier, dummy,
	                                  sizeof dummy);
}

struct lntd_async_task *
lntd_window_task_notify_to_async(struct lntd_window_task_notify *task)
{
	return lntd_io_task_write_to_async(task->parent);
}

void *lntd_window_task_notify_data(struct lntd_window_task_notify *task)
{
	return task->data;
}
