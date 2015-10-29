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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/window.h"

#include "linted/io.h"
#include "linted/mem.h"
#include "linted/rpc.h"
#include "linted/util.h"

#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <unistd.h>

struct linted_window_task_notify {
	struct linted_io_task_write *parent;
	void *data;
};

struct linted_window_task_watch {
	struct linted_io_task_read *parent;
	void *data;
	char dummy[1U];
};

static const char dummy[1U];

linted_error linted_window_write(linted_window window, uint_fast32_t in)
{
	linted_error err;

	char buf[LINTED_RPC_UINT32_SIZE];
	linted_rpc_pack_uint32(in, buf);

	/*
	 * From POSIX
	 *
	 * If write() is interrupted by a signal after it successfully
	 * writes some data, it shall return the number of bytes
	 * written.
	 *
	 * So, make writes atomic by preventing signals during the
	 * write.
	 */

	sigset_t oldset;
	sigfillset(&oldset);

	err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (err != 0)
		return err;

	if (-1 == pwrite(window, buf, sizeof buf, 0U)) {
		err = errno;
		LINTED_ASSUME(err != 0);
	}

	linted_error restore_err =
	    pthread_sigmask(SIG_SETMASK, &oldset, 0);
	if (0 == err)
		err = restore_err;

	/*
	 * From POSIX
	 *
	 * Writes can be serialized with respect to other reads and
	 * writes. If a read() of file data can be proven (by any
	 * means) to occur after a write() of the data, it must
	 * reflect that write(), even if the calls are made by
	 * different processes. A similar requirement applies to
	 * multiple write operations to the same file position. This
	 * is needed to guarantee the propagation of data from write()
	 * calls to subsequent read() calls. This requirement is
	 * particularly significant for networked file systems, where
	 * some caching schemes violate these semantics.
	 *
	 * So, no fdatasync call needs to be made.
	 */

	return err;
}

linted_error linted_window_read(linted_window window,
                                uint_fast32_t *outp)
{
	linted_error err;

	char buf[LINTED_RPC_UINT32_SIZE];

	sigset_t oldset;
	sigfillset(&oldset);

	err = pthread_sigmask(SIG_BLOCK, &oldset, &oldset);
	if (err != 0)
		return err;

	ssize_t bytes = pread(window, buf, sizeof buf, 0U);
	if (-1 == bytes) {
		err = errno;
		LINTED_ASSUME(err != 0);
	}

	linted_error restore_err =
	    pthread_sigmask(SIG_SETMASK, &oldset, 0);
	if (0 == err)
		err = restore_err;

	if (err != 0)
		return err;

	if (bytes != sizeof buf)
		return EPROTO;

	*outp = linted_rpc_unpack_uint32(buf);
	return 0;
}

linted_error
linted_window_task_watch_create(struct linted_window_task_watch **taskp,
                                void *data)
{
	linted_error err;
	struct linted_window_task_watch *task;
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

void linted_window_task_watch_destroy(
    struct linted_window_task_watch *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void linted_window_task_watch_prepare(
    struct linted_window_task_watch *task,
    union linted_async_ck task_ck, linted_ko notifier)
{
	linted_io_task_read_prepare(task->parent, task_ck, notifier,
	                            task->dummy, sizeof task->dummy);
}

struct linted_window_task_watch *
linted_window_task_watch_from_async(struct linted_async_task *task)
{
	return linted_io_task_read_data(
	    linted_io_task_read_from_async(task));
}

struct linted_async_task *
linted_window_task_watch_to_async(struct linted_window_task_watch *task)
{
	return linted_io_task_read_to_async(task->parent);
}

void *
linted_window_task_watch_data(struct linted_window_task_watch *task)
{
	return task->data;
}

linted_error linted_window_task_notify_create(
    struct linted_window_task_notify **taskp, void *data)
{
	linted_error err;
	struct linted_window_task_notify *task;
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

void linted_window_task_notify_destroy(
    struct linted_window_task_notify *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void linted_window_task_notify_prepare(
    struct linted_window_task_notify *task,
    union linted_async_ck task_ck, linted_ko notifier)
{
	linted_io_task_write_prepare(task->parent, task_ck, notifier,
	                             dummy, sizeof dummy);
}

struct linted_window_task_notify *
linted_window_task_notify_from_async(struct linted_async_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_async(task));
}

struct linted_async_task *linted_window_task_notify_to_async(
    struct linted_window_task_notify *task)
{
	return linted_io_task_write_to_async(task->parent);
}

void *
linted_window_task_notify_data(struct linted_window_task_notify *task)
{
	return task->data;
}
