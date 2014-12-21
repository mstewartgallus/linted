/*
 * Copyright 2014 Steven Stewart-Gallus
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

#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <sys/socket.h>
#include <sys/types.h>

struct linted_log_task_receive
{
	struct linted_ko_task_recv *parent;
	void *data;
	char *buf;
};

linted_error
linted_log_task_receive_create(struct linted_log_task_receive **taskp,
                               void *data)
{
	linted_error errnum;
	struct linted_log_task_receive *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_ko_task_recv *parent;
	{
		struct linted_ko_task_recv *xx;
		errnum = linted_ko_task_recv_create(&xx, task);
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

void linted_log_task_receive_destroy(struct linted_log_task_receive *task)
{
	linted_ko_task_recv_destroy(task->parent);
	linted_mem_free(task);
}

struct linted_asynch_task *
linted_log_task_receive_to_asynch(struct linted_log_task_receive *task)
{
	return linted_ko_task_recv_to_asynch(task->parent);
}

struct linted_log_task_receive *
linted_log_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_ko_task_recv_data(linted_ko_task_recv_from_asynch(task));
}

void *linted_log_task_receive_data(struct linted_log_task_receive *task)
{
	return task->data;
}

void linted_log_task_receive_prepare(struct linted_log_task_receive *task,
                                     unsigned task_action, linted_log log,
                                     char msg_ptr[static LINTED_LOG_MAX])
{
	linted_ko_task_recv_prepare(task->parent, task_action, log, msg_ptr,
	                            LINTED_LOG_MAX);
	task->buf = msg_ptr;
}

size_t linted_log_task_receive_bytes_read(struct linted_log_task_receive *task)
{
	return linted_ko_task_recv_bytes_read(task->parent);
}

char *linted_log_task_receive_buf(struct linted_log_task_receive *task)
{
	return task->buf;
}

/**
 * @todo Make asynchronous
 */
linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len, struct sockaddr const *addr,
                              size_t addr_len)
{
	if (-1 == sendto(log, msg_ptr, msg_len, MSG_NOSIGNAL, addr, addr_len)) {
		return errno;
	}
	return 0;
}
