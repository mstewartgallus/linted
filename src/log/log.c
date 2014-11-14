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

#include "linted/log.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <mqueue.h>
#include <sys/poll.h>

struct linted_log_task_receive
{
	struct linted_mq_task_receive *parent;
	void *data;
	char *buf;
};

static linted_error poll_one(linted_ko ko, short events, short *revents);
static linted_error check_for_poll_error(linted_ko ko, short revents);

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
	struct linted_mq_task_receive *parent;
	{
		struct linted_mq_task_receive *xx;
		errnum = linted_mq_task_receive_create(&xx, task);
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
	linted_mq_task_receive_destroy(task->parent);
	linted_mem_free(task);
}

struct linted_asynch_task *
linted_log_task_receive_to_asynch(struct linted_log_task_receive *task)
{
	return linted_mq_task_receive_to_asynch(task->parent);
}

struct linted_log_task_receive *
linted_log_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_mq_task_receive_data(
	    linted_mq_task_receive_from_asynch(task));
}

void *linted_log_task_receive_data(struct linted_log_task_receive *task)
{
	return task->data;
}

void linted_log_task_receive_prepare(struct linted_log_task_receive *task,
                                     unsigned task_action, linted_log log,
                                     char msg_ptr[static LINTED_LOG_MAX])
{
	linted_mq_task_receive_prepare(task->parent, task_action, log, msg_ptr,
	                               LINTED_LOG_MAX);
	task->buf = msg_ptr;
}

size_t linted_log_task_receive_bytes_read(struct linted_log_task_receive *task)
{
	return linted_mq_task_receive_bytes_read(task->parent);
}

char *linted_log_task_receive_buf(struct linted_log_task_receive *task)
{
	return task->buf;
}

/**
 * @todo Make asynchronous
 */
linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len)
{
	linted_error errnum = 0;

	for (;;) {
		int send_status;
		do {
			send_status = mq_send(log, msg_ptr, msg_len, 0);
			if (0 == send_status) {
				errnum = 0;
			} else {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			}
		} while (EINTR == errnum);
		if (errnum != EAGAIN)
			break;

		short revents;
		for (;;) {
			short xx;
			errnum = poll_one(log, POLLOUT, &xx);
			switch (errnum) {
			case EINTR:
				continue;

			case 0:
				revents = xx;
				goto check_for_poll_error;

			default:
				return errnum;
			}
		}

	check_for_poll_error:
		errnum = check_for_poll_error(log, revents);
		if (errnum != 0)
			break;
	}
	return errnum;
}

static linted_error check_for_poll_error(linted_ko ko, short revents)
{
	linted_error errnum = 0;

	if ((revents & POLLNVAL) != 0)
		errnum = EBADF;

	return errnum;
}

static linted_error poll_one(linted_ko ko, short events, short *reventsp)
{
	linted_error errnum;

	short revents;
	{
		struct pollfd pollfd = { .fd = ko, .events = events };
		int poll_status = poll(&pollfd, 1U, -1);
		if (-1 == poll_status)
			goto poll_failed;

		revents = pollfd.revents;
		goto poll_succeeded;
	}

poll_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);
	return errnum;

poll_succeeded:
	*reventsp = revents;
	return 0;
}
