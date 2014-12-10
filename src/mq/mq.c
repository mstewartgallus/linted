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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <mqueue.h>
#include <poll.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#define FILE_MAX 255U
#define RANDOM_BYTES 8U

struct linted_mq_task_receive
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char *buf;
	size_t size;
	size_t bytes_read;
	linted_ko ko;
};

struct linted_mq_task_send
{
	struct linted_asynch_task *parent;
	struct linted_asynch_waiter *waiter;
	void *data;
	char const *buf;
	size_t size;
	size_t bytes_wrote;
	linted_ko ko;
};

static void gen_name(char *name, size_t size);

/**
 * Implemented using POSIX message queues.
 */
linted_error linted_mq_create(linted_mq *mqp, char const *debugpath,
                              size_t maxmsg, size_t msgsize,
                              unsigned long flags)
{
	linted_error errnum;
	linted_mq ko;
	int unlink_status;

	if (flags != 0U)
		return EINVAL;

	if (debugpath[0U] != '/')
		return EINVAL;

	size_t path_size = strlen(debugpath + 1U);

	if (path_size > FILE_MAX - 1U - RANDOM_BYTES)
		return ENAMETOOLONG;

	{
		char random_mq_name[FILE_MAX + 1U];

		memcpy(1U + random_mq_name, 1U + debugpath, path_size);
		random_mq_name[0U] = '/';
		random_mq_name[1U + path_size] = '-';
		random_mq_name[1U + path_size + 1U + RANDOM_BYTES] = '\0';

		do {
			gen_name(random_mq_name + 1U + path_size + 1U,
			         RANDOM_BYTES);

			{
				struct mq_attr mq_attr;

				mq_attr.mq_flags = 0;
				mq_attr.mq_curmsgs = 0;
				mq_attr.mq_maxmsg = maxmsg;
				mq_attr.mq_msgsize = msgsize;

				ko = mq_open(random_mq_name,
				             O_RDWR | O_CREAT | O_EXCL |
				                 O_NONBLOCK,
				             0, &mq_attr);
			}
			if (-1 == ko) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			} else {
				errnum = 0;
			}
		} while (EEXIST == errnum);
		if (errnum != 0)
			goto exit_with_error;

		unlink_status = mq_unlink(random_mq_name);
	}
	if (-1 == unlink_status) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto exit_with_error_and_close;
	}

	if (-1 == fchmod(ko, S_IRUSR | S_IWUSR)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto exit_with_error_and_close;
	}

	*mqp = ko;

	return 0;

exit_with_error_and_close:
	mq_close(ko);

exit_with_error:
	return errnum;
}

linted_error
linted_mq_task_receive_create(struct linted_mq_task_receive **taskp, void *data)
{
	linted_error errnum;
	struct linted_mq_task_receive *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(
		    &xx, task, LINTED_ASYNCH_TASK_MQ_RECEIVE);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;

free_parent:
	linted_mem_free(parent);
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_mq_task_receive_destroy(struct linted_mq_task_receive *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_mq_task_receive_prepare(struct linted_mq_task_receive *task,
                                    unsigned task_action, linted_ko ko,
                                    char *buf, size_t msglen)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = msglen;
	task->bytes_read = 0;
}

struct linted_mq_task_receive *
linted_mq_task_receive_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_mq_task_receive_to_asynch(struct linted_mq_task_receive *task)
{
	return task->parent;
}

void *linted_mq_task_receive_data(struct linted_mq_task_receive *task)
{
	return task->data;
}

size_t linted_mq_task_receive_bytes_read(struct linted_mq_task_receive *task)
{
	return task->bytes_read;
}

linted_error linted_mq_task_send_create(struct linted_mq_task_send **taskp,
                                        void *data)
{
	linted_error errnum;
	struct linted_mq_task_send *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_asynch_task *parent;
	{
		struct linted_asynch_task *xx;
		errnum = linted_asynch_task_create(&xx, task,
		                                   LINTED_ASYNCH_TASK_MQ_SEND);
		if (errnum != 0)
			goto free_task;
		parent = xx;
	}

	{
		struct linted_asynch_waiter *xx;
		errnum = linted_asynch_waiter_create(&xx);
		if (errnum != 0)
			goto free_parent;
		task->waiter = xx;
	}

	task->parent = parent;
	task->data = data;
	*taskp = task;
	return 0;

free_parent:
	linted_mem_free(parent);
free_task:
	linted_mem_free(task);
	return errnum;
}

void linted_mq_task_send_destroy(struct linted_mq_task_send *task)
{
	linted_asynch_waiter_destroy(task->waiter);
	linted_asynch_task_destroy(task->parent);
	linted_mem_free(task);
}

void linted_mq_task_send_prepare(struct linted_mq_task_send *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t msglen)
{
	linted_asynch_task_prepare(task->parent, task_action);
	task->ko = ko;
	task->buf = buf;
	task->size = msglen;
	task->bytes_wrote = 0;
}

struct linted_mq_task_send *
linted_mq_task_send_from_asynch(struct linted_asynch_task *task)
{
	return linted_asynch_task_data(task);
}

struct linted_asynch_task *
linted_mq_task_send_to_asynch(struct linted_mq_task_send *task)
{
	return task->parent;
}

void *linted_mq_task_send_data(struct linted_mq_task_send *task)
{
	return task->data;
}

void linted_mq_do_receive(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task)
{
	struct linted_mq_task_receive *task_receive =
	    linted_mq_task_receive_from_asynch(task);
	linted_error errnum = 0;

	linted_ko ko = task_receive->ko;
	char *buf = task_receive->buf;
	size_t size = task_receive->size;

	ssize_t result = mq_receive(ko, buf, size, NULL);
	if (-1 == result) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum)
		goto submit_retry;

	if (EAGAIN == errnum)
		goto wait_on_poll;

	linted_asynch_task_seterrnum(task, errnum);
	task_receive->bytes_read = result;

	linted_asynch_pool_complete(pool, task);
	return;

submit_retry:
	linted_asynch_pool_submit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, task_receive->waiter, task, ko,
	                                POLLIN);
}

void linted_mq_do_send(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task)
{
	struct linted_mq_task_send *task_send =
	    linted_mq_task_send_from_asynch(task);
	size_t bytes_wrote = 0U;
	linted_error errnum = 0;

	linted_ko ko = task_send->ko;
	char const *buf = task_send->buf;
	size_t size = task_send->size;

	if (-1 == mq_send(ko, buf, size, 0)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (EINTR == errnum)
		goto submit_retry;

	if (EAGAIN == errnum)
		goto wait_on_poll;

	linted_asynch_task_seterrnum(task, errnum);
	task_send->bytes_wrote = bytes_wrote;

	linted_asynch_pool_complete(pool, task);
	return;

submit_retry:
	linted_asynch_pool_submit(pool, task);
	return;

wait_on_poll:
	linted_asynch_pool_wait_on_poll(pool, task_send->waiter, task, ko,
	                                POLLOUT);
}

static void gen_name(char *name, size_t size)
{
	for (size_t ii = 0U; ii < size; ++ii) {
		char random_char;
		do {
			/* Normally using the modulus would give
			 * a bad
			 * distribution but CHAR_MAX + 1U is a
			 * power of two
			 */
			random_char = linted_random_fast() % (CHAR_MAX + 1U);

			/* Throw out results and retry for an
			 * even
			 * distribution
			 */
		} while (!((random_char >= 'a' && random_char <= 'z') ||
		           (random_char >= 'A' && random_char <= 'Z') ||
		           (random_char >= '0' && random_char <= '9')));

		name[ii] = random_char;
	}
}
