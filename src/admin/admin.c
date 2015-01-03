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

#include "linted/admin.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>

struct linted_admin_task_accept
{
	struct linted_io_task_accept *parent;
	void *data;
};

struct linted_admin_task_recv_request
{
	struct linted_io_task_read *parent;
	void *data;
	union linted_admin_request request;
};

struct linted_admin_task_send_reply
{
	struct linted_io_task_write *data;
	void *parent;
	union linted_admin_reply reply;
};

linted_error
linted_admin_task_accept_create(struct linted_admin_task_accept **taskp,
                                void *data)
{
	linted_error errnum;
	struct linted_admin_task_accept *task;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *task);
		if (errnum != 0)
			return errnum;
		task = xx;
	}
	struct linted_io_task_accept *parent;
	{
		struct linted_io_task_accept *xx;
		errnum = linted_io_task_accept_create(&xx, task);
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

void linted_admin_task_accept_destroy(struct linted_admin_task_accept *task)
{
	linted_io_task_accept_destroy(task->parent);
	linted_mem_free(task);
}

void linted_admin_task_accept_prepare(struct linted_admin_task_accept *task,
                                      unsigned task_action, linted_ko ko)
{
	linted_io_task_accept_prepare(task->parent, task_action, ko);
}

void *linted_admin_task_accept_data(struct linted_admin_task_accept *task)
{
	return task->data;
}

linted_admin
linted_admin_task_accept_returned_ko(struct linted_admin_task_accept *task)
{
	return linted_io_task_accept_returned_ko(task->parent);
}

struct linted_asynch_task *
linted_admin_task_accept_to_asynch(struct linted_admin_task_accept *task)
{
	return linted_io_task_accept_to_asynch(task->parent);
}

struct linted_admin_task_accept *
linted_admin_task_accept_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_accept_data(
	    linted_io_task_accept_from_asynch(task));
}

linted_error linted_admin_task_recv_request_create(
    struct linted_admin_task_recv_request **taskp, void *data)
{
	linted_error errnum;
	struct linted_admin_task_recv_request *task;
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

void linted_admin_task_recv_request_destroy(
    struct linted_admin_task_recv_request *task)
{
	linted_io_task_read_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_admin_task_recv_request_data(struct linted_admin_task_recv_request *task)
{
	return task->data;
}

linted_admin
linted_admin_task_recv_request_ko(struct linted_admin_task_recv_request *task)
{
	return linted_io_task_read_ko(task->parent);
}

union linted_admin_request const *linted_admin_task_recv_request_request(
    struct linted_admin_task_recv_request *task)
{
	return &task->request;
}

void linted_admin_task_recv_request_prepare(
    struct linted_admin_task_recv_request *task, unsigned task_action,
    linted_ko ko)
{
	linted_io_task_read_prepare(task->parent, task_action, ko,
	                            (char *)&task->request,
	                            sizeof task->request);
}

struct linted_asynch_task *linted_admin_task_recv_request_to_asynch(
    struct linted_admin_task_recv_request *task)
{
	return linted_io_task_read_to_asynch(task->parent);
}

struct linted_admin_task_recv_request *
linted_admin_task_recv_request_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_read_data(linted_io_task_read_from_asynch(task));
}

linted_error
linted_admin_task_send_reply_create(struct linted_admin_task_send_reply **taskp,
                                    void *data)
{
	linted_error errnum;
	struct linted_admin_task_send_reply *task;
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
linted_admin_task_send_reply_destroy(struct linted_admin_task_send_reply *task)
{
	linted_io_task_write_destroy(task->parent);
	linted_mem_free(task);
}

void *
linted_admin_task_send_reply_data(struct linted_admin_task_send_reply *task)
{
	return task->data;
}

void
linted_admin_task_send_reply_prepare(struct linted_admin_task_send_reply *task,
                                     unsigned task_action, linted_ko ko,
                                     union linted_admin_reply const *reply)
{
	linted_io_task_write_prepare(task->parent, task_action, ko,
	                             (char const *)&task->reply,
	                             sizeof task->reply);
	task->reply = *reply;
}

struct linted_asynch_task *linted_admin_task_send_reply_to_asynch(
    struct linted_admin_task_send_reply *task)
{
	return linted_io_task_write_to_asynch(task->parent);
}

struct linted_admin_task_send_reply *
linted_admin_task_send_reply_from_asynch(struct linted_asynch_task *task)
{
	return linted_io_task_write_data(
	    linted_io_task_write_from_asynch(task));
}

linted_error linted_admin_bind(linted_admin *admin, int backlog,
                               char const *path, size_t path_len)
{
	linted_error errnum = 0;

	if (NULL == path && path_len != 0U)
		return EINVAL;

	if (path_len > LINTED_ADMIN_PATH_MAX)
		return ENAMETOOLONG;

	int sock = socket(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0);
	if (-1 == sock) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	int bind_status;
	{
		struct sockaddr_un address = { 0 };

		address.sun_family = AF_UNIX;

		if (path != NULL) {
			memcpy(address.sun_path, path, path_len);
			if ('@' == address.sun_path[0U])
				address.sun_path[0U] = '\0';
		}

		bind_status = bind(sock, (void *)&address,
		                   sizeof(sa_family_t) + path_len);
	}
	if (-1 == bind_status) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_sock;
	}

	/* Set the nonblock status after binding because asynchronous
	 * binding is a pain to deal with.
	 */
	int flags = fcntl(sock, F_GETFL);
	if (-1 == flags) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_sock;
	}

	if (-1 == fcntl(sock, F_SETFL, (long)flags | O_NONBLOCK)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_sock;
	}

	if (-1 == listen(sock, backlog)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_sock;
	}

	if (-1 == shutdown(sock, SHUT_WR)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_sock;
	}

	*admin = sock;
	return 0;

close_sock:
	linted_ko_close(sock);
	return errnum;
}

linted_error linted_admin_connect(linted_admin *admin, char const *path,
                                  size_t path_len)
{
	linted_error errnum = 0;

	if (path_len > LINTED_ADMIN_PATH_MAX)
		return ENAMETOOLONG;

	int sock = socket(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0);
	if (-1 == sock) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	int connect_status;
	{
		struct sockaddr_un address = { 0 };

		address.sun_family = AF_UNIX;
		memcpy(address.sun_path, path, path_len);

		if ('@' == address.sun_path[0U])
			address.sun_path[0U] = '\0';

		connect_status = connect(sock, (void *)&address,
		                         sizeof(sa_family_t) + path_len);
	}
	if (-1 == connect_status) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_sock;
	}

	*admin = sock;
	return 0;

close_sock:
	linted_ko_close(sock);
	return errnum;
}

linted_error linted_admin_path(linted_admin admin,
                               char buf[static LINTED_ADMIN_PATH_MAX],
                               size_t *len)
{
	{
		struct sockaddr_un address = { 0 };

		socklen_t addr_len;
		{
			socklen_t xx = sizeof address;
			if (-1 == getsockname(admin, (void *)&address, &xx))
				goto getsockname_failed;
			addr_len = xx;
		}

		*len = addr_len - sizeof(sa_family_t);
		memcpy(buf, address.sun_path, *len);
	}

	if ('\0' == buf[0U])
		buf[0U] = '@';

	return 0;

getsockname_failed:
	;
	linted_error errnum = errno;
	LINTED_ASSUME(errnum != 0);
	return errnum;
}

linted_error
linted_admin_send_request(linted_admin admin,
                          union linted_admin_request const *request)
{
	return linted_io_write_all(admin, NULL, request, sizeof *request);
}

linted_error linted_admin_recv_reply(linted_admin admin,
                                     union linted_admin_reply *reply,
                                     size_t *size)
{
	linted_error errnum;

	errnum = linted_io_read_all(admin, size, reply, sizeof *reply);

	if (errnum != 0)
		return errnum;

	/* Sent malformed input */
	if (*size != sizeof *reply)
		return EPROTO;

	return 0;
}
