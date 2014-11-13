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
#define _GNU_SOURCE

#include "config.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/admin.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>

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

void linted_admin_accept(struct linted_admin_task_accept *task,
                         unsigned task_action, linted_admin admin)
{
	linted_ko_task_accept(LINTED_UPCAST(task), task_action, admin);
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

void linted_admin_recv_request(struct linted_admin_task_recv_request *task,
                               unsigned task_action, linted_admin admin)
{
	linted_ko_task_read(LINTED_UPCAST(task), task_action, admin,
	                    (char *)&task->request, sizeof task->request);
}

void linted_admin_send_reply(struct linted_admin_task_send_reply *task,
                             unsigned task_action, linted_admin admin,
                             union linted_admin_reply const *reply)
{
	task->reply = *reply;
	linted_ko_task_write(LINTED_UPCAST(task), task_action, admin,
	                     (char const *)&task->reply, sizeof task->reply);
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
