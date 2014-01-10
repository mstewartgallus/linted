/*
 * Copyright 2013 Steven Stewart-Gallus
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

#include "linted/task.h"

#include "linted/sprintf.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

struct request_data {
	linted_task_func_t func;
};

struct reply_data {
	int error_status;
	pid_t pid;
};

static int fork_server_run(linted_task_spawner_t spawner, int request_reader);

int linted_task_spawner_init(linted_task_spawner_t * spawner)
{
	/* First we fork from a known good state and serve out forks of
	 * this known good state. This avoids several problems with
	 * inheritance of corrupted state that aren't even fixable with
	 * exec. It also allows us to avoid the nasty command line
	 * interface exec forces us into.
	 */
	int fork_server_request_fds[2];
	int const request_socket_status = socketpair(AF_UNIX,
						     SOCK_STREAM | SOCK_CLOEXEC,
						     0,
						     fork_server_request_fds);
	if (-1 == request_socket_status) {
		return -1;
	}

	int const fork_server_request_reader = fork_server_request_fds[0];
	int const fork_server_request_writer = fork_server_request_fds[1];

	spawner->_request_writer = fork_server_request_writer;

	pid_t const child_pid = fork();
	if (0 == child_pid) {
		int const exit_status = fork_server_run(*spawner,
							fork_server_request_reader);
		exit(exit_status);
	}

	int error_status = -1;
	if (child_pid != -1) {
		error_status = 0;
	}

	int const request_close_status = close(fork_server_request_reader);
	if (-1 == request_close_status) {
		error_status = -1;
	}

	return error_status;
}

int linted_task_spawner_close(linted_task_spawner_t spawner)
{
	return close(spawner._request_writer);
}

int linted_task_spawn(linted_task_t * const task,
		      linted_task_spawner_t const spawner,
		      linted_task_func_t const func, int const inbox)
{
	int error_status = -1;

	int reply_fds[2];
	int const reply_fds_status = pipe2(reply_fds, O_CLOEXEC);
	if (-1 == reply_fds_status) {
		goto finish;
	}

	{
		int const reply_reader = reply_fds[0];
		int const reply_writer = reply_fds[1];

		{
			struct request_data request_data = {
				.func = func
			};

			struct iovec iovecs[] = {
				(struct iovec){
					       .iov_base = &request_data,
					       .iov_len = sizeof request_data}
			};

			struct msghdr message;
			memset(&message, 0, sizeof message);

			message.msg_iov = iovecs;
			message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

			int const sent_fildes[] = {
				reply_writer,
				inbox
			};
			char control_message[CMSG_SPACE(sizeof sent_fildes)];
			message.msg_control = control_message;
			message.msg_controllen = sizeof control_message;

			struct cmsghdr *const control_message_header =
			    CMSG_FIRSTHDR(&message);
			control_message_header->cmsg_level = SOL_SOCKET;
			control_message_header->cmsg_type = SCM_RIGHTS;
			control_message_header->cmsg_len = CMSG_LEN(sizeof sent_fildes);

			void *const control_message_data =
			    CMSG_DATA(control_message_header);
			memcpy(control_message_data, sent_fildes, sizeof sent_fildes);

			ssize_t bytes_written;
			do {
				bytes_written = sendmsg(spawner._request_writer,
							&message, 0);
			} while (bytes_written != sizeof message && errno == EINTR);
			if (-1 == bytes_written) {
				goto finish_and_free_reply_fds;
			}
		}

		error_status = 0;

 finish_and_free_reply_fds:;
		if (error_status != -1) {
			struct reply_data reply_data;
			ssize_t bytes_read;
			do {
				bytes_read = read(reply_reader,
						  &reply_data, sizeof reply_data);
			} while (-1 == bytes_read && errno == EINTR);
			if (-1 == bytes_read) {
				error_status = -1;
			}

			int const reply_error_status = reply_data.error_status;
			if (reply_error_status != 0) {
				errno = reply_error_status;
				error_status = -1;
			} else {
				task->_pid = reply_data.pid;
			}
		}

		int const reply_writer_close_status = close(reply_writer);
		if (-1 == reply_writer_close_status) {
			error_status = -1;
		}

		int const reply_reader_close_status = close(reply_reader);
		if (-1 == reply_reader_close_status) {
			error_status = -1;
		}
	}
 finish:
	return error_status;
}

static int fork_server_run(linted_task_spawner_t const spawner, int const request_reader)
{
	/* Posix requires an exact copy of process memory so passing
	 * around function pointers through pipes is allowed.
	 */

	struct sigaction action;
	memset(&action, 0, sizeof action);
	action.sa_handler = SIG_IGN;

	struct sigaction old_action;
	int const sig_status = sigaction(SIGCHLD, &action, &old_action);
	if (-1 == sig_status) {
		LINTED_ERROR("Could not ignore child processes: %s\n", strerror(errno));
	}

	for (;;) {
		struct msghdr message;
		memset(&message, 0, sizeof message);

		struct request_data request_data;

		struct iovec iov[] = {
			(struct iovec){
				       .iov_base = &request_data,
				       .iov_len = sizeof request_data}
		};
		message.msg_iov = iov;
		message.msg_iovlen = LINTED_ARRAY_SIZE(iov);

		int sent_fildes[2] = { -1 };
		char control_message[CMSG_SPACE(sizeof sent_fildes)];
		memset(control_message, 0, sizeof control_message);

		message.msg_control = control_message;
		message.msg_controllen = sizeof control_message;

		ssize_t bytes_read;
		do {
			bytes_read = recvmsg(request_reader,
					     &message, MSG_CMSG_CLOEXEC | MSG_WAITALL);
		} while (bytes_read != sizeof request_data && errno == EINTR);
		if (-1 == bytes_read) {
			LINTED_ERROR
			    ("Could not read bytes from fork request socket: %s\n",
			     strerror(errno));
		}
		/* No more listeners to serve so exit. */
		if (0 == bytes_read) {
			break;
		}

		struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
		void *const control_message_data = CMSG_DATA(control_message_header);

		memcpy(sent_fildes, control_message_data, sizeof sent_fildes);

		int const reply_writer = sent_fildes[0];
		int const inbox = sent_fildes[1];

		pid_t const child_pid = fork();
		if (0 == child_pid) {
			/* Restore the old signal behaviour */
			int const retry_sig_status = sigaction(SIGCHLD,
							       &old_action,
							       &action);
			if (-1 == retry_sig_status) {
				LINTED_ERROR
				    ("Could not restore child signal behaviour: %s\n",
				     strerror(errno));
			}

			int const reply_close_status = close(reply_writer);
			if (-1 == reply_close_status) {
				LINTED_ERROR
				    ("Forked child could not close reply file descriptor: %s\n",
				     strerror(errno));
			}
			return request_data.func(spawner, inbox);
		}

		int const inbox_close_status = close(inbox);
		if (-1 == inbox_close_status) {
			LINTED_ERROR
			    ("Fork server could not close inbox file descriptor: %s\n",
			     strerror(errno));
		}

		{
			struct reply_data reply_data;
			if (-1 == child_pid) {
				reply_data.error_status = errno;
			} else {
				reply_data.error_status = 0;
				reply_data.pid = child_pid;
			}

			int const reply_write_status = write(reply_writer,
							     &reply_data,
							     sizeof reply_data);
			if (-1 == reply_write_status) {
				LINTED_ERROR
				    ("Fork server could not write reply to child requester: %s\n",
				     strerror(errno));
			}
		}

		int const reply_close_status = close(reply_writer);
		if (-1 == reply_close_status) {
			LINTED_ERROR
			    ("Fork server could not close reply file descriptor: %s\n",
			     strerror(errno));
		}
	}

	int const request_close_status = close(request_reader);
	if (-1 == request_close_status) {
		LINTED_ERROR
		    ("Could not close read end of fork request socket: %s\n",
		     strerror(errno));
	}

	int const spawner_close_status = linted_task_spawner_close(spawner);
	if (-1 == spawner_close_status) {
		LINTED_ERROR("Could not close spawner: %s\n", strerror(errno));
	}

	return EXIT_SUCCESS;
}
