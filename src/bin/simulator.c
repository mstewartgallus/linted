/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

#include "linted/simulator.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

enum message_type {
	SIMULATOR_TICK
};

struct message_data {
	enum message_type message_type;
};

struct reply_data {
	struct linted_simulator_tick_results tick_results;
};

static int simulator_run(linted_task_spawner_t const spawner, int inbox);

int linted_simulator_spawn(linted_simulator_t * const simulator,
			   linted_task_spawner_t const spawner)
{
	int exit_status = 0;

	int simulator_fds[2];
	int const simulator_fds_status = socketpair(AF_UNIX,
						    SOCK_STREAM | SOCK_CLOEXEC,
						    0,
						    simulator_fds);
	if (-1 == simulator_fds_status) {
		goto finish;
	}

	{
		int const simulator_reader = simulator_fds[0];
		int const simulator_writer = simulator_fds[1];

		int const spawn_status = linted_task_spawn(&simulator->_task, spawner,
							   simulator_run,
							   simulator_reader);
		if (-1 == spawn_status) {
			close(simulator_writer);
			goto finish_and_close_reader;
		}
		simulator->_inbox = simulator_writer;

 finish_and_close_reader:;
		int const simulator_reader_close_status = close(simulator_reader);
		if (-1 == simulator_reader_close_status) {
			goto finish;
		}
		exit_status = 0;
	}
 finish:
	return exit_status;
}

int linted_simulator_send_tick(struct linted_simulator_tick_results *const
			       tick_results, linted_simulator_t const simulator)
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

		struct message_data message_data = {
			.message_type = SIMULATOR_TICK
		};

		struct iovec iovecs[] = {
			(struct iovec){
				       .iov_base = &message_data,
				       .iov_len = sizeof message_data}
		};

		struct msghdr message;
		memset(&message, 0, sizeof message);

		message.msg_iov = iovecs;
		message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);

		int const sent_fildes[] = { reply_writer };
		char control_message[CMSG_SPACE(sizeof sent_fildes)];
		message.msg_control = control_message;
		message.msg_controllen = sizeof control_message;

		struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
		control_message_header->cmsg_level = SOL_SOCKET;
		control_message_header->cmsg_type = SCM_RIGHTS;
		control_message_header->cmsg_len = CMSG_LEN(sizeof sent_fildes);

		void *const control_message_data = CMSG_DATA(control_message_header);
		memcpy(control_message_data, sent_fildes, sizeof sent_fildes);

		ssize_t bytes_written;
		do {
			bytes_written = sendmsg(simulator._inbox, &message, 0);
		} while (bytes_written != sizeof message && errno == EINTR);
		if (-1 == bytes_written) {
			goto finish_and_free_reply_fds;
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
			} else {
				*tick_results = reply_data.tick_results;
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

int linted_simulator_close(linted_simulator_t const simulator)
{
	return close(simulator._inbox);
}

static int simulator_run(linted_task_spawner_t const spawner, int const inbox)
{
	int const spawner_close_status = linted_task_spawner_close(spawner);
	if (-1 == spawner_close_status) {
		LINTED_ERROR("Could not close spawner: %s\n", strerror(errno));
	}

	uint8_t x_position = 0;
	uint8_t y_position = 0;

	for (;;) {
		struct msghdr message;
		memset(&message, 0, sizeof message);

		struct message_data message_data;

		struct iovec iov[] = {
			(struct iovec){
				       .iov_base = &message_data,
				       .iov_len = sizeof message_data}
		};
		message.msg_iov = iov;
		message.msg_iovlen = LINTED_ARRAY_SIZE(iov);

		int sent_fildes[1] = { -1 };
		char control_message[CMSG_SPACE(sizeof sent_fildes)];
		memset(control_message, 0, sizeof control_message);

		message.msg_control = control_message;
		message.msg_controllen = sizeof control_message;

		ssize_t bytes_read;
		do {
			bytes_read = recvmsg(inbox,
					     &message, MSG_CMSG_CLOEXEC | MSG_WAITALL);
		} while (bytes_read != sizeof message_data && errno == EINTR);
		if (-1 == bytes_read) {
			LINTED_ERROR("Could not read simulator inbox: %s\n",
				     strerror(errno));
		}

		/* All users have closed off */
		if (0 == bytes_read) {
			goto exit;
		}

		struct cmsghdr *const control_message_header = CMSG_FIRSTHDR(&message);
		void *const control_message_data = CMSG_DATA(control_message_header);

		memcpy(sent_fildes, control_message_data, sizeof sent_fildes);

		int const reply_writer = sent_fildes[0];

		switch (message_data.message_type) {
		case SIMULATOR_TICK:{
				x_position = x_position % 255 + 3;
				y_position = y_position % 255 + 5;
				//@ assert x_position ≤ 255;
				//@ assert y_position ≤ 255;

				struct reply_data const reply_data = {
					.tick_results =
					    (struct linted_simulator_tick_results){
										   .
										   x_position
										   =
										   x_position,
										   .
										   y_position
										   =
										   y_position}
				};
				ssize_t bytes_written;
				do {
					bytes_written = write(reply_writer,
							      &reply_data,
							      sizeof reply_data);
				} while (-1 == bytes_written && errno == EINTR);
				if (-1 == bytes_written) {
					LINTED_ERROR
					    ("Could not read from simulator inbox: %s\n",
					     strerror(errno));
				}
				break;
			}

		default:
			LINTED_ERROR("Received unexpected message type: %d.\n",
				     message_data.message_type);
		}
		int const reply_close_status = close(reply_writer);
		if (-1 == reply_close_status) {
			LINTED_ERROR("Could not close reply writer: %s\n",
				     strerror(errno));
		}
	}
 exit:	;
	int const inbox_close_status = close(inbox);
	if (-1 == inbox_close_status) {
		LINTED_ERROR("Could not close simulator inbox: %s\n", strerror(errno));
	}

	return EXIT_SUCCESS;
}
