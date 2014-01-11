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
#include <poll.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
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
	sa_family_t const address_type = AF_UNIX;

	int const server = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
	if (-1 == server) {
		goto error;
	}

	/* Make the socket abstract and autobind */
	if (-1 == bind(server,
		       (struct sockaddr const *)&address_type, sizeof address_type)) {
		goto error_and_close_server;
	}

	if (-1 == listen(server, 128)) {
		goto error_and_close_server;
	}

	if (-1 == linted_task_spawn(&simulator->_task, spawner, simulator_run, server)) {
		goto error_and_close_server;
	}

	simulator->_server = server;

	return 0;

 error_and_close_server:
	close(server);

 error:
	return -1;
}

int linted_simulator_send_tick(struct linted_simulator_tick_results *const
			       tick_results, linted_simulator_t const simulator)
{
	int const connection = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
	if (-1 == connection) {
		goto finish_with_error;
	}

	{
		/* Our server socket is anonymous so we need the name */
		struct sockaddr_un address;
		socklen_t address_size;

		memset(&address, 0, sizeof address);
		address.sun_family = AF_UNIX;

		address_size = sizeof address;
		if (-1 ==
		    getsockname(simulator._server, (struct sockaddr *)&address,
				&address_size)) {
			goto finish_with_error_and_close_connection;
		}

		if (-1 ==
		    connect(connection, (struct sockaddr const *)&address,
			    address_size)) {
			goto finish_with_error_and_close_connection;
		}
	}

	{
		struct message_data message_data = {
			.message_type = SIMULATOR_TICK
		};

		ssize_t bytes_written;
		do {
			bytes_written = write(connection,
					      &message_data, sizeof message_data);
		} while (-1 == bytes_written && EINTR == EINTR);
		if (-1 == bytes_written) {
			goto finish_with_error_and_close_connection;
		}
	}

	{
		struct reply_data reply_data;
		ssize_t bytes_read;
		do {
			bytes_read = read(connection, &reply_data, sizeof reply_data);
		} while (-1 == bytes_read && EINTR == EINTR);
		if (-1 == bytes_read) {
			goto finish_with_error_and_close_connection;
		}

		*tick_results = reply_data.tick_results;
	}

	if (-1 == close(connection)) {
		goto finish_with_error;
	}

	return 0;

 finish_with_error_and_close_connection:
	close(connection);

 finish_with_error:
	return -1;
}

int linted_simulator_close(linted_simulator_t const simulator)
{
	return close(simulator._server);
}

static int simulator_run(linted_task_spawner_t const spawner, int const inbox)
{
	int const spawner_close_status = linted_task_spawner_close(spawner);
	if (-1 == spawner_close_status) {
		LINTED_ERROR("Could not close spawner: %m", errno);
	}

	uint8_t x_position = 0;
	uint8_t y_position = 0;

	/* TODO: Handle multiple connections at once */
	for (;;) {
		int connection;
		do {
			connection = accept4(inbox, NULL, NULL, SOCK_CLOEXEC);
		} while (-1 == connection && EINTR == errno);
		if (-1 == connection) {
			LINTED_ERROR("Could not accept simulator connection: %m", errno);
		}

		struct message_data message_data;
		ssize_t bytes_read;
		do {
			bytes_read = read(connection, &message_data, sizeof message_data);
		} while (-1 == bytes_read && EINTR == errno);
		if (-1 == bytes_read) {
			LINTED_ERROR("Could not read from simulator connection: %m",
				     errno);
		}

		struct reply_data reply_data;
		switch (message_data.message_type) {
		case SIMULATOR_TICK:
			x_position = x_position % 255 + 3;
			y_position = y_position % 255 + 5;
			//@ assert x_position ≤ 255;
			//@ assert y_position ≤ 255;

			reply_data.tick_results.x_position = x_position;
			reply_data.tick_results.y_position = y_position;
			break;

		default:
			LINTED_ERROR("Received unexpected message type: %d",
				     message_data.message_type);
		}

		ssize_t bytes_written;
		do {
			bytes_written = write(connection, &reply_data, sizeof reply_data);
		} while (-1 == bytes_written && errno == EINTR);
		if (-1 == bytes_written) {
			LINTED_ERROR("Could not write to simulator connection: %m",
				     errno);
		}

		if (-1 == close(connection)) {
			LINTED_ERROR("Could close simulator connection: %m", errno);
		}
	}

	int const inbox_close_status = close(inbox);
	if (-1 == inbox_close_status) {
		LINTED_ERROR("Could not close simulator inbox: %m", errno);
	}

	return EXIT_SUCCESS;
}
