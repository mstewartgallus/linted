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

#include "linted/io.h"

#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

int linted_io_create_local_server(void)
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

	return server;

 error_and_close_server:
	close(server);
 error:
	return -1;
}

int linted_io_connect_to_local_socket(int const local)
{
	/* Our server socket is anonymous so we need the name */
	struct sockaddr_un address;

	memset(&address, 0, sizeof address);
	address.sun_family = AF_UNIX;

	socklen_t address_size = sizeof address;
	if (-1 == getsockname(local, (struct sockaddr *)&address, &address_size)) {
		return -1;
	}

	int const connection = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
	if (-1 == connection) {
		return -1;
	}

	if (-1 == connect(connection, (struct sockaddr const *)&address, address_size)) {
		close(connection);
		return -1;
	}

	return connection;
}
