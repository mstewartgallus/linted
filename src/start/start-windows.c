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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/start.h"

#include "linted/asynch.h"
#include "linted/environment.h"
#include "linted/log.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>
#include <winsock2.h>

int main(int argc, char *argv[])
{
	/* Cannot fail, return value is only the previous state */
	SetErrorMode(SEM_FAILCRITICALERRORS);

	/**
	 * @todo Open up standard handles if GetStdHandle returns a
	 *       null pointer for any of them (they weren't set by the
	 *       spawner).
	 */
	char const *process_name = 0;

	bool missing_name = false;

	char const *service = linted_environment_get("LINTED_SERVICE");
	if (service != 0) {
		if (0 == (process_name = strdup(service)))
			return EXIT_FAILURE;
	} else if (argc > 0) {
		process_name = argv[0U];
	} else {
		process_name = linted_start_config.canonical_process_name;
		missing_name = true;
	}

	linted_log_open(process_name);

	if (missing_name) {
		linted_log(LINTED_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	int error_code;
	{
		WSADATA xx;
		error_code = WSAStartup(MAKEWORD(2, 2), &xx);
	}
	if (error_code != 0) {
		linted_log(LINTED_LOG_ERROR, "WSAStartup: %s",
		           linted_error_string(error_code));
		return EXIT_FAILURE;
	}

	return linted_start_config.start(process_name, argc,
	                                 (char const * const *)argv);
}
