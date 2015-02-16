/*
 * Copyright 2015 Steven Stewart-Gallus
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
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/environment.h"
#include "linted/error.h"
#include "linted/log.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <libgen.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>

/**
 * @todo Contain everything in a job.
 * @todo Get a proper process handle out of linted_spawn.
 */

static unsigned char init_start(char const *process_name, size_t argc,
                                char const *const argv[]);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-init", .start = init_start};

static unsigned char init_start(char const *process_name, size_t argc,
                                char const *const argv[])
{
	linted_error errnum;

	char const *monitor;
	{
		char *xx;
		errnum = linted_environment_get("LINTED_MONITOR", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		monitor = xx;
	}

	char *monitor_dup = strdup(monitor);
	if (0 == monitor_dup) {
		linted_log(LINTED_LOG_ERROR, "strdup: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	char *monitor_base = basename(monitor_dup);

	struct linted_spawn_attr *attr;

	{
		struct linted_spawn_attr *xx;
		errnum = linted_spawn_attr_init(&xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_spawn_attr_init: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		attr = xx;
	}

	for (;;) {
		fprintf(stderr, "%s: spawning %s\n", process_name,
		        monitor_base);

		linted_ko monitor_handle;
		{
			pid_t xx;
			errnum = linted_spawn(
			    &xx, LINTED_KO_CWD, monitor, 0, attr,
			    (char const *const[]){monitor_base, 0}, 0);
			if (errnum != 0) {
				linted_log(LINTED_LOG_ERROR, "linted_spawn: %s",
				           linted_error_string(errnum));
				return EXIT_FAILURE;
			}
			monitor_handle = (linted_ko)xx;
		}

		switch (WaitForSingleObject(monitor_handle, INFINITE)) {
		case WAIT_OBJECT_0:
			break;

		case WAIT_FAILED:
			linted_log(LINTED_LOG_ERROR, "WaitObject: %s",
			           linted_error_string(GetLastError()));
			return EXIT_FAILURE;

		default:
			assert(false);
		}

		DWORD exit_status;
		{
			DWORD xx;
			if (!GetExitCodeProcess(monitor_handle, &xx)) {
				linted_log(LINTED_LOG_ERROR,
				           "GetExitCodeProcess: %s",
				           linted_error_string(GetLastError()));
				return EXIT_FAILURE;
			}
			exit_status = xx;
		}
		if (EXIT_SUCCESS == exit_status)
			goto exit_loop;

		fprintf(stderr, "monitor exited with %lu\n", exit_status);

		linted_ko_close(monitor_handle);
	}
exit_loop:
	return EXIT_SUCCESS;
}
