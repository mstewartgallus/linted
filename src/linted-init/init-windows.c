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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/environment.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/utf.h"
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

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-init",
    .start = init_start};

static unsigned char init_start(char const *process_name, size_t argc,
                                char const *const argv[])
{
	linted_error err;

	char const *monitor;
	{
		char *xx;
		err = linted_environment_get("LINTED_MONITOR", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		monitor = xx;
	}

	if (0 == monitor) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_MONITOR");
		return EXIT_FAILURE;
	}

	char *monitor_dup;
	{
		char *xx;
		err = linted_str_dup(&xx, monitor);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_str_dup: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_dup = xx;
	}

	char *monitor_base = basename(monitor_dup);

	wchar_t *monitor_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(monitor, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_utf_1_to_2: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_utf2 = xx;
	}

	wchar_t *monitor_base_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(monitor_base, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_utf_1_to_2: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_base_utf2 = xx;
	}

	for (;;) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: spawning %s\n",
		                       process_name, monitor_base);

		linted_ko monitor_handle;
		linted_ko thread_handle;
		{
			DWORD creation_flags = 0U;
			STARTUPINFO startup_info = {0};

			startup_info.cb = sizeof startup_info;
			startup_info.dwFlags = STARTF_USESTDHANDLES;
			startup_info.hStdInput = LINTED_KO_STDIN;
			startup_info.hStdOutput = LINTED_KO_STDOUT;
			startup_info.hStdError = LINTED_KO_STDERR;

			PROCESS_INFORMATION process_information;
			if (!CreateProcessW(
			        monitor_utf2, monitor_base_utf2, 0, 0,
			        false, creation_flags, 0, 0,
			        &startup_info, &process_information)) {
				linted_log(LINTED_LOG_ERROR,
				           "CreateProcessW: %s",
				           linted_error_string(
				               GetLastError()));
				return EXIT_FAILURE;
			}
			monitor_handle = process_information.hProcess;
			thread_handle = process_information.hThread;
		}
		linted_ko_close(thread_handle);

		switch (WaitForSingleObject(monitor_handle, INFINITE)) {
		case WAIT_OBJECT_0:
			break;

		case WAIT_FAILED:
			linted_log(LINTED_LOG_ERROR,
			           "WaitForSingleObject: %s",
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
				           linted_error_string(
				               GetLastError()));
				return EXIT_FAILURE;
			}
			exit_status = xx;
		}
		if (EXIT_SUCCESS == exit_status)
			goto exit_loop;

		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "monitor exited with %lu\n",
		                       exit_status);

		linted_ko_close(monitor_handle);
	}
exit_loop:
	return EXIT_SUCCESS;
}
