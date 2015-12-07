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

#include "config.h"

#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/path.h"
#include "lntd/start.h"
#include "lntd/str.h"
#include "lntd/utf.h"
#include "lntd/util.h"

#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>

/**
 * @file
 *
 * @todo Windows: delegate exit signals from `init` to `monitor`.
 */

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-init", 0};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err;

	char const *monitor;
	{
		char *xx;
		err = lntd_env_get("LINTED_MONITOR", &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_env_get: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		monitor = xx;
	}

	if (0 == monitor) {
		lntd_log(LNTD_LOG_ERROR,
		         "%s is a required environment variable",
		         "LINTED_MONITOR");
		return EXIT_FAILURE;
	}

	char *monitor_base;
	{
		char *xx;
		err = lntd_path_base(&xx, monitor);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_path_base: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_base = xx;
	}

	wchar_t *monitor_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(monitor, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_utf_1_to_2: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_utf2 = xx;
	}

	wchar_t *monitor_base_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(monitor_base, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_utf_1_to_2: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_base_utf2 = xx;
	}

	for (;;) {
		lntd_io_write_format(LNTD_KO_STDERR, 0,
		                     "%s: spawning %s\n", process_name,
		                     monitor_base);

		lntd_ko monitor_handle;
		lntd_ko thread_handle;
		{
			DWORD creation_flags = 0U;
			STARTUPINFO startup_info = {0};

			startup_info.cb = sizeof startup_info;
			startup_info.dwFlags = STARTF_USESTDHANDLES;
			startup_info.hStdInput = LNTD_KO_STDIN;
			startup_info.hStdOutput = LNTD_KO_STDOUT;
			startup_info.hStdError = LNTD_KO_STDERR;

			PROCESS_INFORMATION process_information;
			if (!CreateProcessW(
			        monitor_utf2, monitor_base_utf2, 0, 0,
			        false, creation_flags, 0, 0,
			        &startup_info, &process_information)) {
				lntd_log(LNTD_LOG_ERROR,
				         "CreateProcessW: %s",
				         lntd_error_string(
				             HRESULT_FROM_WIN32(
				                 GetLastError())));
				return EXIT_FAILURE;
			}
			monitor_handle = process_information.hProcess;
			thread_handle = process_information.hThread;
		}
		lntd_ko_close(thread_handle);

		switch (WaitForSingleObject(monitor_handle, INFINITE)) {
		case WAIT_OBJECT_0:
			break;

		case WAIT_FAILED:
			lntd_log(LNTD_LOG_ERROR,
			         "WaitForSingleObject: %s",
			         lntd_error_string(HRESULT_FROM_WIN32(
			             GetLastError())));
			return EXIT_FAILURE;

		default:
			LNTD_ASSERT(false);
		}

		DWORD exit_status;
		{
			DWORD xx;
			if (!GetExitCodeProcess(monitor_handle, &xx)) {
				lntd_log(LNTD_LOG_ERROR,
				         "GetExitCodeProcess: %s",
				         lntd_error_string(
				             HRESULT_FROM_WIN32(
				                 GetLastError())));
				return EXIT_FAILURE;
			}
			exit_status = xx;
		}
		if (EXIT_SUCCESS == exit_status)
			goto exit_loop;

		lntd_io_write_format(LNTD_KO_STDERR, 0,
		                     "monitor exited with %lu\n",
		                     exit_status);

		lntd_ko_close(monitor_handle);
	}
exit_loop:
	return EXIT_SUCCESS;
}
