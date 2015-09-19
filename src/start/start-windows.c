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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN
#define LINTED_START__NO_MAIN 1

#include "config.h"

#include "linted/start.h"

#include "linted/async.h"
#include "linted/environment.h"
#include "linted/mem.h"
#include "linted/signal.h"
#include "linted/str.h"
#include "linted/utf.h"
#include "linted/log.h"

#include <errno.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <windows.h>

#include <shellapi.h>
#include <winsock2.h>

static int show_command;

int linted_start_show_command(void)
{
	return show_command;
}

int linted_start__main(struct linted_start_config const *config,
                       char const **_process_namep, size_t *_argcp,
                       char const *const **_argvp)
{
	/* Cannot fail, return value is only the previous state */
	SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOOPENFILEERRORBOX |
	             SEM_NOGPFAULTERRORBOX);

	/* Prevent dynamic DLL Loading from loading from the current
	 * directory */
	if (!SetDllDirectoryW(L""))
		return EXIT_FAILURE;

	{
		STARTUPINFO info = {0};
		GetStartupInfo(&info);
		show_command =
		    (info.dwFlags & STARTF_USESHOWWINDOW) != 0
		        ? info.wShowWindow
		        : SW_SHOWDEFAULT;
	}

	linted_error err;

	wchar_t *raw_command_line = GetCommandLineW();

	wchar_t **wide_argv;
	size_t argc;
	{
		int xx;
		wide_argv = CommandLineToArgvW(raw_command_line, &xx);
		if (0 == wide_argv)
			return EXIT_FAILURE;
		argc = xx;
	}

	char **argv;
	{
		void *xx;
		err = linted_mem_alloc_array(&xx, argc + 1U,
		                             sizeof argv[0U]);
		if (err != 0)
			return EXIT_FAILURE;
		argv = xx;
	}

	for (size_t ii = 0U; ii < argc; ++ii) {
		err = linted_utf_2_to_1(wide_argv[ii], &argv[ii]);
		if (err != 0)
			return EXIT_FAILURE;
	}

	LocalFree(wide_argv);

	/**
	 * @todo Open up standard handles if GetStdHandle returns a
	 *       null pointer for any of them (they weren't set by the
	 *       spawner).
	 */
	char const *process_name = 0;

	bool missing_name = false;

	char const *service;
	{
		char *xx;
		err = linted_environment_get("LINTED_SERVICE", &xx);
		if (err != 0)
			return EXIT_FAILURE;
		service = xx;
	}
	if (service != 0) {
		process_name = service;
	} else if (argc > 0) {
		process_name = argv[0U];
	} else {
		process_name = config->canonical_process_name;
		missing_name = true;
	}

	char *process_basename;
	{
		char *xx;
		err = linted_str_dup(&xx, process_name);
		if (err != 0)
			return EXIT_FAILURE;
		process_basename = xx;
	}

	process_basename = basename(process_basename);

	if (!config->dont_init_logging)
		linted_log_open(process_basename);

	if (missing_name) {
		linted_log(LINTED_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	if (!config->dont_init_signals) {
		err = linted_signal_init();
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_signal_init: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
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

	*_process_namep = process_basename;
	*_argcp = argc;
	*_argvp = (char const *const *)argv;

	return EXIT_SUCCESS;
}
