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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#define LINTED_START_OBJECT

#include "linted/start.h"

#include "linted/asynch.h"
#include "linted/environment.h"
#include "linted/mem.h"
#include "linted/signal.h"
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

/* This is an awful hack to force the library to be linked in to users.
 */
char const linted_start__useme;

int linted_start_show_command(void)
{
	return show_command;
}

int WINAPI wWinMain(HINSTANCE program_instance,
                    HINSTANCE prev_instance_unused,
                    wchar_t *command_line_unused, int show_command_arg)
{
	/* Cannot fail, return value is only the previous state */
	SetErrorMode(SEM_FAILCRITICALERRORS);

	show_command = show_command_arg;

	linted_error errnum;

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
		errnum = linted_mem_alloc_array(&xx, argc + 1U,
		                                sizeof argv[0U]);
		if (errnum != 0)
			return EXIT_FAILURE;
		argv = xx;
	}

	for (size_t ii = 0U; ii < argc; ++ii) {
		errnum = linted_utf_2_to_1(wide_argv[ii], &argv[ii]);
		if (errnum != 0)
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
		errnum = linted_environment_get("LINTED_SERVICE", &xx);
		if (errnum != 0)
			return EXIT_FAILURE;
		service = xx;
	}
	if (service != 0) {
		process_name = service;
	} else if (argc > 0) {
		process_name = argv[0U];
	} else {
		process_name =
		    linted_start_config.canonical_process_name;
		missing_name = true;
	}

	char *process_basename = strdup(process_name);
	if (0 == process_basename)
		return EXIT_FAILURE;
	process_basename = basename(process_basename);

	linted_log_open(process_basename);

	if (missing_name) {
		linted_log(LINTED_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	errnum = linted_signal_init();
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_signal_init: %s",
		           linted_error_string(errnum));
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

	return linted_start_config.start(process_basename, argc,
	                                 (char const *const *)argv);
}
