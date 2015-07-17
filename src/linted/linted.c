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
#include "config.h"

#ifdef HAVE_WINDOWS_API
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN
#endif

#ifdef HAVE_POSIX_API
#define _POSIX_C_SOURCE 200809L
#endif

#include "privilege.h"
#include "settings.h"

#include "linted/environment.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/locale.h"
#include "linted/log.h"
#include "linted/start.h"
#include "linted/util.h"

#ifdef HAVE_WINDOWS_API
#include "linted/utf.h"
#endif

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <libgen.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_WINDOWS_API
#include <windows.h>
#endif

struct envvar
{
	char const *key;
	char const *value;
};

enum { HELP, VERSION_OPTION };

extern char **environ;

static unsigned char main_start(char const *const process_name,
                                size_t argc, char const *const *argv);

static linted_error do_help(linted_ko ko, char const *process_name,
                            char const *package_name,
                            char const *package_url,
                            char const *package_bugreport);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-linted",
    .start = main_start};

static struct envvar const default_envvars[] = {
    {"LINTED_PROCESS_NAME", "linted"},
    {"LINTED_UNIT_PATH", LINTED_UNIT_PATH},
    {"LINTED_INIT", LINTED_INIT},
    {"LINTED_MONITOR", LINTED_MONITOR},
    {"LINTED_SANDBOX", LINTED_SANDBOX},
    {"LINTED_WAITER", LINTED_WAITER},
    {"LINTED_GUI", LINTED_GUI},
    {"LINTED_GUI_FSTAB", LINTED_GUI_FSTAB},
    {"LINTED_SIMULATOR", LINTED_SIMULATOR},
    {"LINTED_SIMULATOR_FSTAB", LINTED_SIMULATOR_FSTAB},
    {"LINTED_DRAWER", LINTED_DRAWER},
    {"LINTED_DRAWER_FSTAB", LINTED_DRAWER_FSTAB},
    {"LINTED_WINDOW", LINTED_WINDOW},
    {"LINTED_WINDOW_FSTAB", LINTED_WINDOW_FSTAB}};

static char const *const argstrs[] = {[HELP] = "--help",
                                      [VERSION_OPTION] = "--version"};

static unsigned char main_start(char const *const process_name,
                                size_t argc,
                                char const *const *const argv)
{
	linted_error err = linted_linted_privilege_check();
	if (err != 0) {
		linted_log(
		    LINTED_LOG_ERROR,
		    "%s should not be run with high privileges: %s",
		    PACKAGE_NAME, linted_error_string(err));
		return EXIT_FAILURE;
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(default_envvars);
	     ++ii) {
		struct envvar const *envvar = &default_envvars[ii];

		err = linted_environment_set(envvar->key, envvar->value,
		                             false);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_set: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	{
		char
		    pid_str[LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) + 1U];
		sprintf(pid_str, "%" PRIuMAX, (uintmax_t)getpid());

		err =
		    linted_environment_set("MANAGERPID", pid_str, true);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_set: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	char const *init;
	{
		char *xx;
		err = linted_environment_get("LINTED_INIT", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		init = xx;
	}
	assert(init != 0);

	bool need_help = false;
	bool need_version = false;

	char const *bad_option = 0;

	for (size_t ii = 1U; ii < argc; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(argstrs);
		     ++jj) {
			if (0 == strcmp(argument, argstrs[jj])) {
				arg = jj;
				break;
			}
		}

		switch (arg) {
		case -1:
			bad_option = argument;
			break;

		case HELP:
			need_help = true;
			break;

		case VERSION_OPTION:
			need_version = true;
			break;
		}
	}

	if (need_help) {
		do_help(LINTED_KO_STDOUT, process_name, PACKAGE_NAME,
		        PACKAGE_URL, PACKAGE_BUGREPORT);
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR,
		                            process_name, bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT, PACKAGE_STRING,
		                      COPYRIGHT_YEAR);
		return EXIT_SUCCESS;
	}

	linted_io_write_format(LINTED_KO_STDOUT, 0,
	                       "LINTED_PID=%" PRIuMAX "\n",
	                       (uintmax_t)getpid());

	char *init_dup = strdup(init);
	if (0 == init_dup) {
		linted_log(LINTED_LOG_ERROR, "strdup: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}
	char *init_base = basename(init_dup);

#ifdef HAVE_WINDOWS_API
	wchar_t *init_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(init, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_utf_1_to_2: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		init_utf2 = xx;
	}

	wchar_t *init_base_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(init_base, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_utf_1_to_2: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		init_base_utf2 = xx;
	}

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
		if (!CreateProcess(init_utf2, init_base_utf2, 0, 0,
		                   false, creation_flags, 0, 0,
		                   &startup_info,
		                   &process_information)) {
			linted_log(LINTED_LOG_ERROR,
			           "CreateProcessW: %s",
			           linted_error_string(GetLastError()));
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
		linted_log(LINTED_LOG_ERROR, "WaitForSingleObject: %s",
		           linted_error_string(GetLastError()));
		return EXIT_FAILURE;

	default:
		assert(false);
	}

	DWORD xx;
	if (!GetExitCodeProcess(monitor_handle, &xx)) {
		linted_log(LINTED_LOG_ERROR, "GetExitCodeProcess: %s",
		           linted_error_string(GetLastError()));
		return EXIT_FAILURE;
	}
	return xx;

#else
	char const *const init_argv[] = {init_base, 0};
	execve(init, (char *const *)init_argv, environ);
	linted_log(LINTED_LOG_ERROR, "execve: %s",
	           linted_error_string(errno));
	return EXIT_FAILURE;
#endif
}

static linted_error do_help(linted_ko ko, char const *process_name,
                            char const *package_name,
                            char const *package_url,
                            char const *package_bugreport)
{
	linted_error err;

	err = linted_io_write_string(ko, 0, "Usage: ");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, process_name);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, " [OPTIONS]\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "Play the game.\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
  LINTED_UNIT_PATH    a `:' separated list of directories units are from\n\
  LINTED_LOGGER_FSTAB the location of the logger fstab\n\
  LINTED_LOGGER       the location of the logger executable\n\
  LINTED_GUI_FSTAB    the location of the GUI fstab\n\
  LINTED_GUI          the location of the GUI executable\n\
  LINTED_SIMULATOR_FSTAB the location of the simulator fstab\n\
  LINTED_SIMULATOR    the location of the simulator executable\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "Report bugs to <");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, package_bugreport);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, package_name);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, " home page: <");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, package_url);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	return 0;
}
