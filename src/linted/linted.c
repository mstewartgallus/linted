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

#ifndef HAVE_WINDOWS_API
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN
#endif

#ifdef HAVE_POSIX_API
#define _POSIX_C_SOURCE 200809L
#endif

#include "settings.h"

#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/locale.h"
#include "lntd/log.h"
#include "lntd/path.h"
#include "lntd/pid.h"
#include "lntd/start.h"
#include "lntd/util.h"

#ifdef HAVE_WINDOWS_API
#include "lntd/utf.h"
#endif

#include <errno.h>
#include <inttypes.h>
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

#ifdef HAVE_POSIX_API
extern char **environ;
#endif

struct envvar {
	char const *key;
	char const *value;
};

enum { HELP, VERSION_OPTION };

static lntd_error do_help(lntd_ko ko, char const *process_name,
                          char const *package_name,
                          char const *package_url,
                          char const *package_bugreport);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-linted",
    .check_privilege = true,
    .sanitize_fds = true};

static struct envvar const default_envvars[] = {
    {"LINTED_PROCESS_NAME", "linted"},
    {"LINTED_SYSTEM_CONF_PATH", LNTD_SYSTEM_CONF_PATH},
    {"LINTED_UNIT_PATH", LNTD_UNIT_PATH},
    {"LINTED_INIT", LNTD_INIT},
    {"LINTED_MONITOR", LNTD_MONITOR},
    {"LINTED_STARTUP", LNTD_STARTUP},
    {"LINTED_SANDBOX", LNTD_SANDBOX},
    {"LINTED_WAITER", LNTD_WAITER},
    {"LINTED_AUDIO", LNTD_AUDIO},
    {"LINTED_AUDIO_FSTAB", LNTD_AUDIO_FSTAB},
    {"LINTED_GUI", LNTD_GUI},
    {"LINTED_GUI_FSTAB", LNTD_GUI_FSTAB},
    {"LINTED_SIMULATOR", LNTD_SIMULATOR},
    {"LINTED_SIMULATOR_FSTAB", LNTD_SIMULATOR_FSTAB},
    {"LINTED_DRAWER", LNTD_DRAWER},
    {"LINTED_DRAWER_FSTAB", LNTD_DRAWER_FSTAB},
    {"LINTED_WINDOW", LNTD_WINDOW},
    {"LINTED_WINDOW_FSTAB", LNTD_WINDOW_FSTAB}};

static char const *const argstrs[] = {[HELP] = "--help",
                                      [VERSION_OPTION] = "--version"};

static unsigned char lntd_start_main(char const *const process_name,
                                     size_t argc,
                                     char const *const *const argv)
{
	lntd_error err = 0;

	bool need_help = false;
	bool need_version = false;

	char const *bad_option = 0;

	for (size_t ii = 1U; ii < argc; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LNTD_ARRAY_SIZE(argstrs);
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
		do_help(LNTD_KO_STDOUT, process_name, PACKAGE_NAME,
		        PACKAGE_URL, PACKAGE_BUGREPORT);
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		lntd_locale_on_bad_option(LNTD_KO_STDERR, process_name,
		                          bad_option);
		lntd_locale_try_for_more_help(LNTD_KO_STDERR,
		                              process_name, "--help");
		return EXIT_FAILURE;
	}

	if (need_version) {
		lntd_locale_version(LNTD_KO_STDOUT, PACKAGE_STRING,
		                    COPYRIGHT_YEAR);
		return EXIT_SUCCESS;
	}

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(default_envvars);
	     ++ii) {
		struct envvar const *envvar = &default_envvars[ii];

		err = lntd_env_set(envvar->key, envvar->value, false);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_env_set: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	lntd_pid self = lntd_pid_get_pid();

	{
		char pid_str[LNTD_NUMBER_TYPE_STRING_SIZE(lntd_pid) +
		             1U];
		sprintf(pid_str, "%" PRIuMAX, (uintmax_t)self);

		err = lntd_env_set("MANAGERPID", pid_str, true);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_env_set: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	char const *init;
	{
		char *xx;
		err = lntd_env_get("LINTED_INIT", &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_env_get: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		init = xx;
	}
	LNTD_ASSERT(init != 0);

	lntd_io_write_format(LNTD_KO_STDOUT, 0,
	                     "LINTED_PID=%" PRIuMAX "\n",
	                     (uintmax_t)self);

	char *init_base;
	{
		char *xx;
		err = lntd_path_base(&xx, init);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_path_base: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		init_base = xx;
	}

#ifdef HAVE_WINDOWS_API
	wchar_t *init_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(init, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_utf_1_to_2: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		init_utf2 = xx;
	}

	wchar_t *init_base_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(init_base, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_utf_1_to_2: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		init_base_utf2 = xx;
	}

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
		if (!CreateProcessW(init_utf2, init_base_utf2, 0, 0,
		                    false, creation_flags, 0, 0,
		                    &startup_info,
		                    &process_information)) {
			lntd_log(LNTD_LOG_ERROR, "CreateProcessW: %s",
			         lntd_error_string(HRESULT_FROM_WIN32(
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
		lntd_log(LNTD_LOG_ERROR, "WaitForSingleObject: %s",
		         lntd_error_string(
		             HRESULT_FROM_WIN32(GetLastError())));
		return EXIT_FAILURE;

	default:
		LNTD_ASSERT(false);
	}

	DWORD xx;
	if (!GetExitCodeProcess(monitor_handle, &xx)) {
		lntd_log(LNTD_LOG_ERROR, "GetExitCodeProcess: %s",
		         lntd_error_string(
		             HRESULT_FROM_WIN32(GetLastError())));
		return EXIT_FAILURE;
	}
	return xx;

#else
	char const *const init_argv[] = {init_base, 0};
	execve(init, (char *const *)init_argv, environ);
	lntd_log(LNTD_LOG_ERROR, "execve: %s",
	         lntd_error_string(errno));
	return EXIT_FAILURE;
#endif
}

static lntd_error do_help(lntd_ko ko, char const *process_name,
                          char const *package_name,
                          char const *package_url,
                          char const *package_bugreport)
{
	lntd_error err;

	err = lntd_io_write_string(ko, 0, "Usage: ");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, process_name);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, " [OPTIONS]\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "Play the game.\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\
  LNTD_UNIT_PATH    a `:' separated list of directories units are from\n\
  LNTD_LOGGER_FSTAB the location of the logger fstab\n\
  LNTD_LOGGER       the location of the logger executable\n\
  LNTD_GUI_FSTAB    the location of the GUI fstab\n\
  LNTD_GUI          the location of the GUI executable\n\
  LNTD_SIMULATOR_FSTAB the location of the simulator fstab\n\
  LNTD_SIMULATOR    the location of the simulator executable\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "Report bugs to <");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, package_bugreport);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, package_name);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, " home page: <");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, package_url);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	return 0;
}
