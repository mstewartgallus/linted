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

#include "linted/dir.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/path.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/utf.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <windows.h>

/**
 * @file
 *
 * Sandbox applications.
 */

enum { STOP_OPTIONS,
       HELP,
       VERSION_OPTION,
       WAIT,
       TRACEME,
       DROP_CAPS,
       NO_NEW_PRIVS,
       CHDIR,
       PRIORITY,
       CHROOTDIR,
       FSTAB,
       WAITER,
       NEWUSER_ARG,
       NEWPID_ARG,
       NEWIPC_ARG,
       NEWNET_ARG,
       NEWNS_ARG,
       NEWUTS_ARG };

static char const *const argstrs[] = {
        /**/[STOP_OPTIONS] = "--",
        /**/ [HELP] = "--help",
        /**/ [VERSION_OPTION] = "--version",
        /**/ [WAIT] = "--wait",
        /**/ [TRACEME] = "--traceme",
        /**/ [DROP_CAPS] = "--dropcaps",
        /**/ [NO_NEW_PRIVS] = "--nonewprivs",
        /**/ [CHDIR] = "--chdir",
        /**/ [PRIORITY] = "--priority",
        /**/ [CHROOTDIR] = "--chrootdir",
        /**/ [FSTAB] = "--fstab",
        /**/ [WAITER] = "--waiter",
        /**/ [NEWUSER_ARG] = "--clone-newuser",
        /**/ [NEWPID_ARG] = "--clone-newpid",
        /**/ [NEWIPC_ARG] = "--clone-newipc",
        /**/ [NEWNET_ARG] = "--clone-newnet",
        /**/ [NEWNS_ARG] = "--clone-newns",
        /**/ [NEWUTS_ARG] = "--clone-newuts"};

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-sandbox",
    .dont_init_signals = true,
    0};

/**
 * @todo Write stderr streams of the child to the system log.
 * @todo Pass full command line arguments to process.
 */
static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err;

	size_t arguments_length = argc;

	char const *bad_option = 0;
	bool need_version = false;
	bool need_help = false;

	bool wait = false;
	bool traceme = false;
	bool no_new_privs = false;
	bool drop_caps = false;

	bool clone_newuser = false;
	bool clone_newpid = false;
	bool clone_newipc = false;
	bool clone_newnet = false;
	bool clone_newns = false;
	bool clone_newuts = false;

	char const *chdir_path = 0;
	char const *priority = 0;
	char const *chrootdir = 0;
	char const *fstab = 0;
	char const *waiter = 0;
	bool have_command = false;
	size_t command_start;

	for (size_t ii = 1U; ii < arguments_length; ++ii) {
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

		case STOP_OPTIONS:
			have_command = true;
			command_start = ii;
			goto exit_loop;

		case HELP:
			need_help = true;
			break;

		case VERSION_OPTION:
			need_version = true;
			break;

		case WAIT:
			wait = true;
			break;

		case TRACEME:
			traceme = true;
			break;

		case NO_NEW_PRIVS:
			no_new_privs = true;
			break;

		case DROP_CAPS:
			drop_caps = true;
			break;

		case CHDIR:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			chdir_path = argv[ii];
			break;

		case PRIORITY:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			priority = argv[ii];
			break;

		case CHROOTDIR:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			chrootdir = argv[ii];
			break;

		case FSTAB:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			fstab = argv[ii];
			break;

		case WAITER:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			waiter = argv[ii];
			break;

		case NEWUSER_ARG:
			clone_newuser = true;
			break;

		case NEWPID_ARG:
			clone_newpid = true;
			break;

		case NEWIPC_ARG:
			clone_newipc = true;
			break;

		case NEWNET_ARG:
			clone_newnet = true;
			break;

		case NEWNS_ARG:
			clone_newns = true;
			break;

		case NEWUTS_ARG:
			clone_newuts = true;
			break;
		}
	}
exit_loop:
	if (!have_command) {
		linted_log(LINTED_LOG_ERROR, "need command");
		return EXIT_FAILURE;
	}

	if (bad_option != 0) {
		linted_log(LINTED_LOG_ERROR, "bad option: %s",
		           bad_option);
		return EXIT_FAILURE;
	}

	if ((fstab != 0 && 0 == chrootdir) ||
	    (0 == fstab && chrootdir != 0)) {
		linted_log(
		    LINTED_LOG_ERROR,
		    "--chrootdir and --fstab are required together");
		return EXIT_FAILURE;
	}

	struct setting {
		char const *name;
		bool set;
	};

	struct setting const settings[] = {
	    {"--chdir", chdir_path},
	    {"--traceme", traceme},
	    {"--nonewprivs", no_new_privs},
	    {"--dropcaps", drop_caps},
	    {"--fstab", fstab},
	    {"--chrootdir", chrootdir != 0},
	    {"--priority", priority != 0},
	    {"--waiter", waiter != 0},
	    {"--clone-newuser", clone_newuser},
	    {"--clone-newpid", clone_newpid},
	    {"--clone-newipc", clone_newipc},
	    {"--clone-newnet", clone_newnet},
	    {"--clone-newns", clone_newns},
	    {"--clone-newuts", clone_newuts}};
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(settings); ++ii) {
		struct setting const *setting = &settings[ii];
		char const *name = setting->name;
		bool set = setting->set;

		if (set) {
			linted_log(LINTED_LOG_ERROR,
			           "the option `%s' is not "
			           "implemented on this "
			           "platform",
			           name);
			return EXIT_FAILURE;
		}
	}

	char const **command = (char const **)argv + 1U + command_start;

	char *command_base;
	{
		char *xx;
		err = linted_path_base(&xx, command[0U]);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_path_base: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		command_base = xx;
	}

	char const *binary = command[0U];
	command[0U] = command_base;

	char *binary_base;
	{
		char *xx;
		err = linted_path_base(&xx, binary);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_path_base: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		binary_base = xx;
	}

	wchar_t *binary_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(binary, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_utf_1_to_2: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		binary_utf2 = xx;
	}

	wchar_t *binary_base_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(binary_base, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_utf_1_to_2: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		binary_base_utf2 = xx;
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
		if (!CreateProcess(binary_utf2, binary_base_utf2, 0, 0,
		                   false, creation_flags, 0, 0,
		                   &startup_info,
		                   &process_information)) {
			linted_log(
			    LINTED_LOG_ERROR, "CreateProcessW: %s",
			    linted_error_string(
			        HRESULT_FROM_WIN32(GetLastError())));
			return EXIT_FAILURE;
		}
		monitor_handle = process_information.hProcess;
		thread_handle = process_information.hThread;
	}
	linted_ko_close(thread_handle);

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "spawning: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	linted_ko_close(LINTED_KO_STDIN);
	linted_ko_close(LINTED_KO_STDOUT);

	if (!wait)
		return EXIT_SUCCESS;

	switch (WaitForSingleObject(monitor_handle, INFINITE)) {
	case WAIT_OBJECT_0:
		break;

	case WAIT_FAILED:
		linted_log(LINTED_LOG_ERROR, "WaitForSingleObject: %s",
		           linted_error_string(
		               HRESULT_FROM_WIN32(GetLastError())));
		return EXIT_FAILURE;

	default:
		LINTED_ASSERT(false);
	}

	DWORD xx;
	if (!GetExitCodeProcess(monitor_handle, &xx)) {
		linted_log(LINTED_LOG_ERROR, "GetExitCodeProcess: %s",
		           linted_error_string(
		               HRESULT_FROM_WIN32(GetLastError())));
		return EXIT_FAILURE;
	}
	return xx;
}
