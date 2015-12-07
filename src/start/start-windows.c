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

#include "config.h"

#define LNTD_START__NO_MAIN 1
#include "lntd/start.h"

#include "lntd/async.h"
#include "lntd/env.h"
#include "lntd/mem.h"
#include "lntd/path.h"
#include "lntd/signal.h"
#include "lntd/str.h"
#include "lntd/utf.h"
#include "lntd/log.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <windows.h>

#include <shellapi.h>
#include <winsock2.h>

/**
 * @file
 *
 * @todo Windows: open standard handles if they are closed
 * (`GetStdHandle` returns a null pointer.)
 */

static int show_command;
lntd_error privilege_check(void);

int lntd_start_show_command(void)
{
	return show_command;
}

int lntd_start__main(struct lntd_start_config const *config,
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

	lntd_error err;

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
		err = lntd_mem_alloc_array(&xx, argc + 1U,
		                           sizeof argv[0U]);
		if (err != 0)
			return EXIT_FAILURE;
		argv = xx;
	}

	for (size_t ii = 0U; ii < argc; ++ii) {
		err = lntd_utf_2_to_1(wide_argv[ii], &argv[ii]);
		if (err != 0)
			return EXIT_FAILURE;
	}

	LocalFree(wide_argv);

	char const *process_name = 0;

	bool missing_name = false;

	char const *service;
	{
		char *xx;
		err = lntd_env_get("LINTED_SERVICE", &xx);
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
		err = lntd_path_base(&xx, process_name);
		if (err != 0)
			return EXIT_FAILURE;
		process_basename = xx;
	}

	lntd_log_open(process_basename);

	if (missing_name) {
		lntd_log(LNTD_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	if (config->check_privilege) {
		err = privilege_check();
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "privilege_check: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (!config->dont_init_signals) {
		err = lntd_signal_init();
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_signal_init: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	int error_code;
	{
		WSADATA xx;
		error_code = WSAStartup(MAKEWORD(2, 2), &xx);
	}
	if (error_code != 0) {
		lntd_log(LNTD_LOG_ERROR, "WSAStartup: %s",
		         lntd_error_string(error_code));
		return EXIT_FAILURE;
	}

	*_process_namep = process_basename;
	*_argcp = argc;
	*_argvp = (char const *const *)argv;

	return EXIT_SUCCESS;
}

/*
 * The following count as enabled under Wine.  I should learn how to
 * create a restricted account under Wine and then check for these.
 *
 * SE_CHANGE_NOTIFY_NAME
 * SE_CREATE_GLOBAL_NAME
 * SE_IMPERSONATE_NAME
 * SE_LOAD_DRIVER_NAME
 *
 * The following aren't found under Wine.  I should probably just
 * handle not found permissions by ignoring them.
 *
 * SE_CREATE_SYMBOLIC_LINK_NAME
 * SE_INC_WORKING_SET_NAME
 * SE_RELABEL_NAME
 * SE_TIME_ZONE_NAME
 * SE_TRUSTED_CREDMAN_ACCESS_NAME
 * SE_UNSOLICITED_INPUT_NAME
 */

static wchar_t const *const access_names[] = {
    SE_ASSIGNPRIMARYTOKEN_NAME,  /**/
    SE_ASSIGNPRIMARYTOKEN_NAME,  /**/
    SE_AUDIT_NAME,               /**/
    SE_BACKUP_NAME,              /**/
    SE_CREATE_PAGEFILE_NAME,     /**/
    SE_CREATE_PERMANENT_NAME,    /**/
    SE_CREATE_TOKEN_NAME,        /**/
    SE_DEBUG_NAME,               /**/
    SE_ENABLE_DELEGATION_NAME,   /**/
    SE_INC_BASE_PRIORITY_NAME,   /**/
    SE_INCREASE_QUOTA_NAME,      /**/
    SE_LOCK_MEMORY_NAME,         /**/
    SE_MACHINE_ACCOUNT_NAME,     /**/
    SE_MANAGE_VOLUME_NAME,       /**/
    SE_PROF_SINGLE_PROCESS_NAME, /**/
    SE_RESTORE_NAME,             /**/
    SE_SECURITY_NAME,            /**/
    SE_SHUTDOWN_NAME,            /**/
    SE_SYNC_AGENT_NAME,          /**/
    SE_SYSTEM_ENVIRONMENT_NAME,  /**/
    SE_SYSTEM_PROFILE_NAME,      /**/
    SE_SYSTEMTIME_NAME,          /**/
    SE_TAKE_OWNERSHIP_NAME,      /**/
    SE_TCB_NAME,                 /**/
    SE_UNDOCK_NAME};

lntd_error privilege_check(void)
{
	lntd_error err = 0;

	struct {
		DWORD PrivilegeCount;
		DWORD Control;
		LUID_AND_ATTRIBUTES
		Privilege[LNTD_ARRAY_SIZE(access_names)];
	} privileges = {0};
	privileges.PrivilegeCount = LNTD_ARRAY_SIZE(access_names);
	privileges.Control = 0;

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(access_names); ++ii) {
		LUID xx;

		if (!LookupPrivilegeValue(0, access_names[ii], &xx)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);
			return err;
		}

		privileges.Privilege[ii].Luid = xx;
		privileges.Privilege[ii].Attributes =
		    SE_PRIVILEGE_ENABLED;
	}

	HANDLE current_process_access_token;
	{
		HANDLE xx;
		if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY,
		                      &xx)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);
			return err;
		}
		current_process_access_token = xx;
	}

	BOOL result;
	if (!PrivilegeCheck(current_process_access_token,
	                    (void *)&privileges, &result)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
	}

	CloseHandle(current_process_access_token);

	if (err != 0)
		return err;

	if (result)
		return LNTD_ERROR_PERMISSION;
	return 0;
}
