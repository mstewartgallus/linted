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

#include "privilege.h"

#include "linted/error.h"
#include "linted/log.h"
#include "linted/util.h"

#include <windows.h>

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

linted_error linted_linted_privilege_check(void)
{
	linted_error err = 0;

	struct
	{
		DWORD PrivilegeCount;
		DWORD Control;
		LUID_AND_ATTRIBUTES
		Privilege[LINTED_ARRAY_SIZE(access_names)];
	} privileges = {0};
	privileges.PrivilegeCount = LINTED_ARRAY_SIZE(access_names);
	privileges.Control = 0;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(access_names);
	     ++ii) {
		LUID xx;

		if (!LookupPrivilegeValue(0, access_names[ii], &xx)) {
			err = GetLastError();
			LINTED_ASSUME(err != 0);
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
			err = GetLastError();
			LINTED_ASSUME(err != 0);
			return err;
		}
		current_process_access_token = xx;
	}

	BOOL result;
	if (!PrivilegeCheck(current_process_access_token,
	                    (void *)&privileges, &result)) {
		err = GetLastError();
		LINTED_ASSUME(err != 0);
	}

	CloseHandle(current_process_access_token);

	if (err != 0)
		return err;

	if (result)
		return LINTED_ERROR_PERMISSION;
	return 0;
}
