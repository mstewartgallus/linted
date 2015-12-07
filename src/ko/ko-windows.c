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

#define _WIN32_WINNT 0x0600

#include "config.h"

#include "lntd/ko.h"

#include "lntd/mem.h"
#include "lntd/error.h"
#include "lntd/str.h"
#include "lntd/utf.h"
#include "lntd/util.h"

#include <direct.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include <winsock2.h>
#include <windows.h>

lntd_error lntd_ko_open(lntd_ko *kop, lntd_ko dirko,
                        char const *pathname, unsigned long flags)
{
	lntd_error err;

	if ((flags & ~LNTD_KO_RDONLY & ~LNTD_KO_WRONLY & ~LNTD_KO_RDWR &
	     ~LNTD_KO_APPEND & ~LNTD_KO_SYNC & ~LNTD_KO_DIRECTORY) !=
	    0U)
		return LNTD_ERROR_INVALID_PARAMETER;

	bool ko_rdonly = (flags & LNTD_KO_RDONLY) != 0U;
	bool ko_wronly = (flags & LNTD_KO_WRONLY) != 0U;
	bool ko_rdwr = (flags & LNTD_KO_RDWR) != 0U;

	bool ko_append = (flags & LNTD_KO_APPEND) != 0U;
	bool ko_sync = (flags & LNTD_KO_SYNC) != 0U;

	bool ko_directory = (flags & LNTD_KO_DIRECTORY) != 0U;

	if (ko_rdonly && ko_wronly)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (ko_rdwr && ko_rdonly)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (ko_rdwr && ko_wronly)
		return LNTD_ERROR_INVALID_PARAMETER;

	if (ko_append && !ko_wronly)
		return LNTD_ERROR_INVALID_PARAMETER;

	if ((ko_directory && ko_rdonly) ||
	    (ko_directory && ko_wronly) || (ko_directory && ko_rdwr) ||
	    (ko_directory && ko_sync))
		return LNTD_ERROR_INVALID_PARAMETER;

	DWORD desired_access = 0;

	if (ko_rdonly)
		desired_access |= GENERIC_READ;

	if (ko_wronly)
		desired_access |= GENERIC_WRITE;

	if (ko_rdwr)
		desired_access |= GENERIC_READ | GENERIC_WRITE;

	char *real_path;
	{
		char *xx;
		err = lntd_ko_real_path(&xx, dirko, pathname);
		if (err != 0)
			return err;
		real_path = xx;
	}

	wchar_t *pathname_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(real_path, &xx);
		if (err != 0)
			goto free_real_path;
		pathname_utf2 = xx;
	}

	lntd_ko ko = CreateFile(pathname_utf2, desired_access, 0, 0,
	                        OPEN_EXISTING, 0, 0);
	if (INVALID_HANDLE_VALUE == ko) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
	}

	lntd_mem_free(pathname_utf2);

free_real_path:
	lntd_mem_free(real_path);

	if (err != 0)
		return err;

	*kop = ko;

	return 0;
}

lntd_error lntd_ko_close(lntd_ko ko)
{
	lntd_error err;

	if (SOCKET_ERROR == closesocket((uintptr_t)ko)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
		if (err != WSAENOTSOCK)
			return err;
	}

	if (!CloseHandle(ko)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}

lntd_ko lntd_ko__get_stdin(void)
{
	return (lntd_ko)GetStdHandle(STD_INPUT_HANDLE);
}

lntd_ko lntd_ko__get_stdout(void)
{
	return (lntd_ko)GetStdHandle(STD_OUTPUT_HANDLE);
}

lntd_ko lntd_ko__get_stderr(void)
{
	return (lntd_ko)GetStdHandle(STD_ERROR_HANDLE);
}

lntd_error lntd_ko_change_directory(char const *pathname)
{
	lntd_error err = 0;

	wchar_t *pathname_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(pathname, &xx);
		if (err != 0)
			return err;
		pathname_utf2 = xx;
	}

	if (!SetCurrentDirectoryW(pathname_utf2)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
	}

	lntd_mem_free(pathname_utf2);

	return err;
}

lntd_error lntd_ko_symlink(char const *oldpath, char const *newpath)
{
	lntd_error err = 0;

	wchar_t *oldpath_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(oldpath, &xx);
		if (err != 0)
			return err;
		oldpath_utf2 = xx;
	}

	wchar_t *newpath_utf2;
	{
		wchar_t *xx;
		err = lntd_utf_1_to_2(newpath, &xx);
		if (err != 0)
			goto free_oldpath;
		newpath_utf2 = xx;
	}

	if (!CreateSymbolicLinkW(newpath_utf2, oldpath_utf2,
	                         SYMBOLIC_LINK_FLAG_DIRECTORY)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
	}

	lntd_mem_free(newpath_utf2);

free_oldpath:
	lntd_mem_free(oldpath_utf2);

	return err;
}

/**
 * @bug Windows NT: work on directories other than `LNTD_KO_CWD`
 * @bug Windows NT: actually resolve the path
 */
lntd_error lntd_ko_real_path(char **resultp, lntd_ko dirko,
                             char const *pathname)
{
	lntd_error err = 0;

	LNTD_ASSERT(resultp != 0);
	LNTD_ASSERT(pathname != 0);

	if (dirko != LNTD_KO_CWD)
		return LNTD_ERROR_INVALID_PARAMETER;

	char *result;
	{
		char *xx;
		err = lntd_str_dup(&xx, pathname);
		if (err != 0)
			return err;
		result = xx;
	}

	*resultp = result;

	return 0;
}
