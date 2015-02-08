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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "linted/ko.h"

#include "linted/mem.h"
#include "linted/error.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <winsock2.h>

/**
 * @bug dirko is not respected.
 */
linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
{
	linted_error errnum;

	if ((flags & ~LINTED_KO_RDONLY & ~LINTED_KO_WRONLY & ~LINTED_KO_RDWR &
	     ~LINTED_KO_APPEND & ~LINTED_KO_SYNC & ~LINTED_KO_DIRECTORY) != 0U)
		return EINVAL;

	bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0U;
	bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0U;
	bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0U;

	bool ko_append = (flags & LINTED_KO_APPEND) != 0U;
	bool ko_sync = (flags & LINTED_KO_SYNC) != 0U;

	bool ko_directory = (flags & LINTED_KO_DIRECTORY) != 0U;

	if (ko_rdonly && ko_wronly)
		return EINVAL;

	if (ko_rdwr && ko_rdonly)
		return EINVAL;

	if (ko_rdwr && ko_wronly)
		return EINVAL;

	if (ko_append && !ko_wronly)
		return EINVAL;

	if ((ko_directory && ko_rdonly) || (ko_directory && ko_wronly) ||
	    (ko_directory && ko_rdwr) || (ko_directory && ko_sync))
		return EINVAL;

	DWORD desired_access = 0;

	if (ko_rdonly)
		desired_access |= GENERIC_READ;

	if (ko_wronly)
		desired_access |= GENERIC_WRITE;

	if (ko_rdwr)
		desired_access |= GENERIC_READ | GENERIC_WRITE;

	size_t buffer_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
	                                         pathname, -1, 0, 0);
	if (0 == buffer_size) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	wchar_t *buffer;
	{
		void *xx;
		errnum =
		    linted_mem_alloc_array(&xx, buffer_size, sizeof buffer[0U]);
		if (errnum != 0)
			return errnum;
		buffer = xx;
	}

	if (0 == MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, pathname,
	                             -1, buffer, buffer_size)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		goto free_buffer;
	}

	linted_ko ko =
	    CreateFile(buffer, desired_access, 0, 0, OPEN_EXISTING, 0, 0);
	if (INVALID_HANDLE_VALUE == ko) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
	}

free_buffer:
	linted_mem_free(buffer);

	if (errnum != 0)
		return errnum;

	*kop = ko;

	return 0;
}

linted_error linted_ko_close(linted_ko ko)
{
	linted_error errnum;

	if (SOCKET_ERROR == closesocket((uintptr_t)ko)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		if (errnum != WSAENOTSOCK)
			return errnum;
	}

	if (!CloseHandle(ko)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

linted_ko linted_ko__get_stdin(void)
{
	return (linted_ko)GetStdHandle(STD_INPUT_HANDLE);
}

linted_ko linted_ko__get_stdout(void)
{
	return (linted_ko)GetStdHandle(STD_OUTPUT_HANDLE);
}

linted_ko linted_ko__get_stderr(void)
{
	return (linted_ko)GetStdHandle(STD_ERROR_HANDLE);
}
