/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#define WINVER 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/utf.h"

#include "linted/mem.h"
#include "linted/util.h"

#include <windows.h>

linted_error linted_utf_2_to_1(wchar_t const *input, char **outputp)
{
	linted_error errnum;

	size_t buffer_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
	                                         input, -1, 0, 0, 0, 0);
	if (0 == buffer_size) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	char *buffer;
	{
		void *xx;
		errnum =
		    linted_mem_alloc_array(&xx, buffer_size, sizeof buffer[0U]);
		if (errnum != 0)
			return errnum;
		buffer = xx;
	}

	if (0 == WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, input, -1,
	                             buffer, buffer_size, 0, 0)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);

		linted_mem_free(buffer);

		return errnum;
	}

	*outputp = buffer;

	return 0;
}

linted_error linted_utf_1_to_2(char const *input, wchar_t **outputp)
{
	linted_error errnum;

	size_t buffer_size =
	    MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, input, -1, 0, 0);
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

	if (0 == MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, input, -1,
	                             buffer, buffer_size)) {
		errnum = GetLastError();
		LINTED_ASSUME(errnum != 0);

		linted_mem_free(buffer);

		return errnum;
	}

	*outputp = buffer;

	return 0;
}
