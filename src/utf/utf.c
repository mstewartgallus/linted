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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define WINVER 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "lntd/utf.h"

#include "lntd/mem.h"
#include "lntd/util.h"

#include <windows.h>

lntd_error lntd_utf_2_to_1(wchar_t const *input, char **outputp)
{
	lntd_error err;

	size_t buffer_size = WideCharToMultiByte(
	    CP_UTF8, WC_ERR_INVALID_CHARS, input, -1, 0, 0, 0, 0);
	if (0 == buffer_size) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
		return err;
	}

	char *buffer;
	{
		void *xx;
		err = lntd_mem_alloc_array(&xx, buffer_size,
		                           sizeof buffer[0U]);
		if (err != 0)
			return err;
		buffer = xx;
	}

	if (0 == WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
	                             input, -1, buffer, buffer_size, 0,
	                             0)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);

		lntd_mem_free(buffer);

		return err;
	}

	*outputp = buffer;

	return 0;
}

lntd_error lntd_utf_1_to_2(char const *input, wchar_t **outputp)
{
	lntd_error err;

	size_t buffer_size = MultiByteToWideChar(
	    CP_UTF8, MB_ERR_INVALID_CHARS, input, -1, 0, 0);
	if (0 == buffer_size) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);
		return err;
	}

	wchar_t *buffer;
	{
		void *xx;
		err = lntd_mem_alloc_array(&xx, buffer_size,
		                           sizeof buffer[0U]);
		if (err != 0)
			return err;
		buffer = xx;
	}

	if (0 == MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
	                             input, -1, buffer, buffer_size)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		LNTD_ASSUME(err != 0);

		lntd_mem_free(buffer);

		return err;
	}

	*outputp = buffer;

	return 0;
}
