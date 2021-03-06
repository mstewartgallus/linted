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
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "lntd/error.h"
#include "lntd/mem.h"
#include "lntd/utf.h"
#include "lntd/util.h"

#include <limits.h>
#include <string.h>
#include <windows.h>

static char const invalid_error_string[] = "invalid error number";
static char const out_of_memory_string[] =
    "cannot print error, out of memory";

char const *lntd_error_string(lntd_error err_to_print)
{
	lntd_error err;

	if (FACILITY_WINDOWS == HRESULT_FACILITY(err_to_print))
		err_to_print = HRESULT_CODE(err_to_print);

	wchar_t *message;
	if (0 ==
	    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
	                      FORMAT_MESSAGE_FROM_SYSTEM,
	                  0, err_to_print,
	                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
	                  (void *)&message, 0, 0)) {
		err = HRESULT_FROM_WIN32(GetLastError());
		if (LNTD_ERROR_OUT_OF_MEMORY == err)
			return out_of_memory_string;
		return invalid_error_string;
	}

	char *buffer;
	{
		char *xx;
		err = lntd_utf_2_to_1(message, &xx);
		if (err != 0)
			goto free_message;
		buffer = xx;
	}

	return buffer;

free_message:
	LocalFree(message);
	return out_of_memory_string;
}

void lntd_error_string_free(char const *str)
{
	if (invalid_error_string == str)
		return;

	if (out_of_memory_string == str)
		return;

	lntd_mem_free((char *)str);
}
