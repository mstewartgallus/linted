/*
 * Copyright 2014 Steven Stewart-Gallus
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

#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <limits.h>
#include <string.h>
#include <windows.h>

static char const invalid_error_string[] = "invalid error number";
static char const out_of_memory_string[] = "cannot print error, out of memory";

char const *linted_error_string(linted_error errnum_to_print)
{
	linted_error errnum;

	if (errnum_to_print > UINT32_MAX)
		return invalid_error_string;

	wchar_t *message;
	if (0 == FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
	                           FORMAT_MESSAGE_FROM_SYSTEM,
	                       0, errnum_to_print, 0, (void *)&message, 0, 0))
		return out_of_memory_string;

	size_t buffer_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
	                                         message, -1, 0, 0, 0, 0);
	if (0 == buffer_size)
		goto free_message;

	char *buffer;
	{
		void *xx;
		errnum =
		    linted_mem_alloc_array(&xx, buffer_size, sizeof buffer[0U]);
		if (errnum != 0)
			goto free_message;
		buffer = xx;
	}

	if (0 == WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, message, -1,
	                             buffer, buffer_size, 0, 0)) {
		linted_mem_free(buffer);
		goto free_message;
	}

	return buffer;

free_message:
	LocalFree(message);
	return out_of_memory_string;
}

void linted_error_string_free(char const *str)
{
	if (invalid_error_string == str)
		return;

	if (out_of_memory_string == str)
		return;

	linted_mem_free((char *)str);
}