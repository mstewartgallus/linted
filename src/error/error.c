/*
 * Copyright 2013 Steven Stewart-Gallus
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
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <string.h>

static char const invalid_error_string[] = "invalid error number";
static char const out_of_memory_string[] = "cannot print error, out of memory";

char const *linted_error_string(linted_error errnum_to_print)
{
	if (errnum_to_print > INT_MAX)
		return invalid_error_string;

	linted_error errnum;

	char *buf = 0;
	size_t buf_size = 0U;

	for (;;) {
		buf_size = 2U * buf_size + 1U;
		{
			void *xx;
			errnum = linted_mem_realloc(&xx, buf, buf_size);
			if (errnum != 0)
				break;
			buf = xx;
		}

		errnum = strerror_r(errnum_to_print, buf, buf_size);
		if (0 == errnum)
			break;

		if (errnum != ERANGE)
			break;
	}

	if (0 == errnum) {
		void *xx;
		errnum = linted_mem_realloc(&xx, buf, strlen(buf) + 1U);
		if (0 == errnum)
			buf = xx;
	}

	if (errnum != 0)
		linted_mem_free(buf);

	switch (errnum) {
	case 0:
		return buf;

	case EINVAL:
		return invalid_error_string;

	case ENOMEM:
		return out_of_memory_string;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

void linted_error_string_free(char const *str)
{
	if (invalid_error_string == str)
		return;

	if (out_of_memory_string == str)
		return;

	linted_mem_free((char *)str);
}
