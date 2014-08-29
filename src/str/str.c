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
#include "config.h"

#include "linted/str.h"

#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static linted_error valloc_sprintf(char **strbp, size_t *sizep, const char *fmt,
                                   va_list ap);

linted_error linted_str_append(char **restrict bufp, size_t *restrict capp,
                               size_t *restrict sizep, char const *str,
                               size_t strsize)
{
	linted_error errnum;
	char *buf = *bufp;
	size_t cap = *capp;
	size_t size = *sizep;

	if (size > SIZE_MAX - strsize)
		return ENOMEM;

	size_t new_size = size + strsize;

	size_t new_cap = cap;
	if (new_size > cap) {
		new_cap = new_size;

		char *new_buf;
		{
			void *xx;
			errnum = linted_mem_realloc(&xx, buf, new_cap);
			if (errnum != 0)
				return errnum;
			new_buf = xx;
		}
		buf = new_buf;
	}

	memcpy(buf + size, str, strsize);

	cap = new_cap;
	size = new_size;

	*bufp = buf;
	*capp = cap;
	*sizep = size;

	return 0;
}

linted_error linted_str_append_str(char **restrict bufp, size_t *restrict capp,
                                   size_t *restrict sizep,
                                   struct linted_str str)
{
	return linted_str_append(bufp, capp, sizep, str.bytes, str.size);
}

linted_error linted_str_append_cstring(char **restrict bufp,
                                       size_t *restrict capp,
                                       size_t *restrict sizep, char const *str)
{
	return linted_str_append(bufp, capp, sizep, str, strlen(str));
}

linted_error linted_str_append_format(char **restrict bufp,
                                      size_t *restrict capp,
                                      size_t *restrict sizep, char const *fmt,
                                      ...)
{
	linted_error errnum = 0;
	size_t strsize;
	char *str;

	va_list ap;
	va_start(ap, fmt);

	{
		char *xx;
		size_t yy;
		if ((errnum = valloc_sprintf(&xx, &yy, fmt, ap)) != 0)
			goto free_ap;
		str = xx;
		strsize = yy;
	}

	errnum = linted_str_append(bufp, capp, sizep, str, strsize);
	if (errnum != 0)
		goto free_str;

free_str:
	linted_mem_free(str);

free_ap:
	va_end(ap);

	return errnum;
}

static linted_error valloc_sprintf(char **strp, size_t *sizep, const char *fmt,
                                   va_list ap)
{
	linted_error errnum = 0;

	va_list ap_copy;
	va_copy(ap_copy, ap);

	int bytes_should_write = vsnprintf(NULL, 0, fmt, ap);
	if (bytes_should_write < 0) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_ap_copy;
	}

	{
		size_t string_size = bytes_should_write + 1U;

		char *string;
		{
			void *xx;
			errnum = linted_mem_alloc(&xx, string_size);
			if (errnum != 0)
				goto free_ap_copy;
			string = xx;
		}

		if (vsnprintf(string, string_size, fmt, ap_copy) < 0) {
			linted_mem_free(string);

			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_ap_copy;
		}

		*sizep = string_size;
		*strp = string;
	}

free_ap_copy:
	va_end(ap_copy);

	return errnum;
}
