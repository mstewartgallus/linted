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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "lntd/str.h"

#include "lntd/error.h"
#include "lntd/mem.h"
#include "lntd/util.h"

#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static lntd_error valloc_sprintf(char **strbp, size_t *sizep,
                                 const char *fmt, va_list ap)
    LNTD_FORMAT(__printf__, 3, 0);

lntd_error lntd_str_dup(char **resultp, char const *input)
{
	return lntd_str_dup_len(resultp, input, SIZE_MAX);
}

#if defined HAVE_WINDOWS_API
lntd_error lntd_str_dup_len(char **resultp, char const *input, size_t n)
{
	size_t ii = 0U;
	for (; ii < n; ++ii)
		if ('\0' == input[ii])
			break;

	size_t len = ii;

	lntd_error err = 0;

	char *copy;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, len + 1U);
		if (err != 0)
			return err;
		copy = xx;
	}

	memcpy(copy, input, len);
	copy[len] = '\0';

	*resultp = copy;

	return 0;
}
#else
lntd_error lntd_str_dup_len(char **resultp, char const *input, size_t n)
{
	char *result = strndup(input, n);
	if (0 == result) {
		lntd_error err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	*resultp = result;
	return 0;
}
#endif

lntd_error lntd_str_append(char **bufp, size_t *capp, size_t *sizep,
                           char const *str, size_t strsize)
{
	lntd_error err;
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
			err = lntd_mem_realloc(&xx, buf, new_cap);
			if (err != 0)
				return err;
			new_buf = xx;
		}
		buf = new_buf;
	}

	if (strsize > 0U)
		memcpy(buf + size, str, strsize);

	cap = new_cap;
	size = new_size;

	*bufp = buf;
	*capp = cap;
	*sizep = size;

	return 0;
}

lntd_error lntd_str_append_cstring(char **bufp, size_t *capp,
                                   size_t *sizep, char const *str)
{
	return lntd_str_append(bufp, capp, sizep, str, strlen(str));
}

lntd_error lntd_str_format(char **strp, char const *format, ...)
{
	va_list ap;
	va_start(ap, format);

	lntd_error err = valloc_sprintf(strp, 0, format, ap);

	va_end(ap);

	return err;
}

lntd_error lntd_str_append_format(char **bufp, size_t *capp,
                                  size_t *sizep, char const *fmt, ...)
{
	lntd_error err = 0;
	size_t strsize;
	char *str;

	{
		va_list ap;
		va_start(ap, fmt);

		{
			char *xx;
			size_t yy;
			err = valloc_sprintf(&xx, &yy, fmt, ap);
			if (err != 0)
				goto free_ap;
			str = xx;
			strsize = yy;
		}

	free_ap:
		va_end(ap);

		if (err != 0)
			return err;
	}

	err = lntd_str_append(bufp, capp, sizep, str, strsize);

	lntd_mem_free(str);

	return err;
}

static lntd_error valloc_sprintf(char **strp, size_t *sizep,
                                 const char *fmt, va_list ap)
{
	lntd_error err = 0;

	va_list ap_copy;
	va_copy(ap_copy, ap);

	int bytes_should_write;

#if defined HAVE_WINDOWS_API
	bytes_should_write = vsprintf_s(0, 0, fmt, ap);
#else
	bytes_should_write = vsnprintf(0, 0, fmt, ap);
#endif

	if (bytes_should_write < 0) {
		err = errno;
		LNTD_ASSUME(err != 0);
		goto free_ap_copy;
	}

	{
		size_t string_size = 1U + (unsigned)bytes_should_write;

		char *string;
		{
			void *xx;
			err = lntd_mem_alloc(&xx, string_size);
			if (err != 0)
				goto free_ap_copy;
			string = xx;
		}

		int res;

#if defined HAVE_WINDOWS_API
		res = vsprintf_s(string, string_size, fmt, ap_copy);
#else
		res = vsnprintf(string, string_size, fmt, ap_copy);
#endif
		if (res < 0) {
			lntd_mem_free(string);

			err = errno;
			LNTD_ASSUME(err != 0);
			goto free_ap_copy;
		}

		if (sizep != 0)
			*sizep = (unsigned)bytes_should_write;
		*strp = string;
	}

free_ap_copy:
	va_end(ap_copy);

	return err;
}
