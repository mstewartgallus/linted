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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static linted_error valloc_sprintf(char **strp, size_t *sizep, const char *fmt,
                                   va_list ap);

linted_error linted_str_append(char **bufp, size_t *capp, size_t *sizep,
                               char const *str, size_t strsize)
{
    char *buf = *bufp;
    size_t cap = *capp;
    size_t size = *sizep;

    size_t new_size = size + strsize;

    size_t new_cap = cap;
    if (new_size > cap) {
        new_cap = new_size;
        char *new_buf = realloc(buf, new_cap);
        if (NULL == new_buf) {
            return errno;
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

linted_error linted_str_append_str(char **bufp, size_t *capp, size_t *sizep,
                                   struct linted_str str)
{
    return linted_str_append(bufp, capp, sizep, str.bytes, str.size);
}

linted_error linted_str_append_format(char **bufp, size_t *capp, size_t *sizep,
                                      char const *fmt, ...)
{
    linted_error errnum = 0;
    size_t strsize;
    char *str;

    va_list ap;
    va_start(ap, fmt);

    if ((errnum = valloc_sprintf(&str, &strsize, fmt, ap)) != 0) {
        goto free_ap;
    }

    if ((errnum = linted_str_append(bufp, capp, sizep, str, strsize)) != 0) {
        goto free_str;
    }

free_str:
    free(str);

free_ap:
    va_end(ap);

    return errnum;
}

static linted_error valloc_sprintf(char **strp, size_t *sizep, const char *fmt,
                                   va_list ap)
{
    linted_error error_status = 0;

    va_list ap_copy;
    va_copy(ap_copy, ap);

    int bytes_should_write = vsnprintf(NULL, 0, fmt, ap);
    if (bytes_should_write < 0) {
        error_status = errno;
        goto free_ap_copy;
    }

    {
        size_t string_size = bytes_should_write + 1;

        char *string = malloc(string_size);
        if (NULL == string) {
            error_status = errno;
            goto free_ap_copy;
        }

        if (vsnprintf(string, string_size, fmt, ap_copy) < 0) {
            free(string);

            error_status = errno;
            goto free_ap_copy;
        }

        *sizep = string_size;
        *strp = string;
    }

free_ap_copy:
    va_end(ap_copy);

    return error_status;
}
