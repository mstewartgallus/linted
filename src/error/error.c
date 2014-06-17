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

#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>

static char const no_memory_string[] = "\
could not allocate memory for error string";

static char const unknown_error_string[] = "unknown error";

char const *linted_error_string_alloc(linted_error errnum_to_print)
{
    linted_error errnum;
    size_t buf_size = 40u;

    char *buf = linted_mem_alloc(&errnum, buf_size);
    if (errnum != 0) {
        return no_memory_string;
    }

    for (;;) {
        if (0 == strerror_r(errnum_to_print, buf, buf_size)) {
            break;
        } else {
            errnum = errno;
            switch (errnum) {
            case ERANGE:
                break;

            case 0:
            case EINVAL:
                goto unknown_error;

            default:
                assert(0);
            }
        }

        if (SIZE_MAX / 3 < buf_size) {
            errnum = ENOMEM;
            goto out_of_memory;
        }
        buf_size = (buf_size * 3u) / 2u;
        char *newbuf = linted_mem_realloc(&errnum, buf, buf_size);
        if (errnum != 0) {
            goto out_of_memory;
        }
        buf = newbuf;
    }

    /* Save on excess memory, also give debugging allocators more
     * information.
     */
    char *newbuf = linted_mem_realloc(&errnum, buf, strlen(buf) + 1u);
    if (errnum != 0) {
        goto out_of_memory;
    }
    buf = newbuf;

    return buf;

out_of_memory:
    linted_mem_free(buf);

    return no_memory_string;

unknown_error:
    linted_mem_free(buf);

    return unknown_error_string;
}

void linted_error_string_free(char const *error_string)
{
    if (error_string != no_memory_string &&
        error_string != unknown_error_string) {
        linted_mem_free((void *)error_string);
    }
}
