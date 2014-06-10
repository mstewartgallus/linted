/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

#include "linted/io.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

linted_error linted_io_read_all(linted_ko ko, size_t *bytes_read_out, void *buf,
                                size_t size)
{
    struct linted_asynch_task_read read_task;
    linted_asynch_read(&read_task, 0, ko, buf, size);
    linted_asynch_pool_submit(NULL, LINTED_UPCAST(&read_task));

    if (bytes_read_out != NULL) {
        *bytes_read_out = read_task.bytes_read;
    }
    return LINTED_UPCAST(&read_task)->errnum;
}

linted_error linted_io_write_all(linted_ko ko, size_t *bytes_wrote_out,
                                 void const *buf, size_t size)
{
    struct linted_asynch_task_write write_task;
    linted_asynch_write(&write_task, 0, ko, buf, size);
    linted_asynch_pool_submit(NULL, LINTED_UPCAST(&write_task));

    if (bytes_wrote_out != NULL) {
        *bytes_wrote_out = write_task.bytes_wrote;
    }
    return LINTED_UPCAST(&write_task)->errnum;
}

linted_error linted_io_write_str(linted_ko ko, size_t *bytes_wrote,
                                 struct linted_str str)
{
    return linted_io_write_all(ko, bytes_wrote, str.bytes, str.size);
}

linted_error linted_io_write_string(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s)
{
    return linted_io_write_all(ko, bytes_wrote_out, s, strlen(s));
}

linted_error linted_io_write_format(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *format_str, ...)
{
    linted_error errnum = 0;

    va_list ap;
    va_start(ap, format_str);

    va_list ap_copy;
    va_copy(ap_copy, ap);

    int bytes_should_write = vsnprintf(NULL, 0, format_str, ap);
    if (bytes_should_write < 0) {
        errnum = errno;
        assert(errnum != 0);
        goto free_va_lists;
    }

    {
        size_t string_size = (unsigned)bytes_should_write + 1u;

        char *string = linted_mem_alloc(&errnum, string_size);
        if (errnum != 0) {
            goto free_va_lists;
        }

        if (vsnprintf(string, string_size, format_str, ap_copy) < 0) {
            errnum = errno;
            assert(errnum != 0);
            goto free_string;
        }

        errnum = linted_io_write_string(ko, bytes_wrote_out, string);

    free_string:
        linted_mem_free(string);
    }

free_va_lists:
    va_end(ap);
    va_end(ap_copy);

    return errnum;
}
