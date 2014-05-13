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

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/util.h"

#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <limits.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

void linted_io_poll(struct linted_asynch_task_poll* task,
                    int task_action,
                    struct pollfd* fds, size_t size)
{
    memset(task, 0, sizeof *task);

    task->parent.type = LINTED_ASYNCH_TASK_POLL;
    task->parent.task_action = task_action;
    task->fds = fds;
    task->size = size;
}

void linted_io_read(struct linted_asynch_task_read* task,
                    int task_action,
                    linted_ko ko, char* buf, size_t size)
{
    memset(task, 0, sizeof *task);

    task->parent.type = LINTED_ASYNCH_TASK_READ;
    task->parent.task_action = task_action;
    task->ko = ko;
    task->buf = buf;
    task->size = size;
}

void linted_io_mq_receive(struct linted_asynch_task_mq_receive* task,
                          int task_action,
                          linted_ko ko, char* buf, size_t size)
{
    memset(task, 0, sizeof *task);

    task->parent.type = LINTED_ASYNCH_TASK_MQ_RECEIVE;
    task->parent.task_action = task_action;
    task->ko = ko;
    task->buf = buf;
    task->size = size;
}

void linted_io_mq_send(struct linted_asynch_task_mq_send* task,
                       int task_action,
                       linted_ko ko, char const* buf, size_t size)
{
    memset(task, 0, sizeof *task);

    task->parent.type = LINTED_ASYNCH_TASK_MQ_SEND;
    task->parent.task_action = task_action;
    task->ko = ko;
    task->buf = buf;
    task->size = size;
}

linted_error linted_io_read_all(int fd, size_t* bytes_read_out, void* buf,
                                size_t count)
{
    linted_error error_status = 0;
    size_t total_bytes_read = 0;

    do {
        ssize_t bytes_read
            = read(fd, (char*)buf + total_bytes_read, count - total_bytes_read);

        if (0 == bytes_read) {
            /* Hang up */
            goto output_bytes_read;
        }

        linted_error read_status = -1 == bytes_read ? errno : 0;
        if (EINTR == read_status) {
            continue;
        }

        if (read_status != 0) {
            error_status = EWOULDBLOCK == read_status ? EAGAIN : read_status;
            goto output_bytes_read;
        }

        total_bytes_read += bytes_read;
    } while (total_bytes_read != count);

output_bytes_read:
    if (bytes_read_out != NULL) {
        *bytes_read_out = total_bytes_read;
    }
    return error_status;
}

linted_error linted_io_write_all(int fd, size_t* bytes_wrote_out,
                                 void const* buf, size_t count)
{
    linted_error error_status = 0;
    size_t total_bytes_wrote = 0;

    do {
        ssize_t bytes_wrote = write(fd, (char const*)buf + total_bytes_wrote,
                                    count - total_bytes_wrote);
        linted_error write_status = -1 == bytes_wrote ? errno : 0;
        if (EINTR == write_status) {
            continue;
        }

        if (write_status != 0) {
            error_status = EWOULDBLOCK == write_status ? EAGAIN : write_status;
            goto output_bytes_wrote;
        }

        total_bytes_wrote += bytes_wrote;
    } while (total_bytes_wrote != count);

output_bytes_wrote:
    if (bytes_wrote_out != NULL) {
        *bytes_wrote_out = total_bytes_wrote;
    }
    return error_status;
}

linted_error linted_io_write_str(int fd, size_t* bytes_wrote,
                                 struct linted_str str)
{
    return linted_io_write_all(fd, bytes_wrote, str.bytes, str.size);
}

linted_error linted_io_write_string(int fd, size_t* bytes_wrote_out,
                                    char const* s)
{
    return linted_io_write_all(fd, bytes_wrote_out, s, strlen(s));
}

linted_error linted_io_write_format(int fd, size_t* bytes_wrote_out,
                                    char const* format_str, ...)
{
    linted_error error_status = 0;

    va_list ap;
    va_start(ap, format_str);

    va_list ap_copy;
    va_copy(ap_copy, ap);

    int bytes_should_write = vsnprintf(NULL, 0, format_str, ap);
    if (bytes_should_write < 0) {
        error_status = errno;
        goto free_va_lists;
    }

    {
        size_t string_size = bytes_should_write + 1;

        char* string = malloc(string_size);
        if (NULL == string) {
            error_status = errno;
            goto free_va_lists;
        }

        if (vsnprintf(string, string_size, format_str, ap_copy) < 0) {
            error_status = errno;
            goto free_string;
        }

        {
            linted_error errnum
                = linted_io_write_string(fd, bytes_wrote_out, string);
            if (errnum != 0) {
                error_status = errnum;
                goto free_string;
            }
        }

    free_string:
        free(string);
    }

free_va_lists:
    va_end(ap);
    va_end(ap_copy);

    return error_status;
}
