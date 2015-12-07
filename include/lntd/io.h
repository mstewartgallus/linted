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
#ifndef LNTD_IO_H
#define LNTD_IO_H

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/util.h"

#include <stdarg.h>
#include <stddef.h>

/**
 * @file
 *
 * Abstracts over chunked input and output.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

struct lntd_io_task_poll;
struct lntd_io_task_read;
struct lntd_io_task_write;

lntd_error lntd_io_read_all(lntd_ko ko, size_t *bytes_wrote, void *buf,
                            size_t count);

/**
 * The lntd_io_write_all function repeatedly writes to ko until of
 * buf is written or an error occurs (except for EINTR).
 *
 * For example, a bit could be written and then ko could be closed and
 * an error would be returned but some bytes would still have been
 * written.
 *
 * @param ko The kernel object to be written to.
 *
 * @param bytes_wrote The amount written. Is set to no larger than
 *                    count bytes and at least zero bytes. If 0
 *                    then is not set.
 *
 * @param buf The buffer to be written from. Must be at least count
 *bytes
 *            long.
 *
 * @param count The amount to write. No more than these many bytes is
 *              written.
 *
 * @returns Zero on success or an error code.
 *
 * @error EAGAIN The file descriptor has been marked nonblocking and
 *               one of the writes would block.
 *
 * @error EBADF ko is not a valid file descriptor or is not open for
 *              writing.
 *
 * @error EWOULDBLOCK Means the same as EAGAIN but may be a different
 *                    value.
 *
 * @error EFAULT buf is not accessible.
 *
 * @error LNTD_ERROR_INVALID_PARAMETER ko is not writable or the file
 *was
 *               opened with `O_DIRECT` and alignment restrictions
 *weren't
 *               satisfied.
 *
 * @error EIO I/O error.
 *
 * @error ENOSPC No room for the data.
 *
 * @error EPIPE ko is a pipe or socket with a closed read end.
 *
 * @error EISDIR ko is a directory.
 */
lntd_error lntd_io_write_all(lntd_ko ko, size_t *bytes_wrote,
                             void const *buf, size_t count);

lntd_error lntd_io_write_string(lntd_ko ko, size_t *bytes_wrote_out,
                                char const *s);

lntd_error lntd_io_write_format(lntd_ko ko, size_t *bytes_wrote_out,
                                char const *s, ...)
    LNTD_FORMAT(__printf__, 3, 4);

lntd_error lntd_io_write_va_list(lntd_ko ko, size_t *bytes_wrote_out,
                                 char const *s, va_list list)
    LNTD_FORMAT(__printf__, 3, 0);

lntd_error lntd_io_task_poll_create(struct lntd_io_task_poll **taskp,
                                    void *data);
void lntd_io_task_poll_destroy(struct lntd_io_task_poll *task);

struct lntd_async_task *
lntd_io_task_poll_to_async(struct lntd_io_task_poll *);
struct lntd_async_task *
lntd_io_task_poll_prepare(struct lntd_io_task_poll *task,
                          union lntd_async_ck task_ck, void *userstate,
                          lntd_ko ko, int flags);
void *lntd_io_task_poll_data(struct lntd_io_task_poll *task);

lntd_error lntd_io_task_read_create(struct lntd_io_task_read **taskp,
                                    void *data);
void lntd_io_task_read_destroy(struct lntd_io_task_read *task);

struct lntd_async_task *
lntd_io_task_read_to_async(struct lntd_io_task_read *);
struct lntd_async_task *
lntd_io_task_read_prepare(struct lntd_io_task_read *task,
                          union lntd_async_ck task_ck, void *userstate,
                          lntd_ko ko, char *buf, size_t size);
void *lntd_io_task_read_data(struct lntd_io_task_read *task);
lntd_ko lntd_io_task_read_ko(struct lntd_io_task_read *task);
size_t lntd_io_task_read_bytes_read(struct lntd_io_task_read *task);

lntd_error lntd_io_task_write_create(struct lntd_io_task_write **taskp,
                                     void *data);
void lntd_io_task_write_destroy(struct lntd_io_task_write *task);

struct lntd_async_task *
lntd_io_task_write_to_async(struct lntd_io_task_write *);
struct lntd_async_task *
lntd_io_task_write_prepare(struct lntd_io_task_write *task,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_ko ko, char const *buf, size_t size);
void *lntd_io_task_write_data(struct lntd_io_task_write *task);

void lntd_io_do_poll(struct lntd_async_pool *pool,
                     struct lntd_async_task *task);

void lntd_io_do_read(struct lntd_async_pool *pool,
                     struct lntd_async_task *task);

/**
 * @warning Consumes pending SIGPIPEs
 */
void lntd_io_do_write(struct lntd_async_pool *pool,
                      struct lntd_async_task *task);

#endif /* LNTD_IO_H */
