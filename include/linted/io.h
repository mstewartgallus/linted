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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_IO_H
#define LINTED_IO_H

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/util.h"

#include <stddef.h>

struct linted_asynch_pool;
struct linted_asynch_task;

struct linted_io_task_poll;
struct linted_io_task_read;
struct linted_io_task_write;
struct linted_io_task_recv;
struct linted_io_task_sendto;
struct linted_io_task_accept;

struct sockaddr;

/**
 * @file
 *
 * Abstracts over chunked input and output.
 */

linted_error linted_io_read_all(linted_ko ko, size_t *bytes_wrote, void *buf,
                                size_t count);

/**
 * The linted_io_write_all function repeatedly writes to ko until of
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
 * @param buf The buffer to be written from. Must be at least count bytes
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
 * @error EINVAL ko is not writable or the file was opened with
 *               O_DIRECT and alignment restrictions weren't
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
linted_error linted_io_write_all(linted_ko ko, size_t *bytes_wrote,
                                 void const *buf, size_t count);

linted_error linted_io_write_string(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s);

linted_error linted_io_write_format(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s, ...)
    LINTED_FORMAT(__printf__, 3, 4);

linted_error linted_io_task_poll_create(struct linted_io_task_poll **taskp,
                                        void *data);
void linted_io_task_poll_destroy(struct linted_io_task_poll *task);

struct linted_io_task_poll *
linted_io_task_poll_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_io_task_poll_to_asynch(struct linted_io_task_poll *);
void linted_io_task_poll_prepare(struct linted_io_task_poll *task,
                                 unsigned task_action, linted_ko ko, int flags);
void *linted_io_task_poll_data(struct linted_io_task_poll *task);

linted_error linted_io_task_read_create(struct linted_io_task_read **taskp,
                                        void *data);
void linted_io_task_read_destroy(struct linted_io_task_read *task);

struct linted_io_task_read *
linted_io_task_read_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_io_task_read_to_asynch(struct linted_io_task_read *);
void linted_io_task_read_prepare(struct linted_io_task_read *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t size);
void *linted_io_task_read_data(struct linted_io_task_read *task);
linted_ko linted_io_task_read_ko(struct linted_io_task_read *task);
size_t linted_io_task_read_bytes_read(struct linted_io_task_read *task);

linted_error linted_io_task_write_create(struct linted_io_task_write **taskp,
                                         void *data);
void linted_io_task_write_destroy(struct linted_io_task_write *task);

struct linted_io_task_write *
linted_io_task_write_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_io_task_write_to_asynch(struct linted_io_task_write *);
void linted_io_task_write_prepare(struct linted_io_task_write *task,
                                  unsigned task_action, linted_ko ko,
                                  char const *buf, size_t size);
void *linted_io_task_write_data(struct linted_io_task_write *task);

linted_error linted_io_task_recv_create(struct linted_io_task_recv **taskp,
                                        void *data);
void linted_io_task_recv_destroy(struct linted_io_task_recv *task);

struct linted_io_task_recv *
linted_io_task_recv_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_io_task_recv_to_asynch(struct linted_io_task_recv *);
void linted_io_task_recv_prepare(struct linted_io_task_recv *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t size);
void *linted_io_task_recv_data(struct linted_io_task_recv *task);
linted_ko linted_io_task_recv_ko(struct linted_io_task_recv *task);
size_t linted_io_task_recv_bytes_read(struct linted_io_task_recv *task);
void linted_io_task_recv_src_addr(struct linted_io_task_recv *task,
                                  struct sockaddr *addr, size_t *addr_len);

linted_error linted_io_task_sendto_create(struct linted_io_task_sendto **taskp,
                                          void *data);
void linted_io_task_sendto_destroy(struct linted_io_task_sendto *task);

struct linted_io_task_sendto *
linted_io_task_sendto_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_io_task_sendto_to_asynch(struct linted_io_task_sendto *);
void linted_io_task_sendto_prepare(struct linted_io_task_sendto *task,
                                   unsigned task_action, linted_ko ko,
                                   char const *buf, size_t size,
                                   struct sockaddr const *addr,
                                   size_t dest_addr_size);
void *linted_io_task_sendto_data(struct linted_io_task_sendto *task);

linted_error linted_io_task_accept_create(struct linted_io_task_accept **taskp,
                                          void *data);
void linted_io_task_accept_destroy(struct linted_io_task_accept *task);

struct linted_io_task_accept *
linted_io_task_accept_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_io_task_accept_to_asynch(struct linted_io_task_accept *);
void linted_io_task_accept_prepare(struct linted_io_task_accept *task,
                                   unsigned task_action, linted_ko ko);
void *linted_io_task_accept_data(struct linted_io_task_accept *task);
linted_ko linted_io_task_accept_returned_ko(struct linted_io_task_accept *task);

void linted_io_do_poll(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task);

void linted_io_do_read(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task);

/**
 * @warning Consumes pending SIGPIPEs
 */
void linted_io_do_write(struct linted_asynch_pool *pool,
                        struct linted_asynch_task *task);

void linted_io_do_recv(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task);

void linted_io_do_sendto(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task);

void linted_io_do_accept(struct linted_asynch_pool *pool,
                         struct linted_asynch_task *task);

#endif /* LINTED_IO_H */
