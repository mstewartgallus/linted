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
#ifndef LINTED_IO_H
#define LINTED_IO_H

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/str.h"

#include <stddef.h>
#include <sys/types.h>

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
 *                    count bytes and at least zero bytes. If NULL
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

linted_error linted_io_write_str(linted_ko ko, size_t *bytes_wrote,
                                 struct linted_str str);

linted_error linted_io_write_string(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s);

linted_error linted_io_write_format(linted_ko ko, size_t *bytes_wrote_out,
                                    char const *s, ...);

#endif /* LINTED_IO_H */
