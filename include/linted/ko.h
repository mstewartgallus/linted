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
#ifndef LINTED_KO_H
#define LINTED_KO_H

#include "linted/error.h"
#include "linted/str.h"

#include <stddef.h>

/**
 * @file
 *
 * This module abstracts over the concept of a kernel object.
 */

#if defined _WIN32
#include <windows/wtypes.h>
#endif

#if defined __unix__
typedef int linted_ko;
#elif defined _WIN32
struct linted_ko_vtable;

typedef struct linted_ko
{
    struct linted_ko_vtable const* vtable;
    char contents[];
}* linted_ko;

struct linted_ko_vtable
{
    linted_error (*close)(linted_ko ko);
};

#else
#error no known most primitive platform kernel object type
#endif

linted_error linted_ko_strtofd(char const* ptr, linted_ko* ko);

linted_error linted_ko_read_all(linted_ko ko, size_t* bytes_wrote, void* buf,
                                size_t count);

/**
 * The linted_ko_write_all function repeatedly writes to fd until of
 * buf is written or an error occurs (except for EINTR).
 *
 * For example, a bit could be written and then fd could be closed and
 * an error would be returned but some bytes would still have been
 * written.
 *
 * @param fd The file to be written to.
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
 * @error EBADF fd is not a valid file descriptor or is not open for
 *              writing.
 *
 * @error EWOULDBLOCK Means the same as EAGAIN but may be a different
 *                    value.
 *
 * @error EFAULT buf is not accessible.
 *
 * @error EINVAL fd is not writable or the file was opened with
 *               O_DIRECT and alignment restrictions weren't
 *               satisfied.
 *
 * @error EIO I/O error.
 *
 * @error ENOSPC No room for the data.
 *
 * @error EPIPE fd is a pipe or socket with a closed read end.
 *
 * @error EISDIR fd is a directory.
 */
linted_error linted_ko_write_all(linted_ko ko, size_t* bytes_wrote,
                                 void const* buf, size_t count);

linted_error linted_ko_write_str(linted_ko ko, size_t* bytes_wrote,
                                 struct linted_str str);

linted_error linted_ko_write_string(linted_ko ko, size_t* bytes_wrote_out,
                                    char const* s);

linted_error linted_ko_write_format(linted_ko ko, size_t* bytes_wrote_out,
                                    char const* s, ...);

/**
 * The linted_io_close function closes a file descriptor. The state of
 * a file descriptor after close gives an EINTR error is unspecified
 * by POSIX so this function avoids the problem by simply blocking all
 * signals.
 *
 * @warning This function blocks on close.
 *
 * @param fd The file to close.
 *
 * @returns Zero on success or an error code.
 *
 * @error EIO I/O error.
 *
 * @error EBADF Not a valid file descriptor.
 */
linted_error linted_ko_close(linted_ko ko);

linted_error linted_ko_dummy(linted_ko* kp, int flags);

#endif /* LINTED_KO_H */
