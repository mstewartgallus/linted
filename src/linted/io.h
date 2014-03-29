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

#include <sys/select.h>
#include <sys/types.h>

/**
 * @file
 *
 * Utility functions for working with file descriptors.
 */

/**
 * The linted_io_read_all function repeatedly reads from fd until buf
 * is full or an error occurs (except for EINTR).
 *
 * The read may be successful and read less than count if the end of
 * file is reached.
 *
 * For example, a bit could be read and then fd could be closed and an
 * error would be returned but some bytes would still have been read.
 *
 * @param fd The file to be read from.
 *
 * @param bytes_read The amount read. Is set to no larger than count
 *                   bytes and at least zero bytes. If NULL then is
 *                   not set.
 *
 * @param buf The buffer to be read into. Must be at least count bytes
 *            long.
 *
 * @param count The amount to read. No more than these many bytes is
 *              read.
 *
 * @returns Zero on success. -1 on error, and errno is set
 *          appropriately.
 *
 * @error EAGAIN The file descriptor has been marked nonblocking and
 *               one of the reads would block.
 *
 * @error EWOULDBLOCK Means the same as EAGAIN but may be a different
 *                    value.
 *
 * @error EBADF fd is not a valid file descriptor or is not open for
 *              reading.
 *
 * @error EFAULT buf is not accessible.
 *
 * @error EINVAL fd is not readable or the file was opened with
 *               O_DIRECT and alignment restrictions weren't
 *               satisfied.
 *
 * @error EIO I/O error.
 *
 * @error EISDIR fd is a directory.
 */
int linted_io_read_all(int fd, size_t * bytes_read, void *buf, size_t count);

/**
 * The linted_io_write_all function repeatedly writes to fd until of
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
 * @returns Zero on success. -1 on error, and errno is set
 *          appropriately.
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
int linted_io_write_all(int fd, size_t * bytes_wrote,
                        void const *buf, size_t count);

int linted_io_write_string(int fd, size_t * bytes_wrote_out, char const *s);

int linted_io_write_format(int fd, size_t * bytes_wrote_out,
                           char const *s, ...);

int linted_io_strtofd(char const * ptr);

/**
 *
 * The linted_io_close_fds_except function closes all file descriptors
 * in the process except for those listed in fds.
 *
 * @warning This function is not thread safe.
 *
 * @par
 *
 * @warning On error, only some files may be closed.
 *
 * @param fds The files not to close.
 *
 * @returns Zero on success. -1 on error, and errno is set
 *          appropriately.
 *
 * @error EMFILE Too many process file descriptors in use.
 *
 * @error ENFILE Too many system file descriptors in use.
 *
 * @error ENOMEM Insufficient memory.
*/
int linted_io_close_fds_except(fd_set const *fds);

#endif                          /* LINTED_IO_H */
