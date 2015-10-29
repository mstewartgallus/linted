/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
#define _XOPEN_SOURCE 700

#include "config.h"

#include "linted/file.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <sys/stat.h>

linted_error linted_file_create(linted_ko *kop, linted_ko dirko,
                                char const *pathname,
                                unsigned long flags, mode_t mode)
{
	linted_error err;

	int dirfd;
	if (LINTED_KO_CWD == dirko) {
		dirfd = AT_FDCWD;
	} else if (dirko > INT_MAX) {
		return EINVAL;
	} else {
		dirfd = dirko;
	}

	if (0 == kop) {
		if (flags != 0U)
			return EINVAL;

		if (-1 == mknodat(dirfd, pathname, mode | S_IFREG, 0)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			if (EEXIST == err)
				return 0;
			return err;
		}

		return 0;
	}

	if ((flags & ~LINTED_FILE_RDONLY & ~LINTED_FILE_WRONLY &
	     ~LINTED_FILE_RDWR & ~LINTED_FILE_SYNC &
	     ~LINTED_FILE_EXCL) != 0U)
		return EINVAL;

	bool file_rdonly = (flags & LINTED_FILE_RDONLY) != 0U;
	bool file_wronly = (flags & LINTED_FILE_WRONLY) != 0U;
	bool file_rdwr = (flags & LINTED_FILE_RDWR) != 0U;

	bool file_sync = (flags & LINTED_FILE_SYNC) != 0U;

	bool file_excl = (flags & LINTED_FILE_EXCL) != 0U;

	if (file_rdonly && file_wronly)
		return EINVAL;

	if (file_rdwr && file_rdonly)
		return EINVAL;

	if (file_rdwr && file_wronly)
		return EINVAL;

	/*
	 * Always, be safe for execs and use O_NONBLOCK because async
	 * functions handle that anyways and open may block otherwise.
	 */
	int oflags = O_CLOEXEC | O_NONBLOCK | O_CREAT;

	/* FIFO writers give ENXIO for nonblocking opens without
	 * partners */
	if (!file_wronly)
		oflags |= O_NONBLOCK;

	if (file_rdonly)
		oflags |= O_RDONLY;

	if (file_wronly)
		oflags |= O_WRONLY;

	if (file_rdwr)
		oflags |= O_RDWR;

	if (file_sync)
		oflags |= O_SYNC;

	if (file_excl)
		oflags |= O_EXCL;

	int fd;
	do {
		fd = openat(dirfd, pathname, oflags, mode);
		if (-1 == fd) {
			err = errno;
			LINTED_ASSUME(err != 0);
		} else {
			err = 0;
		}
	} while (EINTR == err);
	if (err != 0)
		return err;

	if (file_wronly) {
		if (-1 == fcntl(fd, F_SETFL, O_NONBLOCK | oflags)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			linted_ko_close(fd);
			return err;
		}
	}

	*kop = fd;

	return 0;
}
