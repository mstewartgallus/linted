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
#include "linted/ko.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
{
	linted_error err;

	if (dirko != LINTED_KO_CWD)
		return LINTED_ERROR_INVALID_PARAMETER;

	unsigned long perm_flags =
	    LINTED_KO_RDONLY | LINTED_KO_WRONLY | LINTED_KO_RDWR;
	unsigned long type_flags = LINTED_KO_DIRECTORY | LINTED_KO_FIFO;
	unsigned long misc_flags = LINTED_KO_APPEND | LINTED_KO_SYNC;
	unsigned long all_flags = perm_flags | type_flags | misc_flags;

	if ((flags & ~all_flags) != 0U)
		return LINTED_ERROR_INVALID_PARAMETER;

	bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0U;
	bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0U;
	bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0U;

	bool ko_append = (flags & LINTED_KO_APPEND) != 0U;
	bool ko_sync = (flags & LINTED_KO_SYNC) != 0U;

	bool ko_directory = (flags & LINTED_KO_DIRECTORY) != 0U;
	bool ko_fifo = (flags & LINTED_KO_FIFO) != 0U;

	if (ko_rdonly && ko_wronly)
		return LINTED_ERROR_INVALID_PARAMETER;

	if (ko_rdwr && ko_rdonly)
		return LINTED_ERROR_INVALID_PARAMETER;

	if (ko_rdwr && ko_wronly)
		return LINTED_ERROR_INVALID_PARAMETER;

	if (ko_append && !ko_wronly)
		return LINTED_ERROR_INVALID_PARAMETER;

	if (ko_directory &&
	    (ko_rdonly || ko_wronly || ko_rdwr || ko_append || ko_sync))
		return LINTED_ERROR_INVALID_PARAMETER;

	if (ko_fifo && ko_sync)
		return LINTED_ERROR_INVALID_PARAMETER;

	/*
	 * Always, be safe for execs and use O_NONBLOCK because asynch
	 * functions handle that anyways and open may block otherwise.
	 */
	int oflags = O_CLOEXEC | O_NOCTTY;

	/* FIFO writers give ENXIO for nonblocking opens without
	 * partners */
	if (!ko_wronly)
		oflags |= O_NONBLOCK;

	if (ko_rdonly)
		oflags |= O_RDONLY;

	if (ko_wronly)
		oflags |= O_WRONLY;

	if (ko_rdwr)
		oflags |= O_RDWR;

	if (ko_append)
		oflags |= O_APPEND;

	if (ko_sync)
		oflags |= O_SYNC;

	if (ko_directory)
		oflags |= O_DIRECTORY;

	int fd;
	do {
		fd = open(pathname, oflags);
		if (-1 == fd) {
			err = errno;
			LINTED_ASSUME(err != 0);
		} else {
			err = 0;
		}
	} while (EINTR == err);
	if (err != 0)
		return err;

	if (ko_fifo) {
		mode_t mode;
		{
			struct stat buf;
			if (-1 == fstat(fd, &buf)) {
				err = errno;
				LINTED_ASSUME(err != 0);
				goto close_file;
			}
			mode = buf.st_mode;
		}

		if (!S_ISFIFO(mode)) {
			err = LINTED_ERROR_INVALID_PARAMETER;
			goto close_file;
		}
	}

	if (ko_wronly) {
		if (-1 ==
		    fcntl(fd, F_SETFL, (long)oflags | O_NONBLOCK)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto close_file;
		}
	}

	*kop = fd;

	return 0;

close_file:
	linted_ko_close(fd);
	return err;
}

linted_error linted_ko_close(linted_ko ko)
{
	linted_error err;

	if (-1 == close(ko)) {
		err = errno;
		LINTED_ASSUME(err != 0);
	} else {
		err = 0;
	}

	return err;
}
