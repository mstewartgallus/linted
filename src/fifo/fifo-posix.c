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
#define _GNU_SOURCE

#include "linted/fifo.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/path.h"
#include "linted/str.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <unistd.h>

linted_error linted_fifo_pair(linted_fifo *readerp,
                              linted_fifo *writerp, unsigned long flags)
{
	if (flags != 0U)
		return EINVAL;

	int xx[2U];
	if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
		linted_error err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	*readerp = xx[0U];
	*writerp = xx[1U];
	return 0;
}

linted_error linted_fifo_create(linted_fifo *kop, linted_ko dirko,
                                char const *pathname,
                                unsigned long flags, mode_t mode)
{
	linted_error err;
	int fd = -1;

	if (dirko > INT_MAX && dirko != LINTED_KO_CWD)
		return EINVAL;

	if (0 == kop) {
		if (flags != 0U)
			return EINVAL;

		int dirfd;
		if (LINTED_KO_CWD == dirko) {
			dirfd = AT_FDCWD;
		} else if (dirko > INT_MAX) {
			return EINVAL;
		} else {
			dirfd = dirko;
		}

		if (-1 == mkfifoat(dirfd, pathname, mode)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			if (EEXIST == err)
				return 0;

			return err;
		}

		return 0;
	}

	if ((flags & ~LINTED_FIFO_ONLY & ~LINTED_FIFO_RDONLY &
	     ~LINTED_FIFO_WRONLY & ~LINTED_FIFO_RDWR) != 0U)
		return EINVAL;

	bool fifo_only = (flags & LINTED_FIFO_ONLY) != 0U;
	bool fifo_rdonly = (flags & LINTED_FIFO_RDONLY) != 0U;
	bool fifo_wronly = (flags & LINTED_FIFO_WRONLY) != 0U;
	bool fifo_rdwr = (flags & LINTED_FIFO_RDWR) != 0U;

	if (fifo_rdonly && fifo_wronly)
		return EINVAL;

	if (fifo_rdwr && fifo_rdonly)
		return EINVAL;

	if (fifo_rdwr && fifo_wronly)
		return EINVAL;

	unsigned long oflags = 0U;

	if (fifo_only)
		oflags |= LINTED_KO_FIFO;

	if (fifo_rdonly)
		oflags |= LINTED_KO_RDONLY;

	if (fifo_wronly)
		oflags |= LINTED_KO_WRONLY;

	if (fifo_rdwr)
		oflags |= LINTED_KO_RDWR;

	char *pathnamedir;
	{
		char *xx;
		err = linted_path_dir(&xx, pathname);
		if (err != 0)
			return err;
		pathnamedir = xx;
	}

	char *pathnamebase;
	{
		char *xx;
		err = linted_path_base(&xx, pathname);
		if (err != 0)
			goto free_pathnamedir;
		pathnamebase = xx;
	}

	/* To prevent concurrency issues with the fifo pointed to by
	 * pathnamedir being deleted or mounted over we need to be
	 * able to open a file descriptor to it.
	 */
	linted_ko realdir;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, dirko, pathnamedir,
		                     LINTED_KO_DIRECTORY);
		if (err != 0)
			goto free_pathnamebase;
		realdir = xx;
	}

make_fifo:
	if (-1 == mkfifoat(realdir, pathnamebase, mode)) {
		err = errno;
		LINTED_ASSUME(err != 0);

		/* We can't simply turn this test into an error so we
		 * can add a LINTED_FIFO_EXCL flag because the fifo
		 * could be removed by a privileged tmp cleaner style
		 * program and then created by an enemy.
		 */
		if (EEXIST == err)
			goto open_fifo;
		goto close_realdir;
	}

open_fifo : {
	linted_ko xx;
	err = linted_ko_open(&xx, realdir, pathnamebase, oflags);
	if (ENOENT == err)
		goto make_fifo;
	if (err != 0)
		goto free_pathnamebase;
	fd = xx;
}

close_realdir : {
	linted_error close_err = linted_ko_close(realdir);
	LINTED_ASSERT(close_err != EBADF);
	if (0 == err)
		err = close_err;
}

free_pathnamebase:
	linted_mem_free(pathnamebase);

free_pathnamedir:
	linted_mem_free(pathnamedir);

	if (err != 0) {
		if (fd != -1) {
			linted_error close_err = linted_ko_close(fd);
			LINTED_ASSERT(close_err != EBADF);
		}
		return err;
	}

	*kop = fd;
	return 0;
}
