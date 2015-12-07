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

#include "config.h"

#include "lntd/error.h"
#include "lntd/fifo.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/path.h"
#include "lntd/util.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <unistd.h>

#if !defined HAVE_MKFIFOAT && defined HAVE_SYSCALL
#include <sys/syscall.h>
#endif

static lntd_error my_mkfifoat(int dir, char const *pathname,
                              mode_t mode);

lntd_error lntd_fifo_pair(lntd_fifo *readerp, lntd_fifo *writerp,
                          unsigned long flags)
{
	if (flags != 0U)
		return EINVAL;

	int xx[2U];
	if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
		lntd_error err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	*readerp = xx[0U];
	*writerp = xx[1U];
	return 0;
}

lntd_error lntd_fifo_create(lntd_fifo *kop, lntd_ko dirko,
                            char const *pathname, unsigned long flags,
                            mode_t mode)
{
	lntd_error err;
	int fd = -1;

	if (dirko > INT_MAX && dirko != LNTD_KO_CWD)
		return EINVAL;

	if (0 == kop) {
		if (flags != 0U)
			return EINVAL;

		int dirfd;
		if (LNTD_KO_CWD == dirko) {
			dirfd = AT_FDCWD;
		} else if (dirko > INT_MAX) {
			return EINVAL;
		} else {
			dirfd = dirko;
		}

		err = my_mkfifoat(dirfd, pathname, mode);
		if (err != 0) {
			if (EEXIST == err)
				return 0;

			return err;
		}

		return 0;
	}

	if ((flags & ~LNTD_FIFO_ONLY & ~LNTD_FIFO_RDONLY &
	     ~LNTD_FIFO_WRONLY & ~LNTD_FIFO_RDWR) != 0U)
		return EINVAL;

	bool fifo_only = (flags & LNTD_FIFO_ONLY) != 0U;
	bool fifo_rdonly = (flags & LNTD_FIFO_RDONLY) != 0U;
	bool fifo_wronly = (flags & LNTD_FIFO_WRONLY) != 0U;
	bool fifo_rdwr = (flags & LNTD_FIFO_RDWR) != 0U;

	if (fifo_rdonly && fifo_wronly)
		return EINVAL;

	if (fifo_rdwr && fifo_rdonly)
		return EINVAL;

	if (fifo_rdwr && fifo_wronly)
		return EINVAL;

	unsigned long oflags = 0U;

	if (fifo_only)
		oflags |= LNTD_KO_FIFO;

	if (fifo_rdonly)
		oflags |= LNTD_KO_RDONLY;

	if (fifo_wronly)
		oflags |= LNTD_KO_WRONLY;

	if (fifo_rdwr)
		oflags |= LNTD_KO_RDWR;

	char *pathnamedir;
	{
		char *xx;
		err = lntd_path_dir(&xx, pathname);
		if (err != 0)
			return err;
		pathnamedir = xx;
	}

	char *pathnamebase;
	{
		char *xx;
		err = lntd_path_base(&xx, pathname);
		if (err != 0)
			goto free_pathnamedir;
		pathnamebase = xx;
	}

	/* To prevent concurrency issues with the fifo pointed to by
	 * pathnamedir being deleted or mounted over we need to be
	 * able to open a file descriptor to it.
	 */
	lntd_ko realdir;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, dirko, pathnamedir,
		                   LNTD_KO_DIRECTORY);
		if (err != 0)
			goto free_pathnamebase;
		realdir = xx;
	}

make_fifo:
	err = my_mkfifoat(realdir, pathnamebase, mode);
	if (err != 0) {
		LNTD_ASSUME(err != 0);

		/* We can't simply turn this test into an error so we
		 * can add a LNTD_FIFO_EXCL flag because the fifo
		 * could be removed by a privileged tmp cleaner style
		 * program and then created by an enemy.
		 */
		if (EEXIST == err)
			goto open_fifo;
		goto close_realdir;
	}

open_fifo : {
	lntd_ko xx;
	err = lntd_ko_open(&xx, realdir, pathnamebase, oflags);
	if (ENOENT == err)
		goto make_fifo;
	if (err != 0)
		goto free_pathnamebase;
	fd = xx;
}

close_realdir : {
	lntd_error close_err = lntd_ko_close(realdir);
	LNTD_ASSERT(close_err != EBADF);
	if (0 == err)
		err = close_err;
}

free_pathnamebase:
	lntd_mem_free(pathnamebase);

free_pathnamedir:
	lntd_mem_free(pathnamedir);

	if (err != 0) {
		if (fd != -1) {
			lntd_error close_err = lntd_ko_close(fd);
			LNTD_ASSERT(close_err != EBADF);
		}
		return err;
	}

	*kop = fd;
	return 0;
}

#if defined HAVE_MKFIFOAT
static lntd_error my_mkfifoat(int dir, char const *pathname,
                              mode_t mode)
{
	if (-1 == mkfifoat(dir, pathname, mode)) {
		lntd_error err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}
	return 0;
}
#else
#error no mkfifoat implementation for this platform
#endif
