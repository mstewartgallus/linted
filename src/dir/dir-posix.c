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
#define _POSIX_C_SOURCE 200809L

#include "linted/dir.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/str.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

linted_error linted_dir_create(linted_ko *kop, linted_ko dirko,
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

		if (-1 == mkdirat(dirfd, pathname, mode)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			if (EEXIST == err)
				return 0;

			return err;
		}

		return 0;
	}

	if ((flags & ~LINTED_DIR_ONLY) != 0U)
		return EINVAL;

	bool dir_only = (flags & LINTED_DIR_ONLY) != 0U;

	unsigned long oflags = 0U;

	if (dir_only)
		oflags |= LINTED_KO_DIRECTORY;

	char *pathnamedir_buffer;
	{
		char *xx;
		err = linted_str_dup(&xx, pathname);
		if (err != 0)
			return err;
		pathnamedir_buffer = xx;
	}

	char *pathnamebase_buffer;
	{
		char *xx;
		err = linted_str_dup(&xx, pathname);
		if (err != 0)
			goto free_pathnamedir_buffer;
		pathnamebase_buffer = xx;
	}

	char *pathnamedir = dirname(pathnamedir_buffer);
	char *pathnamebase = basename(pathnamebase_buffer);

	/* To prevent concurrency issues with the directory pointed to
	 * by pathnamedir being deleted or mounted over we need to be
	 * able to open a file descriptor to it.
	 */
	linted_ko realdir;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, dirko, pathnamedir, oflags);
		if (err != 0)
			goto free_pathnamebase_buffer;
		realdir = xx;
	}

make_directory:
	if (-1 == mkdirat(realdir, pathnamebase, mode)) {
		err = errno;
		LINTED_ASSUME(err != 0);

		/* We can't simply turn this test into an error so we
		 * can add a LINTED_DIR_EXCL flag because the
		 * directory could be removed by a privileged tmp
		 * cleaner style program and then created by an enemy.
		 */
		if (EEXIST == err)
			goto open_directory;
		goto close_realdir;
	}

open_directory : {
	linted_ko xx;
	err = linted_ko_open(&xx, realdir, pathnamebase,
	                     LINTED_KO_DIRECTORY);
	if (ENOENT == err)
		goto make_directory;
	if (err != 0)
		goto free_pathnamebase_buffer;
	fd = xx;
}

close_realdir : {
	linted_error close_err = linted_ko_close(realdir);
	assert(close_err != EBADF);
	if (0 == err)
		err = close_err;
}

free_pathnamebase_buffer:
	linted_mem_free(pathnamebase_buffer);

free_pathnamedir_buffer:
	linted_mem_free(pathnamedir_buffer);

	if (err != 0) {
		if (fd != -1) {
			linted_error close_err = linted_ko_close(fd);
			assert(close_err != EBADF);
		}
		return err;
	}

	*kop = fd;
	return 0;
}
