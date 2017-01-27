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

#include "config.h"

#include "lntd/dir.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/path.h"
#include "lntd/util.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <sys/stat.h>

lntd_error lntd_dir_create(lntd_ko *kop, lntd_ko dirko,
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

		int mydirfd;
		if (LNTD_KO_CWD == dirko) {
			mydirfd = AT_FDCWD;
		} else if (dirko > INT_MAX) {
			return EINVAL;
		} else {
			mydirfd = dirko;
		}

		if (-1 == mkdirat(mydirfd, pathname, mode)) {
			err = errno;
			LNTD_ASSUME(err != 0);
			if (EEXIST == err)
				return 0;

			return err;
		}

		return 0;
	}

	if ((flags & ~LNTD_DIR_ONLY) != 0U)
		return EINVAL;

	bool dir_only = (flags & LNTD_DIR_ONLY) != 0U;

	unsigned long oflags = 0U;

	if (dir_only)
		oflags |= LNTD_KO_DIRECTORY;

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

	/* To prevent concurrency issues with the directory pointed to
	 * by pathnamedir being deleted or mounted over we need to be
	 * able to open a file descriptor to it.
	 */
	lntd_ko realdir;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, dirko, pathnamedir, oflags);
		if (err != 0)
			goto free_pathnamebase;
		realdir = xx;
	}

make_directory:
	if (-1 == mkdirat(realdir, pathnamebase, mode)) {
		err = errno;
		LNTD_ASSUME(err != 0);

		/* We can't simply turn this test into an error so we
		 * can add a LNTD_DIR_EXCL flag because the
		 * directory could be removed by a privileged tmp
		 * cleaner style program and then created by an enemy.
		 */
		if (EEXIST == err)
			goto open_directory;
		goto close_realdir;
	}

open_directory : {
	lntd_ko xx;
	err =
	    lntd_ko_open(&xx, realdir, pathnamebase, LNTD_KO_DIRECTORY);
	if (ENOENT == err)
		goto make_directory;
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
