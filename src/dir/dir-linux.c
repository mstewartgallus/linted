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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/dir.h"
#include "linted/ko.h"
#include "linted/mem.h"
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
                               char const *pathname, unsigned long flags,
                               mode_t mode)
{
	linted_error errnum;
	int fildes = -1;

	if (dirko > INT_MAX && dirko != LINTED_KO_CWD)
		return EINVAL;

	if (flags != 0UL)
		return EINVAL;

	char *pathnamedir_buffer = strdup(pathname);
	if (NULL == pathnamedir_buffer) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	char *pathnamebase_buffer = strdup(pathname);
	if (NULL == pathnamebase_buffer) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_pathnamedir_buffer;
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
		errnum = linted_ko_open(&xx, dirko, pathnamedir,
		                        LINTED_KO_DIRECTORY);
		if (errnum != 0)
			goto free_pathnamebase_buffer;
		realdir = xx;
	}

make_directory:
	if (-1 == mkdirat(realdir, pathnamebase, mode)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		/* We can't simply turn this test into an error so we
		 * can add a LINTED_DIR_EXCL flag because the
		 * directory could be removed by a privileged tmp
		 * cleaner style program and then created by an enemy.
		 */
		if (EEXIST == errnum)
			goto open_directory;
		goto close_realdir;
	}

open_directory : {
	linted_ko xx;
	errnum =
	    linted_ko_open(&xx, realdir, pathnamebase, LINTED_KO_DIRECTORY);
	if (ENOENT == errnum)
		goto make_directory;
	if (errnum != 0)
		goto free_pathnamebase_buffer;
	fildes = xx;
}

close_realdir : {
	linted_error close_errnum = linted_ko_close(realdir);
	assert(close_errnum != EBADF);
	if (0 == errnum)
		errnum = close_errnum;
}

free_pathnamebase_buffer:
	linted_mem_free(pathnamebase_buffer);

free_pathnamedir_buffer:
	linted_mem_free(pathnamedir_buffer);

	if (errnum != 0) {
		if (fildes != -1) {
			linted_error close_errnum = linted_ko_close(fildes);
			assert(close_errnum != EBADF);
		}
		return errnum;
	}

	*kop = fildes;
	return 0;
}
