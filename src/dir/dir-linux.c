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
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

linted_error linted_dir_create(linted_ko *kop, linted_ko dirko,
                               char const *pathname, unsigned long flags,
                               mode_t mode)
{
	linted_error errnum;

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
	 * by pathnamedir being deleted or mounted over We need to be
	 * able to open a file descriptor to it.
	 */
	linted_ko realdir;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, dirko, pathnamedir, LINTED_KO_DIRECTORY);
		if (errnum != 0)
			goto free_pathnamebase_buffer;
		realdir = xx;
	}

make_directory:
	if (-1 == mkdirat(realdir, pathnamebase, mode)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		if (EEXIST == errnum)
			goto open_directory;
		goto close_realdir;
	}

open_directory:
	;
	int fildes;
	do {
		fildes = openat(realdir, pathnamebase,
		                O_CLOEXEC | O_NONBLOCK | O_DIRECTORY);
		if (-1 == fildes) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		} else {
			errnum = 0;
		}
	} while (EINTR == errnum);

	if (ENOENT == errnum)
		goto make_directory;

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

	if (errnum != 0)
		return errnum;

	*kop = fildes;
	return 0;
}
