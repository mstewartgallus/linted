/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

#include "linted/ko.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

linted_error linted_ko_open(linted_ko *kop, linted_ko dirko,
                            char const *pathname, unsigned long flags)
{
	linted_error errnum;

	if (LINTED_KO_CWD == dirko) {
		dirko = AT_FDCWD;
	} else if (dirko > INT_MAX) {
		return EINVAL;
	}

	unsigned long perm_flags =
	    LINTED_KO_RDONLY | LINTED_KO_WRONLY | LINTED_KO_RDWR;
	unsigned long type_flags = LINTED_KO_DIRECTORY | LINTED_KO_FIFO;
	unsigned long misc_flags = LINTED_KO_APPEND | LINTED_KO_SYNC;
	unsigned long all_flags = perm_flags | type_flags | misc_flags;

	if ((flags & ~all_flags) != 0U)
		return EINVAL;

	bool ko_rdonly = (flags & LINTED_KO_RDONLY) != 0U;
	bool ko_wronly = (flags & LINTED_KO_WRONLY) != 0U;
	bool ko_rdwr = (flags & LINTED_KO_RDWR) != 0U;

	bool ko_append = (flags & LINTED_KO_APPEND) != 0U;
	bool ko_sync = (flags & LINTED_KO_SYNC) != 0U;

	bool ko_directory = (flags & LINTED_KO_DIRECTORY) != 0U;
	bool ko_fifo = (flags & LINTED_KO_FIFO) != 0U;

	if (ko_rdonly && ko_wronly)
		return EINVAL;

	if (ko_rdwr && ko_rdonly)
		return EINVAL;

	if (ko_rdwr && ko_wronly)
		return EINVAL;

	if (ko_append && !ko_wronly)
		return EINVAL;

	if (ko_directory &&
	    (ko_rdonly || ko_wronly || ko_rdwr || ko_append || ko_sync))
		return EINVAL;

	if (ko_fifo && ko_sync)
		return EINVAL;

	/*
	 * Always, be safe for execs and use O_NONBLOCK because asynch
	 * functions handle that anyways and open may block otherwise.
	 */
	int oflags = O_CLOEXEC | O_NOCTTY;

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

	int fildes;
	do {
		fildes = openat(dirko, pathname, oflags);
		if (-1 == fildes) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		} else {
			errnum = 0;
		}
	} while (EINTR == errnum);
	if (errnum != 0)
		return errnum;

	if (ko_fifo) {
		mode_t mode;
		{
			struct stat buf;
			if (-1 == fstat(fildes, &buf)) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto close_file;
			}
			mode = buf.st_mode;
		}

		if (!S_ISFIFO(mode)) {
			errnum = EINVAL;
			goto close_file;
		}
	}

	/* Set nonblock after opening so that we can block on opens */
	int flflags = fcntl(fildes, F_GETFL);
	if (-1 == flflags) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_file;
	}

	if (-1 == fcntl(fildes, F_SETFL, (long)flflags | O_NONBLOCK)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_file;
	}

	*kop = fildes;

	return 0;

close_file:
	linted_ko_close(fildes);
	return errnum;
}

linted_error linted_ko_close(linted_ko ko)
{
	linted_error errnum;
	/*
	 * The state of a file descriptor after close gives an EINTR error
	 * is unspecified by POSIX so this function avoids the problem by
	 * simply blocking all signals.
	 */

	sigset_t sigset;

	/* First use the signal set for the full set */
	sigfillset(&sigset);

	errnum = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
	if (errnum != 0)
		return errnum;

	/* Then reuse the signal set for the old set */

	if (-1 == close(ko)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	} else {
		errnum = 0;
	}

	linted_error mask_errnum = pthread_sigmask(SIG_SETMASK, &sigset, 0);
	if (0 == errnum)
		errnum = mask_errnum;

	return errnum;
}
