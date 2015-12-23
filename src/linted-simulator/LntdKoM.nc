/*
 * Copyright 2015 Steven Stewart-Gallus
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
#include "ko.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <unistd.h>

module LntdKoM
{
	provides interface LntdKo;
}
implementation
{
	lntd_error my_close(lntd_ko ko);

	command lntd_error LntdKo.open(lntd_ko * kop, lntd_ko cwd,
	                               char const *path,
	                               uint_fast64_t flags)
	{
		int fd;
		bool rdwr_flag;
		int oflags;
		lntd_error err = 0;
		int cwdfd;

		if (LNTD_KO_CWD == cwd) {
			cwdfd = AT_FDCWD;
		} else if (cwd > INT_MAX) {
			return EINVAL;
		} else {
			cwdfd = cwd;
		}

		if ((flags & ~LNTD_KO_RDWR) != 0) {
			return EINVAL;
		}

		rdwr_flag = (flags & LNTD_KO_RDWR) != 0;

		oflags = O_CLOEXEC | O_NOCTTY;
		if (rdwr_flag)
			oflags |= O_RDWR;

		for (;;) {
			fd = openat(cwdfd, path, oflags);
			if (fd < 0) {
				err = errno;
				assert(err != 0);
				if (EINTR == err)
					continue;
				return err;
			}
			break;
		}

		if (-1 == fcntl(fd, F_SETFL, oflags | O_NONBLOCK)) {
			err = errno;
			assert(err != 0);
			goto close_fd;
		}

		*kop = fd;
		return 0;

	close_fd:
		my_close(fd);
		return err;
	}

	command lntd_error LntdKo.close(lntd_ko ko)
	{
		return my_close(ko);
	}

	lntd_error my_close(lntd_ko ko)
	{
		lntd_error err;
		sigset_t sigset;
		lntd_error mask_err;

		if (ko > INT_MAX)
			return EINVAL;

		/*
		 * The state of a file descriptor after close gives an
		 * EINTR
		 * error is unspecified by POSIX so this function avoids
		 * the
		 * problem by simply blocking all signals.
		 */

		/* First use the signal set for the full set */
		sigfillset(&sigset);

		err = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
		if (err != 0)
			return err;

		/* Then reuse the signal set for the old set */

		if (-1 == close(ko)) {
			err = errno;
			assert(err != 0);
		} else {
			err = 0;
		}

		mask_err = pthread_sigmask(SIG_SETMASK, &sigset, 0);
		if (0 == err)
			err = mask_err;

		return err;
	}
}
