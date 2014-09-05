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
#define _GNU_SOURCE

#include "config.h"

#include "init.h"

#include "linted/io.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <sys/wait.h>

#include <linux/sched.h>

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-init",
	.kos_size = 0U,
	.kos = NULL
};

unsigned char linted_start(char const *const process_name, size_t argc,
                           char const *const argv[const])
{
	linted_error errnum;

	char const *chrootdir = getenv("LINTED_CHROOT");
	char const *unit_path = getenv("LINTED_UNIT_PATH");

	linted_ko cwd;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, AT_FDCWD, ".", LINTED_KO_DIRECTORY);
		if (errnum != 0) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not open the current working directory: %s\n",
			                       process_name,
			                       linted_error_string(errno));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	if (-1 == chdir("/")) {
		linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not change to the root directory: %s\n",
		                       process_name,
		                       linted_error_string(errno));
		return EXIT_FAILURE;
	}

	gid_t gid = getgid();
	uid_t uid = getuid();

	/* Clone off a child in a new PID namespace. CLONE_NEWUSER is
	 * needed to allow the permissions to work.
	 */
	pid_t child;
	{
		child = syscall(__NR_clone,
		                SIGCHLD | CLONE_NEWUSER | CLONE_NEWPID, NULL);
		if (-1 == child) {
			linted_io_write_format(
			    STDERR_FILENO, NULL,
			    "%s: can't clone unprivileged process: %s\n",
			    process_name, linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (child != 0) {
		siginfo_t info;
		do {
			errnum = -1 == waitid(P_PID, child, &info, WEXITED)
			             ? errno
			             : 0;
		} while (EINTR == errnum);
		if (errnum != 0) {
			assert(errnum != EINVAL);
			assert(errnum != ECHILD);
			LINTED_ASSUME_UNREACHABLE();
		}
		return info.si_status;
	}

	/* Note that writing to uid_map and gid_map will fail if the
	 * binary is not dumpable.  DON'T set the process dumpable and
	 * fail it is nondumpable as presumably the invoker of the
	 * process had good reasons to have the process nondumpable.
	 */
	{
		linted_ko file;
		{
			linted_ko xx;
			errnum =
			    linted_ko_open(&xx, AT_FDCWD, "/proc/self/uid_map",
			                   LINTED_KO_WRONLY);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_open");
				return EXIT_FAILURE;
			}
			file = xx;
		}

		errnum =
		    linted_io_write_format(file, NULL, "%i %i 1\n", uid, uid);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_io_write_format");
			return EXIT_FAILURE;
		}

		errnum = linted_ko_close(file);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_ko_close");
			return EXIT_FAILURE;
		}
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum =
			    linted_ko_open(&xx, AT_FDCWD, "/proc/self/gid_map",
			                   LINTED_KO_WRONLY);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_open");
				return EXIT_FAILURE;
			}
			file = xx;
		}

		errnum =
		    linted_io_write_format(file, NULL, "%i %i 1\n", gid, gid);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_io_write_format");
			return EXIT_FAILURE;
		}

		errnum = linted_ko_close(file);
		if (errnum != 0) {
			perror("linted_ko_close");
			return EXIT_FAILURE;
		}
	}

	if (-1 == prctl(PR_SET_DUMPABLE, 0L, 0L, 0L, 0L)) {
		perror("prctl");
		return EXIT_FAILURE;
	}
	return linted_init_init(cwd, chrootdir, unit_path);
}
