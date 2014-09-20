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
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/sched.h>

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-init",
    .kos_size = 0U,
    .kos = NULL};

static linted_error set_process_name(char const *name);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	char const *repl_process_name = getenv("LINTED_PROCESS_NAME");
	if (repl_process_name != NULL) {
		process_name = repl_process_name;

		errnum = set_process_name(process_name);
		if (errnum != 0) {
			errno = errnum;
			perror("set_process_name");
			return EXIT_FAILURE;
		}
	}

	gid_t gid = getgid();
	uid_t uid = getuid();

	gid_t mapped_gid = gid;
	uid_t mapped_uid = uid;

	/* Create a new user namespace and gain pseudo
	 * capabilities. */

	if (-1 == unshare(CLONE_NEWUSER)) {
		perror("unshare");
		return EXIT_FAILURE;
	}

	/* Note that writing to uid_map and gid_map will fail if the
	 * binary is not dumpable.  DON'T set the process dumpable and
	 * fail if the process is nondumpable as presumably the
	 * invoker of the process had good reasons to have the process
	 * nondumpable.
	 */
	{
		linted_ko file;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/uid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_open");
				return EXIT_FAILURE;
			}
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_uid, uid);
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
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/gid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_open");
				return EXIT_FAILURE;
			}
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_gid, gid);
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

	/* Clone off a child in a new PID namespace. This can't be
	 * unshared because we want to use threads in the child and
	 * want the child to be the init and not this one. */
	pid_t child = syscall(__NR_clone, SIGCHLD | CLONE_NEWPID, NULL);
	if (-1 == child) {
		linted_io_write_format(
		    STDERR_FILENO, NULL,
		    "%s: can't clone unprivileged process: %s\n", process_name,
		    linted_error_string(errno));
		return EXIT_FAILURE;
	}

	if (0 == child)
		return linted_init_init();

	/* Drop unneeded resources */
	if (-1 == chdir("/")) {
		perror("chdir");
		return EXIT_FAILURE;
	}

	siginfo_t info;
	do {
		errnum = -1 == waitid(P_PID, child, &info, WEXITED) ? errno : 0;
	} while (EINTR == errnum);
	if (errnum != 0) {
		assert(errnum != EINVAL);
		assert(errnum != ECHILD);
		assert(false);
	}
	return info.si_status;
}

static linted_error set_process_name(char const *name)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}
	return 0;
}
