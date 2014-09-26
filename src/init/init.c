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

static linted_error set_death_sig(int signum);
static linted_error set_child_subreaper(bool value);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	char const *monitor = getenv("LINTED_MONITOR");
	if (NULL == monitor) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: need LINTED_MONITOR\n",
		                       process_name);
		return EXIT_FAILURE;
	}

	errnum = set_child_subreaper(true);
	if (errnum != 0) {
		errno = errnum;
		perror("set_child_subreaper");
		return EXIT_FAILURE;
	}

	for (;;) {
		pid_t child = fork();
		if (-1 == child) {
			linted_io_write_format(
			    STDERR_FILENO, NULL, "%s: can't fork process: %s\n",
			    process_name, linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (0 == child) {
			errnum = set_death_sig(SIGKILL);
			if (errnum != 0) {
				errno = errnum;
				perror("set_death_sig");
				return EXIT_FAILURE;
			}

			linted_ko stdfiles[] = {STDIN_FILENO, STDOUT_FILENO,
			                        STDERR_FILENO};
			for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(stdfiles);
			     ++ii) {
				linted_ko ko = stdfiles[ii];

				int flags = fcntl(ko, F_GETFD);
				if (-1 == flags) {
					perror("fcntl");
					return EXIT_FAILURE;
				}

				if (-1 == fcntl(ko, F_SETFD,
				                (long)flags & !FD_CLOEXEC)) {
					perror("fcntl");
					return EXIT_FAILURE;
				}
			}

			execve(monitor, (char *const *)(char const *const[]){
			                    monitor, NULL},
			       environ);
			return EXIT_FAILURE;
		}

		siginfo_t info;
		do {

			do {
				errnum = -1 == waitid(P_ALL, -1, &info, WEXITED)
				             ? errno
				             : 0;
			} while (EINTR == errnum);
			if (errnum != 0) {
				assert(errnum != EINVAL);
				assert(errnum != ECHILD);
				assert(false);
			}
		} while (info.si_pid != child);

		/**
		 * @todo log errors
		 */

		if (CLD_EXITED == info.si_code &&
		    EXIT_SUCCESS == info.si_status)
			break;
	}

	return EXIT_SUCCESS;
}

static linted_error set_death_sig(int signum)
{
	linted_error errnum;

	if (-1 ==
	    prctl(PR_SET_PDEATHSIG, (unsigned long)signum, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error set_child_subreaper(bool v)
{
	linted_error errnum;

	unsigned long set_child_subreaper;

#ifdef PR_SET_CHILD_SUBREAPER
	set_child_subreaper = PR_SET_CHILD_SUBREAPER;
#else
	set_child_subreaper = 36UL;
#endif

	if (-1 == prctl(set_child_subreaper, (unsigned long)v, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}
