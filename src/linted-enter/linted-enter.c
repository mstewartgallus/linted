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
#define _GNU_SOURCE

#include "config.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-enter"
};

/* Order of entering matters */
static char const *namespaces[] = { "user", "pid", "ipc", "mnt", "net" };

uint_fast8_t linted_start(char const *const process_name, size_t argc,
                          char const *const argv[])
{
	linted_error errnum = 0;

	if (argc < 2U)
		return EXIT_FAILURE;

	pid_t pid = atoi(argv[1U]);

	linted_ko ns;
	{
		char namespace_path[sizeof "/proc/" - 1U +
		                    LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
		                    sizeof "/ns" - 1U + 1U];
		sprintf(namespace_path, "/proc/%" PRIuMAX "/ns",
		        (uintmax_t)pid);

		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, namespace_path,
		                        LINTED_KO_DIRECTORY);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_ko_open");
			return EXIT_FAILURE;
		}
		ns = xx;
	}

	linted_ko fds[LINTED_ARRAY_SIZE(namespaces)];
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(namespaces); ++ii) {
		linted_ko xx;
		errnum = linted_ko_open(&xx, ns, namespaces[ii], 0);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_ko_open");
			return EXIT_FAILURE;
		}

		fds[ii] = xx;
	}

	/* Open all the fds at once so that one can enter spaces that
	 * lack /proc.
	 */
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(namespaces); ++ii) {
		if (-1 == setns(fds[ii], 0)) {
			perror("setns");
			return EXIT_FAILURE;
		}
		linted_ko_close(fds[ii]);
	}

	linted_ko stdfiles[] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(stdfiles); ++ii) {
		linted_ko ko = stdfiles[ii];

		if (-1 == dup2(ko, ii)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		int flags = fcntl(ii, F_GETFD);
		if (-1 == flags) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		if (-1 == fcntl(ii, F_SETFD, (long)flags & !FD_CLOEXEC)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
	}

	static const char *args[] = { "/bin/sh", 0 };
	execve(args[0U], (char * const *)args, environ);

	perror("execve");

	return EXIT_FAILURE;
}
