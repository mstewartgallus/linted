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
#define _GNU_SOURCE

#include "config.h"

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/pid.h"
#include "linted/start.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <sched.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef __NR_execveat
#if defined __amd64__
#define __NR_execveat 322
#elif defined __i386__
#define __NR_execveat 358
#else
#error No execveat system call number is defined for this platform
#endif
#endif

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-enter",
    .dont_init_signals = true,
    0};

/* Order of entering matters */

static char const *const namespaces[] = {"user", "mnt", "pid", "ipc",
                                         "net"};

static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	if (argc < 2U)
		return EXIT_FAILURE;

	linted_ko sh_ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, "/bin/sh", 0);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		sh_ko = xx;
	}

	linted_pid pid = atoi(argv[1U]);

	{
		char proc_path[sizeof "/proc/" - 1U +
		               LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
		               1U];
		sprintf(proc_path, "/proc/%" PRIuMAX "",
		        (uintmax_t)pid);
		err = linted_ko_change_directory(proc_path);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_change_directory: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	linted_ko ns;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, "ns",
		                     LINTED_KO_DIRECTORY);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		ns = xx;
	}

	linted_ko fds[LINTED_ARRAY_SIZE(namespaces)];
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(namespaces); ++ii) {
		linted_ko xx;
		err = linted_ko_open(&xx, ns, namespaces[ii], 0);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}

		fds[ii] = xx;
	}

	err = linted_ko_change_directory("root");
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_ko_change_directory: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}
	/* Open all the fds at once so that one can enter spaces that
	 * lack /proc.
	 */
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(namespaces); ++ii) {
		if (-1 == setns(fds[ii], 0)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			linted_log(LINTED_LOG_ERROR, "setns: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	err = linted_ko_change_directory("/");
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_ko_change_directory: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	static const char *args[] = {"/bin/sh", 0};
	syscall(__NR_execveat, (int)sh_ko, "", (char *const *)args,
	        environ, AT_EMPTY_PATH);

	linted_log(LINTED_LOG_ERROR, "execve: %s",
	           linted_error_string(errno));
	return EXIT_FAILURE;
}
