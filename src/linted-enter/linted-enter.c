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

#include "lntd/execveat.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/proc.h"
#include "lntd/start.h"
#include "lntd/util.h"

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

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-enter",
    .dont_init_signals = true,
    0};

/* Order of entering matters */

static char const *const namespaces[] = {"user", "mnt", "pid", "ipc",
                                         "net"};

static unsigned char lntd_start_main(char const *const process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	if (argc < 2U)
		return EXIT_FAILURE;

	lntd_ko sh_ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, "/bin/sh", 0);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_ko_open: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		sh_ko = xx;
	}

	lntd_proc pid = atoi(argv[1U]);

	{
		char proc_path[sizeof "/proc/" - 1U +
		               LNTD_NUMBER_TYPE_STRING_SIZE(pid_t) +
		               1U];
		sprintf(proc_path, "/proc/%" PRIuMAX "",
		        (uintmax_t)pid);
		err = lntd_ko_change_directory(proc_path);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_ko_change_directory: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	lntd_ko ns;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, "ns",
		                   LNTD_KO_DIRECTORY);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_ko_open: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		ns = xx;
	}

	lntd_ko fds[LNTD_ARRAY_SIZE(namespaces)];
	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(namespaces); ++ii) {
		lntd_ko xx;
		err = lntd_ko_open(&xx, ns, namespaces[ii], 0);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_ko_open: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}

		fds[ii] = xx;
	}

	err = lntd_ko_change_directory("root");
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_ko_change_directory: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}
	/* Open all the fds at once so that one can enter spaces that
	 * lack /proc.
	 */
	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(namespaces); ++ii) {
		if (-1 == setns(fds[ii], 0)) {
			err = errno;
			LNTD_ASSUME(err != 0);
			lntd_log(LNTD_LOG_ERROR, "setns: %s",
			         lntd_error_string(err));
		}
	}

	err = lntd_ko_change_directory("/");
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_ko_change_directory: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	static const char *args[] = {"/bin/sh", 0};
	err = lntd_execveat(sh_ko, "", (char **)args, environ,
	                    AT_EMPTY_PATH);

	lntd_log(LNTD_LOG_ERROR, "execve: %s", lntd_error_string(err));
	return EXIT_FAILURE;
}
