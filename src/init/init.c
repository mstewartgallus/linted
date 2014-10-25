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

#include "linted/admin.h"
#include "linted/dir.h"
#include "linted/io.h"
#include "linted/spawn.h"
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
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#define BACKLOG 20U

extern char **environ;

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-init",
	.kos_size = 0U,
	.kos = NULL
};

static linted_error spawn_monitor(pid_t *childp, char const *monitor,
                                  linted_admin admin);
static linted_error set_child_subreaper(bool value);
static linted_error set_ptracer(pid_t pid);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	char const *monitor = getenv("LINTED_MONITOR");

	errnum = set_child_subreaper(true);
	if (errnum != 0) {
		errno = errnum;
		perror("set_child_subreaper");
		return EXIT_FAILURE;
	}

	linted_admin admin;
	{
		linted_admin xx;
		errnum = linted_admin_bind(&xx, BACKLOG, NULL, 0);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_admin_bind");
			return EXIT_FAILURE;
		}
		admin = xx;
	}

	{
		char buf[LINTED_ADMIN_PATH_MAX];
		size_t len;
		errnum = linted_admin_path(admin, buf, &len);
		if (errnum != 0)
			return errnum;

		linted_io_write_str(STDOUT_FILENO, NULL,
		                    LINTED_STR("LINTED_ADMIN_SOCKET="));
		linted_io_write_all(STDOUT_FILENO, NULL, buf, len);
		linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));
	}

	for (;;) {
		linted_io_write_str(STDOUT_FILENO, NULL,
		                    LINTED_STR("spawning monitor\n"));

		pid_t child;
		{
			pid_t xx;
			errnum = spawn_monitor(&xx, monitor, admin);
			if (errnum != 0) {
				linted_io_write_format(
				    STDERR_FILENO, NULL,
				    "%s: can't fork process: %s\n",
				    process_name, linted_error_string(errnum));
				return EXIT_FAILURE;
			}
			child = xx;
		}

		errnum = set_ptracer(child);
		if (errnum != 0) {
			linted_io_write_format(
			    STDERR_FILENO, NULL, "%s: set_ptracer: %s\n",
			    process_name, linted_error_string(errnum));
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

static linted_error spawn_monitor(pid_t *childp, char const *monitor,
                                  linted_admin admin)
{
	linted_error errnum = 0;

	struct linted_spawn_file_actions *file_actions;
	struct linted_spawn_attr *attr;

	{
		struct linted_spawn_file_actions *xx;
		errnum = linted_spawn_file_actions_init(&xx);
		if (errnum != 0)
			return errnum;
		file_actions = xx;
	}

	{
		struct linted_spawn_attr *xx;
		errnum = linted_spawn_attr_init(&xx);
		if (errnum != 0)
			goto destroy_file_actions;
		attr = xx;
	}

	linted_spawn_attr_setdeathsig(attr, SIGKILL);

	linted_ko stdfiles[] = { STDIN_FILENO,  STDOUT_FILENO,
		                 STDERR_FILENO, admin };
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(stdfiles); ++ii) {
		linted_ko ko = stdfiles[ii];
		errnum =
		    linted_spawn_file_actions_adddup2(&file_actions, ko, ii);
		if (errnum != 0)
			goto destroy_attr;
	}

	pid_t child;
	{
		pid_t xx;
		errnum =
		    linted_spawn(&xx, LINTED_KO_CWD, monitor, file_actions,
		                 NULL, (char const * const[]) { monitor, NULL },
		                 (char const * const *)environ);
		if (errnum != 0)
			goto destroy_attr;
		child = xx;
	}

destroy_attr:
	linted_spawn_attr_destroy(attr);

destroy_file_actions:
	linted_spawn_file_actions_destroy(file_actions);

	if (0 == errnum)
		*childp = child;

	return errnum;
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

static linted_error set_ptracer(pid_t pid)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_PTRACER, (unsigned long)pid, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}
