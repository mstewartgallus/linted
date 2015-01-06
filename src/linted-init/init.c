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

#include "linted/error.h"
#include "linted/io.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <locale.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

extern char **environ;

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-init"
};

static volatile sig_atomic_t monitor_pid = 0;

static void delegate_signal(int signo);
static linted_error set_child_subreaper(bool value);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	if (NULL == setlocale(LC_ALL, "")) {
		syslog(LOG_ERR, "setlocale: %s", linted_error_string(errno));
		return EXIT_FAILURE;
	}

	static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };

	/* Delegate the exit signal to the monitor child */
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii) {
		struct sigaction action = { 0 };
		action.sa_handler = delegate_signal;
		action.sa_flags = SA_RESTART;
		sigfillset(&action.sa_mask);
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			syslog(LOG_ERR, "sigaction: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	char const *monitor = getenv("LINTED_MONITOR");

	char *monitor_dup = strdup(monitor);
	if (NULL == monitor_dup) {
		syslog(LOG_ERR, "strdup: %s", linted_error_string(errno));
		return EXIT_FAILURE;
	}

	char *monitor_base = basename(monitor_dup);

	errnum = set_child_subreaper(true);
	if (errnum != 0) {
		syslog(LOG_ERR, "set_child_subreaper: %s",
		       linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	struct linted_spawn_attr *attr;

	{
		struct linted_spawn_attr *xx;
		errnum = linted_spawn_attr_init(&xx);
		if (errnum != 0) {
			syslog(LOG_ERR, "linted_spawn_attr_init: %s",
			       linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		attr = xx;
	}

	for (;;) {
		fprintf(stderr, "%s: spawning %s\n", process_name,
		        monitor_base);

		pid_t child;
		{
			pid_t xx;
			errnum = linted_spawn(
			    &xx, LINTED_KO_CWD, monitor, NULL, attr,
			    (char const * const[]) { monitor_base, NULL },
			    (char const * const *)environ);
			if (errnum != 0) {
				syslog(LOG_ERR, "linted_spawn: %s",
				       linted_error_string(errnum));
				return EXIT_FAILURE;
			}
			child = xx;
		}
		monitor_pid = child;

		pid_t pid;
		int code;
		int status;
		for (;;) {
			{
				siginfo_t info;
				if (-1 == waitid(P_ALL, -1, &info, WEXITED))
					goto waitid_failed;
				pid = info.si_pid;
				code = info.si_code;
				status = info.si_status;
				goto waitid_succeeded;
			}
		waitid_failed:
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			if (EINTR == errnum)
				continue;

			assert(errnum != EINVAL);
			assert(errnum != ECHILD);
			assert(false);

		waitid_succeeded:
			if (child == pid)
				break;
		}
		monitor_pid = 0;

		switch (code) {
		case CLD_EXITED:
			if (EXIT_SUCCESS == status)
				goto exit_loop;

			fprintf(stderr, "monitor exited with %i\n", status);
			break;

		case CLD_DUMPED:
		case CLD_KILLED:
			fprintf(stderr, "monitor killed by %s\n",
			        strsignal(status));
			break;

		default:
			LINTED_ASSUME_UNREACHABLE();
		}
	}
exit_loop:
	return EXIT_SUCCESS;
}

static linted_error set_child_subreaper(bool v)
{
	linted_error errnum;

	if (-1 ==
	    prctl(PR_SET_CHILD_SUBREAPER, (unsigned long)v, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static void delegate_signal(int signo)
{
	linted_error old_errnum = errno;

	linted_error errnum = 0;

	/* All signals are blocked here. */

	pid_t the_pid = monitor_pid;
	if (the_pid > 0) {
		/* If WNOHANG was specified in options and there were
		 * no children in a waitable state, then waitid()
		 * returns 0 immediately and the state of the
		 * siginfo_t structure pointed to by infop is
		 * unspecified.
		 *
		 * - WAIT(2) http://www.kernel.org/doc/man-pages/.
		 */
		{
			siginfo_t info = { 0 };
			if (-1 == waitid(P_PID, the_pid, &info,
			                 WEXITED | WNOWAIT | WNOHANG))
				errnum = errno;

			if (ECHILD == errnum)
				goto restore_errno;
			if (errnum != 0) {
				assert(errnum != EINTR);
				assert(errnum != EINVAL);
				assert(false);
			}

			/* The process was killed and is waitable */
			if (info.si_pid != 0)
				goto restore_errno;
		}

		/* The process may be dead but not waited on at least */
		kill(the_pid, signo);
	}

restore_errno:
	errno = old_errnum;
}
