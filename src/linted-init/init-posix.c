/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/env.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/path.h"
#include "linted/pid.h"
#include "linted/prctl.h"
#include "linted/signal.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/util.h"

#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/wait.h>

static void delegate_signal(int signo);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-init",
    .dont_init_signals = true};

static volatile sig_atomic_t monitor_pid = 0;

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err;

	static int const exit_signals[] = {SIGHUP, SIGINT, SIGQUIT,
	                                   SIGTERM};

	/* Delegate the exit signal to the monitor child */
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals);
	     ++ii) {
		struct sigaction action = {0};
		action.sa_handler = delegate_signal;
		action.sa_flags = SA_RESTART;
		sigfillset(&action.sa_mask);
		if (-1 == sigaction(exit_signals[ii], &action, 0)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	char const *monitor;
	{
		char *xx;
		err = linted_env_get("LINTED_MONITOR", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_env_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		monitor = xx;
	}

	if (0 == monitor) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_MONITOR");
		return EXIT_FAILURE;
	}

	char *monitor_base;
	{
		char *xx;
		err = linted_path_base(&xx, monitor);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_path_base: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		monitor_base = xx;
	}

	err = linted_prctl_set_child_subreaper(true);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_prctl_set_child_subreaper: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	struct linted_spawn_attr *attr;
	{
		struct linted_spawn_attr *xx;
		err = linted_spawn_attr_init(&xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_spawn_attr_init: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		attr = xx;
	}

	linted_spawn_attr_set_die_on_parent_death(attr);

	for (;;) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: spawning %s\n",
		                       process_name, monitor_base);

		linted_pid child;
		{
			linted_pid xx;
			err = linted_spawn(
			    &xx, LINTED_KO_CWD, monitor, 0, attr,
			    (char const *const[]){monitor_base, 0}, 0);
			if (err != 0) {
				linted_log(LINTED_LOG_ERROR,
				           "linted_spawn: %s",
				           linted_error_string(err));
				return EXIT_FAILURE;
			}
			child = xx;
		}
		monitor_pid = child;

		linted_pid pid;
		int code;
		int status;
		for (;;) {
			{
				siginfo_t info;
				if (-1 ==
				    waitid(P_ALL, -1, &info, WEXITED))
					goto waitid_failed;
				pid = info.si_pid;
				code = info.si_code;
				status = info.si_status;
				goto waitid_succeeded;
			}
		waitid_failed:
			err = errno;
			LINTED_ASSUME(err != 0);
			if (EINTR == err)
				continue;

			LINTED_ASSERT(err != EINVAL);
			LINTED_ASSERT(err != ECHILD);
			LINTED_ASSERT(0 == err);
			LINTED_ASSUME_UNREACHABLE();

		waitid_succeeded:
			if (child == pid)
				break;
		}
		monitor_pid = 0;

		switch (code) {
		case CLD_EXITED:
			if (EXIT_SUCCESS == status)
				goto exit_loop;

			linted_io_write_format(
			    LINTED_KO_STDERR, 0,
			    "monitor exited with %i\n", status);
			break;

		case CLD_DUMPED:
		case CLD_KILLED:
			linted_io_write_format(
			    LINTED_KO_STDERR, 0,
			    "monitor killed by %s\n",
			    linted_signal_string(status));
			break;

		default:
			LINTED_ASSUME_UNREACHABLE();
		}
	}
exit_loop:
	return EXIT_SUCCESS;
}

static void delegate_signal(int signo)
{
	linted_error old_err = errno;

	linted_error err = 0;

	/* All signals are blocked here. */

	linted_pid the_pid = monitor_pid;
	if (the_pid > 0U) {
		/* If WNOHANG was specified in options and there were
		 * no children in a waitable state, then waitid()
		 * returns 0 immediately and the state of the
		 * siginfo_t structure pointed to by infop is
		 * unspecified.
		 *
		 * - WAIT(2) http://www.kernel.org/doc/man-pages/.
		 */
		{
			siginfo_t info = {0};
			if (-1 == waitid(P_PID, the_pid, &info,
			                 WEXITED | WNOWAIT | WNOHANG))
				err = errno;

			if (ECHILD == err)
				goto restore_errno;
			if (err != 0) {
				LINTED_ASSERT(err != EINTR);
				LINTED_ASSERT(err != EINVAL);
				LINTED_ASSERT(0 == err);
			}

			/* The process was killed and is waitable */
			if (info.si_pid != 0)
				goto restore_errno;
		}

		/* The process may be dead but not waited on at least */
		kill(the_pid, signo);
	}

restore_errno:
	errno = old_err;
}
