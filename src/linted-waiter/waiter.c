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
#include "linted/log.h"
#include "linted/start.h"
#include "linted/util.h"

#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

static unsigned char waiter_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

static linted_error set_name(char const *name);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-waiter", .start = waiter_start
};

static unsigned char waiter_start(char const *process_name, size_t argc,
                                  char const *const argv[])
{
	linted_error errnum = 0;

	char const *service = getenv("LINTED_SERVICE");
	if (service != 0) {
		errnum = set_name(service);
		assert(errnum != EINVAL);
	}

	static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };

	sigset_t sigset;
	sigemptyset(&sigset);
	sigaddset(&sigset, SIGCHLD);
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii)
		sigaddset(&sigset, exit_signals[ii]);

	errnum = pthread_sigmask(SIG_BLOCK, &sigset, NULL);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERR, "pthread_sigmask: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii) {
		struct sigaction action = { 0 };
		action.sa_handler = SIG_IGN;
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			linted_log(LINTED_LOG_ERR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	for (;;) {
		int signo;
		{
			siginfo_t info;
			signo = sigwaitinfo(&sigset, &info);
		}
		switch (signo) {
		case -1:
			errnum = errno;
			LINTED_ASSUME(errnum != 0);

			if (EINTR == errnum)
				continue;

			linted_log(LINTED_LOG_ERR, "sigwaitinfo: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;

		case SIGCHLD:
			for (;;) {
				pid_t pid;
				int wait_status;
				{
					siginfo_t info;
					info.si_pid = 0;
					wait_status = waitid(P_ALL, -1, &info,
					                     WEXITED | WNOHANG);
					pid = info.si_pid;
				}
				if (-1 == wait_status) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);

					if (ECHILD == errnum)
						goto exit_application;
					if (EINTR == errnum)
						continue;

					linted_log(LINTED_LOG_ERR, "waitid: %s",
					           linted_error_string(errnum));
					return EXIT_FAILURE;
				}

				if (0 == pid)
					break;
			}
			break;

		case SIGHUP:
		case SIGINT:
		case SIGQUIT:
		case SIGTERM: {
			sigset_t exitset;
			sigemptyset(&exitset);
			for (size_t ii = 0U;
			     ii < LINTED_ARRAY_SIZE(exit_signals); ++ii)
				sigaddset(&exitset, exit_signals[ii]);

			/* Prevent looping */
			errnum =
			    pthread_sigmask(SIG_UNBLOCK, &exitset, &exitset);
			if (errnum != 0) {
				linted_log(LINTED_LOG_ERR,
				           "pthread_sigmask: %s",
				           linted_error_string(errnum));
				return EXIT_FAILURE;
			}

			if (-1 == kill(-getpgrp(), signo)) {
				errnum = errno;
				linted_log(LINTED_LOG_ERR, "kill: %s",
				           linted_error_string(errnum));
				return EXIT_FAILURE;
			}

			errnum = pthread_sigmask(SIG_SETMASK, &exitset, NULL);
			if (errnum != 0) {
				linted_log(LINTED_LOG_ERR,
				           "pthread_sigmask: %s",
				           linted_error_string(errnum));
				return EXIT_FAILURE;
			}
			break;
		}
		}
	}
exit_application:
	return EXIT_SUCCESS;
}

static linted_error set_name(char const *name)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
}
