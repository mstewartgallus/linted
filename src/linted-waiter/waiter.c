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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

static unsigned char waiter_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

static void on_term(int signo);
static void on_sigchld(int signo);

static linted_error set_name(char const *name);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-waiter", .start = waiter_start
};

static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };

static unsigned char waiter_start(char const *process_name, size_t argc,
                                  char const *const argv[])
{
	linted_error errnum = 0;

	char const *service = getenv("LINTED_SERVICE");
	if (service != 0) {
		errnum = set_name(service);
		assert(errnum != EINVAL);
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii) {
		struct sigaction action = { 0 };
		action.sa_handler = on_term;
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	{
		struct sigaction action = { 0 };
		action.sa_handler = on_sigchld;
		action.sa_flags = SA_NOCLDSTOP;
		if (-1 == sigaction(SIGCHLD, &action, NULL)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	{
		char *buf = 0;
		size_t n = 0U;
		for (;;) {
			errno = 0;
			if (-1 == getline(&buf, &n, stdin)) {
				errnum = errno;
				if (0 == errnum)
					break;
				if (EINTR == errnum)
					continue;
				if (errnum != 0) {
					linted_log(LINTED_LOG_ERROR,
					           "getline: %s",
					           linted_error_string(errnum));
					return EXIT_FAILURE;
				}
			}
			linted_log(LINTED_LOG_ERROR, buf);
		}
	}

	for (;;)
		pause();
}

static void on_term(int signo)
{
	if (-1 == kill(-getpgrp(), signo)) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(false);
	}

	/* Prevent looping */
	for (;;) {
		sigset_t exitset;
		sigemptyset(&exitset);
		for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii)
			sigaddset(&exitset, exit_signals[ii]);

		siginfo_t info;
		if (-1 == sigwaitinfo(&exitset, &info))
			break;
	}
}

static void on_sigchld(int signo)
{
	for (;;) {
		pid_t pid;
		int wait_status;
		{
			siginfo_t info;
			info.si_pid = 0;
			wait_status =
			    waitid(P_ALL, -1, &info, WEXITED | WNOHANG);
			pid = info.si_pid;
		}
		if (-1 == wait_status) {
			linted_error errnum = errno;
			LINTED_ASSUME(errnum != 0);

			if (ECHILD == errnum)
				_Exit(EXIT_SUCCESS);
			if (EINTR == errnum)
				continue;

			assert(false);
		}

		if (0 == pid)
			break;
	}
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
