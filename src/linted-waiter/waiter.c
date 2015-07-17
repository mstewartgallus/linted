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

#include "linted/environment.h"
#include "linted/error.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static unsigned char waiter_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

static void on_term(int signo);
static void on_sigchld(int signo);

static linted_error set_name(char const *name);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-waiter",
    .start = waiter_start};

static int const exit_signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM};

static unsigned char waiter_start(char const *process_name, size_t argc,
                                  char const *const argv[])
{
	linted_error err = 0;

	char *service;
	{
		char *xx;
		err = linted_environment_get("LINTED_SERVICE", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		service = xx;
	}
	if (service != 0) {
		err = set_name(service);
		assert(err != EINVAL);
		linted_mem_free(service);
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals);
	     ++ii) {
		struct sigaction action = {0};
		action.sa_handler = on_term;
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	{
		struct sigaction action = {0};
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
			ssize_t zz;
			{
				char *xx = buf;
				size_t yy = n;

				errno = 0;
				zz = getline(&xx, &yy, stdin);
				buf = xx;
				n = yy;
			}
			if (-1 == zz) {
				err = errno;
				if (0 == err)
					break;
				if (EINTR == err)
					continue;
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "getline: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
			}
			linted_log(LINTED_LOG_ERROR, "%s", buf);
		}
		linted_mem_free(buf);
	}

	/* Make sure to only exit after having fully drained the pipe
	 * of errors to be logged. */
	{
		struct sigaction action = {0};
		action.sa_handler = SIG_DFL;
		action.sa_flags = SA_NOCLDSTOP;
		if (-1 == sigaction(SIGCHLD, &action, NULL)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	for (;;) {
		int wait_status;
		{
			siginfo_t info;
			wait_status = waitid(P_ALL, -1, &info, WEXITED);
		}
		if (-1 == wait_status) {
			err = errno;
			LINTED_ASSUME(err != 0);

			if (ECHILD == err)
				break;
			if (EINTR == err)
				continue;

			assert(false);
		}
	}

	return EXIT_SUCCESS;
}

static void on_term(int signo)
{
	linted_error old_err = errno;

	if (-1 == kill(-getpgrp(), signo)) {
		linted_error err = errno;
		LINTED_ASSUME(err != 0);
		assert(false);
	}

	/* Prevent looping */
	sigset_t exitset;
	sigemptyset(&exitset);
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii)
		sigaddset(&exitset, exit_signals[ii]);

	struct timespec timeout = {0};

	for (;;) {
		siginfo_t info;
		if (-1 == sigtimedwait(&exitset, &info, &timeout)) {
			linted_error err = errno;
			if (EINTR == err)
				continue;
			assert(EAGAIN == err);
			break;
		}
	}

	errno = old_err;
}

static void on_sigchld(int signo)
{
	linted_error old_err = errno;

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
			linted_error err = errno;
			LINTED_ASSUME(err != 0);

			if (ECHILD == err)
				break;
			if (EINTR == err)
				continue;

			assert(false);
		}

		if (0 == pid)
			break;
	}

	errno = old_err;
}

static linted_error set_name(char const *name)
{
	linted_error err;

	if (-1 ==
	    prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL)) {
		err = errno;
		LINTED_ASSUME(err != 0);

		assert(err != EINVAL);

		return err;
	}
	return 0;
}
