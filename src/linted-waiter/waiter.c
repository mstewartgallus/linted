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

#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/prctl.h"
#include "lntd/start.h"
#include "lntd/util.h"

#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static void on_term(int signo);
static void on_sigchld(int signo, siginfo_t *infop, void *foo);
static void do_nothing(int signo);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-waiter",
    .dont_init_signals = true,

    /* Signals are strange for PID 1 so forking doesn't work well.
     * As well we still rely on the hack of scanning environ for the
     * unit name.
     */
    .dont_fork_thread = true};

static int const various_sigs[] = {SIGCONT};

static int const exit_signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	char *service;
	{
		char *xx;
		err = lntd_env_get("LINTED_SERVICE", &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_env_get: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		service = xx;
	}
	if (service != 0) {
		err = lntd_prctl_set_name(service);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_prctl_set_name: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		lntd_mem_free(service);
	}

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(various_sigs); ++ii) {
		struct sigaction action = {0};
		action.sa_handler = do_nothing;
		sigemptyset(&action.sa_mask);
		if (-1 == sigaction(various_sigs[ii], &action, NULL)) {
			lntd_log(LNTD_LOG_ERROR, "sigaction: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(exit_signals); ++ii) {
		struct sigaction action = {0};
		action.sa_handler = on_term;
		sigemptyset(&action.sa_mask);
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			lntd_log(LNTD_LOG_ERROR, "sigaction: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	{
		struct sigaction action = {0};
		action.sa_sigaction = on_sigchld;
		action.sa_flags = SA_SIGINFO;
		sigemptyset(&action.sa_mask);
		if (-1 == sigaction(SIGCHLD, &action, NULL)) {
			lntd_log(LNTD_LOG_ERROR, "sigaction: %s",
			         lntd_error_string(errno));
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
					lntd_log(
					    LNTD_LOG_ERROR,
					    "getline: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
			}
			lntd_log(LNTD_LOG_ERROR, "%s", buf);
		}
		lntd_mem_free(buf);
	}

	/* Make sure to only exit after having fully drained the pipe
	 * of errors to be logged. */
	{
		struct sigaction action = {0};
		action.sa_handler = do_nothing;
		action.sa_flags = 0;
		sigemptyset(&action.sa_mask);
		if (-1 == sigaction(SIGCHLD, &action, NULL)) {
			lntd_log(LNTD_LOG_ERROR, "sigaction: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	for (;;) {
		int wait_status;
		{
			int xx;
			wait_status = waitpid(-1, &xx, __WALL);
		}
		if (-1 == wait_status) {
			err = errno;
			LNTD_ASSUME(err != 0);

			if (ECHILD == err)
				break;
			if (EINTR == err)
				continue;

			LNTD_ASSERT(false);
		}
	}

	return EXIT_SUCCESS;
}

static void on_term(int signo)
{
	lntd_error old_err = errno;

	if (-1 == kill(-getpgrp(), signo)) {
		lntd_error err = errno;
		LNTD_ASSUME(err != 0);

		/* This is fine */
		if (ESRCH == err)
			goto prevent_looping;

		LNTD_ASSERT(err != LNTD_ERROR_INVALID_PARAMETER);
		LNTD_ASSERT(err != LNTD_ERROR_PERMISSION);
		LNTD_ASSERT(false);
	}

prevent_looping:
	;
	sigset_t exitset;
	sigemptyset(&exitset);
	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(exit_signals); ++ii)
		sigaddset(&exitset, exit_signals[ii]);

	struct timespec timeout = {0};

	for (;;) {
		siginfo_t info;
		if (-1 == sigtimedwait(&exitset, &info, &timeout)) {
			lntd_error err = errno;
			if (EINTR == err)
				continue;
			LNTD_ASSERT(EAGAIN == err);
			break;
		}
	}

	errno = old_err;
}

static void on_sigchld(int signo, siginfo_t *infop, void *foo)
{
	lntd_error old_err = errno;

	for (;;) {
		int wait_status;
		{
			int xx;
			wait_status =
			    waitpid(-1, &xx, __WALL | WNOHANG);
		}
		if (-1 == wait_status) {
			lntd_error err = errno;
			LNTD_ASSUME(err != 0);

			if (ECHILD == err)
				break;
			if (EINTR == err)
				continue;

			LNTD_ASSERT(false);
		}

		if (0U == wait_status)
			break;
	}

	errno = old_err;
}

static void do_nothing(int signo)
{
}
