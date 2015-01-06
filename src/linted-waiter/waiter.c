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
#define _GNU_SOURCE

#include "config.h"

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/poll.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <syslog.h>
#include <unistd.h>

static volatile sig_atomic_t waitable_process_pending = false;

static void handle_exit_sig(int signo);
static void sigchld_handler(int signo);

static linted_error set_name(char const *name);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-waiter"
};

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	char const *service = getenv("LINTED_SERVICE");
	if (service != NULL) {
		errnum = set_name(service);
		assert(errnum != EINVAL);
	}

	/* We do not use SA_RESTART here so that we get an EINTR on
	 * ppoll and can check if a waitable process is pending */
	{
		struct sigaction action = { 0 };
		action.sa_flags = SA_NOCLDSTOP;
		action.sa_handler = sigchld_handler;
		sigfillset(&action.sa_mask);
		if (-1 == sigaction(SIGCHLD, &action, NULL)) {
			syslog(LOG_ERR, "sigaction: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	sigset_t sigchld_unblocked;
	sigemptyset(&sigchld_unblocked);
	sigaddset(&sigchld_unblocked, SIGCHLD);
	errnum =
	    pthread_sigmask(SIG_BLOCK, &sigchld_unblocked, &sigchld_unblocked);
	if (errnum != 0) {
		syslog(LOG_ERR, "pthread_sigmask: %s",
		       linted_error_string(errno));
		return EXIT_FAILURE;
	}
	sigdelset(&sigchld_unblocked, SIGCHLD);

	/* Catch signals
	 *
	 * The only signals that can be sent to process ID 1, the init
	 * process, are those for which init has explicitly installed
	 * signal handlers.  This is done to assure the system is not
	 * brought down accidentally.
	 *
	 * - KILL(2) http://www.kernel.org/doc/man-pages/.
	 *
	 * This applies to sandboxes to if they use CLONE_NEWPID.
	 *
	 * We want to explicitly handle the signal so that the monitor
	 * can observe it and propagate the signal to it's children as
	 * well. Also we want to do this anyways for SUBREAPER
	 * sandboxes as well. Maybe we should propagate the signals
	 * ourselves though.
	 */
	static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };

	/* Delegate the exit signals to children and then exit when
	 * they do. */
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals); ++ii) {
		struct sigaction action = { 0 };
		action.sa_handler = handle_exit_sig;
		action.sa_flags = 0;
		sigfillset(&action.sa_mask);
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			assert(errno != EINVAL);
			assert(0);
		}
	}

	for (;;) {
		int xx;
		switch (waitpid(-1, &xx, 0)) {
		case -1:
			switch (errno) {
			case ECHILD:
				goto exit_application;
			case EINTR:
				continue;
			default:
				syslog(LOG_ERR, "waitpid: %s",
				       linted_error_string(errno));
				return EXIT_FAILURE;
			}

		default:
			continue;
		}
	}
exit_application:
	return EXIT_SUCCESS;
}

static void handle_exit_sig(int signo)
{
	/* Let the sandbox kill the children */
	_Exit(signo);
}

static void sigchld_handler(int signo)
{
	waitable_process_pending = true;
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
