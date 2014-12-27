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
#include "linted/io.h"
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
#include <unistd.h>

static volatile sig_atomic_t waitable_process_pending = false;

static void do_nothing(int signo);
static void sigchld_handler(int signo);

static void drain_from_to(int in, int out);

static linted_error set_name(char const *name);

static linted_ko kos[3U];

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-waiter",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos
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

	linted_ko stdin_writer = kos[0U];
	linted_ko stdout_reader = kos[1U];
	linted_ko stderr_reader = kos[2U];

	/* We do not use SA_RESTART here so that we get an EINTR on
	 * ppoll and can check if a waitable process is pending */
	{
		struct sigaction action = { 0 };
		action.sa_flags = SA_NOCLDSTOP;
		action.sa_handler = sigchld_handler;
		sigfillset(&action.sa_mask);
		if (-1 == sigaction(SIGCHLD, &action, NULL)) {
			perror("sigaction");
			return EXIT_FAILURE;
		}
	}

	sigset_t sigchld_unblocked;
	sigemptyset(&sigchld_unblocked);
	sigaddset(&sigchld_unblocked, SIGCHLD);
	errnum =
	    pthread_sigmask(SIG_BLOCK, &sigchld_unblocked, &sigchld_unblocked);
	if (errnum != 0) {
		errno = errnum;
		perror("pthread_sigmask");
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
		action.sa_handler = do_nothing;
		action.sa_flags = 0;
		sigfillset(&action.sa_mask);
		if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
			assert(errno != EINVAL);
			assert(0);
		}
	}

	bool out_closed = false;
	bool err_closed = false;
	bool input_closed = false;
	for (;;) {
		enum { STDOUT_FD, STDERR_FD, IN_FD, FDS_COUNT };

		struct pollfd fds[FDS_COUNT];

		fds[STDOUT_FD].fd = out_closed ? -1 : (int)stdout_reader;
		fds[STDOUT_FD].events = POLLIN;

		fds[STDERR_FD].fd = err_closed ? -1 : (int)stderr_reader;
		fds[STDERR_FD].events = POLLIN;

		fds[IN_FD].fd = input_closed ? -1 : STDIN_FILENO;
		fds[IN_FD].events = POLLIN;

		if (-1 == ppoll(fds, LINTED_ARRAY_SIZE(fds), NULL,
		                &sigchld_unblocked)) {
			if (EINTR == errno)
				goto on_interrupt;
			perror("ppoll");
			return EXIT_FAILURE;
		}

		if (!out_closed) {
			if ((fds[STDOUT_FD].revents & POLLHUP) != 0) {
				out_closed = 1;
				close(stdout_reader);
				close(STDOUT_FILENO);
			}

			if ((fds[STDOUT_FD].revents & POLLIN) != 0)
				drain_from_to(stdout_reader, STDOUT_FILENO);
		}

		if (!err_closed) {
			if ((fds[STDERR_FD].revents & POLLHUP) != 0) {
				err_closed = 1;
				close(stderr_reader);
				close(STDERR_FILENO);
			}

			if ((fds[STDERR_FD].revents & POLLIN) != 0)
				drain_from_to(stderr_reader, STDERR_FILENO);
		}

		if (!input_closed) {
			if ((fds[IN_FD].revents & POLLHUP) != 0) {
				input_closed = 1;
				close(STDIN_FILENO);
				close(stdin_writer);
			}

			if ((fds[IN_FD].revents & POLLIN) != 0)
				drain_from_to(STDIN_FILENO, stdin_writer);
		}
		continue;

	on_interrupt:
		if (!waitable_process_pending)
			continue;

		for (;;) {
			int xx;
			switch (waitpid(-1, &xx, WNOHANG)) {
			case -1:
				switch (errno) {
				case ECHILD:
					goto exit_application;
				case EINTR:
					continue;
				default:
					perror("waitpid");
					return EXIT_FAILURE;
				}

			case 0:
				goto no_more_pending_waitable_processes;

			default:
				continue;
			}
		}
	no_more_pending_waitable_processes:
		waitable_process_pending = false;
	}
exit_application:
	return EXIT_SUCCESS;
}

static void do_nothing(int signo)
{
	/* Do nothing, monitor will notify our children for us. If
	 * they choose to exit then we will exit afterwards. */
}

static void sigchld_handler(int signo)
{
	waitable_process_pending = true;
}

static void drain_from_to(int in, int out)
{
	for (;;) {
		char buf[80U];
		ssize_t bytes_read = read(in, buf, sizeof buf);
		switch (bytes_read) {
		case -1:
			if (EAGAIN == errno)
				return;
			if (EINTR == errno)
				return;
			exit(EXIT_FAILURE);

		default:
			if (-1 == write(out, buf, bytes_read)) {
				exit(EXIT_FAILURE);
			}
			break;
		}
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
