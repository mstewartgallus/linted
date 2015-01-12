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
#include <inttypes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <syslog.h>
#include <unistd.h>

static linted_error kill_pid_children(pid_t pid, int signo);

static linted_error pid_children(pid_t pid, pid_t **childrenp, size_t *lenp);

static linted_error set_name(char const *name);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-waiter"
};

unsigned char linted_start(char const *process_name, size_t argc,
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
		syslog(LOG_ERR, "pthread_sigmask: %s",
		       linted_error_string(errno));
		return EXIT_FAILURE;
	}

	siginfo_t info;
	for (;;) {
		int signo = sigwaitinfo(&sigset, &info);
		switch (signo) {
		case -1:
			if (EINTR == errno)
				continue;
			syslog(LOG_ERR, "sigwaitinfo: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;

		case SIGCHLD:
			for (;;) {
				int xx;
				pid_t pid = waitpid(-1, &xx, WNOHANG);
				switch (pid) {
				case -1:
					switch (errno) {
					case ECHILD:
						goto exit_application;
					case EINTR:
						continue;
					default:
						syslog(
						    LOG_ERR, "waitpid: %s",
						    linted_error_string(errno));
						return EXIT_FAILURE;
					}

				case 0:
					goto continue_waiting_on_signals;

				default:
					syslog(LOG_ERR, "reaped: %i", pid);
					continue;
				}
			}
		continue_waiting_on_signals:
			break;

		case SIGHUP:
		case SIGINT:
		case SIGQUIT:
		case SIGTERM:
			kill_pid_children(getpid(), signo);
			break;
		}
	}
exit_application:
	return EXIT_SUCCESS;
}

static linted_error kill_pid_children(pid_t pid, int signo)
{
	linted_error errnum = 0;

	pid_t *children;
	size_t len;
	{
		pid_t *xx;
		size_t yy;
		errnum = pid_children(pid, &xx, &yy);
		if (errnum != 0)
			return errnum;
		children = xx;
		len = yy;
	}

	for (size_t ii = 0U; ii < len; ++ii) {
		if (-1 == kill(children[ii], signo)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_children;
		}
	}

free_children:
	linted_mem_free(children);

	return errnum;
}

static linted_error pid_children(pid_t pid, pid_t **childrenp, size_t *lenp)
{
	linted_error errnum;

	char path[sizeof "/proc/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/task/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/children" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/task/%" PRIuMAX "/children",
	                  (uintmax_t)pid, (uintmax_t)pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	linted_ko children_ko;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
		if (errnum != 0)
			return errnum;
		children_ko = xx;
	}

	FILE *file = fdopen(children_ko, "r");
	if (0 == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(children_ko);

		return errnum;
	}

	/* Get the child all at once to avoid raciness. */
	char *buf = 0;

	{
		char *xx = buf;
		size_t yy = 0U;

		errno = 0;
		ssize_t zz = getline(&xx, &yy, file);
		if (-1 == zz) {
			errnum = errno;
			/* May be zero */
			goto set_childrenp;
		}
		buf = xx;
	}

set_childrenp:
	if (EOF == fclose(file)) {
		if (0 == errnum) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

	if (errnum != 0) {
		linted_mem_free(buf);
		return errnum;
	}

	size_t ii = 0U;
	char const *start = buf;
	pid_t *children = 0;

	if (0 == buf)
		goto finish;

	for (;;) {
		errno = 0;
		pid_t child = strtol(start, 0, 10);
		errnum = errno;
		if (errnum != 0)
			goto free_buf;

		{
			void *xx;
			errnum = linted_mem_realloc_array(
			    &xx, children, ii + 1U, sizeof children[0U]);
			if (errnum != 0) {
				linted_mem_free(children);
				linted_mem_free(buf);
				return errnum;
			}
			children = xx;
		}
		children[ii] = child;
		++ii;

		start = strchr(start, ' ');
		if (0 == start)
			break;
		if ('\n' == *start)
			break;
		if ('\0' == *start)
			break;
		++start;
		if ('\n' == *start)
			break;
		if ('\0' == *start)
			break;
	}

free_buf:
	linted_mem_free(buf);

finish:
	*lenp = ii;
	*childrenp = children;

	return 0;
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
