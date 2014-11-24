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
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>

enum {
	STOP_OPTIONS,
	HELP,
	VERSION_OPTION,
	DROP_CAPS,
	NO_NEW_PRIVS,
	CHDIR,
	PRIORITY,
	PID_OUT_FD
};

static struct sock_fprog const default_filter;

static char const *const argstrs[] = {
	    /**/[STOP_OPTIONS] = "--",
	    /**/ [HELP] = "--help",
	    /**/ [VERSION_OPTION] = "--version",
	    /**/ [DROP_CAPS] = "--dropcaps",
	    /**/ [NO_NEW_PRIVS] = "--nonewprivs",
	    /**/ [CHDIR] = "--chdir",
	    /**/ [PRIORITY] = "--priority",
	    /**/ [PID_OUT_FD] = "--pidoutfd"
};

static void propagate_signal(int signo);
static linted_error set_name(char const *name);

static linted_error set_child_subreaper(bool v);
static linted_error set_no_new_privs(bool b);
static linted_error set_seccomp(struct sock_fprog const *program);

int main(int argc, char *argv[])
{
	linted_error errnum;

	size_t arguments_length = argc;

	char const *service = getenv("LINTED_SERVICE");
	char const *listen_fds = getenv("LISTEN_FDS");
	if (NULL == listen_fds) {
		fprintf(stderr, "need LISTEN_FDS\n");
		return EXIT_FAILURE;
	}

	if (service != NULL) {
		errnum = set_name(service);
		assert(errnum != EINVAL);
	}

	char const *bad_option = NULL;
	bool need_version = false;
	bool need_help = false;
	bool no_new_privs = false;
	bool drop_caps = false;
	char const *chdir_path = NULL;
	char const *priority = NULL;
	char const *pid_out_fd = NULL;
	bool have_command = false;
	size_t command_start;

	for (size_t ii = 1U; ii < arguments_length; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(argstrs); ++jj) {
			if (0 == strcmp(argument, argstrs[jj])) {
				arg = jj;
				break;
			}
		}

		switch (arg) {
		case -1:
			bad_option = argument;
			break;

		case STOP_OPTIONS:
			have_command = true;
			command_start = ii;
			goto exit_loop;

		case HELP:
			need_help = true;
			break;

		case VERSION_OPTION:
			need_version = true;
			break;

		case NO_NEW_PRIVS:
			no_new_privs = true;
			break;

		case DROP_CAPS:
			drop_caps = true;
			break;

		case CHDIR:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			chdir_path = argv[ii];
			break;

		case PRIORITY:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			priority = argv[ii];
			break;

		case PID_OUT_FD:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			pid_out_fd = argv[ii];
			break;
		}
	}
exit_loop:
	if (!have_command) {
		fprintf(stderr, "need command\n");
		return EXIT_FAILURE;
	}

	if (bad_option != NULL) {
		fprintf(stderr, "bad option: %s\n", bad_option);
		return EXIT_FAILURE;
	}

	char const *const *command =
	    (char const * const *)argv + 1U + command_start;

	if (pid_out_fd != NULL) {
		int fd = atoi(pid_out_fd);

		char dummy[] = { 0 };
		struct iovec iovecs[] = { { .iov_base = dummy,
				            .iov_len = sizeof dummy } };

		struct msghdr message = { 0 };
		message.msg_iov = iovecs;
		message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);
		message.msg_control = NULL;
		message.msg_controllen = 0U;

		if (-1 == sendmsg(fd, &message, MSG_NOSIGNAL)) {
			perror("sendmsg");
			return EXIT_FAILURE;
		}
		linted_ko_close(fd);
	}

	if (priority != NULL) {
		if (-1 == setpriority(PRIO_PROCESS, 0, atoi(priority))) {
			perror("setpriority");
			return EXIT_FAILURE;
		}
	}

	/*
	 * This isn't needed if CLONE_NEWPID was used but it doesn't
	 * hurt,
	 */
	errnum = set_child_subreaper(true);
	if (errnum != 0) {
		errno = errnum;
		perror("set_child_subreaper");
		return EXIT_FAILURE;
	}

	if (chdir_path != NULL) {
		if (-1 == chdir(chdir_path)) {
			perror("chdir");
			return EXIT_FAILURE;
		}
	}

	/* Drop all capabilities I might possibly have. I'm not sure I
	 * need to do this and I probably can do this in a better
	 * way. Note that currently we do not use PR_SET_KEEPCAPS and
	 * do not map our sandboxed user to root but if we did in the
	 * future we would need this.
	 */

	if (drop_caps) {
		cap_t caps = cap_get_proc();
		if (NULL == caps) {
			perror("cap_get_proc");
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
			perror("cap_clear_flag");
			return EXIT_FAILURE;
		}
		if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
			perror("cap_clear_flag");
			return EXIT_FAILURE;
		}
		if (-1 == cap_clear_flag(caps, CAP_INHERITABLE)) {
			perror("cap_clear_flag");
			return EXIT_FAILURE;
		}

		if (-1 == cap_set_proc(caps)) {
			perror("cap_set_proc");
			return EXIT_FAILURE;
		}

		if (-1 == cap_free(caps)) {
			perror("cap_free");
			return EXIT_FAILURE;
		}
	}

	if (no_new_privs) {
		/* Must appear before the seccomp filter */
		errnum = set_no_new_privs(true);
		if (errnum != 0) {
			errno = errnum;
			perror("set_no_new_privs");
			return EXIT_FAILURE;
		}

		errnum = set_seccomp(&default_filter);
		if (errnum != 0) {
			errno = errnum;
			perror("set_seccomp");
			return EXIT_FAILURE;
		}
	}

	pid_t child = fork();
	if (-1 == child) {
		perror("fork");
		return EXIT_FAILURE;
	}

	if (0 == child) {
		{
			char xx[] = "XXXXXXXXXXXXXXXXX";
			sprintf(xx, "%i", getpid());
			if (-1 == setenv("LISTEN_PID", xx, true)) {
				perror("setenv");
				return EXIT_FAILURE;
			}
		}

		{
			char xx[] = "XXXXXXXXXXXXXXXXX";
			sprintf(xx, "%i", atoi(listen_fds) - 1);
			if (-1 == setenv("LISTEN_FDS", xx, true)) {
				perror("setenv");
				return EXIT_FAILURE;
			}
		}

		execve(command[0U], (char * const *)command, environ);
		perror("execve");
		return EXIT_FAILURE;
	}

	/* Catch signals
	 *
	 * The only signals that can be sent to process ID 1, the init
	 * process, are those for which init has explicitly installed
	 * signal handlers.  This is done to assure the system is not
	 * brought down accidentally.
	 *
	 * - KILL(2) http://www.kernel.org/doc/man-pages/.
	 *
	 * This applies to sandboxs to if they use CLONE_NEWPID.
	 *
	 * We want to explicitly handle the signal so that it is
	 * propagated to children of init as well.
	 */
	static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };
	if (1 == getpid()) {
		/* Delegate the exit signal to children and then exit */
		for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals);
		     ++ii) {
			struct sigaction action = { 0 };
			action.sa_handler = propagate_signal;
			action.sa_flags = 0;
			sigfillset(&action.sa_mask);
			if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
				assert(errno != EINVAL);
				assert(0);
			}
		}
	}

	for (;;) {
		int wait_status;
		{
			siginfo_t info;
			wait_status = waitid(P_ALL, -1, &info, WEXITED);
		}
		if (-1 == wait_status) {
			errnum = errno;
			assert(errnum != 0);
			assert(errnum != EINVAL);
			if (errnum != EINTR)
				break;
		}
	}

	return errnum;
}

static void propagate_signal(int signo)
{
	kill(-1, signo);

	/* Sadly, it is impossible to kill oneself with the proper
	 * signal as init. */
	raise(SIGKILL);
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

static linted_error set_no_new_privs(bool b)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NO_NEW_PRIVS, (unsigned long)b, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(errnum != EINVAL);
		return errnum;
	}

	return 0;
}

static linted_error set_seccomp(struct sock_fprog const *program)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_SECCOMP, (unsigned long)SECCOMP_MODE_FILTER,
	                program, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
}

#if defined __amd64__
#include "sandbox-amd64.c"
#elif defined __i386__
#include "sandbox-i386.c"
#else
#error No default seccomp filter has been defined for this architecture
#endif
