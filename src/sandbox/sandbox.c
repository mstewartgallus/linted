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
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/poll.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/**
 * @file
 *
 * Sandbox applications.
 */

enum {
	STOP_OPTIONS,
	HELP,
	VERSION_OPTION,
	TRACEME,
	CHDIR,
	WAITER
};

static char const *const argstrs[] = {
	    /**/[STOP_OPTIONS] = "--",
	    /**/ [HELP] = "--help",
	    /**/ [VERSION_OPTION] = "--version",
	    /**/ [TRACEME] = "--traceme",
	    /**/ [CHDIR] = "--chdir",
	    /**/ [WAITER] = "--waiter"
};

static pid_t do_first_fork(
    linted_ko err_reader, linted_ko err_writer, linted_ko cwd,
    char const *chdir_path,
    char *listen_pid_str, char *listen_fds_str, linted_ko stdin_writer,
    linted_ko stdout_reader, linted_ko stderr_reader, linted_ko stdin_reader,
    linted_ko stdout_writer, linted_ko stderr_writer, char const *waiter,
    char const *const *env_copy, char const *const *command, size_t num_fds);

static pid_t do_second_fork(linted_ko err_writer, linted_ko stdin_reader,
                            linted_ko stdout_writer, linted_ko stderr_writer,
                            char *listen_pid_str, char const *const *argv,
                            char const *const *env);

static void exit_with_error(linted_ko writer, linted_error errnum);

static void pid_to_str(char *buf, pid_t pid);

static linted_error set_child_subreaper(bool v);

int main(int argc, char *argv[])
{
	linted_error errnum;

	size_t arguments_length = argc;

	char const *listen_fds = getenv("LISTEN_FDS");
	if (NULL == listen_fds) {
		fprintf(stderr, "need LISTEN_FDS\n");
		return EXIT_FAILURE;
	}

	char const *bad_option = NULL;
	bool need_version = false;
	bool need_help = false;

	bool traceme = false;

	char const *chdir_path = NULL;
	char const *waiter = NULL;
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

		case TRACEME:
			traceme = true;
			break;

		case CHDIR:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			chdir_path = argv[ii];
			break;

		case WAITER:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			waiter = argv[ii];
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

	if (NULL == waiter) {
		fprintf(stderr, "need waiter\n");
		return EXIT_FAILURE;
	}

	if (traceme) {
		if (-1 == ptrace(PTRACE_TRACEME, (pid_t)0, (void *)NULL,
		                 (void *)NULL)) {
			perror("ptrace");
			return EXIT_FAILURE;
		}

		/* Register with the parent */
		if (-1 == raise(SIGSTOP)) {
			perror("raise");
			return EXIT_FAILURE;
		}
	}

	char const *process_name = argv[0U];

	char const *const *command =
	    (char const * const *)argv + 1U + command_start;

	linted_ko cwd;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, ".",
		                        LINTED_KO_DIRECTORY);
		if (errnum != 0) {
			linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not open the current working directory: %s\n",
			                       process_name,
			                       linted_error_string(errno));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	linted_ko stdin_reader;
	linted_ko stdin_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stdin_reader = xx[0U];
		stdin_writer = xx[1U];
	}

	linted_ko stdout_reader;
	linted_ko stdout_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stdout_reader = xx[0U];
		stdout_writer = xx[1U];
	}

	linted_ko stderr_reader;
	linted_ko stderr_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stderr_reader = xx[0U];
		stderr_writer = xx[1U];
	}

	char **env_copy = NULL;
	size_t env_size = 0U;
	for (char const *const *env = (char const * const *)environ;
	     *env != NULL; ++env)
		++env_size;

	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, env_size + 3U,
		                                sizeof env_copy[0U]);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_mem_alloc_array");
			return EXIT_FAILURE;
		}
		env_copy = xx;
	}

	for (size_t ii = 0U; ii < env_size; ++ii)
		env_copy[2U + ii] = environ[ii];

	size_t num_fds = atoi(listen_fds);
	char listen_fds_str[] = "LISTEN_FDS=XXXXXXXXXXXXXXXXXX";
	char listen_pid_str[] = "LISTEN_PID=XXXXXXXXXXXXXXXXXX";

	sprintf(listen_fds_str, "LISTEN_FDS=%lu", num_fds);

	env_copy[0U] = listen_fds_str;
	env_copy[1U] = listen_pid_str;
	env_copy[env_size + 2U] = NULL;

	linted_ko err_reader;
	linted_ko err_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		err_reader = xx[0U];
		err_writer = xx[1U];
	}

	pid_t child = do_first_fork(
	    err_reader, err_writer, cwd,
	    chdir_path,
	    listen_pid_str, listen_fds_str, stdin_writer,
	    stdout_reader, stderr_reader, stdin_reader, stdout_writer,
	    stderr_writer, waiter, (char const * const *)env_copy, command,
	    num_fds);
	if (-1 == child) {
		perror("clone");
		return EXIT_FAILURE;
	}

	linted_ko_close(err_writer);

	{
		size_t xx;
		linted_error yy;
		errnum = linted_io_read_all(err_reader, &xx, &yy, sizeof yy);
		if (errnum != 0)
			goto close_err_reader;

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			errnum = yy;
	}
close_err_reader:
	linted_ko_close(err_reader);

	if (errnum != 0) {
		errno = errnum;
		perror("spawning");
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

pid_t do_first_fork(
    linted_ko err_reader, linted_ko err_writer, linted_ko cwd,
    char const *chdir_path,
    char *listen_pid_str, char *listen_fds_str, linted_ko stdin_writer,
    linted_ko stdout_reader, linted_ko stderr_reader, linted_ko stdin_reader,
    linted_ko stdout_writer, linted_ko stderr_writer, char const *waiter,
    char const *const *env_copy, char const *const *command, size_t num_fds)
{
	pid_t child = fork();
	if (child != 0)
		return child;

	linted_error errnum = 0;

	linted_ko_close(err_reader);

	if (chdir_path != NULL) {
		if (-1 == chdir(chdir_path))
			exit_with_error(err_writer, errno);
	}

	/*
	 * This isn't needed if CLONE_NEWPID was used but it doesn't
	 * hurt,
	 */
	errnum = set_child_subreaper(true);
	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	{
		sigset_t sigset;
		sigemptyset(&sigset);
		sigaddset(&sigset, SIGCHLD);
		errnum = pthread_sigmask(SIG_UNBLOCK, &sigset, NULL);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	sigset_t sigchld_unblocked;
	{
		sigset_t sigset;
		sigemptyset(&sigset);
		sigaddset(&sigset, SIGCHLD);
		errnum =
		    pthread_sigmask(SIG_BLOCK, &sigset, &sigchld_unblocked);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	linted_ko vfork_err_reader;
	linted_ko vfork_err_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK))
			exit_with_error(err_writer, errno);
		vfork_err_reader = xx[0U];
		vfork_err_writer = xx[1U];
	}

	pid_t grand_child = do_second_fork(
	    vfork_err_writer, stdin_reader, stdout_writer, stderr_writer,
	    listen_pid_str, command, env_copy);
	if (-1 == grand_child)
		exit_with_error(err_writer, errno);

	linted_ko_close(vfork_err_writer);

	linted_ko_close(stdin_reader);
	linted_ko_close(stdout_writer);
	linted_ko_close(stderr_writer);

	{
		size_t xx;
		linted_error yy;
		errnum =
		    linted_io_read_all(vfork_err_reader, &xx, &yy, sizeof yy);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			errnum = yy;
	}
	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	linted_ko_close(cwd);

	for (size_t ii = 0U; ii < num_fds; ++ii)
		linted_ko_close(3 + ii);

	if (-1 == dup2(stdin_writer, 3))
		exit_with_error(err_writer, errno);
	if (-1 == dup2(stdout_reader, 4))
		exit_with_error(err_writer, errno);
	if (-1 == dup2(stderr_reader, 5))
		exit_with_error(err_writer, errno);

	pid_to_str(listen_fds_str + strlen("LISTEN_FDS="), 3);
	pid_to_str(listen_pid_str + strlen("LISTEN_PID="), getpid());

	char const *arguments[] = { waiter, NULL };
	execve(waiter, (char * const *)arguments, (char * const *)env_copy);
	exit_with_error(err_writer, errno);
	/* Not reached */
	return 0;
}

static pid_t do_second_fork(linted_ko err_writer, linted_ko stdin_reader,
                            linted_ko stdout_writer, linted_ko stderr_writer,
                            char *listen_pid_str, char const *const *argv,
                            char const *const *env)
{
	pid_t child = fork();
	if (child != 0)
		return child;

	pid_to_str(listen_pid_str + strlen("LISTEN_PID="), getpid());

	/* Terminals are really ugly and horrible, avoid them. */
	int tty = open("/dev/tty", O_CLOEXEC);
	if (-1 == tty) {
		if (errno != ENXIO)
			exit_with_error(err_writer, errno);
	} else {
		if (-1 == ioctl(tty, TIOCNOTTY))
			exit_with_error(err_writer, errno);
	}

	if (-1 == dup2(stdin_reader, STDIN_FILENO))
		exit_with_error(err_writer, errno);

	if (-1 == dup2(stdout_writer, STDOUT_FILENO))
		exit_with_error(err_writer, errno);

	if (-1 == dup2(stderr_writer, STDERR_FILENO))
		exit_with_error(err_writer, errno);

	if (-1 == setsid())
		exit_with_error(err_writer, errno);

	execve(argv[0U], (char * const *)argv, (char * const *)env);
	exit_with_error(err_writer, errno);

	return 0;
}

static void exit_with_error(linted_ko writer, linted_error errnum)
{
	linted_io_write_all(writer, NULL, &errnum, sizeof errnum);
	_Exit(EXIT_FAILURE);
}

static void pid_to_str(char *buf, pid_t pid)
{
	size_t strsize = 0U;

	assert(pid > 0);

	for (;;) {
		memmove(buf + 1U, buf, strsize);

		pid_t digit = pid % 10;

		*buf = '0' + digit;

		pid /= 10;
		++strsize;

		if (0 == pid)
			break;
	}

	buf[strsize] = '\0';
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
