/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#define _GNU_SOURCE

#include "config.h"

#include "linted/spawn.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <syscall.h>
#include <unistd.h>

#ifndef __NR_execveat
#define __NR_execveat 322
#endif

extern char **environ;

struct linted_spawn_file_actions {
	linted_ko new_stdin;
	linted_ko new_stdout;
	linted_ko new_stderr;
	bool set_stdin : 1U;
	bool set_stdout : 1U;
	bool set_stderr : 1U;
};

struct linted_spawn_attr {
	char dummy;
};

struct fork_args {
	sigset_t const *sigset;
	struct linted_spawn_file_actions const *file_actions;
	char const *const *argv;
	char const *const *envp;
	char const *binary;
	linted_ko dirko;
	linted_ko err_writer;
};

static int fork_routine(void *args);

static pid_t safe_vfork(int (*f)(void *), void *args);

static linted_error duplicate_to(linted_ko new, linted_ko old);

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp)
{
	linted_error err;
	struct linted_spawn_attr *attr;

	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *attr);
		if (err != 0)
			return err;
		attr = xx;
	}

	*attrp = attr;
	return 0;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr *attr)
{
	linted_mem_free(attr);
}

linted_error linted_spawn_file_actions_init(
    struct linted_spawn_file_actions **file_actionsp)
{
	linted_error err;
	struct linted_spawn_file_actions *file_actions;

	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *file_actions);
		if (err != 0)
			return err;
		file_actions = xx;
	}

	file_actions->set_stdin = false;
	file_actions->set_stdout = false;
	file_actions->set_stderr = false;

	*file_actionsp = file_actions;
	return 0;
}

void linted_spawn_file_actions_set_stdin(
    struct linted_spawn_file_actions *file_actions, linted_ko newko)
{
	file_actions->new_stdin = newko;
	file_actions->set_stdin = true;
}

void linted_spawn_file_actions_set_stdout(
    struct linted_spawn_file_actions *file_actions, linted_ko newko)
{
	file_actions->new_stdout = newko;
	file_actions->set_stdout = true;
}

void linted_spawn_file_actions_set_stderr(
    struct linted_spawn_file_actions *file_actions, linted_ko newko)
{
	file_actions->new_stderr = newko;
	file_actions->set_stderr = true;
}

void linted_spawn_file_actions_destroy(
    struct linted_spawn_file_actions *file_actions)
{
	linted_mem_free(file_actions);
}

linted_error
linted_spawn(pid_t *childp, linted_ko dirko, char const *binary,
             struct linted_spawn_file_actions const *file_actions,
             struct linted_spawn_attr const *attr,
             char const *const argv[], char const *const envp[])
{
	linted_error err = 0;

	if (LINTED_KO_CWD != dirko && dirko > INT_MAX)
		return EBADF;

	if ('/' == binary[0U])
		dirko = LINTED_KO_CWD;

	sigset_t const *child_mask = 0;

	linted_ko err_reader;
	linted_ko err_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			return err;
		}
		err_reader = xx[0U];
		err_writer = xx[1U];
	}

	/* Greater than standard input, standard output and standard
	 * error */
	unsigned greatest = 4U;

	/* Copy file descriptors in case they get overridden */
	if (file_actions != 0 && err_writer < greatest) {
		int err_writer_copy =
		    fcntl(err_writer, F_DUPFD_CLOEXEC, (long)greatest);
		if (-1 == err_writer_copy) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto close_err_pipes;
		}

		linted_ko_close(err_writer);

		err_writer = err_writer_copy;
	}

	linted_ko dirko_copy;
	bool dirko_copied = false;
	if (LINTED_KO_CWD == dirko) {
		dirko_copy = LINTED_KO_CWD;
	} else if (file_actions != 0 && dirko < greatest) {
		int fd = fcntl(dirko, F_DUPFD_CLOEXEC, (long)greatest);
		if (-1 == fd) {
			err = errno;
			LINTED_ASSUME(err != 0);
			goto close_err_pipes;
		}
		dirko_copy = fd;
		dirko_copied = true;
	} else {
		dirko_copy = dirko;
	}

	pid_t child;
	{
		sigset_t sigset;
		sigfillset(&sigset);

		if (0 == child_mask)
			child_mask = &sigset;

		err = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
		if (err != 0)
			goto close_dirko_copy;

		struct fork_args fork_args = {.sigset = child_mask,
		                              .file_actions =
		                                  file_actions,
		                              .err_writer = err_writer,
		                              .argv = argv,
		                              .envp = envp,
		                              .dirko = dirko_copy,
		                              .binary = binary};
		child = safe_vfork(fork_routine, &fork_args);
		if (-1 == child) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}

		linted_error mask_err =
		    pthread_sigmask(SIG_SETMASK, &sigset, 0);
		if (0 == err)
			err = mask_err;
	}

close_dirko_copy:
	if (dirko_copied)
		linted_ko_close(dirko_copy);

close_err_pipes : {
	linted_error close_err = linted_ko_close(err_writer);
	if (0 == err)
		err = close_err;
}

	if (err != 0)
		goto close_err_reader;

	{
		size_t xx;
		linted_error yy;
		err =
		    linted_io_read_all(err_reader, &xx, &yy, sizeof yy);
		if (err != 0)
			goto close_err_reader;

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy) {
			err = yy;
			LINTED_ASSUME(err != 0);
		}
	}

close_err_reader : {
	linted_error close_err = linted_ko_close(err_reader);
	if (0 == err)
		err = close_err;
}

	if (err != 0)
		return err;

	if (childp != 0)
		*childp = child;

	return 0;
}

LINTED_NO_SANITIZE_ADDRESS static int fork_routine(void *arg)
{
	struct fork_args *args = arg;
	sigset_t const *sigset = args->sigset;
	struct linted_spawn_file_actions const *file_actions =
	    args->file_actions;
	linted_ko err_writer = args->err_writer;
	char const *const *argv = args->argv;
	char const *const *envp = args->envp;
	linted_ko dirko = args->dirko;
	char const *binary = args->binary;

	linted_error err = 0;

	/*
	 * Get rid of signal handlers so that they can't be called
	 * before execve.
	 */

	/* We need to use the direct system call to trample over OS
	 * signals. */
	for (int ii = 1; ii < NSIG; ++ii) {
		/* Uncatchable, avoid Valgrind warnings */
		if (SIGSTOP == ii || SIGKILL == ii)
			continue;

		struct sigaction action;
		if (-1 ==
		    syscall(__NR_rt_sigaction, ii, 0, &action, 8U)) {
			err = errno;
			goto fail;
		}

		if (SIG_IGN == action.sa_handler)
			continue;

		action.sa_handler = SIG_DFL;

		if (-1 ==
		    syscall(__NR_rt_sigaction, ii, &action, 0, 8U)) {
			err = errno;
			goto fail;
		}
	}

	err = pthread_sigmask(SIG_SETMASK, sigset, 0);
	if (err != 0)
		goto fail;

	linted_ko new_stdin;
	linted_ko new_stdout;
	linted_ko new_stderr;
	bool set_stdin = false;
	bool set_stdout = false;
	bool set_stderr = false;

	if (file_actions != 0) {
		set_stdin = file_actions->set_stdin;
		set_stdout = file_actions->set_stderr;
		set_stderr = file_actions->set_stdout;

		if (set_stdin)
			new_stdin = file_actions->new_stdin;

		if (set_stdout)
			new_stdout = file_actions->new_stdout;

		if (set_stderr)
			new_stderr = file_actions->new_stderr;
	}

	if (set_stdin) {
		err = duplicate_to(new_stdin, STDIN_FILENO);
		if (err != 0)
			goto fail;
	}

	if (set_stdout) {
		err = duplicate_to(new_stdout, STDOUT_FILENO);
		if (err != 0)
			goto fail;
	}

	if (set_stderr) {
		err = duplicate_to(new_stderr, STDERR_FILENO);
		if (err != 0)
			goto fail;
	}

	if (0 == envp)
		envp = (char const *const *)environ;

	if (LINTED_KO_CWD == dirko) {
		execve(binary, (char *const *)argv,
		       (char *const *)envp);
	} else {
		syscall(__NR_execveat, dirko, binary,
		        (char *const *)argv, (char *const *)envp, 0);
	}
	err = errno;

fail : {
	linted_error xx = err;
	linted_io_write_all(err_writer, 0, &xx, sizeof xx);
}

	return EXIT_FAILURE;
}

LINTED_NO_SANITIZE_ADDRESS
static linted_error duplicate_to(linted_ko new, linted_ko old)
{
	linted_error err = 0;

	if (new == old) {
		int flags = fcntl(old, F_GETFD);
		if (-1 == flags) {
			err = errno;
			LINTED_ASSUME(err != 0);
			return err;
		}

		if (-1 == fcntl(new, F_SETFD, flags & ~FD_CLOEXEC)) {
			err = errno;
			LINTED_ASSUME(err != 0);
			return err;
		}

		return 0;
	}

	/*
	 * The state of a file descriptor after close gives an EINTR
	 * error is unspecified by POSIX so this function avoids the
	 * problem by simply blocking all signals.
	 */

	sigset_t sigset;

	/* First use the signal set for the full set */
	sigfillset(&sigset);

	/* Then reuse the signal set for the old set */

	err = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
	if (err != 0)
		return err;

	if (-1 == dup2(new, old)) {
		err = errno;
		LINTED_ASSUME(err != 0);
	} else {
		err = 0;
	}

	linted_error mask_err =
	    pthread_sigmask(SIG_SETMASK, &sigset, 0);
	if (0 == err)
		err = mask_err;

	return err;
}

/* Most compilers can't handle the weirdness of vfork so contain it in
 * a safe abstraction.
 */
LINTED_NOINLINE LINTED_NOCLONE LINTED_NO_SANITIZE_ADDRESS static pid_t
safe_vfork(int (*volatile f)(void *), void *volatile arg)
{
	pid_t child = vfork();
	if (0 == child)
		_Exit(f(arg));

	return child;
}
