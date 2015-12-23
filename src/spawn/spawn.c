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

#include "lntd/spawn.h"

#include "lntd/execveat.h"
#include "lntd/error.h"
#include "lntd/fifo.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/proc.h"
#include "lntd/prctl.h"
#include "lntd/util.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <syscall.h>
#include <unistd.h>

extern char **environ;

enum { LNTD_SIGNAL_HUP,
       LNTD_SIGNAL_CHLD,
       LNTD_SIGNAL_INT,
       LNTD_SIGNAL_TERM,
       LNTD_SIGNAL_QUIT,
       NUM_SIGS };

static int const signals[NUM_SIGS] = {[LNTD_SIGNAL_HUP] = SIGHUP,
                                      [LNTD_SIGNAL_CHLD] = SIGCHLD,
                                      [LNTD_SIGNAL_INT] = SIGINT,
                                      [LNTD_SIGNAL_QUIT] = SIGQUIT,
                                      [LNTD_SIGNAL_TERM] = SIGTERM};

struct lntd_spawn_file_actions {
	lntd_ko new_stdin;
	lntd_ko new_stdout;
	lntd_ko new_stderr;
	bool set_stdin : 1U;
	bool set_stdout : 1U;
	bool set_stderr : 1U;
};

struct lntd_spawn_attr {
	bool die_on_parent_death : 1U;
};

struct fork_args {
	sigset_t const *sigset;
	struct lntd_spawn_file_actions const *file_actions;
	char const *const *argv;
	char const *const *envp;
	char const *binary;
	lntd_ko dirko;
	lntd_ko err_writer;
	bool die_on_parent_death : 1U;
};

static int fork_routine(void *args);
static pid_t safe_vfork(int (*f)(void *), void *args);
static lntd_error duplicate_to(lntd_ko new, lntd_ko old);

lntd_error lntd_spawn_attr_init(struct lntd_spawn_attr **attrp)
{
	lntd_error err;
	struct lntd_spawn_attr *attr;

	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *attr);
		if (err != 0)
			return err;
		attr = xx;
	}

	attr->die_on_parent_death = false;
	*attrp = attr;
	return 0;
}

void lntd_spawn_attr_destroy(struct lntd_spawn_attr *attr)
{
	lntd_mem_free(attr);
}

void lntd_spawn_attr_set_die_on_parent_death(
    struct lntd_spawn_attr *attrp)
{
	attrp->die_on_parent_death = true;
}

lntd_error lntd_spawn_file_actions_init(
    struct lntd_spawn_file_actions **file_actionsp)
{
	lntd_error err;
	struct lntd_spawn_file_actions *file_actions;

	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *file_actions);
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

void lntd_spawn_file_actions_set_stdin(
    struct lntd_spawn_file_actions *file_actions, lntd_ko newko)
{
	file_actions->new_stdin = newko;
	file_actions->set_stdin = true;
}

void lntd_spawn_file_actions_set_stdout(
    struct lntd_spawn_file_actions *file_actions, lntd_ko newko)
{
	file_actions->new_stdout = newko;
	file_actions->set_stdout = true;
}

void lntd_spawn_file_actions_set_stderr(
    struct lntd_spawn_file_actions *file_actions, lntd_ko newko)
{
	file_actions->new_stderr = newko;
	file_actions->set_stderr = true;
}

void lntd_spawn_file_actions_destroy(
    struct lntd_spawn_file_actions *file_actions)
{
	lntd_mem_free(file_actions);
}

lntd_error
lntd_spawn(lntd_proc *childp, lntd_ko dirko, char const *binary,
           struct lntd_spawn_file_actions const *file_actions,
           struct lntd_spawn_attr const *attr, char const *const argv[],
           char const *const envp[])
{
	lntd_error err = 0;

	if (LNTD_KO_CWD != dirko && dirko > INT_MAX)
		return EBADF;

	if ('/' == binary[0U])
		dirko = LNTD_KO_CWD;

	sigset_t const *child_mask = 0;

	bool die_on_parent_death = false;
	if (attr != 0) {
		die_on_parent_death = attr->die_on_parent_death;
	}

	lntd_ko err_reader;
	lntd_ko err_writer;
	{
		lntd_ko xx;
		lntd_ko yy;
		err = lntd_fifo_pair(&xx, &yy, 0);
		if (err != 0)
			return err;
		err_reader = xx;
		err_writer = yy;
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
			LNTD_ASSUME(err != 0);
			goto close_err_pipes;
		}

		lntd_ko_close(err_writer);

		err_writer = err_writer_copy;
	}

	lntd_ko dirko_copy;
	bool dirko_copied = false;
	if (LNTD_KO_CWD == dirko) {
		dirko_copy = LNTD_KO_CWD;
	} else if (file_actions != 0 && dirko < greatest) {
		int fd = fcntl(dirko, F_DUPFD_CLOEXEC, (long)greatest);
		if (-1 == fd) {
			err = errno;
			LNTD_ASSUME(err != 0);
			goto close_err_pipes;
		}
		dirko_copy = fd;
		dirko_copied = true;
	} else {
		dirko_copy = dirko;
	}

	pid_t child = -1;
	{
		sigset_t sigset;
		sigfillset(&sigset);

		if (0 == child_mask)
			child_mask = &sigset;

		err = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
		if (err != 0)
			goto close_dirko_copy;

		struct fork_args fork_args = {
		    .sigset = child_mask,
		    .file_actions = file_actions,
		    .err_writer = err_writer,
		    .die_on_parent_death = die_on_parent_death,
		    .argv = argv,
		    .envp = envp,
		    .dirko = dirko_copy,
		    .binary = binary};
		child = safe_vfork(fork_routine, &fork_args);
		LNTD_ASSERT(child != 0);

		lntd_error mask_err =
		    pthread_sigmask(SIG_SETMASK, &sigset, 0);
		if (0 == err)
			err = mask_err;
	}

close_dirko_copy:
	if (dirko_copied)
		lntd_ko_close(dirko_copy);

close_err_pipes : {
	lntd_error close_err = lntd_ko_close(err_writer);
	if (0 == err)
		err = close_err;
}

	if (err != 0)
		goto close_err_reader;

	{
		size_t xx;
		lntd_error yy;
		err = lntd_io_read_all(err_reader, &xx, &yy, sizeof yy);
		if (err != 0)
			goto close_err_reader;

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy) {
			err = yy;
			LNTD_ASSUME(err != 0);
		}
	}

close_err_reader : {
	lntd_error close_err = lntd_ko_close(err_reader);
	if (0 == err)
		err = close_err;
}

	if (err != 0)
		return err;

	if (childp != 0)
		*childp = child;

	return 0;
}

LNTD_NO_SANITIZE_ADDRESS static int fork_routine(void *arg)
{
	struct fork_args *args = arg;
	sigset_t const *sigset = args->sigset;
	struct lntd_spawn_file_actions const *file_actions =
	    args->file_actions;
	lntd_ko err_writer = args->err_writer;
	char const *const *argv = args->argv;
	char const *const *envp = args->envp;
	lntd_ko dirko = args->dirko;
	char const *binary = args->binary;
	bool die_on_parent_death = args->die_on_parent_death;

	lntd_error err = 0;

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
	{
		sigset_t set;
		sigemptyset(&set);
		for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(signals);
		     ++ii) {
			sigaddset(&set, signals[ii]);
		}

		err = pthread_sigmask(SIG_UNBLOCK, &set, 0);
		if (err != 0)
			goto fail;
	}

	lntd_ko new_stdin;
	lntd_ko new_stdout;
	lntd_ko new_stderr;
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

	if (die_on_parent_death) {
		err = lntd_prctl_set_death_sig(SIGKILL);
		if (err != 0)
			goto fail;
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

	if (LNTD_KO_CWD == dirko || '/' == binary[0U]) {
		execve(binary, (char *const *)argv,
		       (char *const *)envp);
		err = errno;
	} else {
		err = lntd_execveat(dirko, binary, (char **)argv,
		                    (char **)envp, 0);
	}

fail : {
	lntd_error xx = err;
	lntd_io_write_all(err_writer, 0, &xx, sizeof xx);
}

	return EXIT_FAILURE;
}

LNTD_NO_SANITIZE_ADDRESS
static lntd_error duplicate_to(lntd_ko new, lntd_ko old)
{
	lntd_error err = 0;

	if (new == old) {
		int flags = fcntl(old, F_GETFD);
		if (-1 == flags) {
			err = errno;
			LNTD_ASSUME(err != 0);
			return err;
		}

		if (-1 == fcntl(new, F_SETFD, flags & ~FD_CLOEXEC)) {
			err = errno;
			LNTD_ASSUME(err != 0);
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
		LNTD_ASSUME(err != 0);
	} else {
		err = 0;
	}

	lntd_error mask_err = pthread_sigmask(SIG_SETMASK, &sigset, 0);
	if (0 == err)
		err = mask_err;

	return err;
}

/* Most compilers can't handle the weirdness of vfork so contain it in
 * a safe abstraction.
 */
LNTD_NOINLINE LNTD_NOCLONE LNTD_NO_SANITIZE_ADDRESS static pid_t
safe_vfork(int (*volatile f)(void *), void *volatile arg)
{
	pid_t child = vfork();
	if (0 == child)
		_Exit(f(arg));
	return child;
}
