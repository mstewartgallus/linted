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
#include "linted/fifo.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <syscall.h>
#include <sys/mman.h>
#include <unistd.h>

#ifndef __NR_execveat
#if defined __amd64__
#define __NR_execveat 322
#elif defined __i386__
#define __NR_execveat 358
#else
#error No execveat system call number is defined for this platform
#endif
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
	long clone_flags;
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
	attr->clone_flags = 0;

	*attrp = attr;
	return 0;
}

void linted_spawn_attr_set_untraced(struct linted_spawn_attr *attr)
{
	attr->clone_flags |= CLONE_UNTRACED;
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
linted_spawn(linted_pid *childp, linted_ko dirko, char const *binary,
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

	long maybe_page_size = sysconf(_SC_PAGE_SIZE);
	assert(maybe_page_size >= 0);

	long maybe_stack_min_size = sysconf(_SC_THREAD_STACK_MIN);
	assert(maybe_stack_min_size >= 0);

	size_t page_size = maybe_page_size;
	size_t stack_min_size = maybe_stack_min_size;

	size_t stack_size = 8U * page_size + stack_min_size;

	size_t stack_plus_guard_size = stack_size + 2U * page_size;

	char *child_stack = mmap(
	    0, stack_plus_guard_size, PROT_READ | PROT_WRITE,
	    MAP_ANONYMOUS | MAP_PRIVATE | MAP_GROWSDOWN | MAP_STACK, -1,
	    0);
	if (MAP_FAILED == child_stack) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	pid_t child = -1;
	if (-1 == mprotect(child_stack, page_size, PROT_NONE)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto unmap_stack;
	}

	if (-1 == mprotect(child_stack + page_size + stack_size,
	                   page_size, PROT_NONE)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto unmap_stack;
	}

	linted_ko err_reader;
	linted_ko err_writer;
	{
		linted_ko xx;
		linted_ko yy;
		err = linted_fifo_pair(&xx, &yy, 0);
		if (err != 0)
			goto unmap_stack;
		err_reader = xx;
		err_writer = yy;
	}

	/* Greater than standard input, standard output and standard
	 * error */
	unsigned greatest = 4U;

	long clone_flags = 0;
	if (attr != 0) {
		clone_flags = attr->clone_flags;
	}

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

		child = clone(fork_routine,
		              child_stack + page_size + stack_size,
		              SIGCHLD | clone_flags, &fork_args);
		assert(child != 0);

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

unmap_stack:
	munmap(child_stack, stack_plus_guard_size);

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
