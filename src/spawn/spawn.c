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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <syscall.h>
#include <unistd.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

struct adddup2
{
	int oldfildes;
	int newfildes;
};

union file_action
{
	struct adddup2 adddup2;
};

struct linted_spawn_file_actions
{
	size_t action_count;
	union file_action actions[];
};

struct linted_spawn_attr
{
	sigset_t const *mask;
};

struct fork_args
{
	sigset_t const *sigset;
	struct linted_spawn_file_actions const *file_actions;
	char const *const *argv;
	char const *const *envp;
	char const *binary;
	linted_ko err_writer;
};

static int fork_routine(void *args);

static pid_t safe_vfork(int (*f)(void *), void *args);

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp)
{
	linted_error errnum;
	struct linted_spawn_attr *attr;

	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *attr);
		if (errnum != 0)
			return errnum;
		attr = xx;
	}

	attr->mask = 0;

	*attrp = attr;
	return 0;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr *attr)
{
	linted_mem_free(attr);
}

void linted_spawn_attr_setmask(struct linted_spawn_attr *attr,
                               sigset_t const *set)
{
	attr->mask = set;
}

linted_error
linted_spawn_file_actions_init(struct linted_spawn_file_actions **file_actionsp)
{
	linted_error errnum;
	struct linted_spawn_file_actions *file_actions;

	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *file_actions);
		if (errnum != 0)
			return errnum;
		file_actions = xx;
	}

	file_actions->action_count = 0U;

	*file_actionsp = file_actions;
	return 0;
}

linted_error linted_spawn_file_actions_adddup2(
    struct linted_spawn_file_actions **file_actionsp, linted_ko oldfildes,
    linted_ko newfildes)
{
	linted_error errnum;
	struct linted_spawn_file_actions *file_actions;
	struct linted_spawn_file_actions *new_file_actions;
	union file_action *new_action;
	size_t old_count;
	size_t new_count;

	file_actions = *file_actionsp;

	old_count = file_actions->action_count;
	new_count = old_count + 1U;

	size_t new_size =
	    sizeof *file_actions + new_count * sizeof file_actions->actions[0U];
	{
		void *xx;
		errnum = linted_mem_realloc(&xx, file_actions, new_size);
		if (errnum != 0)
			return errnum;
		new_file_actions = xx;
	}

	new_file_actions->action_count = new_count;

	new_action = &new_file_actions->actions[old_count];

	new_action->adddup2.oldfildes = oldfildes;
	new_action->adddup2.newfildes = newfildes;

	*file_actionsp = new_file_actions;

	return 0;
}

void linted_spawn_file_actions_destroy(
    struct linted_spawn_file_actions *file_actions)
{
	linted_mem_free(file_actions);
}

linted_error linted_spawn(pid_t *childp, linted_ko dirko, char const *binary,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char const *const argv[], char const *const envp[])
{
	linted_error errnum = 0;

	if (LINTED_KO_CWD != dirko && dirko > INT_MAX)
		return EBADF;

	sigset_t const *child_mask = 0;

	if (attr != 0) {
		child_mask = attr->mask;
	}

	linted_ko err_reader;
	linted_ko err_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		err_reader = xx[0U];
		err_writer = xx[1U];
	}

	int greatest = -1;
	if (file_actions != 0) {
		union file_action const *actions = file_actions->actions;
		size_t action_count = file_actions->action_count;
		for (size_t ii = 0U; ii < action_count; ++ii) {
			union file_action const *action = &actions[ii];

			int newfildes = action->adddup2.newfildes;

			if (newfildes > greatest)
				greatest = newfildes;
		}
	}

	/* Copy file descriptors in case they get overridden */
	if (file_actions != 0) {
		int err_writer_copy =
		    fcntl(err_writer, F_DUPFD_CLOEXEC, (long)greatest);
		if (-1 == err_writer_copy) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto close_err_pipes;
		}

		linted_ko_close(err_writer);

		err_writer = err_writer_copy;
	}

	char *relative_binary_path = 0;
	char const *real_binary_path = binary;
	linted_ko dirko_copy = LINTED_KO_CWD;
	if (binary[0U] != '/' && dirko != LINTED_KO_CWD) {
		if (file_actions != NULL) {
			int fd = fcntl(dirko, F_DUPFD_CLOEXEC, (long)greatest);
			if (-1 == fd) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto close_err_pipes;
			}
			dirko_copy = fd;
		}

		{
			char *xx;
			if (asprintf(&xx, "/proc/self/fd/%i/%s", dirko_copy,
			             binary) < 0) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto close_dirko_copy;
			}
			relative_binary_path = xx;
		}
		real_binary_path = relative_binary_path;
	}

	pid_t child;
	{
		sigset_t sigset;
		sigfillset(&sigset);

		if (0 == child_mask)
			child_mask = &sigset;

		errnum = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
		if (errnum != 0)
			goto free_relative_binary_path;

		struct fork_args fork_args = { .sigset = child_mask,
			                       .file_actions = file_actions,
			                       .err_writer = err_writer,
			                       .argv = argv,
			                       .envp = envp,
			                       .binary = real_binary_path };
		child = safe_vfork(fork_routine, &fork_args);
		if (-1 == child) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}

		linted_error mask_errnum =
		    pthread_sigmask(SIG_SETMASK, &sigset, 0);
		if (0 == errnum)
			errnum = mask_errnum;
	}

free_relative_binary_path:
	linted_mem_free(relative_binary_path);

close_dirko_copy:
	if (dirko_copy != LINTED_KO_CWD)
		linted_ko_close(dirko_copy);

close_err_pipes : {
	linted_error close_errnum = linted_ko_close(err_writer);
	if (0 == errnum)
		errnum = close_errnum;
}

	if (errnum != 0)
		goto close_err_reader;

	{
		size_t xx;
		linted_error yy;
		errnum = linted_io_read_all(err_reader, &xx, &yy, sizeof yy);
		if (errnum != 0)
			goto close_err_reader;

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy) {
			errnum = yy;
			LINTED_ASSUME(errnum != 0);
		}
	}

close_err_reader : {
	linted_error close_errnum = linted_ko_close(err_reader);
	if (0 == errnum)
		errnum = close_errnum;
}

	if (errnum != 0)
		return errnum;

	if (childp != 0)
		*childp = child;

	return 0;
}

__attribute__((no_sanitize_address)) static int fork_routine(void *arg)
{
	struct fork_args *args = arg;
	sigset_t const *sigset = args->sigset;
	struct linted_spawn_file_actions const *file_actions =
	    args->file_actions;
	linted_ko err_writer = args->err_writer;
	char const *const *argv = args->argv;
	char const *const *envp = args->envp;
	char const *binary = args->binary;

	linted_error errnum = 0;

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
		if (-1 == syscall(__NR_rt_sigaction, ii, 0, &action, 8U)) {
			errnum = errno;
			goto fail;
		}

		if (SIG_IGN == action.sa_handler)
			continue;

		action.sa_handler = SIG_DFL;

		if (-1 == syscall(__NR_rt_sigaction, ii, &action, 0, 8U)) {
			errnum = errno;
			goto fail;
		}
	}

	errnum = pthread_sigmask(SIG_SETMASK, sigset, 0);
	if (errnum != 0)
		goto fail;

	if (file_actions != 0) {
		union file_action const *actions = file_actions->actions;
		size_t action_count = file_actions->action_count;
		for (size_t ii = 0U; ii < action_count; ++ii) {
			union file_action const *action = &actions[ii];

			linted_ko oldfd = action->adddup2.oldfildes;
			linted_ko newfd = action->adddup2.newfildes;

			int flags = fcntl(oldfd, F_GETFD);
			if (-1 == flags) {
				errnum = errno;
				goto fail;
			}

			if (-1 == dup2(oldfd, newfd)) {
				errnum = errno;
				goto fail;
			}

			if (-1 == fcntl(newfd, F_SETFD, flags & ~FD_CLOEXEC)) {
				errnum = errno;
				goto fail;
			}
		}
	}

	execve(binary, (char * const *)argv, (char * const *)envp);
	errnum = errno;

fail : {
	linted_error xx = errnum;
	linted_io_write_all(err_writer, 0, &xx, sizeof xx);
}

	return EXIT_FAILURE;
}

/* Most compilers can't handle the weirdness of vfork so contain it in
 * a safe abstraction.
 */
__attribute__((noinline)) __attribute__((noclone))
__attribute__((no_sanitize_address)) static pid_t safe_vfork(
    int (*volatile f)(void *), void *volatile arg)
{
	__atomic_signal_fence(__ATOMIC_SEQ_CST);

	pid_t child = vfork();
	if (0 == child)
		_Exit(f(arg));

	return child;
}
