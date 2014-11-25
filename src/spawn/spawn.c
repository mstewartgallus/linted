/*
 * Copyright 2014 Steven Stewart-Gallus
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
#include "linted/file.h"
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
#include <unistd.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

enum file_action_type { FILE_ACTION_ADDDUP2 };

struct adddup2
{
	enum file_action_type type;
	int oldfildes;
	int newfildes;
};

union file_action
{
	enum file_action_type type;
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

static void default_signals(linted_ko writer);

static void pid_to_str(char *buf, pid_t pid);

static void exit_with_error(linted_ko writer, linted_error errnum);

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

	attr->mask = NULL;

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
    struct linted_spawn_file_actions **file_actionsp, int oldfildes,
    int newfildes)
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

	new_action->type = FILE_ACTION_ADDDUP2;
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

static char const fd_str[] = " /proc/self/fd/";

/**
 * @bug assert isn't AS-safe.
 */
linted_error linted_spawn(pid_t *childp, linted_ko dirfd, char const *filename,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char const *const argv[], char const *const envp[])
{
	linted_error errnum = 0;
	pid_t child = -1;
	bool is_relative_path = filename[0U] != '/';
	bool at_fdcwd = LINTED_KO_CWD == dirfd;

	if (is_relative_path && !at_fdcwd && dirfd < 0)
		return EBADF;

	sigset_t const *child_mask = NULL;

	if (attr != NULL) {
		child_mask = attr->mask;
	}

	size_t fd_len;
	size_t filename_len;
	char *relative_filename = NULL;
	if (is_relative_path && !at_fdcwd) {
		fd_len = strlen(fd_str);
		filename_len = strlen(filename);

		size_t relative_size = fd_len + 10U + filename_len + 1U;

		void *xx;
		errnum = linted_mem_alloc(&xx, relative_size);
		if (errnum != 0)
			return errnum;
		relative_filename = xx;
	}

	char **envp_copy = NULL;
	size_t env_size = 0U;
	for (char const *const *env = envp; *env != NULL; ++env)
		++env_size;

	if (file_actions != NULL && file_actions->action_count > 0U) {
		void *xx;
		errnum =
		    linted_mem_alloc_array(&xx, env_size + 3U, sizeof envp[0U]);
		if (errnum != 0)
			goto free_relative_path;
		envp_copy = xx;
	}

	linted_ko err_reader;
	linted_ko err_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_env;
		}
		err_reader = xx[0U];
		err_writer = xx[1U];
	}

	{
		sigset_t sigset;
		sigfillset(&sigset);

		errnum = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
		if (errnum != 0)
			goto close_err_pipes;

		sigset_t const *sigmask = &sigset;

		child = fork();
		if (-1 == child) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto unmask_signals;
		}
		if (child != 0)
			goto unmask_signals;

		default_signals(err_writer);

		if (child_mask != NULL)
			sigmask = child_mask;

	unmask_signals:
		;
		linted_error mask_errnum =
		    pthread_sigmask(SIG_SETMASK, sigmask, NULL);
		if (0 == errnum)
			errnum = mask_errnum;
	}

	if (child != 0) {
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
			errnum =
			    linted_io_read_all(err_reader, &xx, &yy, sizeof yy);
			if (errnum != 0)
				goto close_err_reader;

			/* If bytes_read is zero then a succesful exec
			 * occured */
			if (xx == sizeof yy)
				errnum = yy;
		}

	close_err_reader : {
		linted_error close_errnum = linted_ko_close(err_reader);
		if (0 == errnum)
			errnum = close_errnum;
	}

	free_env:
		linted_mem_free(envp_copy);

	free_relative_path:
		linted_mem_free(relative_filename);

		if (errnum != 0)
			return errnum;

		if (childp != NULL)
			*childp = child;

		return 0;
	}

	linted_ko_close(err_reader);

	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	/* Copy file descriptors in case they get overridden */
	if (file_actions != NULL) {
		int greatest = -1;

		union file_action const *actions = file_actions->actions;
		size_t action_count = file_actions->action_count;
		for (size_t ii = 0U; ii < action_count; ++ii) {
			union file_action const *action = &actions[ii];
			switch (action->type) {
			case FILE_ACTION_ADDDUP2: {
				int newfildes = action->adddup2.newfildes;

				if (newfildes > greatest)
					greatest = newfildes;
				break;
			}
			}
		}

		if (!at_fdcwd && is_relative_path) {
			int copy =
			    fcntl(dirfd, F_DUPFD_CLOEXEC, (long)greatest);
			if (-1 == copy)
				exit_with_error(err_writer, errno);

			linted_ko_close(dirfd);

			dirfd = copy;
		}

		int err_writer_copy =
		    fcntl(err_writer, F_DUPFD_CLOEXEC, (long)greatest);
		if (-1 == err_writer_copy)
			exit_with_error(err_writer, errno);

		linted_ko_close(err_writer);

		err_writer = err_writer_copy;
	}

	if (file_actions != NULL) {
		union file_action const *actions = file_actions->actions;
		size_t action_count = file_actions->action_count;
		for (size_t ii = 0U; ii < action_count; ++ii) {
			union file_action const *action = &actions[ii];
			switch (action->type) {
			case FILE_ACTION_ADDDUP2: {
				linted_ko oldfd = action->adddup2.oldfildes;
				linted_ko newfd = action->adddup2.newfildes;

				int flags = fcntl(oldfd, F_GETFD);
				if (-1 == flags)
					exit_with_error(err_writer, errno);

				if (-1 == dup2(oldfd, newfd))
					exit_with_error(err_writer, errno);

				if (-1 ==
				    fcntl(newfd, F_SETFD, flags & ~FD_CLOEXEC))
					exit_with_error(err_writer, errno);
				break;
			}

			default:
				exit_with_error(err_writer, EINVAL);
			}
		}
	}

	char listen_pid[] = "LISTEN_PID=" INT_STRING_PADDING;
	char listen_fds[] = "LISTEN_FDS=" INT_STRING_PADDING;

	if (file_actions != NULL && file_actions->action_count > 0U) {
		memcpy(envp_copy, envp, sizeof envp[0U] * env_size);

		pid_to_str(listen_fds + strlen("LISTEN_FDS="),
		           (int)file_actions->action_count - 3U);

		pid_to_str(listen_pid + strlen("LISTEN_PID="), getpid());

		for (size_t ii = 0U; ii < env_size; ++ii) {
			if (0 == strncmp(envp_copy[ii], "LISTEN_PID=",
			                 strlen("LISTEN_PID=")))
				envp_copy[ii] = listen_fds;

			if (0 == strncmp(envp_copy[ii], "LISTEN_FDS=",
			                 strlen("LISTEN_FDS=")))
				envp_copy[ii] = listen_fds;
		}

		envp_copy[env_size] = listen_pid;
		envp_copy[env_size + 1U] = listen_fds;
		envp_copy[env_size + 2U] = NULL;

		envp = (char const * const *)envp_copy;
	}

	if (relative_filename != NULL) {
		memcpy(relative_filename, fd_str, fd_len);

		pid_to_str(relative_filename + fd_len, dirfd);

		size_t fd_and_dir_len = strlen(relative_filename);

		memcpy(relative_filename + fd_and_dir_len, filename,
		       filename_len);

		relative_filename[fd_and_dir_len + filename_len] = '\0';

		filename = relative_filename;
	}

	execve(filename, (char * const *)argv, (char * const *)envp);
	exit_with_error(err_writer, errno);
	/* Impossible */
	return 0;
}

static void default_signals(linted_ko writer)
{
	linted_error errnum;

	/*
	 * Get rid of signal handlers so that they can't be called
	 * before execve.
	 */
	for (int ii = 1; ii < NSIG; ++ii) {
		/* Uncatchable, avoid Valgrind warnings */
		if (SIGSTOP == ii || SIGKILL == ii)
			continue;

		struct sigaction action;
		if (-1 == sigaction(ii, NULL, &action)) {
			linted_error sigerrnum = errno;
			LINTED_ASSUME(sigerrnum != 0);

			/* If sigerrnum == EINVAL then we are
			 * trampling on OS signals.
			 */
			if (EINVAL == sigerrnum)
				continue;

			errnum = sigerrnum;
			goto sigaction_fail_errnum;
		}

		if (SIG_IGN == action.sa_handler)
			continue;

		action.sa_handler = SIG_DFL;

		if (-1 == sigaction(ii, &action, NULL)) {
			linted_error sigerrnum = errno;

			/* Workaround broken debugging utilities */
			if (EINVAL == sigerrnum)
				continue;

			errnum = sigerrnum;
			goto sigaction_failed;
		}
	}

	return;

sigaction_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);

sigaction_fail_errnum:
	exit_with_error(writer, errnum);
}

static void exit_with_error(linted_ko writer, linted_error errnum)
{
	linted_io_write_all(writer, NULL, &errnum, sizeof errnum);
	_Exit(EXIT_SUCCESS);
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
