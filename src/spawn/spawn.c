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
#include <sys/prctl.h>
#include <sys/ptrace.h>
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
	unsigned long ptrace_options;
	bool ptracing : 1U;
};

static void do_vfork(sigset_t const *sigset,
                     struct linted_spawn_file_actions const *file_actions,
                     pid_t parent, linted_ko err_reader, linted_ko err_writer,
                     bool ptracing, char const *const *argv,
                     char const *const *envp, char const *listen_fds,
                     char *listen_pid, char const *filename);

static linted_error default_signals(void);

static void pid_to_str(char *buf, pid_t pid);

static linted_error set_ptracer(pid_t tracer);
static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);

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
	attr->ptrace_options = 0;
	attr->ptracing = false;

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

void linted_spawn_attr_setptrace(struct linted_spawn_attr *attr,
                                 unsigned long options)
{
	attr->ptracing = true;
	attr->ptrace_options = options;
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

linted_error linted_spawn(pid_t *childp, linted_ko dirko, char const *filename,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char const *const argv[], char const *const envp[])
{
	linted_error errnum = 0;
	bool is_relative_path = filename[0U] != '/';
	bool at_fdcwd = LINTED_KO_CWD == dirko;

	if (is_relative_path && !at_fdcwd && dirko < 0)
		return EBADF;

	sigset_t const *child_mask = NULL;
	bool ptracing = false;
	unsigned long ptrace_options = 0UL;

	if (attr != NULL) {
		child_mask = attr->mask;
		ptracing = attr->ptracing;
		ptrace_options = attr->ptrace_options;
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

	char const **envp_copy = NULL;
	size_t env_size = 0U;
	for (char const *const *env = envp; *env != NULL; ++env)
		++env_size;

	char listen_pid[] = "LISTEN_PID=" INT_STRING_PADDING;
	char listen_fds[] = "LISTEN_FDS=" INT_STRING_PADDING;

	if (file_actions != NULL && file_actions->action_count > 0U) {
		{
			void *xx;
			errnum = linted_mem_alloc_array(&xx, env_size + 3U,
			                                sizeof envp[0U]);
			if (errnum != 0)
				goto free_relative_path;
			envp_copy = xx;
		}

		for (size_t ii = 0U; ii < env_size; ++ii)
			envp_copy[ii] = envp[ii];

		envp_copy[env_size] = listen_pid;
		envp_copy[env_size + 1U] = listen_fds;
		envp_copy[env_size + 2U] = NULL;

		for (size_t ii = 0U; ii < env_size; ++ii) {
			if (0 == strncmp(envp_copy[ii], "LISTEN_PID=",
			                 strlen("LISTEN_PID=")))
				envp_copy[ii] = listen_fds;

			if (0 == strncmp(envp_copy[ii], "LISTEN_FDS=",
			                 strlen("LISTEN_FDS=")))
				envp_copy[ii] = listen_fds;
		}

		pid_to_str(listen_fds + strlen("LISTEN_FDS="),
		           (int)file_actions->action_count - 3U);

		envp = envp_copy;
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

	int greatest = -1;
	if (file_actions != NULL) {
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
	}

	linted_ko dirko_copy;

	/* Copy file descriptors in case they get overridden */
	if (file_actions != NULL) {
		if (!at_fdcwd && is_relative_path) {
			dirko_copy =
			    fcntl(dirko, F_DUPFD_CLOEXEC, (long)greatest);
			if (-1 == dirko_copy)
				goto close_err_pipes;
		}

		int err_writer_copy =
		    fcntl(err_writer, F_DUPFD_CLOEXEC, (long)greatest);
		if (-1 == err_writer_copy)
			goto close_dirko_copy;

		linted_ko_close(err_writer);

		err_writer = err_writer_copy;
	}

	if (relative_filename != NULL) {
		memcpy(relative_filename, fd_str, fd_len);

		pid_to_str(relative_filename + fd_len, dirko_copy);

		size_t fd_and_dir_len = strlen(relative_filename);

		memcpy(relative_filename + fd_and_dir_len, filename,
		       filename_len);

		relative_filename[fd_and_dir_len + filename_len] = '\0';

		filename = relative_filename;
	}

	pid_t parent = getpid();

	sigset_t sigset;
	sigfillset(&sigset);

	if (NULL == child_mask)
		child_mask = &sigset;

	errnum = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
	if (errnum != 0)
		goto close_err_pipes;

	pid_t const child = vfork();
	if (0 == child)
		do_vfork(child_mask, file_actions, parent, err_reader,
		         err_writer, ptracing, argv, envp, listen_fds,
		         listen_pid, filename);
	if (-1 == child) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	{
		linted_error mask_errnum =
		    pthread_sigmask(SIG_SETMASK, &sigset, NULL);
		if (0 == errnum)
			errnum = mask_errnum;
	}

close_dirko_copy:
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

	if (ptracing) {
		errnum = ptrace_seize(child, ptrace_options);
		if (errnum != 0)
			return errnum;
	}

	if (childp != NULL)
		*childp = child;

	return 0;
}

static void do_vfork(sigset_t const *sigset,
                     struct linted_spawn_file_actions const *file_actions,
                     pid_t parent, linted_ko err_reader, linted_ko err_writer,
                     bool ptracing, char const *const *argv,
                     char const *const *envp, char const *listen_fds,
                     char *listen_pid, char const *filename)
{
	linted_error errnum = 0;

	errnum = default_signals();
	if (errnum != 0)
		goto fail;

	errnum = pthread_sigmask(SIG_SETMASK, sigset, NULL);
	if (errnum != 0)
		goto fail;

	linted_ko_close(err_reader);

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
					goto fail;

				if (-1 == dup2(oldfd, newfd))
					goto fail;

				if (-1 ==
				    fcntl(newfd, F_SETFD, flags & ~FD_CLOEXEC))
					goto fail;
				break;
			}

			default:
				goto fail;
			}
		}
	}

	if (ptracing) {
		errnum = set_ptracer(parent);
		if (errnum != 0)
			goto fail;
	}

	/* This is the ONE write to memory done. */
	if (file_actions != NULL && file_actions->action_count > 0U)
		pid_to_str(listen_pid + strlen("LISTEN_PID="), getpid());

	execve(filename, (char * const *)argv, (char * const *)envp);
	errnum = errno;

fail : {
	linted_error xx = errnum;
	linted_io_write_all(err_writer, NULL, &xx, sizeof xx);
	_Exit(EXIT_SUCCESS);
}
	/* Impossible */
}

static linted_error default_signals(void)
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

	return 0;

sigaction_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);

sigaction_fail_errnum:
	return errnum;
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

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_SEIZE, pid, (void *)NULL, (void *)options)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error set_ptracer(pid_t tracer)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_PTRACER, (unsigned long)tracer, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}
