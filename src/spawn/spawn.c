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
#include <grp.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/filter.h>
#include <linux/seccomp.h>

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

struct mount_args
{
	char *source;
	char *target;
	char *filesystemtype;
	unsigned long mountflags;
	char *data;
	bool mkdir_flag : 1U;
	bool touch_flag : 1U;
};

struct linted_spawn_attr
{
	char const *chrootdir;
	char const *chdir_path;
	size_t mount_args_size;
	struct mount_args *mount_args;
	int clone_flags;
	bool drop_caps : 1U;
	bool no_new_privs : 1U;
};

static void default_signals(linted_ko writer);

static void chroot_process(linted_ko writer, char const *chrootdir,
                           struct mount_args *mount_args,
                           size_t mount_args_size);

static void drop_privileges(linted_ko writer, cap_t caps);

static linted_error my_execveat(int dirfd, char const *filename,
                                char const *const argv[],
                                char const *const envp[]);

static void exit_with_error(linted_ko writer, linted_error errnum);

static void pid_to_str(char *buf, pid_t pid);

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

	attr->clone_flags = 0;
	attr->chrootdir = NULL;
	attr->chdir_path = NULL;
	attr->mount_args_size = 0U;
	attr->mount_args = NULL;
	attr->drop_caps = false;
	attr->no_new_privs = false;

	*attrp = attr;
	return 0;
}

void linted_spawn_attr_destroy(struct linted_spawn_attr *attr)
{
	for (size_t ii = 0U; ii < attr->mount_args_size; ++ii) {
		struct mount_args *mount_arg = &attr->mount_args[ii];
		linted_mem_free(mount_arg->source);
		linted_mem_free(mount_arg->target);
		linted_mem_free(mount_arg->filesystemtype);
		linted_mem_free(mount_arg->data);
	}
	linted_mem_free(attr->mount_args);

	linted_mem_free(attr);
}

void linted_spawn_attr_setnonewprivs(struct linted_spawn_attr *attr, _Bool b)
{
	attr->no_new_privs = b;
}

void linted_spawn_attr_setdropcaps(struct linted_spawn_attr *attr, _Bool b)
{
	attr->drop_caps = b;
}

void linted_spawn_attr_setcloneflags(struct linted_spawn_attr *attr, int flags)
{
	attr->clone_flags = flags;
}

void linted_spawn_attr_setchrootdir(struct linted_spawn_attr *attr,
                                    char const *chrootdir)
{
	attr->chrootdir = chrootdir;
}

void linted_spawn_attr_setchdir(struct linted_spawn_attr *attr, char const *dir)
{
	attr->chdir_path = dir;
}

linted_error linted_spawn_attr_setmount(struct linted_spawn_attr *attr,
                                        char const *source, char const *target,
                                        char const *filesystemtype,
                                        bool mkdir_flag, bool touch_flag,
                                        unsigned long mountflags,
                                        char const *data)
{
	linted_error errnum = 0;
	struct mount_args *mount_args = attr->mount_args;
	size_t size = attr->mount_args_size;

	size_t new_size = size + 1U;
	if (new_size < size)
		return ENOMEM;

	char *source_copy = NULL;
	char *target_copy = NULL;
	char *filesystemtype_copy = NULL;
	char *data_copy = NULL;

	if (source != NULL) {
		source_copy = strdup(source);
		if (NULL == source_copy) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
	}

	if (target != NULL) {
		target_copy = strdup(target);
		if (NULL == target_copy) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_source;
		}
	}

	if (filesystemtype != NULL) {
		filesystemtype_copy = strdup(filesystemtype);
		if (NULL == filesystemtype) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_target;
		}
	}

	if (data != NULL) {
		data_copy = strdup(data);
		if (NULL == data_copy) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_filesystemtype;
		}
	}

	{
		void *xx;
		errnum = linted_mem_realloc_array(&xx, mount_args, new_size,
		                                  sizeof mount_args[0U]);
		if (errnum != 0)
			goto free_data;
		mount_args = xx;
	}

	if (errnum != 0) {
	free_data:
		linted_mem_free(data_copy);

	free_filesystemtype:
		linted_mem_free(filesystemtype_copy);

	free_target:
		linted_mem_free(target_copy);

	free_source:
		linted_mem_free(source_copy);
	} else {
		mount_args[size].source = source_copy;
		mount_args[size].target = target_copy;
		mount_args[size].filesystemtype = filesystemtype_copy;
		mount_args[size].mkdir_flag = mkdir_flag;
		mount_args[size].touch_flag = touch_flag;
		mount_args[size].mountflags = mountflags;
		mount_args[size].data = data_copy;

		attr->mount_args = mount_args;
		attr->mount_args_size = new_size;
	}
	return errnum;
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

/**
 * @bug assert isn't AS-safe.
 */
/* Used ways to sandbox:
 *
 * - CLONE_NEWNS - Coupled with chrooting is good for sandboxing
 *                 files.
 *
 * - CLONE_NEWIPC - Nobody really uses System V IPC objects anymore
 *                  but maybe a few applications on the system have
 *                  some for legacy communication purposes.
 *
 * - CLONE_NEWNET - Prevents processes from connecting to open
 *                  abstract sockets.
 *
 * Unused
 *
 * - CLONE_NEWUTS - Clones the hostname namespace. Pretty useless.
 *
 * - CLONE_NEWUSER - Allows to create one's own users and enable some
 *                   more sandboxes. Otherwise, it is pretty
 *                   useless. Not permitted to use under the existing
 *                   sandbox.
 *
 * - CLONE_NEWPID - Prevents processes from ptracing and signalling
 *                  other processes. Unfortunately, PID 1 can't send
 *                  itself signals so this is unusable for many
 *                  applications.
 *
 */
linted_error linted_spawn(pid_t *childp, int dirfd, char const *filename,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char const *const argv[], char const *const envp[])
{
	linted_error errnum = 0;
	pid_t child = -1;
	bool is_relative_path = filename[0U] != '/';
	bool at_fdcwd = AT_FDCWD == dirfd;

	if (is_relative_path && !at_fdcwd && dirfd < 0)
		return EBADF;

	int clone_flags = 0;
	char const *chrootdir = NULL;
	char const *chdir_path = NULL;
	size_t mount_args_size = 0U;
	struct mount_args *mount_args = NULL;
	bool drop_caps = false;
	bool no_new_privs = false;

	if (attr != NULL) {
		clone_flags = attr->clone_flags;
		chrootdir = attr->chrootdir;
		chdir_path = attr->chdir_path;
		mount_args_size = attr->mount_args_size;
		mount_args = attr->mount_args;
		drop_caps = attr->drop_caps;
		no_new_privs = attr->no_new_privs;
	}

	if (chrootdir != NULL && !((clone_flags & CLONE_NEWNS) != 0))
		return EINVAL;

	/*
	 * So adddup2 works use memory mapping instead of a pipe to
	 * communicate an error.
	 */
	linted_ko reader;
	linted_ko writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK))
			return errno;
		reader = xx[0U];
		writer = xx[1U];
	}

	cap_t caps;
	if (drop_caps) {
		caps = cap_get_proc();
		if (NULL == caps) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto close_pipes;
		}

		if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_caps;
		}

		if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_caps;
		}
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
			goto free_caps;
		envp_copy = xx;
	}

	{
		/*
		 * To save stack space reuse the same sigset for the full set
		 * and
		 * the old set.
		 */
		sigset_t sigset;
		sigfillset(&sigset);

		errnum = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
		if (errnum != 0) {
			goto free_caps;
		}

		child = fork();

		if (-1 == child) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}

		if (0 == child) {
			default_signals(writer);
		}

		linted_error mask_errnum =
		    pthread_sigmask(SIG_SETMASK, &sigset, NULL);
		if (0 == errnum)
			errnum = mask_errnum;
	}

	if (child != 0) {
		linted_mem_free(envp_copy);

	free_caps:
		if (drop_caps)
			cap_free(caps);

	close_pipes : {
		linted_error close_errnum = linted_ko_close(writer);
		if (0 == errnum)
			errnum = close_errnum;
	}

		if (errnum != 0)
			goto close_reader;

		linted_error spawn_error;
		size_t bytes_read;
		{
			size_t xx;
			linted_error yy;
			errnum = linted_io_read_all(reader, &xx, &yy,
			                            sizeof spawn_error);
			if (errnum != 0)
				goto close_reader;
			bytes_read = xx;
			spawn_error = yy;
		}

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (bytes_read == sizeof spawn_error)
			errnum = spawn_error;

	close_reader : {
		linted_error close_errnum = linted_ko_close(reader);
		if (0 == errnum)
			errnum = close_errnum;
	}

		if (errnum != 0)
			return errnum;

		*childp = child;

		return 0;
	}

	if (errnum != 0)
		exit_with_error(writer, errnum);

	linted_ko_close(reader);

	/* Copy file descriptors in case they get overridden */
	if (file_actions != NULL) {
		int greatest_fd = -1;

		union file_action const *actions = file_actions->actions;
		size_t action_count = file_actions->action_count;
		for (size_t ii = 0U; ii < action_count; ++ii) {
			union file_action const *action = &actions[ii];
			switch (action->type) {
			case FILE_ACTION_ADDDUP2: {
				int newfildes = action->adddup2.newfildes;

				if (newfildes > greatest_fd)
					greatest_fd = newfildes;
				break;
			}
			}
		}

		if (!at_fdcwd && is_relative_path) {
			int dirfd_copy =
			    fcntl(dirfd, F_DUPFD, (long)greatest_fd);
			if (-1 == dirfd_copy)
				exit_with_error(writer, errno);

			linted_ko_close(dirfd);

			dirfd = dirfd_copy;
		}

		int writer_copy = fcntl(writer, F_DUPFD, (long)greatest_fd);
		if (-1 == writer_copy)
			exit_with_error(writer, errno);

		linted_ko_close(writer);

		writer = writer_copy;
	}

	if (-1 == unshare(clone_flags))
		exit_with_error(writer, errno);

	if (chrootdir != NULL) {
		chroot_process(writer, chrootdir, mount_args, mount_args_size);
	}

	if (chdir_path != NULL) {
		if (-1 == chdir(chdir_path))
			exit_with_error(writer, errno);
	}

	if (file_actions != NULL) {
		union file_action const *actions = file_actions->actions;
		size_t action_count = file_actions->action_count;
		for (size_t ii = 0U; ii < action_count; ++ii) {
			union file_action const *action = &actions[ii];
			switch (action->type) {
			case FILE_ACTION_ADDDUP2:
				if (-1 == dup2(action->adddup2.oldfildes,
				               action->adddup2.newfildes))
					exit_with_error(writer, errno);
				break;

			default:
				exit_with_error(writer, EINVAL);
			}
		}
	}

	if (drop_caps)
		drop_privileges(writer, caps);

	if (no_new_privs)
		if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1UL, 0UL, 0UL, 0UL))
			exit_with_error(writer, errno);

	char listen_pid[] = "LISTEN_PID=XXXXXXXXXX";
	char listen_fds[] = "LISTEN_FDS=XXXXXXXXXX";

	if (file_actions != NULL && file_actions->action_count > 0U) {
		memcpy(envp_copy, envp, sizeof envp[0U] * env_size);

		envp_copy[env_size] = listen_pid;
		envp_copy[env_size + 1U] = listen_fds;
		envp_copy[env_size + 2U] = NULL;

		pid_to_str(listen_fds + strlen("LISTEN_FDS="),
		           (int)file_actions->action_count - 3U);
		pid_to_str(listen_pid + strlen("LISTEN_PID="), getpid());

		envp = (char const *const *)envp_copy;
	}

	errnum = my_execveat(dirfd, filename, argv, envp);

	exit_with_error(writer, errnum);
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
			if (sigerrnum != EINVAL)
				exit_with_error(writer, sigerrnum);

			/* Skip setting this action */
			continue;
		}

		if (SIG_IGN == action.sa_handler)
			continue;

		action.sa_handler = SIG_DFL;

		if (-1 == sigaction(ii, &action, NULL)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			exit_with_error(writer, errnum);
		}
	}
}

static void chroot_process(linted_ko writer, char const *chrootdir,
                           struct mount_args *mount_args,
                           size_t mount_args_size)
{
	linted_error errnum;

	if (-1 == mount(NULL, chrootdir, "tmpfs", 0, NULL))
		exit_with_error(writer, errno);

	if (-1 == chdir(chrootdir))
		exit_with_error(writer, errno);

	for (size_t ii = 0U; ii < mount_args_size; ++ii) {
		struct mount_args *mount_arg = &mount_args[ii];

		if (mount_arg->mkdir_flag) {
			if (-1 == mkdir(mount_arg->target, S_IRWXU))
				exit_with_error(writer, errno);
		} else if (mount_arg->touch_flag) {
			linted_ko xx;
			errnum =
			    linted_file_create(&xx, AT_FDCWD, mount_arg->target,
			                       LINTED_FILE_EXCL, S_IRWXU);
			if (errnum != 0)
				exit_with_error(writer, errnum);
			linted_ko_close(xx);
		}

		unsigned long mountflags = mount_arg->mountflags;

		if (-1 == mount(mount_arg->source, mount_arg->target,
		                mount_arg->filesystemtype, mountflags,
		                mount_arg->data))
			exit_with_error(writer, errno);

		if ((mountflags & MS_BIND) != 0U) {
			mountflags |= MS_REMOUNT;
			if (-1 == mount(mount_arg->source, mount_arg->target,
			                mount_arg->filesystemtype, mountflags,
			                mount_arg->data))
				exit_with_error(writer, errno);
		}
	}

	/* Magic incantation that clears up /proc/mounts more than
	 * mount MS_MOVE
	 */
	int old_root = open("/", O_DIRECTORY | O_CLOEXEC);
	if (-1 == old_root)
		exit_with_error(writer, errno);

	if (-1 == syscall(__NR_pivot_root, ".", "."))
		exit_with_error(writer, errno);

	/* pivot_root() may or may not affect its current working
	 * directory.  It is therefore recommended to call chdir("/")
	 * immediately after pivot_root().
	 *
	 * - http://man7.org/linux/man-pages/man2/pivot_root.2.html
	 */

	if (-1 == fchdir(old_root))
		exit_with_error(writer, errno);

	errnum = linted_ko_close(old_root);
	if (errnum != 0)
		exit_with_error(writer, errnum);

	if (-1 == umount2(".", MNT_DETACH))
		exit_with_error(writer, errno);

	if (-1 == chdir("/"))
		exit_with_error(writer, errno);
}

static void drop_privileges(linted_ko writer, cap_t caps)
{
	linted_error errnum;

	/* Favor other processes over this process hierarchy.  Only
	 * superuser may lower priorities so this is not
	 * stoppable. This also makes the process hierarchy nicer for
	 * the OOM killer.
	 */
	errno = 0;
	int priority = getpriority(PRIO_PROCESS, 0);
	if (-1 == priority) {
		errnum = errno;
		if (errnum != 0)
			exit_with_error(writer, errnum);
	}

	if (-1 == setpriority(PRIO_PROCESS, 0, priority + 1))
		exit_with_error(writer, errno);

	/* We need to use the raw system call because GLibc messes
	 * around with signals to synchronize the permissions of every
	 * thread. Of course, after a fork there is only one thread
	 * and there is no need for the synchronization.
	 *
	 * See the following for problems related to the nonatomic
	 * setxid calls.
	 *
	 * https://www.redhat.com/archives/libvir-list/2013-November/msg00577.html
	 * https://lists.samba.org/archive/samba-technical/2012-June/085101.html
	 */
	if (-1 == syscall(__NR_setgroups, 0U, NULL))
		exit_with_error(writer, errno);

	/* Drop all capabilities I might possibly have. I'm not sure I
	 * need to do this and I probably can do this in a better
	 * way. Note that currently we do not use PR_SET_KEEPCAPS and
	 * do not map our sandboxed user to root but if we did in the
	 * future we would need this.
	 */
	if (-1 == cap_set_proc(caps))
		exit_with_error(writer, errno);
}

static linted_error my_execveat(int dirfd, char const *filename,
                                char const *const argv[],
                                char const *const envp[])
{
	linted_error errnum;
	bool is_relative_path = filename[0U] != '/';
	bool at_fdcwd = AT_FDCWD == dirfd;
	char *new_path = NULL;

	if (is_relative_path && !at_fdcwd) {
		{
			void *xx;
			errnum = linted_mem_alloc(
			    &xx, strlen("/proc/self/fd/") + 10U +
			             strlen(filename) + 1U);
			if (errnum != 0)
				return errnum;
			new_path = xx;
		}
		sprintf(new_path, "/proc/self/fd/%i/%s", dirfd, filename);
		filename = new_path;
	}

	execve(filename, (char *const *)argv, (char *const *)envp);
	errnum = errno;

	linted_mem_free(new_path);

	return errnum;
}

static void exit_with_error(linted_ko writer, linted_error errnum)
{
	linted_io_write_all(writer, NULL, &errnum, sizeof errnum);
	_Exit(0);
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
