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
#include <mntent.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
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
	sigset_t const *mask;
	char const *chrootdir;
	char const *fstab;
	int clone_flags;
	bool deparent : 1U;
};

static void default_signals(linted_ko writer);
static void set_id_maps(linted_ko writer, uid_t mapped_uid, uid_t uid,
                        gid_t mapped_gid, gid_t gid);
static void chroot_process(linted_ko writer, char const *chrootdir,
                           struct mount_args *mount_args,
                           size_t mount_args_size);

static void pid_to_str(char *buf, pid_t pid);

static void exit_with_error(linted_ko writer, linted_error errnum);

static linted_error parse_fstab(struct mount_args **mount_argsp, size_t *sizep,
                                linted_ko cwd, char const *fstab_path);
static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);
static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type);

static pid_t real_getpid(void);

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
	attr->mask = NULL;
	attr->chrootdir = NULL;
	attr->fstab = NULL;
	attr->deparent = false;

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

void linted_spawn_attr_setdeparent(struct linted_spawn_attr *attr, bool val)
{
	attr->deparent = val;
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

void linted_spawn_attr_setfstab(struct linted_spawn_attr *attr,
                                char const *fstab)
{
	attr->fstab = fstab;
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

	int clone_flags = 0;
	char const *chrootdir = NULL;
	char const *fstab = NULL;
	bool deparent = false;
	sigset_t const *child_mask = NULL;

	if (attr != NULL) {
		clone_flags = attr->clone_flags;
		chrootdir = attr->chrootdir;
		fstab = attr->fstab;
		deparent = attr->deparent;
		child_mask = attr->mask;
	}

	if (!((clone_flags & CLONE_NEWUSER) != 0) &&
	    (chrootdir != NULL || fstab != NULL))
		return EINVAL;

	if (chrootdir != NULL && !((clone_flags & CLONE_NEWNS) != 0))
		return EINVAL;

	gid_t gid = getgid();
	uid_t uid = getuid();

	gid_t mapped_gid = gid;
	uid_t mapped_uid = uid;

	struct mount_args *mount_args = NULL;
	size_t mount_args_size = 0U;

	if (fstab != NULL) {
		struct mount_args *xx;
		size_t yy;
		errnum = parse_fstab(&xx, &yy, dirfd, fstab);
		if (errnum != 0)
			return errnum;
		mount_args = xx;
		mount_args_size = yy;
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
			goto free_mount_args;
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

		if (deparent) {
			child = fork();
		} else {
			child =
			    syscall(__NR_clone, SIGCHLD | clone_flags, NULL);
		}
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

	free_mount_args:
		for (size_t ii = 0U; ii < mount_args_size; ++ii) {
			struct mount_args *mount_arg = &mount_args[ii];
			linted_mem_free(mount_arg->source);
			linted_mem_free(mount_arg->target);
			linted_mem_free(mount_arg->filesystemtype);
			linted_mem_free(mount_arg->data);
		}
		linted_mem_free(mount_args);

		if (errnum != 0)
			return errnum;

		if (childp != NULL)
			*childp = child;

		return 0;
	}

	linted_ko_close(err_reader);

	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	if (deparent) {
		child = syscall(__NR_clone, SIGCHLD | clone_flags, NULL);
		if (-1 == child)
			exit_with_error(err_writer, errno);

		if (child != 0)
			_Exit(0);
	}

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

	if ((clone_flags & CLONE_NEWUSER) != 0) {
		set_id_maps(err_writer, mapped_uid, uid, mapped_gid, gid);

		/* We need to use the raw system call because GLibc
		 * messes around with signals to synchronize the
		 * permissions of every thread. Of course, after a
		 * fork there is only one thread and there is no need
		 * for the synchronization.
		 *
		 * See the following for problems related to the nonatomic
		 * setxid calls.
		 *
		 * https://www.redhat.com/archives/libvir-list/2013-November/msg00577.html
		 * https://lists.samba.org/archive/samba-technical/2012-June/085101.html
		 */
		if (-1 == syscall(__NR_setgroups, 0U, NULL))
			exit_with_error(err_writer, errno);
	}

	if (chrootdir != NULL)
		chroot_process(err_writer, chrootdir, mount_args,
		               mount_args_size);

	char listen_pid[] = "LISTEN_PID=" INT_STRING_PADDING;
	char listen_fds[] = "LISTEN_FDS=" INT_STRING_PADDING;

	if (file_actions != NULL && file_actions->action_count > 0U) {
		memcpy(envp_copy, envp, sizeof envp[0U] * env_size);

		pid_to_str(listen_fds + strlen("LISTEN_FDS="),
		           (int)file_actions->action_count - 3U);

		pid_to_str(listen_pid + strlen("LISTEN_PID="), real_getpid());

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

static void set_id_maps(linted_ko writer, uid_t mapped_uid, uid_t uid,
                        gid_t mapped_gid, gid_t gid)
{
	linted_error errnum;

	/**
	 * @todo write to the uid and gid maps in an async-signal-safe
	 *       way.
	 */
	/* Note that writing to uid_map and gid_map will fail if the
	 * binary is not dumpable.  DON'T set the process dumpable and
	 * fail if the process is nondumpable as presumably the
	 * invoker of the process had good reasons to have the process
	 * nondumpable.
	 */
	{
		linted_ko file;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/uid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0)
				exit_with_error(writer, errnum);
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_uid, uid);
		if (errnum != 0)
			exit_with_error(writer, errnum);

		errnum = linted_ko_close(file);
		if (errnum != 0)
			exit_with_error(writer, errnum);
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/gid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0)
				exit_with_error(writer, errnum);
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_gid, gid);
		if (errnum != 0)
			exit_with_error(writer, errnum);

		errnum = linted_ko_close(file);
		if (errnum != 0)
			exit_with_error(writer, errnum);
	}
}

static void chroot_process(linted_ko writer, char const *chrootdir,
                           struct mount_args *mount_args,
                           size_t mount_args_size)
{
	linted_error errnum;

	if (-1 == mount(NULL, chrootdir, "tmpfs", 0, "mode=700"))
		exit_with_error(writer, errno);

	if (-1 == chdir(chrootdir))
		exit_with_error(writer, errno);

	for (size_t ii = 0U; ii < mount_args_size; ++ii) {
		struct mount_args *mount_arg = &mount_args[ii];

		if (mount_arg->mkdir_flag) {
			if (-1 == mkdir(mount_arg->target, S_IRWXU))
				exit_with_error(writer, errno);
		} else if (mount_arg->touch_flag) {
			if (-1 ==
			    mknod(mount_arg->target, S_IRWXU | S_IFREG, 0))
				exit_with_error(writer, errno);
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

static linted_error parse_fstab(struct mount_args **mount_argsp, size_t *sizep,
                                linted_ko cwd, char const *fstab_path)
{
	linted_error errnum = 0;

	struct mount_args *mount_args = NULL;
	size_t size = 0U;

	FILE *fstab;
	{
		FILE *xx;
		errnum = my_setmntentat(&xx, cwd, fstab_path, "re");
		if (errnum != 0)
			return errnum;
		fstab = xx;
	}

	for (;;) {
		errno = 0;
		struct mntent *entry = getmntent(fstab);
		if (NULL == entry) {
			errnum = errno;
			if (errnum != 0)
				goto close_file;
			break;
		}

		char const *fsname = entry->mnt_fsname;
		char const *dir = entry->mnt_dir;
		char const *type = entry->mnt_type;
		char const *opts = entry->mnt_opts;

		if (0 == strcmp("none", fsname))
			fsname = NULL;

		if (0 == strcmp("none", opts))
			opts = NULL;

		bool mkdir_flag = false;
		bool touch_flag = false;
		unsigned long mountflags = 0U;
		char const *data = NULL;
		if (opts != NULL) {
			bool xx;
			bool yy;
			unsigned long zz;
			char const *ww;
			errnum = parse_mount_opts(opts, &xx, &yy, &zz, &ww);
			if (errnum != 0)
				goto close_file;
			mkdir_flag = xx;
			touch_flag = yy;
			mountflags = zz;
			data = ww;
		}

		char *source_copy = NULL;
		char *target_copy = NULL;
		char *filesystemtype_copy = NULL;
		char *data_copy = NULL;

		if (fsname != NULL) {
			source_copy = strdup(fsname);
			if (NULL == source_copy) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto close_file;
			}
		}

		if (dir != NULL) {
			target_copy = strdup(dir);
			if (NULL == target_copy) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_source;
			}
		}

		if (type != NULL) {
			filesystemtype_copy = strdup(type);
			if (NULL == filesystemtype_copy) {
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

		size_t old_size = size;
		size_t new_size = size + 1U;
		{
			void *xx;
			errnum = linted_mem_realloc_array(
			    &xx, mount_args, new_size, sizeof mount_args[0U]);
			if (errnum != 0)
				goto free_data;
			mount_args = xx;
			size = new_size;
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

			goto close_file;
		}

		mount_args[old_size].source = source_copy;
		mount_args[old_size].target = target_copy;
		mount_args[old_size].filesystemtype = filesystemtype_copy;
		mount_args[old_size].mkdir_flag = mkdir_flag;
		mount_args[old_size].touch_flag = touch_flag;
		mount_args[old_size].mountflags = mountflags;
		mount_args[old_size].data = data_copy;
	}

close_file:
	if (endmntent(fstab) != 1 && 0 == errnum) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (errnum != 0) {
		for (size_t ii = 0U; ii < size; ++ii) {
			struct mount_args *mount_arg = &mount_args[ii];
			linted_mem_free(mount_arg->source);
			linted_mem_free(mount_arg->target);
			linted_mem_free(mount_arg->filesystemtype);
			linted_mem_free(mount_arg->data);
		}
		linted_mem_free(mount_args);

		return errnum;
	}

	*mount_argsp = mount_args;
	*sizep = size;

	return 0;
}

enum { MKDIR, TOUCH, BIND, RBIND, RO, RW, SUID, NOSUID, NODEV, NOEXEC };

static char const *const mount_options[] = {[MKDIR] = "mkdir",        /*  */
	                                    [TOUCH] = "touch",        /*  */
	                                    [BIND] = "bind",          /*  */
	                                    [RBIND] = "rbind",        /*  */
	                                    [RO] = MNTOPT_RO,         /*  */
	                                    [RW] = MNTOPT_RW,         /*  */
	                                    [SUID] = MNTOPT_SUID,     /*  */
	                                    [NOSUID] = MNTOPT_NOSUID, /*  */
	                                    [NODEV] = "nodev",        /*  */
	                                    [NOEXEC] = "noexec",      /*  */
	                                    NULL };

static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp)
{
	linted_error errnum;

	bool touch_flag = false;
	bool mkdir_flag = false;
	bool bind = false;
	bool rec = false;
	bool readonly = false;
	bool readwrite = false;
	bool suid = true;
	bool dev = true;
	bool exec = true;
	char *leftovers = NULL;

	char *subopts_str = strdup(opts);
	if (NULL == subopts_str) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	char *subopts = subopts_str;
	char *value = NULL;

	while (*subopts != '\0') {
		int token;
		{
			char *xx = subopts;
			char *yy = value;
			token =
			    getsubopt(&xx, (char * const *)mount_options, &yy);
			subopts = xx;
			value = yy;
		}
		switch (token) {
		case MKDIR:
			mkdir_flag = true;
			break;

		case TOUCH:
			touch_flag = true;
			break;

		case BIND:
			bind = true;
			break;

		case RBIND:
			bind = true;
			rec = true;
			break;

		case RO:
			readonly = true;
			break;

		case RW:
			readwrite = true;
			break;

		case SUID:
			suid = true;
			break;

		case NOSUID:
			suid = false;
			break;

		case NODEV:
			dev = false;
			break;

		case NOEXEC:
			exec = false;
			break;

		default:
			leftovers = strstr(opts, value);
			goto free_subopts_str;
		}
	}

free_subopts_str:
	linted_mem_free(subopts_str);

	if (readwrite && readonly)
		return EINVAL;

	if (bind && rec && readonly)
		/*
		 * Due to a completely idiotic kernel bug (see
		 * https://bugzilla.kernel.org/show_bug.cgi?id=24912) using a
		 * recursive bind mount as readonly would fail completely
		 * silently and there is no way to workaround this.
		 *
		 * Even after working around by remounting it will fail for
		 * the recursive case. For example, /home directory that is
		 * recursively bind mounted as readonly and that has encrypted
		 * user directories as an example. The /home directory will be
		 * readonly but the user directory /home/user will not be.
		 */
		return EINVAL;

	if (mkdir_flag && touch_flag)
		return EINVAL;

	unsigned long mountflags = 0U;

	if (bind)
		mountflags |= MS_BIND;

	if (rec)
		mountflags |= MS_REC;

	if (readonly)
		mountflags |= MS_RDONLY;

	if (!suid)
		mountflags |= MS_NOSUID;

	if (!dev)
		mountflags |= MS_NODEV;

	if (!exec)
		mountflags |= MS_NOEXEC;

	*leftoversp = leftovers;
	*mkdir_flagp = mkdir_flag;
	*touch_flagp = touch_flag;
	*mountflagsp = mountflags;
	return 0;
}

static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type)
{
	linted_error errnum;

	char const *abspath;
	if (filename[0U] != '/') {
		{
			char *xx;
			if (-1 ==
			    asprintf(&xx, "/proc/self/fd/%i/%s", cwd, filename))
				goto asprintf_failed;
			abspath = xx;
			goto asprintf_succeeded;
		}
	asprintf_failed:
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	asprintf_succeeded:
		;
	} else {
		abspath = filename;
	}

	FILE *file = setmntent(abspath, type);
	errnum = errno;

	if (abspath != filename)
		linted_mem_free((char *)abspath);

	if (NULL == file) {
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*filep = file;
	return 0;
}

static pid_t real_getpid(void)
{
	return syscall(__NR_getpid);
}
