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

enum file_action_type {
	FILE_ACTION_ADDDUP2
};

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
	size_t mount_args_size;
	struct mount_args *mount_args;
	bool drop_caps : 1U;
};

struct spawn_error
{
	linted_error errnum;
};

static struct sock_fprog const ban_forks_filter;

static linted_error my_execveat(int dirfd, const char *filename,
                                char *const argv[], char *const envp[]);
static void exit_with_error(linted_ko writer, linted_error errnum);

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp)
{
	linted_error errnum;
	struct linted_spawn_attr *attr;

	{
		void *xx;
		if ((errnum = linted_mem_alloc(&xx, sizeof *attr)) != 0) {
			return errnum;
		}
		attr = xx;
	}

	attr->chrootdir = NULL;
	attr->mount_args_size = 0U;
	attr->mount_args = NULL;
	attr->drop_caps = false;

	*attrp = attr;
	return 0;
}

void linted_spawn_attr_drop_caps(struct linted_spawn_attr *attr)
{
	attr->drop_caps = true;
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

void linted_spawn_attr_setchrootdir(struct linted_spawn_attr *attr,
                                    char const *chrootdir)
{
	attr->chrootdir = chrootdir;
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
	if (new_size < size) {
		return ENOMEM;
	}
	size = new_size;

	char *source_copy;
	char *target_copy;
	char *filesystemtype_copy;
	char *data_copy;

	if (NULL == source) {
		source_copy = NULL;
	} else {
		source_copy = strdup(source);
		if (NULL == source_copy) {
			return errno;
		}
	}

	if (NULL == target) {
		target_copy = NULL;
	} else {
		target_copy = strdup(target);
		if (NULL == target_copy) {
			goto free_source;
		}
	}

	if (NULL == filesystemtype) {
		filesystemtype_copy = NULL;
	} else {
		filesystemtype_copy = strdup(filesystemtype);
		if (NULL == filesystemtype) {
			goto free_target;
		}
	}

	if (NULL == data) {
		data_copy = NULL;
	} else {
		data_copy = strdup(data);
		if (NULL == data_copy) {
			goto free_filesystemtype;
		}
	}

	{
		void *xx;
		if ((errnum = linted_mem_realloc_array(
		         &xx, mount_args, size, sizeof mount_args[0U])) != 0) {
			goto free_data;
		}
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
		mount_args[size - 1U].source = source_copy;
		mount_args[size - 1U].target = target_copy;
		mount_args[size - 1U].filesystemtype = filesystemtype_copy;
		mount_args[size - 1U].mkdir_flag = mkdir_flag;
		mount_args[size - 1U].touch_flag = touch_flag;
		mount_args[size - 1U].mountflags = mountflags;
		mount_args[size - 1U].data = data_copy;

		attr->mount_args = mount_args;
		attr->mount_args_size = size;
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
		if ((errnum = linted_mem_alloc(&xx, sizeof *file_actions)) !=
		    0) {
			return errnum;
		}
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
	{
		void *xx;
		if ((errnum = linted_mem_realloc(
		         &xx, file_actions,
		         sizeof *file_actions +
		             new_count * sizeof file_actions->actions[0U])) !=
		    0) {
			return errnum;
		}
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

linted_error linted_spawn(pid_t *childp, int dirfd, char const *filename,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char *const argv[], char *const input_envp[])
{
	linted_error errnum = 0;
	bool is_relative_path = filename[0U] != '/';
	bool at_fdcwd = AT_FDCWD == dirfd;
	if (is_relative_path && !at_fdcwd && dirfd < 0) {
		return EBADF;
	}

	/*
	 * So adddup2 works use memory mapping instead of a pipe to
	 * communicate an error.
	 */
	linted_ko reader;
	linted_ko writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			return errno;
		}
		reader = xx[0U];
		writer = xx[1U];
	}

	size_t count = 0U;
	for (char *const *env = input_envp; *env != NULL; ++env) {
		++count;
	}

	char listen_pid[] = "LISTEN_PID=XXXXXXXXXX";
	char listen_fds[] = "LISTEN_FDS=XXXXXXXXXX";

	char **envp;

	if (file_actions != NULL && file_actions->action_count > 0U) {
		{
			void *xx;
			if ((errnum = linted_mem_alloc_array(
			         &xx, count + 3U, sizeof input_envp[0U])) !=
			    0) {
				goto close_pipes;
			}
			envp = xx;
		}

		for (size_t ii = 0U; ii < count; ++ii) {
			envp[ii] = input_envp[ii];
		}

		envp[count] = listen_pid;
		envp[count + 1U] = listen_fds;
		envp[count + 2U] = NULL;
		sprintf(listen_fds, "LISTEN_FDS=%i",
		        (int)file_actions->action_count - 3U);
	} else {
		envp = (char **)input_envp;
	}

	pid_t child;
	{
		/*
		 * To save stack space reuse the same sigset for the full set
		 * and
		 * the old set.
		 */
		sigset_t sigset;
		sigfillset(&sigset);

		pthread_sigmask(SIG_BLOCK, &sigset, &sigset);

		child = fork();

		if (-1 == child) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}

		if (0 == child) {
			/*
			 * Get rid of signal handlers so that they can't be
			 * called before
			 * execve.
			 */
			for (int ii = 1; ii < NSIG; ++ii) {
				/* Uncatchable, avoid Valgrind warnings */
				if (SIGSTOP == ii || SIGKILL == ii) {
					continue;
				}

				struct sigaction action;
				sigaction(ii, NULL, &action);

				if (action.sa_handler != SIG_IGN) {
					action.sa_handler = SIG_DFL;

					sigaction(ii, &action, NULL);
				}
			}
		}

		pthread_sigmask(SIG_SETMASK, &sigset, NULL);
	}

	if (child != 0) {
		if (envp != input_envp) {
			linted_mem_free(envp);
		}

	close_pipes : {
		linted_error close_errnum = linted_ko_close(writer);
		if (0 == errnum) {
			errnum = close_errnum;
		}
	}

		{
			linted_error spawn_error;
			size_t bytes_wrote;
			linted_error read_errnum = linted_io_read_all(
			    reader, &bytes_wrote, &spawn_error,
			    sizeof spawn_error);
			if (0 == errnum) {
				errnum = read_errnum;
			}

			if (0 == errnum) {
				/* If bytes_wrote is zero then a succesful exec
				 * occured */
				if (bytes_wrote == sizeof spawn_error) {
					errnum = spawn_error;
				}
			}
		}

		{
			linted_error close_errnum = linted_ko_close(reader);
			if (0 == errnum) {
				errnum = close_errnum;
			}
		}

		if (0 == errnum) {
			*childp = child;
		}
		return errnum;
	}

	if (file_actions != NULL && file_actions->action_count > 0U) {
		sprintf(listen_pid, "LISTEN_PID=%i", getpid());
	}

	linted_ko_close(reader);

	if (attr != NULL) {
		if (attr->chrootdir != NULL) {
			/* Used ways to sandbox:
			 *
			 * - CLONE_NEWNS - Coupled with chrooting is good for
			 *                 sandboxing files.
			 *
			 * - CLONE_NEWIPC - Nobody really uses System V IPC
			 *                  objects anymore but maybe a few
			 *                  applications on the system have some
			 *                  for legacy communication purposes.
			 *
			 * - CLONE_NEWNET - Prevents processes from connecting
			 *to
			 *                  open abstract sockets.
			 *
			 * Unused
			 *
			 * - CLONE_NEWUTS - Clones the hostname namespace.
			 *Pretty
			 *                  useless.
			 *
			 * - CLONE_NEWUSER - Allows to create one's own users
			 *and
			 *                   enable some more
			 *                   sandboxes. Otherwise, it is pretty
			 *                   useless. Not permitted to use under
			 *                   the existing sandbox.
			 *
			 * - CLONE_NEWPID - Prevents processes from ptracing and
			 *                  signalling other
			 *                  processes. Unfortunately, PID 1
			 *can't
			 *                  send itself signals so this is
			 *                  unusable for many applications.
			 *
			 */
			if (-1 == unshare(CLONE_NEWIPC | CLONE_NEWNET |
			                  CLONE_NEWNS)) {
				exit_with_error(writer, errno);
			}

			if (-1 ==
			    mount(NULL, attr->chrootdir, "tmpfs", 0, NULL)) {
				exit_with_error(writer, errno);
			}

			if (-1 == chdir(attr->chrootdir)) {
				exit_with_error(writer, errno);
			}

			for (size_t ii = 0U; ii < attr->mount_args_size; ++ii) {
				struct mount_args *mount_arg =
				    &attr->mount_args[ii];

				if (mount_arg->mkdir_flag) {
					if (-1 ==
					    mkdir(mount_arg->target, S_IRWXU)) {
						exit_with_error(writer, errno);
					}
				} else if (mount_arg->touch_flag) {
					linted_ko xx;
					if ((errnum = linted_file_create(
					         &xx, AT_FDCWD,
					         mount_arg->target,
					         LINTED_FILE_EXCL, S_IRWXU)) !=
					    0) {
						exit_with_error(writer, errno);
					}
					linted_ko_close(xx);
				}

				unsigned long mountflags =
				    mount_arg->mountflags;

				if (-1 == mount(mount_arg->source,
				                mount_arg->target,
				                mount_arg->filesystemtype,
				                mountflags, mount_arg->data)) {
					exit_with_error(writer, errno);
				}

				if ((mountflags & MS_BIND) != 0U) {
					mountflags |= MS_REMOUNT;
					if (-1 ==
					    mount(mount_arg->source,
					          mount_arg->target,
					          mount_arg->filesystemtype,
					          mountflags,
					          mount_arg->data)) {
						exit_with_error(writer, errno);
					}
				}
			}

			/* Magic incantation that clears up /proc/mounts more
			 * than
			 * mount MS_MOVE
			 */
			if (-1 == syscall(__NR_pivot_root, ".", ".")) {
				exit_with_error(writer, errno);
			}

			if (-1 == umount2(".", MNT_DETACH)) {
				exit_with_error(writer, errno);
			}
		}

		if (attr->drop_caps) {
			/* Favor other processes over this process hierarchy.
			 * Only
			 * superuser may lower priorities so this is not
			 * stoppable. This
			 * also makes the process hierarchy nicer for the OOM
			 * killer.
			 */
			errno = 0;
			int priority = getpriority(PRIO_PROCESS, 0);
			if (-1 == priority) {
				errnum = errno;
				if (errnum != 0) {
					exit_with_error(writer, errnum);
				}
			}

			if (-1 == setpriority(PRIO_PROCESS, 0, priority + 1)) {
				exit_with_error(writer, errno);
			}

			if (-1 == setgroups(0U, NULL)) {
				exit_with_error(writer, errno);
			}
		}
	}

	/* Copy it in case it is overwritten */

	int dirfd_copy;

	if (at_fdcwd) {
		dirfd_copy = AT_FDCWD;
	} else if (is_relative_path && at_fdcwd) {
		if (-1 == (dirfd_copy = fcntl(dirfd, F_DUPFD_CLOEXEC, 0L))) {
			exit_with_error(writer, errno);
		}
	} else {
		dirfd_copy = -1;
	}

	if (file_actions != NULL) {
		for (size_t ii = 0U; ii < file_actions->action_count; ++ii) {
			union file_action const *action =
			    &file_actions->actions[ii];
			switch (action->type) {
			case FILE_ACTION_ADDDUP2: {
				int newfildes = action->adddup2.newfildes;

				if (dirfd_copy >= 0 &&
				    dirfd_copy == newfildes) {
					/* We don't need to close the old dirfd
					 * copy
					 * because it is closed by the following
					 * dup2.
					*/
					dirfd_copy = fcntl(dirfd_copy,
					                   F_DUPFD_CLOEXEC, 0L);
					if (-1 == dirfd_copy) {
						exit_with_error(writer, errno);
					}
				}

				if (writer == newfildes) {
					linted_ko new_writer =
					    fcntl(writer, F_DUPFD_CLOEXEC, 0L);
					if (-1 == new_writer) {
						exit_with_error(writer, errno);
					}
					writer = new_writer;
				}

				if (-1 == dup2(action->adddup2.oldfildes,
				               newfildes)) {
					exit_with_error(writer, errno);
				}
				break;
			}

			default:
				exit_with_error(writer, EINVAL);
			}
		}
	}

	if (attr != NULL) {
		if (attr->drop_caps) {
			/* Drop all privileges I might possibly have. I'm not
			 * sure I
			 * need to do this and I probably can do this in a
			 * better
			 * way. Note that currently we do not use
			 * PR_SET_KEEPCAPS and
			 * do not map our sandboxed user to root but if we did
			 * in the
			 * future we would need this.
			 */
			cap_t caps = cap_get_proc();
			if (NULL == caps) {
				exit_with_error(writer, errno);
			}

			/* Drop all capabilities after exec */
			if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
				exit_with_error(writer, errno);
			}
			if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
				exit_with_error(writer, errno);
			}

			if (-1 == cap_set_proc(caps)) {
				exit_with_error(writer, errno);
			}

			if (-1 == cap_free(caps)) {
				exit_with_error(writer, errno);
			}
		}
	}

	if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1UL, 0UL, 0UL, 0UL)) {
		exit_with_error(writer, errno);
	}

	/* Ban the process from spawning new ones so that resource
	 * limits can be enforced properly.
	 */
	if (-1 == prctl(PR_SET_SECCOMP, (unsigned long)SECCOMP_MODE_FILTER,
	                &ban_forks_filter, 0UL, 0UL)) {
		exit_with_error(writer, errno);
	}

	errnum = my_execveat(dirfd_copy, filename, argv, (char * const *)envp);

	exit_with_error(writer, errnum);
	return 0;
}

static linted_error my_execveat(int dirfd, const char *filename,
                                char *const argv[], char *const envp[])
{
	linted_error errnum;
	bool is_relative_path = filename[0U] != '/';
	bool at_fdcwd = AT_FDCWD == dirfd;
	char *new_path = NULL;

	if (is_relative_path && !at_fdcwd) {
		{
			void *xx;
			if ((errnum = linted_mem_alloc(
			         &xx, strlen("/proc/self/fd/") + 10U +
			                  strlen(filename) + 1U)) != 0) {
				return errnum;
			}
			new_path = xx;
		}
		sprintf(new_path, "/proc/self/fd/%i/%s", dirfd, filename);
		filename = new_path;
	}

	execve(filename, argv, envp);
	errnum = errno;

	linted_mem_free(new_path);

	return errnum;
}

static void exit_with_error(linted_ko writer, linted_error errnum)
{

	linted_io_write_all(writer, NULL, &errnum, sizeof errnum);
	_Exit(0);
}

#define THREAD_CLONE                                                           \
	(CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD |    \
	 CLONE_SYSVSEM | CLONE_SETTLS | CLONE_PARENT_SETTID |                  \
	 CLONE_CHILD_CLEARTID)

static struct sock_filter const real_filter[] = {
	/*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	                offsetof(struct seccomp_data, nr)),

	/*  */ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_vfork, 0U, 1U),
	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),

	/*  */ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_fork, 0U, 1U),
	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL),

	/*  */ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_clone, 1U, 0U),
	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

	/*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	                offsetof(struct seccomp_data, args) +
	                    (sizeof(uint64_t) * 2U)),

	/*  */ BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, THREAD_CLONE, 1U, 0U),
	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW),

	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
};

static struct sock_fprog const ban_forks_filter = {
	.len = LINTED_ARRAY_SIZE(real_filter),
	.filter = (struct sock_filter *)real_filter
};
