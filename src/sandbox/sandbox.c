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
#include <grp.h>
#include <sched.h>
#include <mntent.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>

enum {
	STOP_OPTIONS,
	HELP,
	VERSION_OPTION,
	DROP_CAPS,
	NO_NEW_PRIVS,
	CHDIR,
	PRIORITY,
	CHROOTDIR,
	FSTAB,
	NEWUSER_ARG,
	NEWPID_ARG,
	NEWIPC_ARG,
	NEWNET_ARG,
	NEWNS_ARG,
	NEWUTS_ARG,
	PID_OUT_FD
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

static struct sock_fprog const default_filter;

static char const *const argstrs[] = {
	    /**/[STOP_OPTIONS] = "--",
	    /**/ [HELP] = "--help",
	    /**/ [VERSION_OPTION] = "--version",
	    /**/ [DROP_CAPS] = "--dropcaps",
	    /**/ [NO_NEW_PRIVS] = "--nonewprivs",
	    /**/ [CHDIR] = "--chdir",
	    /**/ [PRIORITY] = "--priority",
	    /**/ [CHROOTDIR] = "--chrootdir",
	    /**/ [FSTAB] = "--fstab",
	    /**/ [NEWUSER_ARG] = "--clone-newuser",
	    /**/ [NEWPID_ARG] = "--clone-newpid",
	    /**/ [NEWIPC_ARG] = "--clone-newipc",
	    /**/ [NEWNET_ARG] = "--clone-newnet",
	    /**/ [NEWNS_ARG] = "--clone-newns",
	    /**/ [NEWUTS_ARG] = "--clone-newuts",
	    /**/ [PID_OUT_FD] = "--pidoutfd"
};

static void exit_with_error(linted_error errnum)
{
	errno = errnum;
	perror("stuff");
	_Exit(EXIT_FAILURE);
}

static linted_error set_ptracer(pid_t pid);

static linted_error parse_fstab(struct mount_args **mount_argsp, size_t *sizep,
                                linted_ko cwd, char const *fstab_path);
static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);
static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type);

static void set_id_maps(uid_t mapped_uid, uid_t uid, gid_t mapped_gid,
                        gid_t gid);
static void chroot_process(char const *chrootdir, struct mount_args *mount_args,
                           size_t mount_args_size);

static void propagate_signal(int signo);
static linted_error set_name(char const *name);

static linted_error set_child_subreaper(bool v);
static linted_error set_no_new_privs(bool b);
static linted_error set_seccomp(struct sock_fprog const *program);

int main(int argc, char *argv[])
{
	linted_error errnum;

	size_t arguments_length = argc;

	char const *service = getenv("LINTED_SERVICE");
	char const *listen_fds = getenv("LISTEN_FDS");
	if (NULL == listen_fds) {
		fprintf(stderr, "need LISTEN_FDS\n");
		return EXIT_FAILURE;
	}

	if (service != NULL) {
		errnum = set_name(service);
		assert(errnum != EINVAL);
	}

	char const *bad_option = NULL;
	bool need_version = false;
	bool need_help = false;
	bool no_new_privs = false;
	bool drop_caps = false;

	unsigned long clone_flags = 0U;

	char const *chdir_path = NULL;
	char const *priority = NULL;
	char const *chrootdir = NULL;
	char const *fstab = NULL;
	char const *pid_out_fd = NULL;
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

		case NO_NEW_PRIVS:
			no_new_privs = true;
			break;

		case DROP_CAPS:
			drop_caps = true;
			break;

		case CHDIR:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			chdir_path = argv[ii];
			break;

		case PRIORITY:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			priority = argv[ii];
			break;

		case CHROOTDIR:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			chrootdir = argv[ii];
			break;

		case FSTAB:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			fstab = argv[ii];
			break;

		case NEWUSER_ARG:
			clone_flags |= CLONE_NEWUSER;
			break;

		case NEWPID_ARG:
			clone_flags |= CLONE_NEWPID;
			break;

		case NEWIPC_ARG:
			clone_flags |= CLONE_NEWIPC;
			break;

		case NEWNET_ARG:
			clone_flags |= CLONE_NEWNET;
			break;

		case NEWNS_ARG:
			clone_flags |= CLONE_NEWNS;
			break;

		case NEWUTS_ARG:
			clone_flags |= CLONE_NEWUTS;
			break;

		case PID_OUT_FD:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			pid_out_fd = argv[ii];
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

	if ((fstab != NULL && NULL == chrootdir) ||
	    (NULL == fstab && chrootdir != NULL)) {
		fprintf(stderr,
		        "--chrootdir and --fstab are required together\n");
		return EXIT_FAILURE;
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

	gid_t gid = getgid();
	uid_t uid = getuid();

	gid_t mapped_gid = gid;
	uid_t mapped_uid = uid;

	pid_t spawner = getppid();

	{
		pid_t child;

		if (fstab != NULL)
			child =
			    syscall(__NR_clone, SIGCHLD | clone_flags, NULL);
		else
			child = fork();

		if (-1 == child)
			exit_with_error(errno);

		if (child != 0)
			_Exit(EXIT_SUCCESS);
	}

	/* Due to a kernel bug new users aren't protected from ptrace
	 * anyways.
	 */
	if (!((clone_flags & CLONE_NEWUSER) != 0)) {
		errnum = set_ptracer(spawner);
		if (errnum != 0) {
			linted_io_write_format(
			    STDERR_FILENO, NULL, "%s: set_ptracer: %s\n",
			    process_name, linted_error_string(errnum));
			return EXIT_FAILURE;
		}
	}

	if (fstab != NULL) {
		struct mount_args *mount_args = NULL;
		size_t mount_args_size = 0U;

		{
			struct mount_args *xx;
			size_t yy;
			errnum = parse_fstab(&xx, &yy, cwd, fstab);
			if (errnum != 0)
				return errnum;
			mount_args = xx;
			mount_args_size = yy;
		}

		set_id_maps(mapped_uid, uid, mapped_gid, gid);

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
		if (-1 == setgroups(0U, NULL))
			exit_with_error(errno);

		chroot_process(chrootdir, mount_args, mount_args_size);

		for (size_t ii = 0U; ii < mount_args_size; ++ii) {
			struct mount_args *mount_arg = &mount_args[ii];
			linted_mem_free(mount_arg->source);
			linted_mem_free(mount_arg->target);
			linted_mem_free(mount_arg->filesystemtype);
			linted_mem_free(mount_arg->data);
		}
		linted_mem_free(mount_args);
	}

	if (pid_out_fd != NULL) {
		int fd = atoi(pid_out_fd);

		char dummy[] = { 0 };
		struct iovec iovecs[] = { { .iov_base = dummy,
				            .iov_len = sizeof dummy } };

		struct msghdr message = { 0 };
		message.msg_iov = iovecs;
		message.msg_iovlen = LINTED_ARRAY_SIZE(iovecs);
		message.msg_control = NULL;
		message.msg_controllen = 0U;

		if (-1 == sendmsg(fd, &message, MSG_NOSIGNAL)) {
			perror("sendmsg");
			return EXIT_FAILURE;
		}
		linted_ko_close(fd);
	}

	if (priority != NULL) {
		if (-1 == setpriority(PRIO_PROCESS, 0, atoi(priority))) {
			perror("setpriority");
			return EXIT_FAILURE;
		}
	}

	/*
	 * This isn't needed if CLONE_NEWPID was used but it doesn't
	 * hurt,
	 */
	errnum = set_child_subreaper(true);
	if (errnum != 0) {
		errno = errnum;
		perror("set_child_subreaper");
		return EXIT_FAILURE;
	}

	if (chdir_path != NULL) {
		if (-1 == chdir(chdir_path)) {
			perror("chdir");
			return EXIT_FAILURE;
		}
	}

	/* Drop all capabilities I might possibly have. I'm not sure I
	 * need to do this and I probably can do this in a better
	 * way. Note that currently we do not use PR_SET_KEEPCAPS and
	 * do not map our sandboxed user to root but if we did in the
	 * future we would need this.
	 */

	if (drop_caps) {
		cap_t caps = cap_get_proc();
		if (NULL == caps) {
			perror("cap_get_proc");
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
			perror("cap_clear_flag");
			return EXIT_FAILURE;
		}
		if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
			perror("cap_clear_flag");
			return EXIT_FAILURE;
		}
		if (-1 == cap_clear_flag(caps, CAP_INHERITABLE)) {
			perror("cap_clear_flag");
			return EXIT_FAILURE;
		}

		if (-1 == cap_set_proc(caps)) {
			perror("cap_set_proc");
			return EXIT_FAILURE;
		}

		if (-1 == cap_free(caps)) {
			perror("cap_free");
			return EXIT_FAILURE;
		}
	}

	if (no_new_privs) {
		/* Must appear before the seccomp filter */
		errnum = set_no_new_privs(true);
		if (errnum != 0) {
			errno = errnum;
			perror("set_no_new_privs");
			return EXIT_FAILURE;
		}

		errnum = set_seccomp(&default_filter);
		if (errnum != 0) {
			errno = errnum;
			perror("set_seccomp");
			return EXIT_FAILURE;
		}
	}

	pid_t child = fork();
	if (-1 == child) {
		perror("fork");
		return EXIT_FAILURE;
	}

	if (0 == child) {
		{
			char xx[] = "XXXXXXXXXXXXXXXXX";
			sprintf(xx, "%i", getpid());
			if (-1 == setenv("LISTEN_PID", xx, true)) {
				perror("setenv");
				return EXIT_FAILURE;
			}
		}

		{
			char xx[] = "XXXXXXXXXXXXXXXXX";
			sprintf(xx, "%i", atoi(listen_fds) - 1);
			if (-1 == setenv("LISTEN_FDS", xx, true)) {
				perror("setenv");
				return EXIT_FAILURE;
			}
		}

		execve(command[0U], (char * const *)command, environ);
		perror("execve");
		return EXIT_FAILURE;
	}

	/* Catch signals
	 *
	 * The only signals that can be sent to process ID 1, the init
	 * process, are those for which init has explicitly installed
	 * signal handlers.  This is done to assure the system is not
	 * brought down accidentally.
	 *
	 * - KILL(2) http://www.kernel.org/doc/man-pages/.
	 *
	 * This applies to sandboxs to if they use CLONE_NEWPID.
	 *
	 * We want to explicitly handle the signal so that it is
	 * propagated to children of init as well.
	 */
	static int const exit_signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGTERM };
	if (1 == getpid()) {
		/* Delegate the exit signal to children and then exit */
		for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(exit_signals);
		     ++ii) {
			struct sigaction action = { 0 };
			action.sa_handler = propagate_signal;
			action.sa_flags = 0;
			sigfillset(&action.sa_mask);
			if (-1 == sigaction(exit_signals[ii], &action, NULL)) {
				assert(errno != EINVAL);
				assert(0);
			}
		}
	}

	for (;;) {
		int wait_status;
		{
			siginfo_t info;
			wait_status = waitid(P_ALL, -1, &info, WEXITED);
		}
		if (-1 == wait_status) {
			errnum = errno;
			assert(errnum != 0);
			assert(errnum != EINVAL);
			if (errnum != EINTR)
				break;
		}
	}

	return errnum;
}

static linted_error set_ptracer(pid_t pid)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_PTRACER, (unsigned long)pid, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static void set_id_maps(uid_t mapped_uid, uid_t uid, gid_t mapped_gid,
                        gid_t gid)
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
				exit_with_error(errnum);
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_uid, uid);
		if (errnum != 0)
			exit_with_error(errnum);

		errnum = linted_ko_close(file);
		if (errnum != 0)
			exit_with_error(errnum);
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/gid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0)
				exit_with_error(errnum);
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_gid, gid);
		if (errnum != 0)
			exit_with_error(errnum);

		errnum = linted_ko_close(file);
		if (errnum != 0)
			exit_with_error(errnum);
	}
}

static void chroot_process(char const *chrootdir, struct mount_args *mount_args,
                           size_t mount_args_size)
{
	linted_error errnum;

	if (-1 == mount(NULL, chrootdir, "tmpfs", 0, "mode=700"))
		exit_with_error(errno);

	if (-1 == chdir(chrootdir))
		exit_with_error(errno);

	for (size_t ii = 0U; ii < mount_args_size; ++ii) {
		struct mount_args *mount_arg = &mount_args[ii];

		if (mount_arg->mkdir_flag) {
			if (-1 == mkdir(mount_arg->target, S_IRWXU))
				exit_with_error(errno);
		} else if (mount_arg->touch_flag) {
			if (-1 ==
			    mknod(mount_arg->target, S_IRWXU | S_IFREG, 0))
				exit_with_error(errno);
		}

		unsigned long mountflags = mount_arg->mountflags;

		if (-1 == mount(mount_arg->source, mount_arg->target,
		                mount_arg->filesystemtype, mountflags,
		                mount_arg->data))
			exit_with_error(errno);

		if ((mountflags & MS_BIND) != 0U) {
			mountflags |= MS_REMOUNT;
			if (-1 == mount(mount_arg->source, mount_arg->target,
			                mount_arg->filesystemtype, mountflags,
			                mount_arg->data))
				exit_with_error(errno);
		}
	}

	/* Magic incantation that clears up /proc/mounts more than
	 * mount MS_MOVE
	 */
	int old_root = open("/", O_DIRECTORY | O_CLOEXEC);
	if (-1 == old_root)
		exit_with_error(errno);

	if (-1 == syscall(__NR_pivot_root, ".", "."))
		exit_with_error(errno);

	/* pivot_root() may or may not affect its current working
	 * directory.  It is therefore recommended to call chdir("/")
	 * immediately after pivot_root().
	 *
	 * - http://man7.org/linux/man-pages/man2/pivot_root.2.html
	 */

	if (-1 == fchdir(old_root))
		exit_with_error(errno);

	errnum = linted_ko_close(old_root);
	if (errnum != 0)
		exit_with_error(errnum);

	if (-1 == umount2(".", MNT_DETACH))
		exit_with_error(errno);

	if (-1 == chdir("/"))
		exit_with_error(errno);
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

static void propagate_signal(int signo)
{
	kill(-1, signo);

	/* Sadly, it is impossible to kill oneself with the proper
	 * signal as init. Also, using raise(SIGKILL) doesn't work
	 * either. */
	_Exit(signo);
}

static linted_error set_name(char const *name)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NAME, (unsigned long)name, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
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

static linted_error set_no_new_privs(bool b)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_NO_NEW_PRIVS, (unsigned long)b, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(errnum != EINVAL);
		return errnum;
	}

	return 0;
}

static linted_error set_seccomp(struct sock_fprog const *program)
{
	linted_error errnum;

	if (-1 == prctl(PR_SET_SECCOMP, (unsigned long)SECCOMP_MODE_FILTER,
	                program, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		assert(errnum != EINVAL);

		return errnum;
	}
	return 0;
}

#if defined __amd64__
#include "sandbox-amd64.c"
#elif defined __i386__
#include "sandbox-i386.c"
#else
#error No default seccomp filter has been defined for this architecture
#endif
