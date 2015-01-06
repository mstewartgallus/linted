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

#include "linted/dir.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <mntent.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/ioctl.h>
#include <syslog.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>

/**
 * @file
 *
 * Sandbox applications.
 */

enum {
	STOP_OPTIONS,
	HELP,
	VERSION_OPTION,
	WAIT,
	TRACEME,
	DROP_CAPS,
	NO_NEW_PRIVS,
	CHDIR,
	PRIORITY,
	CHROOTDIR,
	FSTAB,
	WAITER,
	NEWUSER_ARG,
	NEWPID_ARG,
	NEWIPC_ARG,
	NEWNET_ARG,
	NEWNS_ARG,
	NEWUTS_ARG
};

struct mount_args
{
	char const *fsname;
	char const *dir;
	char const *type;
	char const *data;

	unsigned long mountflags;

	bool mkdir_flag : 1U;
	bool touch_flag : 1U;
	bool nomount_flag : 1U;
};

static struct sock_fprog const default_filter;

static char const *const argstrs[] = {
	    /**/[STOP_OPTIONS] = "--",
	    /**/ [HELP] = "--help",
	    /**/ [VERSION_OPTION] = "--version",
	    /**/ [WAIT] = "--wait",
	    /**/ [TRACEME] = "--traceme",
	    /**/ [DROP_CAPS] = "--dropcaps",
	    /**/ [NO_NEW_PRIVS] = "--nonewprivs",
	    /**/ [CHDIR] = "--chdir",
	    /**/ [PRIORITY] = "--priority",
	    /**/ [CHROOTDIR] = "--chrootdir",
	    /**/ [FSTAB] = "--fstab",
	    /**/ [WAITER] = "--waiter",
	    /**/ [NEWUSER_ARG] = "--clone-newuser",
	    /**/ [NEWPID_ARG] = "--clone-newpid",
	    /**/ [NEWIPC_ARG] = "--clone-newipc",
	    /**/ [NEWNET_ARG] = "--clone-newnet",
	    /**/ [NEWNS_ARG] = "--clone-newns",
	    /**/ [NEWUTS_ARG] = "--clone-newuts"
};

struct first_fork_args
{
	linted_ko err_reader;
	linted_ko err_writer;
	char const *uid_map;
	char const *gid_map;
	unsigned long clone_flags;
	linted_ko cwd;
	char const *chrootdir;
	char const *chdir_path;
	cap_t caps;
	struct mount_args *mount_args;
	size_t mount_args_size;
	bool no_new_privs;
	char const *waiter;
	char const *waiter_base;
	char const *const *command;
	char const *binary;
};

struct second_fork_args
{
	linted_ko err_writer;
	char const *const *argv;
	char const *binary;
	bool no_new_privs;
};

static int first_fork_routine(void *arg);
static linted_error
do_first_fork(linted_ko err_reader, char const *uid_map, char const *gid_map,
              unsigned long clone_flags, linted_ko cwd, char const *chrootdir,
              char const *chdir_path, cap_t caps, struct mount_args *mount_args,
              size_t mount_args_size, bool no_new_privs, char const *waiter,
              char const *waiter_base, char const *const *command,
              char const *binary);

static int second_fork_routine(void *arg);
static linted_error do_second_fork(char const *binary, char const *const *argv,
                                   bool no_new_privs);

/* Clang fucks up generating code for this for some reason when using
 * an inline function and address sanitizer. It might also just be
 * that address sanitizer doesn't work together well with the
 * weirdness of forking.
 */
#define exit_with_error(writer, errnum)                                        \
	do {                                                                   \
		linted_error xx = errnum;                                      \
		linted_io_write_all(writer, NULL, &xx, sizeof xx);             \
		_Exit(EXIT_FAILURE);                                           \
	} while (0)

static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp, bool *nomount_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);

static linted_error set_id_maps(char const *uid_map, char const *gid_map);
static linted_error chroot_process(linted_ko cwd, char const *chrootdir,
                                   struct mount_args const *mount_args,
                                   size_t size);

static linted_error set_child_subreaper(bool v);
static linted_error set_no_new_privs(bool b);
static linted_error set_seccomp(struct sock_fprog const *program);

static pid_t safe_vfork(int (*f)(void *), void *args);
static pid_t safe_vclone(int clone_flags, int (*f)(void *), void *args);
static int my_setgroups(size_t size, gid_t const *list);
static int my_pivot_root(char const *new_root, char const *put_old);

int main(int argc, char *argv[])
{
	linted_error errnum;

	size_t arguments_length = argc;

	char const *bad_option = NULL;
	bool need_version = false;
	bool need_help = false;

	bool wait = false;
	bool traceme = false;
	bool no_new_privs = false;
	bool drop_caps = false;

	unsigned long clone_flags = 0U;

	char const *chdir_path = NULL;
	char const *priority = NULL;
	char const *chrootdir = NULL;
	char const *fstab = NULL;
	char const *waiter = NULL;
	bool have_command = false;
	size_t command_start;

	/* Currently, don't use LOG_PID because syslog is confused by
	 * CLONE_NEWPID.
	 */
	openlog(argv[0U], LOG_CONS | LOG_NDELAY | LOG_PERROR, LOG_USER);

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

		case WAIT:
			wait = true;
			break;

		case TRACEME:
			traceme = true;
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

		case WAITER:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			waiter = argv[ii];
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
		}
	}
exit_loop:
	if (!have_command) {
		syslog(LOG_ERR, "need command");
		return EXIT_FAILURE;
	}

	if (bad_option != NULL) {
		syslog(LOG_ERR, "bad option: %s", bad_option);
		return EXIT_FAILURE;
	}

	if (NULL == waiter) {
		syslog(LOG_ERR, "need waiter");
		return EXIT_FAILURE;
	}

	if ((fstab != NULL && NULL == chrootdir) ||
	    (NULL == fstab && chrootdir != NULL)) {
		syslog(LOG_ERR,
		       "--chrootdir and --fstab are required together");
		return EXIT_FAILURE;
	}

	linted_ko cwd;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, ".",
		                        LINTED_KO_DIRECTORY);
		if (errnum != 0) {
			syslog(LOG_ERR, "linted_ko_open: %s",
			       linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	if (traceme) {
		if (-1 == ptrace(PTRACE_TRACEME, (pid_t)0, (void *)NULL,
		                 (void *)NULL)) {
			syslog(LOG_ERR, "ptrace: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}

		/* Register with the parent */
		if (-1 == raise(SIGSTOP)) {
			syslog(LOG_ERR, "raise: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	char const **command = (char const **)argv + 1U + command_start;

	char *command_dup = strdup(command[0U]);
	if (NULL == command_dup) {
		syslog(LOG_ERR, "strdup: %s", linted_error_string(errno));
		return EXIT_FAILURE;
	}

	char *waiter_dup = strdup(waiter);
	if (NULL == waiter_dup) {
		syslog(LOG_ERR, "strdup: %s", linted_error_string(errno));
		return EXIT_FAILURE;
	}

	char const *command_base = basename(command_dup);
	char const *waiter_base = basename(waiter_dup);

	char const *binary = command[0U];
	command[0U] = command_base;

	size_t mount_args_size = 0U;
	struct mount_args *mount_args = NULL;
	if (fstab != NULL) {
		FILE *fstab_file = setmntent(fstab, "re");
		if (NULL == fstab_file) {
			syslog(LOG_ERR, "setmntent: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}

		for (;;) {
			errno = 0;
			struct mntent *entry = getmntent(fstab_file);
			if (NULL == entry) {
				errnum = errno;
				if (errnum != 0) {
					syslog(LOG_ERR, "getmntent: %s",
					       linted_error_string(errnum));
					return EXIT_FAILURE;
				}
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
			bool nomount_flag = false;
			unsigned long mountflags = 0U;
			char const *data = NULL;
			if (opts != NULL) {
				bool xx;
				bool yy;
				bool zz;
				unsigned long ww;
				char const *uu;
				errnum = parse_mount_opts(opts, &xx, &yy, &zz,
				                          &ww, &uu);
				if (errnum != 0) {
					syslog(LOG_ERR, "parse_mount_opts: %s",
					       linted_error_string(errnum));
					return EXIT_FAILURE;
				}
				mkdir_flag = xx;
				touch_flag = yy;
				nomount_flag = zz;
				mountflags = ww;
				data = uu;
			}

			size_t new_mount_args_size = mount_args_size + 1U;
			{
				void *xx;
				errnum = linted_mem_realloc_array(
				    &xx, mount_args, new_mount_args_size,
				    sizeof mount_args[0U]);
				if (errnum != 0) {
					syslog(LOG_ERR,
					       "linted_mem_realloc_array: %s",
					       linted_error_string(errnum));
					return EXIT_FAILURE;
				}
				mount_args = xx;
			}

			if (fsname != NULL) {
				fsname = strdup(fsname);
				if (NULL == fsname) {
					syslog(LOG_ERR, "strdup: %s",
					       linted_error_string(errno));
					return EXIT_FAILURE;
				}
			}

			if (dir != NULL) {
				dir = strdup(dir);
				if (NULL == dir) {
					syslog(LOG_ERR, "strdup: %s",
					       linted_error_string(errno));
					return EXIT_FAILURE;
				}
			}

			if (type != NULL) {
				type = strdup(type);
				if (NULL == type) {
					syslog(LOG_ERR, "strdup: %s",
					       linted_error_string(errno));
					return EXIT_FAILURE;
				}
			}

			if (data != NULL) {
				data = strdup(data);
				if (NULL == data) {
					syslog(LOG_ERR, "strdup: %s",
					       linted_error_string(errno));
					return EXIT_FAILURE;
				}
			}

			mount_args[mount_args_size].fsname = fsname;
			mount_args[mount_args_size].dir = dir;
			mount_args[mount_args_size].type = type;
			mount_args[mount_args_size].data = data;
			mount_args[mount_args_size].mountflags = mountflags;
			mount_args[mount_args_size].mkdir_flag = mkdir_flag;
			mount_args[mount_args_size].touch_flag = touch_flag;
			mount_args[mount_args_size].nomount_flag = nomount_flag;
			mount_args_size = new_mount_args_size;
		}

		if (endmntent(fstab_file) != 1) {
			syslog(LOG_ERR, "endmntent: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	cap_t caps = NULL;
	if (drop_caps) {
		caps = cap_get_proc();
		if (NULL == caps) {
			syslog(LOG_ERR, "cap_get_proc: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
			syslog(LOG_ERR, "cap_clear_flag: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
			syslog(LOG_ERR, "cap_clear_flag: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_INHERITABLE)) {
			syslog(LOG_ERR, "cap_clear_flag: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (priority != NULL) {
		if (-1 == setpriority(PRIO_PROCESS, 0, atoi(priority))) {
			syslog(LOG_ERR, "setpriority: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	gid_t gid = getgid();
	uid_t uid = getuid();

	gid_t mapped_gid = gid;
	uid_t mapped_uid = uid;

	char uid_map[LINTED_NUMBER_TYPE_STRING_SIZE(uid_t) + 1U +
	             LINTED_NUMBER_TYPE_STRING_SIZE(uid_t) + 1U + sizeof "1\n" -
	             1U + 1U];
	char gid_map[LINTED_NUMBER_TYPE_STRING_SIZE(gid_t) + 1U +
	             LINTED_NUMBER_TYPE_STRING_SIZE(gid_t) + 1U + sizeof "1\n" -
	             1U + 1U];

	sprintf(uid_map, "%i %i 1\n", mapped_uid, uid);
	sprintf(gid_map, "%i %i 1\n", mapped_gid, gid);

	linted_ko err_reader;
	linted_ko err_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			syslog(LOG_ERR, "pipe2: %s",
			       linted_error_string(errno));
			return EXIT_FAILURE;
		}
		err_reader = xx[0U];
		err_writer = xx[1U];
	}

	struct first_fork_args args = { .err_reader = err_reader,
		                        .err_writer = err_writer,
		                        .uid_map = uid_map,
		                        .gid_map = gid_map,
		                        .clone_flags = clone_flags,
		                        .cwd = cwd,
		                        .chrootdir = chrootdir,
		                        .chdir_path = chdir_path,
		                        .caps = caps,
		                        .mount_args = mount_args,
		                        .mount_args_size = mount_args_size,
		                        .no_new_privs = no_new_privs,
		                        .waiter_base = waiter_base,
		                        .waiter = waiter,
		                        .command = command,
		                        .binary = binary };

	pid_t child;
	if (0 == clone_flags) {
		child = safe_vfork(first_fork_routine, &args);
	} else {
		child = safe_vclone(SIGCHLD | clone_flags, first_fork_routine,
		                    &args);
	}
	if (-1 == child) {
		syslog(LOG_ERR, "clone: %s", linted_error_string(errno));
		return EXIT_FAILURE;
	}

	linted_ko_close(err_writer);

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
close_err_reader:
	linted_ko_close(err_reader);

	if (errnum != 0) {
		syslog(LOG_ERR, "spawning: %s", linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	linted_ko_close(STDIN_FILENO);
	linted_ko_close(STDOUT_FILENO);

	if (!wait)
		return EXIT_SUCCESS;

	for (;;) {
		int xx;
		switch (waitpid(child, &xx, 0)) {
		case -1:
			switch (errno) {
			case EINTR:
				continue;
			default:
				syslog(LOG_ERR, "waitpid: %s",
				       linted_error_string(errno));
				return EXIT_FAILURE;
			}

		default:
			return EXIT_SUCCESS;
		}
	}

	return EXIT_SUCCESS;
}

static int first_fork_routine(void *arg)
{
	struct first_fork_args const *args = arg;

	linted_ko err_reader = args->err_reader;
	linted_ko err_writer = args->err_writer;
	char const *uid_map = args->uid_map;
	char const *gid_map = args->gid_map;
	unsigned long clone_flags = args->clone_flags;
	linted_ko cwd = args->cwd;
	char const *chrootdir = args->chrootdir;
	char const *chdir_path = args->chdir_path;
	cap_t caps = args->caps;
	struct mount_args *mount_args = args->mount_args;
	size_t mount_args_size = args->mount_args_size;
	bool no_new_privs = args->no_new_privs;
	char const *waiter = args->waiter;
	char const *waiter_base = args->waiter_base;
	char const *const *command = args->command;
	char const *binary = args->binary;

	linted_error errnum = do_first_fork(
	    err_reader, uid_map, gid_map, clone_flags, cwd, chrootdir,
	    chdir_path, caps, mount_args, mount_args_size, no_new_privs, waiter,
	    waiter_base, command, binary);
	exit_with_error(err_writer, errnum);
	/* Never reached */
	return 0;
}

static linted_error
do_first_fork(linted_ko err_reader, char const *uid_map, char const *gid_map,
              unsigned long clone_flags, linted_ko cwd, char const *chrootdir,
              char const *chdir_path, cap_t caps, struct mount_args *mount_args,
              size_t mount_args_size, bool no_new_privs, char const *waiter,
              char const *waiter_base, char const *const *command,
              char const *binary)
{
	linted_error errnum = 0;

	linted_ko_close(err_reader);

	/* Terminals are really ugly and horrible, avoid them. */
	int tty = open("/dev/tty", O_CLOEXEC);
	if (-1 == tty) {
		if (errno != ENXIO)
			return errno;
	} else {
		if (-1 == ioctl(tty, TIOCNOTTY))
			return errno;
	}
	linted_ko_close(tty);

	/* First things first set the id mapping */
	if ((clone_flags & CLONE_NEWUSER) != 0) {
		errnum = set_id_maps(uid_map, gid_map);
		if (errnum != 0)
			return errnum;

		if (-1 == my_setgroups(0U, NULL))
			return errno;
	}

	if (mount_args_size > 0U) {
		errnum =
		    chroot_process(cwd, chrootdir, mount_args, mount_args_size);
		if (errnum != 0)
			return errnum;
	}

	if (chdir_path != NULL) {
		if (-1 == chdir(chdir_path))
			return errno;
	}

	/*
	 * This isn't needed if CLONE_NEWPID was used but it doesn't
	 * hurt,
	 */
	errnum = set_child_subreaper(true);
	if (errnum != 0)
		return errnum;

	/* Drop all capabilities I might possibly have. Note that
	 * currently we do not use PR_SET_KEEPCAPS and do not map our
	 * sandboxed user to root but if we did in the future we would
	 * need this.
	 */

	if (caps != NULL) {
		if (-1 == cap_set_proc(caps))
			return errno;
	}

	{
		sigset_t sigset;
		sigemptyset(&sigset);
		sigaddset(&sigset, SIGCHLD);
		errnum = pthread_sigmask(SIG_UNBLOCK, &sigset, NULL);
		if (errnum != 0)
			return errnum;
	}

	sigset_t sigchld_unblocked;
	{
		sigset_t sigset;
		sigemptyset(&sigset);
		sigaddset(&sigset, SIGCHLD);
		errnum =
		    pthread_sigmask(SIG_BLOCK, &sigset, &sigchld_unblocked);
		if (errnum != 0)
			return errnum;
	}

	linted_ko vfork_err_reader;
	linted_ko vfork_err_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK))
			return errno;
		vfork_err_reader = xx[0U];
		vfork_err_writer = xx[1U];
	}

	if (no_new_privs) {
		/* Must appear before the seccomp filter */
		errnum = set_no_new_privs(true);
		if (errnum != 0)
			return errnum;
	}

	struct second_fork_args args = { .err_writer = vfork_err_writer,
		                         .binary = binary,
		                         .argv = command,
		                         .no_new_privs = no_new_privs };
	pid_t grand_child = safe_vfork(second_fork_routine, &args);
	if (-1 == grand_child)
		return errno;

	linted_ko_close(vfork_err_writer);

	{
		size_t xx;
		linted_error yy;
		errnum =
		    linted_io_read_all(vfork_err_reader, &xx, &yy, sizeof yy);
		if (errnum != 0)
			return errnum;

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			errnum = yy;
	}
	if (errnum != 0)
		return errnum;

	linted_ko_close(cwd);

	char const *arguments[] = { waiter_base, NULL };
	execve(waiter, (char * const *)arguments, environ);
	return errno;
}

static int second_fork_routine(void *arg)
{
	struct second_fork_args const *args = arg;

	linted_ko err_writer = args->err_writer;
	char const *binary = args->binary;
	char const *const *argv = args->argv;
	bool no_new_privs = args->no_new_privs;

	linted_error errnum = do_second_fork(binary, argv, no_new_privs);
	exit_with_error(err_writer, errnum);
	return 0;
}

static linted_error do_second_fork(char const *binary, char const *const *argv,
                                   bool no_new_privs)
{
	if (-1 == setsid())
		return errno;

	/* Do seccomp filter last of all */
	if (no_new_privs) {
		linted_error errnum = set_seccomp(&default_filter);
		if (errnum != 0)
			return errnum;
	}

	execve(binary, (char * const *)argv, environ);
	return errno;
}

static linted_error set_id_maps(char const *uid_map, char const *gid_map)
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
				return errnum;
			file = xx;
		}

		errnum = linted_io_write_string(file, NULL, uid_map);
		if (errnum != 0)
			return errnum;

		errnum = linted_ko_close(file);
		if (errnum != 0)
			return errnum;
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/gid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0)
				return errnum;
			file = xx;
		}

		errnum = linted_io_write_string(file, NULL, gid_map);
		if (errnum != 0)
			return errnum;

		errnum = linted_ko_close(file);
		if (errnum != 0)
			return errnum;
	}

	return 0;
}

static linted_error chroot_process(linted_ko cwd, char const *chrootdir,
                                   struct mount_args const *mount_args,
                                   size_t size)
{
	linted_error errnum;

	if (-1 == mount(chrootdir, chrootdir, NULL, MS_BIND, NULL))
		return errno;

	if (-1 == chdir(chrootdir))
		return errno;

	for (size_t ii = 0U; ii < size; ++ii) {
		char const *fsname = mount_args[ii].fsname;
		char const *dir = mount_args[ii].dir;
		char const *type = mount_args[ii].type;
		char const *data = mount_args[ii].data;
		bool mkdir_flag = mount_args[ii].mkdir_flag;
		bool touch_flag = mount_args[ii].touch_flag;
		bool nomount_flag = mount_args[ii].nomount_flag;
		unsigned long mountflags = mount_args[ii].mountflags;

		if (mkdir_flag) {
			errnum = linted_dir_create(NULL, LINTED_KO_CWD, dir, 0U,
			                           S_IRWXU);
			if (errnum != 0)
				return errnum;
		} else if (touch_flag) {
			errnum = linted_file_create(NULL, LINTED_KO_CWD, dir,
			                            0U, S_IRWXU);
			if (errnum != 0)
				return errnum;
		}

		if (nomount_flag)
			continue;

		unsigned long aliasflags =
		    mountflags & (MS_BIND | MS_SHARED | MS_SLAVE);
		if (0 == aliasflags) {
			if (-1 == mount(fsname, dir, type, mountflags, data))
				return errno;
			continue;
		}

		if (-1 == mount(fsname, dir, type, aliasflags, NULL))
			return errno;

		if (NULL == data && 0 == (mountflags & ~aliasflags))
			continue;

		if (-1 ==
		    mount(fsname, dir, type, MS_REMOUNT | mountflags, data))
			return errno;
	}

	/* Magic incantation that clears up /proc/mounts more than
	 * mount MS_MOVE
	 */
	int old_root = open("/", O_DIRECTORY | O_CLOEXEC);
	if (-1 == old_root)
		return errno;

	if (-1 == my_pivot_root(".", "."))
		return errno;

	/* pivot_root() may or may not affect its current working
	 * directory.  It is therefore recommended to call chdir("/")
	 * immediately after pivot_root().
	 *
	 * - http://man7.org/linux/man-pages/man2/pivot_root.2.html
	 */

	if (-1 == fchdir(old_root))
		return errno;

	errnum = linted_ko_close(old_root);
	if (errnum != 0)
		return errnum;

	if (-1 == umount2(".", MNT_DETACH))
		return errno;

	if (-1 == chdir("/"))
		return errno;

	return 0;
}

enum {
	MKDIR,
	TOUCH,
	NOMOUNT,
	SHARED,
	SLAVE,
	BIND,
	RBIND,
	RO,
	RW,
	SUID,
	NOSUID,
	NODEV,
	NOEXEC
};

static char const *const mount_options[] = {[MKDIR] = "mkdir",        /**/
	                                    [TOUCH] = "touch",        /**/
	                                    [NOMOUNT] = "nomount",    /**/
	                                    [SHARED] = "shared",      /**/
	                                    [SLAVE] = "slave",        /**/
	                                    [BIND] = "bind",          /**/
	                                    [RBIND] = "rbind",        /**/
	                                    [RO] = MNTOPT_RO,         /**/
	                                    [RW] = MNTOPT_RW,         /**/
	                                    [SUID] = MNTOPT_SUID,     /**/
	                                    [NOSUID] = MNTOPT_NOSUID, /**/
	                                    [NODEV] = "nodev",        /**/
	                                    [NOEXEC] = "noexec",      /**/
	                                    NULL };

static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp, bool *nomount_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp)
{
	linted_error errnum;

	bool nomount_flag = false;
	bool touch_flag = false;
	bool mkdir_flag = false;
	bool shared = false;
	bool slave = false;
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

		case NOMOUNT:
			nomount_flag = true;
			break;

		case SHARED:
			shared = true;
			break;

		case SLAVE:
			slave = true;
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

	if (bind && (rec || shared || slave) &&
	    (readonly || !suid || !dev || !exec))
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

	if (nomount_flag && bind)
		return EINVAL;

	unsigned long mountflags = 0U;

	if (shared)
		mountflags |= MS_SHARED;

	if (slave)
		mountflags |= MS_SLAVE;

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
	*nomount_flagp = nomount_flag;
	*mountflagsp = mountflags;
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

/* Most compilers can't handle the weirdness of vfork so contain it in
 * a safe abstraction.
 */
__attribute__((noinline)) static pid_t safe_vfork(int (*f)(void *), void *arg)
{
	void *volatile arg_copy = arg;
	int (*volatile f_copy)(void *) = f;

	__atomic_signal_fence(__ATOMIC_SEQ_CST);

	pid_t child = fork();
	if (0 == child)
		_Exit(f_copy(arg_copy));
	return child;
}

static pid_t safe_vclone(int clone_flags, int (*f)(void *), void *arg)
{
	long maybe_page_size = sysconf(_SC_PAGE_SIZE);
	assert(maybe_page_size >= 0);

	long maybe_stack_min_size = sysconf(_SC_THREAD_STACK_MIN);
	assert(maybe_stack_min_size >= 0);

	size_t page_size = maybe_page_size;
	size_t stack_min_size = maybe_stack_min_size;

	/* We need an extra page for signals */
	size_t stack_size = stack_min_size + page_size;

	size_t stack_and_guard_size = page_size + stack_size + page_size;
	void *child_stack = mmap(
	    NULL, stack_and_guard_size, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK, -1, 0);
	if (NULL == child_stack)
		return -1;

	/* Guard pages are shared between the stacks */
	if (-1 == mprotect((char *)child_stack, page_size, PROT_NONE))
		goto on_err;

	if (-1 == mprotect((char *)child_stack + page_size + stack_size,
	                   page_size, PROT_NONE))
		goto on_err;

	void *stack_start = (char *)child_stack + page_size + stack_size;

	__atomic_signal_fence(__ATOMIC_SEQ_CST);

	pid_t child = clone(f, stack_start, clone_flags | CLONE_VFORK, arg);
	if (-1 == child)
		goto on_err;

	munmap(child_stack, stack_and_guard_size);

	return child;

on_err:
	;
	int errnum = errno;
	munmap(child_stack, stack_and_guard_size);
	errno = errnum;
	return -1;
}

/* Avoid setXid synchronization after vfork */
static int my_setgroups(size_t size, gid_t const *list)
{
	return syscall(__NR_setgroups, size, list);
}

static int my_pivot_root(char const *new_root, char const *put_old)
{
	return syscall(__NR_pivot_root, new_root, put_old);
}

#if defined __amd64__
#include "sandbox-amd64.c"
#elif defined __i386__
#include "sandbox-i386.c"
#else
#error No default seccomp filter has been defined for this architecture
#endif
