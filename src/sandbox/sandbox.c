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
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/poll.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
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
	char *listen_pid_str;
	char *listen_fds_str;
	linted_ko stdin_writer;
	linted_ko stdout_reader;
	linted_ko stderr_reader;
	linted_ko stdin_reader;
	linted_ko stdout_writer;
	linted_ko stderr_writer;
	char const *waiter;
	char const *waiter_base;
	char const *const *env_copy;
	char const *const *command;
	char const *binary;
	size_t num_fds;
};

struct second_fork_args
{
	linted_ko err_writer;
	linted_ko stdin_reader;
	linted_ko stdout_writer;
	linted_ko stderr_writer;
	char *listen_pid_str;
	char const *const *argv;
	char const *const *env;
	char const *binary;
	bool no_new_privs;
};

static int first_fork_routine(void *arg);
static linted_error
do_first_fork(linted_ko err_reader, char const *uid_map, char const *gid_map,
              unsigned long clone_flags, linted_ko cwd, char const *chrootdir,
              char const *chdir_path, cap_t caps, struct mount_args *mount_args,
              size_t mount_args_size, bool no_new_privs, char *listen_pid_str,
              char *listen_fds_str, linted_ko stdin_writer,
              linted_ko stdout_reader, linted_ko stderr_reader,
              linted_ko stdin_reader, linted_ko stdout_writer,
              linted_ko stderr_writer, char const *waiter,
              char const *waiter_base, char const *const *env_copy,
              char const *const *command, char const *binary, size_t num_fds);

static int second_fork_routine(void *arg);
static linted_error do_second_fork(linted_ko stdin_reader,
                                   linted_ko stdout_writer,
                                   linted_ko stderr_writer,
                                   char *listen_pid_str, char const *binary,
                                   char const *const *argv,
                                   char const *const *env, bool no_new_privs);

static void exit_with_error(linted_ko writer, linted_error errnum);

static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp, bool *nomount_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);
static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type);

static linted_error set_id_maps(char const *uid_map, char const *gid_map);
static linted_error chroot_process(linted_ko cwd, char const *chrootdir,
                                   struct mount_args const *mount_args,
                                   size_t size);

static void pid_to_str(char *buf, pid_t pid);

static linted_error set_child_subreaper(bool v);
static linted_error set_no_new_privs(bool b);
static linted_error set_seccomp(struct sock_fprog const *program);

static pid_t safe_vfork(int (*f)(void *), void *args);
static pid_t safe_vclone(int clone_flags, int (*f)(void *), void *args);
static pid_t real_getpid(void);
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
		fprintf(stderr, "need command\n");
		return EXIT_FAILURE;
	}

	if (bad_option != NULL) {
		fprintf(stderr, "bad option: %s\n", bad_option);
		return EXIT_FAILURE;
	}

	if (NULL == waiter) {
		fprintf(stderr, "need waiter\n");
		return EXIT_FAILURE;
	}

	if ((fstab != NULL && NULL == chrootdir) ||
	    (NULL == fstab && chrootdir != NULL)) {
		fprintf(stderr,
		        "--chrootdir and --fstab are required together\n");
		return EXIT_FAILURE;
	}

	if (traceme) {
		if (-1 == ptrace(PTRACE_TRACEME, (pid_t)0, (void *)NULL,
		                 (void *)NULL)) {
			perror("ptrace");
			return EXIT_FAILURE;
		}

		/* Register with the parent */
		if (-1 == raise(SIGSTOP)) {
			perror("raise");
			return EXIT_FAILURE;
		}
	}

	char const *process_name = argv[0U];

	char const **command = (char const **)argv + 1U + command_start;

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

	char *command_dup = strdup(command[0U]);
	if (NULL == command_dup) {
		perror("strdup");
		return EXIT_FAILURE;
	}

	char *waiter_dup = strdup(waiter);
	if (NULL == waiter_dup) {
		perror("strdup");
		return EXIT_FAILURE;
	}

	char const *command_base = basename(command_dup);
	char const *waiter_base = basename(waiter_dup);

	char const *binary = command[0U];
	command[0U] = command_base;

	linted_ko stdin_reader;
	linted_ko stdin_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stdin_reader = xx[0U];
		stdin_writer = xx[1U];
	}

	linted_ko stdout_reader;
	linted_ko stdout_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stdout_reader = xx[0U];
		stdout_writer = xx[1U];
	}

	linted_ko stderr_reader;
	linted_ko stderr_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stderr_reader = xx[0U];
		stderr_writer = xx[1U];
	}

	size_t mount_args_size = 0U;
	struct mount_args *mount_args = NULL;
	if (fstab != NULL) {
		FILE *fstab_file;
		{
			FILE *xx;
			errnum = my_setmntentat(&xx, cwd, fstab, "re");
			if (errnum != 0) {
				errno = errnum;
				perror("setmntent");
				return EXIT_FAILURE;
			}
			fstab_file = xx;
		}

		for (;;) {
			errno = 0;
			struct mntent *entry = getmntent(fstab_file);
			if (NULL == entry) {
				errnum = errno;
				if (errnum != 0) {
					perror("getmntent");
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
					errno = errnum;
					perror("parse_mount_opts");
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
					errno = errnum;
					perror("linted_mem_realloc_array");
					return EXIT_FAILURE;
				}
				mount_args = xx;
			}

			if (fsname != NULL) {
				fsname = strdup(fsname);
				if (NULL == fsname) {
					perror("strdup");
					return EXIT_FAILURE;
				}
			}

			if (dir != NULL) {
				dir = strdup(dir);
				if (NULL == dir) {
					perror("strdup");
					return EXIT_FAILURE;
				}
			}

			if (type != NULL) {
				type = strdup(type);
				if (NULL == type) {
					perror("strdup");
					return EXIT_FAILURE;
				}
			}

			if (data != NULL) {
				data = strdup(data);
				if (NULL == data) {
					perror("strdup");
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
			perror("endmntent");
			return EXIT_FAILURE;
		}
	}

	cap_t caps = NULL;
	if (drop_caps) {
		caps = cap_get_proc();
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
	}

	if (priority != NULL) {
		if (-1 == setpriority(PRIO_PROCESS, 0, atoi(priority))) {
			perror("setpriority");
			return EXIT_FAILURE;
		}
	}

	char **env_copy = NULL;
	size_t env_size = 0U;
	for (char const *const *env = (char const * const *)environ;
	     *env != NULL; ++env)
		++env_size;

	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, env_size + 3U,
		                                sizeof env_copy[0U]);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_mem_alloc_array");
			return EXIT_FAILURE;
		}
		env_copy = xx;
	}

	for (size_t ii = 0U; ii < env_size; ++ii)
		env_copy[2U + ii] = environ[ii];

	char const *listen_fds = getenv("LISTEN_FDS");

	size_t num_fds = NULL == listen_fds ? 0U : atoi(listen_fds);
	char listen_fds_str[] = "LISTEN_FDS=XXXXXXXXXXXXXXXXXX";
	char listen_pid_str[] = "LISTEN_PID=XXXXXXXXXXXXXXXXXX";

	sprintf(listen_fds_str, "LISTEN_FDS=%lu", num_fds);

	env_copy[0U] = listen_fds_str;
	env_copy[1U] = listen_pid_str;
	env_copy[env_size + 2U] = NULL;

	gid_t gid = getgid();
	uid_t uid = getuid();

	gid_t mapped_gid = gid;
	uid_t mapped_uid = uid;

	char uid_map[] = "XXXXXXXXXXXXX XXXXXXXXXXXXX 1\n";
	char gid_map[] = "XXXXXXXXXXXXX XXXXXXXXXXXXX 1\n";

	sprintf(uid_map, "%i %i 1\n", mapped_uid, uid);
	sprintf(gid_map, "%i %i 1\n", mapped_gid, gid);

	linted_ko err_reader;
	linted_ko err_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
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
		                        .listen_pid_str = listen_pid_str,
		                        .listen_fds_str = listen_fds_str,
		                        .stdin_writer = stdin_writer,
		                        .stdout_reader = stdout_reader,
		                        .stderr_reader = stderr_reader,
		                        .stdin_reader = stdin_reader,
		                        .stdout_writer = stdout_writer,
		                        .stderr_writer = stderr_writer,
		                        .waiter_base = waiter_base,
		                        .waiter = waiter,
		                        .env_copy =
		                            (char const * const *)env_copy,
		                        .command = command,
		                        .binary = binary,
		                        .num_fds = num_fds };

	pid_t child =
	    safe_vclone(SIGCHLD | clone_flags, first_fork_routine, &args);
	if (-1 == child) {
		perror("clone");
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
		errno = errnum;
		perror("spawning");
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
				perror("waitpid");
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
	struct first_fork_args *args = arg;

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
	char *listen_pid_str = args->listen_pid_str;
	char *listen_fds_str = args->listen_fds_str;
	linted_ko stdin_writer = args->stdin_writer;
	linted_ko stdout_reader = args->stdout_reader;
	linted_ko stderr_reader = args->stderr_reader;
	linted_ko stdin_reader = args->stdin_reader;
	linted_ko stdout_writer = args->stdout_writer;
	linted_ko stderr_writer = args->stderr_writer;
	char const *waiter = args->waiter;
	char const *waiter_base = args->waiter_base;
	char const *const *env_copy = args->env_copy;
	char const *const *command = args->command;
	char const *binary = args->binary;
	size_t num_fds = args->num_fds;

	linted_error errnum = do_first_fork(
	    err_reader, uid_map, gid_map, clone_flags, cwd, chrootdir,
	    chdir_path, caps, mount_args, mount_args_size, no_new_privs,
	    listen_pid_str, listen_fds_str, stdin_writer, stdout_reader,
	    stderr_reader, stdin_reader, stdout_writer, stderr_writer, waiter,
	    waiter_base, env_copy, command, binary, num_fds);
	exit_with_error(err_writer, errnum);
	/* Never reached */
	return 0;
}

static linted_error
do_first_fork(linted_ko err_reader, char const *uid_map, char const *gid_map,
              unsigned long clone_flags, linted_ko cwd, char const *chrootdir,
              char const *chdir_path, cap_t caps, struct mount_args *mount_args,
              size_t mount_args_size, bool no_new_privs, char *listen_pid_str,
              char *listen_fds_str, linted_ko stdin_writer,
              linted_ko stdout_reader, linted_ko stderr_reader,
              linted_ko stdin_reader, linted_ko stdout_writer,
              linted_ko stderr_writer, char const *waiter,
              char const *waiter_base, char const *const *env_copy,
              char const *const *command, char const *binary, size_t num_fds)
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
		                         .stdin_reader = stdin_reader,
		                         .stdout_writer = stdout_writer,
		                         .stderr_writer = stderr_writer,
		                         .listen_pid_str = listen_pid_str,
		                         .binary = binary,
		                         .argv = command,
		                         .env = env_copy,
		                         .no_new_privs = no_new_privs };
	pid_t grand_child = safe_vfork(second_fork_routine, &args);
	if (-1 == grand_child)
		return errno;

	linted_ko_close(vfork_err_writer);

	linted_ko_close(stdin_reader);
	linted_ko_close(stdout_writer);
	linted_ko_close(stderr_writer);

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

	for (size_t ii = 0U; ii < num_fds; ++ii)
		linted_ko_close(3 + ii);

	if (-1 == dup2(stdin_writer, 3))
		return errno;
	if (-1 == dup2(stdout_reader, 4))
		return errno;
	if (-1 == dup2(stderr_reader, 5))
		return errno;

	pid_to_str(listen_fds_str + strlen("LISTEN_FDS="), 3);
	pid_to_str(listen_pid_str + strlen("LISTEN_PID="), real_getpid());

	char const *arguments[] = { waiter_base, NULL };
	execve(waiter, (char * const *)arguments, (char * const *)env_copy);
	return errno;
}

static int second_fork_routine(void *arg)
{
	struct second_fork_args *args = arg;

	linted_ko err_writer = args->err_writer;
	linted_ko stdin_reader = args->stdin_reader;
	linted_ko stdout_writer = args->stdout_writer;
	linted_ko stderr_writer = args->stderr_writer;
	char *listen_pid_str = args->listen_pid_str;
	char const *binary = args->binary;
	char const *const *argv = args->argv;
	char const *const *env = args->env;
	bool no_new_privs = args->no_new_privs;

	linted_error errnum =
	    do_second_fork(stdin_reader, stdout_writer, stderr_writer,
	                   listen_pid_str, binary, argv, env, no_new_privs);
	exit_with_error(err_writer, errnum);
	return 0;
}

static linted_error do_second_fork(linted_ko stdin_reader,
                                   linted_ko stdout_writer,
                                   linted_ko stderr_writer,
                                   char *listen_pid_str, char const *binary,
                                   char const *const *argv,
                                   char const *const *env, bool no_new_privs)
{
	pid_to_str(listen_pid_str + strlen("LISTEN_PID="), real_getpid());

	if (-1 == dup2(stdin_reader, STDIN_FILENO))
		return errno;

	if (-1 == dup2(stdout_writer, STDOUT_FILENO))
		return errno;

	if (-1 == dup2(stderr_writer, STDERR_FILENO))
		return errno;

	if (-1 == setsid())
		return errno;

	/* Do seccomp filter last of all */
	if (no_new_privs) {
		linted_error errnum = set_seccomp(&default_filter);
		if (errnum != 0)
			return errnum;
	}

	execve(binary, (char * const *)argv, (char * const *)env);
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
			if (-1 == mkdir(dir, S_IRWXU))
				if (errno != EEXIST)
					return errno;
		} else if (touch_flag) {
			if (-1 == mknod(dir, S_IRWXU | S_IFREG, 0))
				if (errno != EEXIST)
					return errno;
		}

		if (nomount_flag)
			continue;

		if (-1 == mount(fsname, dir, type, mountflags, data))
			return errno;

		if (0 == (mountflags & MS_BIND))
			continue;

		if (0 == (mountflags & ~(MS_BIND | MS_SHARED | MS_SLAVE |
		                         MS_PRIVATE | MS_UNBINDABLE)) &&
		    NULL == data)
			continue;

		mountflags |= MS_REMOUNT;
		if (-1 == mount(fsname, dir, type, mountflags, data))
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

	if (((bind && rec) || (bind && shared)) &&
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

static void exit_with_error(linted_ko writer, linted_error errnum)
{
	linted_io_write_all(writer, NULL, &errnum, sizeof errnum);
	_Exit(EXIT_FAILURE);
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

	pid_t child = vfork();
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

	pid_t child =
	    clone(f, stack_start, CLONE_VFORK | CLONE_VM | clone_flags, arg);
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

static pid_t real_getpid(void)
{
	return syscall(__NR_getpid);
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
