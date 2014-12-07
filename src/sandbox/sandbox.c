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
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/poll.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/ptrace.h>
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
 *
 * @todo Use vfork instead of fork for the first fork
 */

enum {
	STOP_OPTIONS,
	HELP,
	VERSION_OPTION,
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

static struct sock_fprog const default_filter;

static char const *const argstrs[] = {
	    /**/[STOP_OPTIONS] = "--",
	    /**/ [HELP] = "--help",
	    /**/ [VERSION_OPTION] = "--version",
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

static pid_t do_vfork(linted_ko err_writer, linted_ko stdin_reader,
                      linted_ko stdout_writer, linted_ko stderr_writer,
                      char *listen_pid_str, char const *const *argv,
                      char const *const *env, bool no_new_privs);

static void exit_with_error(linted_ko writer, linted_error errnum);

static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);
static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type);

static void set_id_maps(linted_ko err_writer, uid_t mapped_uid, uid_t uid,
                        gid_t mapped_gid, gid_t gid);
static void chroot_process(linted_ko err_writer, linted_ko cwd,
                           char const *chrootdir, FILE *file);

static void pid_to_str(char *buf, pid_t pid);

static linted_error set_child_subreaper(bool v);
static linted_error set_no_new_privs(bool b);
static linted_error set_seccomp(struct sock_fprog const *program);

static pid_t real_getpid(void);
static pid_t my_clone(unsigned long flags);
static int my_pivot_root(char const *new_root, char const *put_old);

int main(int argc, char *argv[])
{
	linted_error errnum;

	size_t arguments_length = argc;

	char const *listen_fds = getenv("LISTEN_FDS");
	if (NULL == listen_fds) {
		fprintf(stderr, "need LISTEN_FDS\n");
		return EXIT_FAILURE;
	}

	char const *bad_option = NULL;
	bool need_version = false;
	bool need_help = false;

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

	linted_ko stdin_reader;
	linted_ko stdin_writer;
	{
		linted_ko xx[2U];
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
		linted_ko xx[2U];
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
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		stderr_reader = xx[0U];
		stderr_writer = xx[1U];
	}

	FILE *fstab_file = NULL;
	if (fstab != NULL) {
		FILE *xx;
		errnum = my_setmntentat(&xx, cwd, fstab, "re");
		if (errnum != 0) {
			errno = errnum;
			perror("setmntent");
			return EXIT_FAILURE;
		}
		fstab_file = xx;
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

	size_t num_fds = atoi(listen_fds);
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

	linted_ko err_reader;
	linted_ko err_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			perror("pipe2");
			return EXIT_FAILURE;
		}
		err_reader = xx[0U];
		err_writer = xx[1U];
	}

	{
		pid_t child = clone_flags != 0U
		                  ? my_clone(SIGCHLD | clone_flags)
		                  : fork();

		if (-1 == child) {
			perror("clone");
			return EXIT_FAILURE;
		}

		if (child != 0) {
			linted_ko_close(err_writer);

			{
				size_t xx;
				linted_error yy;
				errnum = linted_io_read_all(err_reader, &xx,
				                            &yy, sizeof yy);
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
				_Exit(EXIT_FAILURE);
			}

			_Exit(EXIT_SUCCESS);
		}
	}
	linted_ko_close(err_reader);

	/* First things first set the id mapping */
	if ((clone_flags & CLONE_NEWUSER) != 0) {
		set_id_maps(err_writer, mapped_uid, uid, mapped_gid, gid);

		if (-1 == setgroups(0U, NULL))
			exit_with_error(err_writer, errno);
	}

	if (fstab_file != NULL)
		chroot_process(err_writer, cwd, chrootdir, fstab_file);

	if (chdir_path != NULL) {
		if (-1 == chdir(chdir_path))
			exit_with_error(err_writer, errno);
	}

	/*
	 * This isn't needed if CLONE_NEWPID was used but it doesn't
	 * hurt,
	 */
	errnum = set_child_subreaper(true);
	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	/* Drop all capabilities I might possibly have. Note that
	 * currently we do not use PR_SET_KEEPCAPS and do not map our
	 * sandboxed user to root but if we did in the future we would
	 * need this.
	 */

	if (caps != NULL) {
		if (-1 == cap_set_proc(caps))
			exit_with_error(err_writer, errno);
	}

	{
		sigset_t sigset;
		sigemptyset(&sigset);
		sigaddset(&sigset, SIGCHLD);
		errnum = pthread_sigmask(SIG_UNBLOCK, &sigset, NULL);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	sigset_t sigchld_unblocked;
	{
		sigset_t sigset;
		sigemptyset(&sigset);
		sigaddset(&sigset, SIGCHLD);
		errnum =
		    pthread_sigmask(SIG_BLOCK, &sigset, &sigchld_unblocked);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	linted_ko vfork_err_reader;
	linted_ko vfork_err_writer;
	{
		linted_ko xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK))
			exit_with_error(err_writer, errno);
		vfork_err_reader = xx[0U];
		vfork_err_writer = xx[1U];
	}

	if (no_new_privs) {
		/* Must appear before the seccomp filter */
		errnum = set_no_new_privs(true);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	pid_t child = do_vfork(vfork_err_writer, stdin_reader, stdout_writer,
	                       stderr_writer, listen_pid_str,
	                       (char const * const *)command,
	                       (char const * const *)env_copy, no_new_privs);
	if (-1 == child)
		exit_with_error(err_writer, errno);

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
			exit_with_error(err_writer, errnum);

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			errnum = yy;
	}
	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	linted_ko_close(cwd);

	for (size_t ii = 0U; ii < num_fds; ++ii)
		linted_ko_close(3 + ii);

	if (-1 == dup2(stdin_writer, 3))
		exit_with_error(err_writer, errno);
	if (-1 == dup2(stdout_reader, 4))
		exit_with_error(err_writer, errno);
	if (-1 == dup2(stderr_reader, 5))
		exit_with_error(err_writer, errno);

	sprintf(listen_fds_str, "LISTEN_FDS=%i", 3);
	pid_to_str(listen_pid_str + strlen("LISTEN_PID="), real_getpid());

	char const *arguments[] = { waiter, NULL };
	execve(waiter, (char * const *)arguments, env_copy);
	perror("execve");
	return EXIT_FAILURE;
}

static pid_t do_vfork(linted_ko err_writer, linted_ko stdin_reader,
                      linted_ko stdout_writer, linted_ko stderr_writer,
                      char *listen_pid_str, char const *const *argv,
                      char const *const *env, bool no_new_privs)
{
	pid_t child = vfork();
	if (child != 0)
		return child;

	pid_to_str(listen_pid_str + strlen("LISTEN_PID="), real_getpid());

	/* Terminals are really ugly and horrible, avoid them. */
	int tty = open("/dev/tty", O_CLOEXEC);
	if (-1 == tty) {
		if (errno != ENXIO)
			exit_with_error(err_writer, errno);
	} else {
		if (-1 == ioctl(tty, TIOCNOTTY))
			exit_with_error(err_writer, errno);
	}

	if (-1 == dup2(stdin_reader, STDIN_FILENO))
		exit_with_error(err_writer, errno);

	if (-1 == dup2(stdout_writer, STDOUT_FILENO))
		exit_with_error(err_writer, errno);

	if (-1 == dup2(stderr_writer, STDERR_FILENO))
		exit_with_error(err_writer, errno);

	if (-1 == setsid())
		exit_with_error(err_writer, errno);

	/* Do seccomp filter last of all */
	if (no_new_privs) {
		linted_error errnum = set_seccomp(&default_filter);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	execve(argv[0U], (char * const *)argv, (char * const *)env);
	exit_with_error(err_writer, errno);

	return 0;
}

static void set_id_maps(linted_ko err_writer, uid_t mapped_uid, uid_t uid,
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
				exit_with_error(err_writer, errnum);
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_uid, uid);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);

		errnum = linted_ko_close(file);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD,
			                        "/proc/self/gid_map",
			                        LINTED_KO_WRONLY);
			if (errnum != 0)
				exit_with_error(err_writer, errnum);
			file = xx;
		}

		errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
		                                mapped_gid, gid);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);

		errnum = linted_ko_close(file);
		if (errnum != 0)
			exit_with_error(err_writer, errnum);
	}
}

static void chroot_process(linted_ko err_writer, linted_ko cwd,
                           char const *chrootdir, FILE *fstab)
{
	linted_error errnum;

	if (-1 == mount(NULL, chrootdir, "tmpfs", 0, "mode=700"))
		exit_with_error(err_writer, errno);

	if (-1 == chdir(chrootdir))
		exit_with_error(err_writer, errno);

	for (;;) {
		errno = 0;
		struct mntent *entry = getmntent(fstab);
		if (NULL == entry) {
			errnum = errno;
			if (errnum != 0)
				exit_with_error(err_writer, errnum);
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

		if (mkdir_flag) {
			if (-1 == mkdir(dir, S_IRWXU))
				exit_with_error(err_writer, errno);
		} else if (touch_flag) {
			if (-1 == mknod(dir, S_IRWXU | S_IFREG, 0))
				exit_with_error(err_writer, errno);
		}

		if (-1 == mount(fsname, dir, type, mountflags, data))
			exit_with_error(err_writer, errno);

		if ((mountflags & MS_BIND) != 0U) {
			mountflags |= MS_REMOUNT;
			if (-1 == mount(fsname, dir, type, mountflags, data))
				exit_with_error(err_writer, errno);
		}
	}

close_file:
	if (endmntent(fstab) != 1)
		exit_with_error(err_writer, errno);

	/* Magic incantation that clears up /proc/mounts more than
	 * mount MS_MOVE
	 */
	int old_root = open("/", O_DIRECTORY | O_CLOEXEC);
	if (-1 == old_root)
		exit_with_error(err_writer, errno);

	if (-1 == my_pivot_root(".", "."))
		exit_with_error(err_writer, errno);

	/* pivot_root() may or may not affect its current working
	 * directory.  It is therefore recommended to call chdir("/")
	 * immediately after pivot_root().
	 *
	 * - http://man7.org/linux/man-pages/man2/pivot_root.2.html
	 */

	if (-1 == fchdir(old_root))
		exit_with_error(err_writer, errno);

	errnum = linted_ko_close(old_root);
	if (errnum != 0)
		exit_with_error(err_writer, errnum);

	if (-1 == umount2(".", MNT_DETACH))
		exit_with_error(err_writer, errno);

	if (-1 == chdir("/"))
		exit_with_error(err_writer, errno);
}

enum { MKDIR, TOUCH, BIND, RBIND, RO, RW, SUID, NOSUID, NODEV, NOEXEC };

static char const *const mount_options[] = {[MKDIR] = "mkdir",        /**/
	                                    [TOUCH] = "touch",        /**/
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

static pid_t real_getpid(void)
{
	return syscall(__NR_getpid);
}

/* Unfortunately, the clone system call interface varies a lot between
 * architectures on Linux.
 */
#if defined __amd64__ || defined __i386__
static pid_t my_clone(unsigned long flags)
{
	return syscall(__NR_clone, flags, NULL, NULL, NULL, NULL);
}
#else
#error No clone implementation has been defined for this architecture
#endif

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
