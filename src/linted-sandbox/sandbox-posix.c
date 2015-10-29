/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define _GNU_SOURCE

#include "config.h"

#include "linted/dir.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/fifo.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/path.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/util.h"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <mntent.h>
#include <wordexp.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <syscall.h>
#include <unistd.h>

#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>

#ifndef __NR_execveat
#if defined __amd64__
#define __NR_execveat 322
#elif defined __i386__
#define __NR_execveat 358
#else
#error No execveat system call number is defined for this platform
#endif
#endif

/**
 * @file
 *
 * Sandbox applications.
 *
 * @bug `mount("/home", "home", "none", MS_SHARED | MS_BIND)` doesn't
 *       work if `/home` has a userspace filesystem mounted inside of
 *       it.
 */

enum { STOP_OPTIONS,
       HELP,
       VERSION_OPTION,
       WAIT,
       TRACEME,
       DROP_CAPS,
       NO_NEW_PRIVS,
       RLIMIT,
       LIMIT_NO_FILE,
       LIMIT_LOCKS,
       LIMIT_MSGQUEUE,
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
       NEWUTS_ARG };

struct mount_args {
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
        /**/ [RLIMIT] = "--rlimit",
        /**/ [LIMIT_NO_FILE] = "--limit-no-file",
        /**/ [LIMIT_LOCKS] = "--limit-locks",
        /**/ [LIMIT_MSGQUEUE] = "--limit-msgqueue",
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
        /**/ [NEWUTS_ARG] = "--clone-newuts"};

struct first_fork_args {
	char const *chdir_path;
	cap_t caps;
	struct mount_args *mount_args;
	size_t mount_args_size;
	char const *waiter;
	char const *waiter_base;
	char const *const *command;
	char const *binary;
	int limit_no_file;
	linted_ko err_writer;
	linted_ko logger_reader;
	linted_ko logger_writer;
	bool use_seccomp : 1U;
};

struct second_fork_args {
	char const *const *argv;
	char const *binary;
	int limit_no_file;
	linted_ko err_writer;
	linted_ko logger_writer;
	bool use_seccomp : 1U;
};

static int first_fork_routine(void *arg);
static int second_fork_routine(void *arg);

static linted_error
parse_mount_opts(char const *opts, bool *mkdir_flagp, bool *touch_flagp,
                 bool *nomount_flagp, unsigned long *mountflagsp,
                 char const **leftoversp);

static linted_error set_id_maps(char const *uid_map,
                                char const *gid_map);

static linted_error
mount_directories(struct mount_args const *mount_args, size_t size);

static linted_error set_child_subreaper(bool v);
static linted_error set_no_new_privs(bool b);

static linted_error parse_int(char const *str, int *resultp);

static linted_error safe_dup2(int oldfd, int newfd);
static pid_t safe_vfork(int (*f)(void *), void *args);
static int my_pivot_root(char const *new_root, char const *put_old);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-sandbox",
    .dont_init_signals = true,
    .dont_fork_thread = true};

static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err;

	size_t arguments_length = argc;

	char const *bad_option = 0;
	bool need_version = false;
	bool need_help = false;

	bool wait = false;
	bool traceme = false;
	bool no_new_privs = false;
	bool drop_caps = false;

	bool clone_newuser = false;
	bool clone_newpid = false;
	bool clone_newipc = false;
	bool clone_newnet = false;
	bool clone_newns = false;
	bool clone_newuts = false;

	char const *chdir_path = 0;
	char const *priority = 0;
	char const *limit_no_file = 0;
	char const *limit_locks = 0;
	char const *limit_msgqueue = 0;
	char const *chrootdir = 0;
	char const *fstab = 0;
	char const *waiter = 0;
	bool have_command = false;
	size_t command_start;

	for (size_t ii = 1U; ii < arguments_length; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(argstrs);
		     ++jj) {
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

		case LIMIT_NO_FILE:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			limit_no_file = argv[ii];
			break;

		case LIMIT_LOCKS:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			limit_locks = argv[ii];
			break;

		case LIMIT_MSGQUEUE:
			++ii;
			if (ii >= arguments_length)
				goto exit_loop;
			limit_msgqueue = argv[ii];
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
			clone_newuser = true;
			break;

		case NEWPID_ARG:
			clone_newpid = true;
			break;

		case NEWIPC_ARG:
			clone_newipc = true;
			break;

		case NEWNET_ARG:
			clone_newnet = true;
			break;

		case NEWNS_ARG:
			clone_newns = true;
			break;

		case NEWUTS_ARG:
			clone_newuts = true;
			break;
		}
	}
exit_loop:
	if (!have_command) {
		linted_log(LINTED_LOG_ERROR, "need command");
		return EXIT_FAILURE;
	}

	if (bad_option != 0) {
		linted_log(LINTED_LOG_ERROR, "bad option: %s",
		           bad_option);
		return EXIT_FAILURE;
	}

	if (0 == waiter) {
		linted_log(LINTED_LOG_ERROR, "need waiter");
		return EXIT_FAILURE;
	}

	if ((fstab != 0 && 0 == chrootdir) ||
	    (0 == fstab && chrootdir != 0)) {
		linted_log(
		    LINTED_LOG_ERROR,
		    "--chrootdir and --fstab are required together");
		return EXIT_FAILURE;
	}

	if (traceme) {
		/* Register with the parent */
		if (-1 == raise(SIGSTOP)) {
			linted_log(LINTED_LOG_ERROR, "raise: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	char const *const *command = argv + 1U + command_start;
	size_t command_size = argc - (1U + command_start);
	char **command_dup;
	{
		void *xx;
		err = linted_mem_alloc_array(&xx, command_size + 1U,
		                             sizeof command_dup[0U]);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_mem_alloc_array: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		command_dup = xx;
	}
	command_dup[command_size] = 0;
	for (size_t ii = 0U; ii < command_size; ++ii) {
		err = linted_str_dup(&command_dup[ii], command[ii]);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_str_dup: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	char *waiter_base;
	{
		char *xx;
		err = linted_path_base(&xx, waiter);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_path_base: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		waiter_base = xx;
	}

	char *command_base;
	{
		char *xx;
		err = linted_path_base(&xx, command_dup[0U]);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_path_base: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		command_base = xx;
	}
	linted_mem_free(command_dup[0U]);
	command_dup[0U] = command_base;

	char const *binary = command[0U];

	int prio_val;
	if (priority != 0) {
		int xx;
		err = parse_int(priority, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR, "parse_int: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		prio_val = xx;
	}

	int limit_no_file_val = -1;
	if (limit_no_file != 0) {
		int xx;
		err = parse_int(limit_no_file, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR, "parse_int: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		limit_no_file_val = xx;
	}

	int limit_locks_val = -1;
	if (limit_locks != 0) {
		int xx;
		err = parse_int(limit_locks, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR, "parse_int: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		limit_locks_val = xx;
	}

	int limit_msgqueue_val = -1;
	if (limit_msgqueue != 0) {
		int xx;
		err = parse_int(limit_msgqueue, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR, "parse_int: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		limit_msgqueue_val = xx;
	}

	size_t mount_args_size = 0U;
	struct mount_args *mount_args = 0;
	if (fstab != 0) {
		FILE *fstab_file = fopen(fstab, "re");
		if (0 == fstab_file) {
			linted_log(LINTED_LOG_ERROR, "fopen(%s): %s",
			           fstab, linted_error_string(errno));
			return EXIT_FAILURE;
		}

		char *buf = 0;
		size_t buf_size = 0U;
		for (;;) {
			errno = 0;
			size_t line_size;
			{
				char *xx = buf;
				size_t yy = buf_size;
				ssize_t zz =
				    getline(&xx, &yy, fstab_file);
				if (zz < 0) {
					err = errno;
					if (err != 0) {
						linted_log(
						    LINTED_LOG_ERROR,
						    "getlines: %s",
						    linted_error_string(
						        err));
						return EXIT_FAILURE;
					}
					break;
				}
				buf = xx;
				buf_size = yy;
				line_size = zz;
			}

			if ('#' == buf[0U])
				continue;

			if ('\0' == buf[0U])
				continue;

			if ('\n' == buf[line_size - 1U])
				buf[line_size - 1U] = '\0';

			wordexp_t expr;
			switch (wordexp(buf, &expr, WRDE_NOCMD)) {
			case WRDE_BADCHAR:
			case WRDE_CMDSUB:
			case WRDE_SYNTAX:
				err = EINVAL;
				break;

			case WRDE_NOSPACE:
				err = ENOMEM;
				break;
			}
			if (err != 0) {
				linted_log(LINTED_LOG_ERROR,
				           "wordexp(%s): %s", buf,
				           linted_error_string(err));
				return EXIT_FAILURE;
			}
			char const *const *words =
			    (char const *const *)expr.we_wordv;

			char const *fsname = 0;
			char const *dir = 0;
			char const *type = 0;
			char const *opts = 0;

			fsname = words[0U];
			if (0 == fsname)
				goto free_words;

			dir = words[1U];
			if (0 == dir)
				goto parse_expr;

			type = words[2U];
			if (0 == type)
				goto parse_expr;

			opts = words[3U];
			if (0 == type)
				goto parse_expr;

		parse_expr:
			if (0 == strcmp("none", fsname))
				fsname = 0;

			if (0 == strcmp("none", opts))
				opts = 0;

			bool mkdir_flag = false;
			bool touch_flag = false;
			bool nomount_flag = false;
			unsigned long mountflags = 0U;
			char const *data = 0;
			if (opts != 0) {
				bool xx;
				bool yy;
				bool zz;
				unsigned long ww;
				char const *uu;
				err = parse_mount_opts(opts, &xx, &yy,
				                       &zz, &ww, &uu);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "parse_mount_opts: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				mkdir_flag = xx;
				touch_flag = yy;
				nomount_flag = zz;
				mountflags = ww;
				data = uu;
			}

			size_t new_mount_args_size =
			    mount_args_size + 1U;
			{
				void *xx;
				err = linted_mem_realloc_array(
				    &xx, mount_args,
				    new_mount_args_size,
				    sizeof mount_args[0U]);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_mem_realloc_"
					    "array: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				mount_args = xx;
			}

			if (fsname != 0) {
				char *xx;
				err = linted_str_dup(&xx, fsname);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_str_dup: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				fsname = xx;
			}

			if (dir != 0) {
				char *xx;
				err = linted_str_dup(&xx, dir);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_str_dup: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				dir = xx;
			}

			if (type != 0) {
				char *xx;
				err = linted_str_dup(&xx, type);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_str_dup: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				type = xx;
			}

			if (data != 0) {
				char *xx;
				err = linted_str_dup(&xx, data);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_str_dup: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				data = xx;
			}

			struct mount_args *new_mount_arg =
			    &mount_args[mount_args_size];
			new_mount_arg->fsname = fsname;
			new_mount_arg->dir = dir;
			new_mount_arg->type = type;
			new_mount_arg->data = data;
			new_mount_arg->mountflags = mountflags;
			new_mount_arg->mkdir_flag = mkdir_flag;
			new_mount_arg->touch_flag = touch_flag;
			new_mount_arg->nomount_flag = nomount_flag;
			mount_args_size = new_mount_args_size;

		free_words:
			wordfree(&expr);
		}

		if (fclose(fstab_file) != 0) {
			linted_log(LINTED_LOG_ERROR, "fclose: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	gid_t gid = getgid();
	uid_t uid = getuid();

	gid_t mapped_gid = gid;
	uid_t mapped_uid = uid;

	char const *uid_map;
	char const *gid_map;

	{
		char *xx;
		if (-1 == asprintf(&xx, "%i %i 1\n", mapped_uid, uid)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		uid_map = xx;
	}

	{
		char *xx;
		if (-1 == asprintf(&xx, "%i %i 1\n", mapped_gid, gid)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		gid_map = xx;
	}

	linted_ko logger_reader;
	linted_ko logger_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC)) {
			linted_log(LINTED_LOG_ERROR, "pipe2: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		logger_reader = xx[0U];
		logger_writer = xx[1U];
	}

	linted_ko err_reader;
	linted_ko err_writer;
	{
		linted_ko xx;
		linted_ko yy;
		err = linted_fifo_pair(&xx, &yy, 0);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_fifo_pair: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		err_reader = xx;
		err_writer = yy;
	}

	cap_t caps = 0;
	if (drop_caps) {
		caps = cap_get_proc();
		if (0 == caps) {
			linted_log(LINTED_LOG_ERROR, "cap_get_proc: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
			linted_log(LINTED_LOG_ERROR,
			           "cap_clear_flag: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
			linted_log(LINTED_LOG_ERROR,
			           "cap_clear_flag: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_INHERITABLE)) {
			linted_log(LINTED_LOG_ERROR,
			           "cap_clear_flag: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	/* Only start actually dropping privileges now */

	if (priority != 0) {
		if (-1 == setpriority(PRIO_PROCESS, 0, prio_val)) {
			linted_log(LINTED_LOG_ERROR, "setpriority: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (no_new_privs) {
		/* Must appear before the seccomp filter */
		err = set_no_new_privs(true);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR, "prctl: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	int clone_flags = 0;
	if (clone_newuser)
		clone_flags |= CLONE_NEWUSER;
	if (clone_newipc)
		clone_flags |= CLONE_NEWIPC;
	if (clone_newns)
		clone_flags |= CLONE_NEWNS;
	if (clone_newuts)
		clone_flags |= CLONE_NEWUTS;
	if (clone_newnet)
		clone_flags |= CLONE_NEWNET;
	if (clone_newpid)
		clone_flags |= CLONE_NEWPID;

	if (clone_flags != 0) {
		if (-1 == unshare(clone_flags)) {
			linted_log(LINTED_LOG_ERROR, "unshare: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (clone_newuser) {
		err = set_id_maps(uid_map, gid_map);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR, "set_id_maps: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (chrootdir != 0) {
		if (-1 == mount(chrootdir, chrootdir, 0, MS_BIND, 0)) {
			linted_log(LINTED_LOG_ERROR, "mount: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == chdir(chrootdir)) {
			linted_log(LINTED_LOG_ERROR, "chdir: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (limit_locks_val >= 0) {
		struct rlimit const lim = {
		    .rlim_cur = limit_locks_val,
		    .rlim_max = limit_locks_val,
		};

		if (-1 == setrlimit(RLIMIT_LOCKS, &lim)) {
			linted_log(RLIMIT_LOCKS, "setrlimit: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (limit_msgqueue_val >= 0) {
		struct rlimit const lim = {
		    .rlim_cur = limit_msgqueue_val,
		    .rlim_max = limit_msgqueue_val,
		};

		if (-1 == setrlimit(RLIMIT_MSGQUEUE, &lim)) {
			linted_log(RLIMIT_MSGQUEUE, "setrlimit: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	pid_t child;
	{
		struct first_fork_args args = {
		    .err_writer = err_writer,
		    .limit_no_file = limit_no_file_val,
		    .logger_reader = logger_reader,
		    .logger_writer = logger_writer,
		    .chdir_path = chdir_path,
		    .caps = caps,
		    .mount_args = mount_args,
		    .mount_args_size = mount_args_size,
		    .use_seccomp = no_new_privs,
		    .waiter_base = waiter_base,
		    .waiter = waiter,
		    .command = command,
		    .binary = binary};
		child = safe_vfork(first_fork_routine, &args);
	}
	if (-1 == child) {
		linted_log(LINTED_LOG_ERROR, "clone: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	linted_ko_close(err_writer);

	{
		size_t xx;
		linted_error yy;
		err =
		    linted_io_read_all(err_reader, &xx, &yy, sizeof yy);
		if (err != 0)
			goto close_err_reader;
		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			err = yy;
	}
close_err_reader:
	linted_ko_close(err_reader);

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "spawning: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	if (!wait)
		return EXIT_SUCCESS;

	linted_ko_close(LINTED_KO_STDIN);
	linted_ko_close(LINTED_KO_STDOUT);

	for (;;) {
		int xx;
		switch (waitpid(child, &xx, 0)) {
		case -1:
			switch (errno) {
			case EINTR:
				continue;
			default:
				linted_log(LINTED_LOG_ERROR,
				           "waitpid: %s",
				           linted_error_string(errno));
				return EXIT_FAILURE;
			}

		default:
			return EXIT_SUCCESS;
		}
	}
}

static linted_error set_id_maps(char const *uid_map,
                                char const *gid_map)
{
	linted_error err;

	linted_ko set_groups;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     "/proc/thread-self/setgroups",
		                     LINTED_KO_WRONLY);
		if (err != 0)
			return err;
		set_groups = xx;
	}

	err = linted_io_write_string(set_groups, 0, "deny");
	if (err != 0)
		return err;

	err = linted_ko_close(set_groups);
	if (err != 0)
		return err;

	/* Note that writing to uid_map and gid_map will fail if the
	 * binary is not dumpable.  DON'T set the process dumpable and
	 * fail if the process is nondumpable as presumably the
	 * invoker of the process had good reasons to have the process
	 * nondumpable.
	 */
	linted_ko uid_file;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     "/proc/thread-self/uid_map",
		                     LINTED_KO_WRONLY);
		if (err != 0)
			return err;
		uid_file = xx;
	}

	err = linted_io_write_string(uid_file, 0, uid_map);
	if (err != 0)
		return err;

	err = linted_ko_close(uid_file);
	if (err != 0)
		return err;

	linted_ko gid_file;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     "/proc/thread-self/gid_map",
		                     LINTED_KO_WRONLY);
		if (err != 0)
			return err;
		gid_file = xx;
	}

	err = linted_io_write_string(gid_file, 0, gid_map);
	if (err != 0)
		return err;

	err = linted_ko_close(gid_file);
	if (err != 0)
		return err;

	return 0;
}

LINTED_NO_SANITIZE_ADDRESS static int
first_fork_routine(void *void_args)
{
	linted_error err = 0;

	struct first_fork_args const *first_fork_args = void_args;

	int limit_no_file = first_fork_args->limit_no_file;
	linted_ko err_writer = first_fork_args->err_writer;
	linted_ko logger_reader = first_fork_args->logger_reader;
	linted_ko logger_writer = first_fork_args->logger_writer;
	char const *chdir_path = first_fork_args->chdir_path;
	cap_t caps = first_fork_args->caps;
	struct mount_args *mount_args = first_fork_args->mount_args;
	size_t mount_args_size = first_fork_args->mount_args_size;
	bool use_seccomp = first_fork_args->use_seccomp;
	char const *waiter = first_fork_args->waiter;
	char const *waiter_base = first_fork_args->waiter_base;
	char const *const *command = first_fork_args->command;
	char const *binary = first_fork_args->binary;

	/* The setsid() function creates a new session, if the calling
	 * process is not a process group leader. Upon return the
	 * calling process will be the session leader of this new
	 * session, will be the process group leader of a new process
	 * group, and will have no controlling terminal.
	 *
	 * - setsid(2) The Single UNIX ® Specification, Version 2
	 */
	/* So we don't need to explicitly set that there is no
	 * controlling terminal. */
	if (-1 == setsid()) {
		err = errno;
		goto fail;
	}

	/* For some reason we must mount directories after we actually
	 * become PID 1 in the new pid namespace so that we can mount
	 * procfs */
	linted_ko waiter_ko;
	if (mount_args_size > 0U) {
		{
			linted_ko xx;
			err = linted_ko_open(&xx, LINTED_KO_CWD, waiter,
			                     0);
			if (err != 0)
				goto fail;
			waiter_ko = xx;
		}

		err = mount_directories(mount_args, mount_args_size);
		if (err != 0)
			goto fail;
	}

	if (chdir_path != 0) {
		if (-1 == chdir(chdir_path)) {
			err = errno;
			goto fail;
		}
	}

	/*
	 * This isn't needed if CLONE_NEWPID was used but it doesn't
	 * hurt,
	 */
	err = set_child_subreaper(true);
	if (err != 0)
		goto fail;

	/* Drop all capabilities I might possibly have. Note that
	 * currently we do not use PR_SET_KEEPCAPS and do not map our
	 * sandboxed user to root but if we did in the future we would
	 * need this.
	 */

	if (caps != 0) {
		if (-1 == cap_set_proc(caps)) {
			err = errno;
			goto fail;
		}
	}

	linted_ko vfork_err_reader;
	linted_ko vfork_err_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC | O_NONBLOCK)) {
			err = errno;
			goto fail;
		}
		vfork_err_reader = xx[0U];
		vfork_err_writer = xx[1U];
	}

	pid_t grand_child;
	struct second_fork_args args = {.err_writer = vfork_err_writer,
	                                .limit_no_file = limit_no_file,
	                                .logger_writer = logger_writer,
	                                .binary = binary,
	                                .argv = command,
	                                .use_seccomp = use_seccomp};
	grand_child = safe_vfork(second_fork_routine, &args);
	if (-1 == grand_child) {
		err = errno;
		goto fail;
	}

	linted_ko_close(vfork_err_writer);

	{
		size_t xx;
		linted_error yy;
		err = linted_io_read_all(vfork_err_reader, &xx, &yy,
		                         sizeof yy);
		if (err != 0)
			goto fail;

		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			err = yy;
	}
	if (err != 0)
		goto fail;

	err = safe_dup2(logger_reader, LINTED_KO_STDIN);
	if (err != 0)
		goto fail;

	if (mount_args_size > 0U) {
		char const *const arguments[] = {waiter_base, 0};
		syscall(__NR_execveat, (int)waiter_ko, "",
		        (char *const *)arguments, environ,
		        AT_EMPTY_PATH);
	} else {
		char const *const arguments[] = {waiter_base, 0};
		execve(waiter, (char *const *)arguments, environ);
	}
	err = errno;

fail : {
	linted_error xx = err;
	linted_io_write_all(err_writer, 0, &xx, sizeof xx);
}
	return EXIT_FAILURE;
}

LINTED_NO_SANITIZE_ADDRESS static int second_fork_routine(void *arg)
{
	linted_error err;

	struct second_fork_args const *args = arg;

	int limit_no_file = args->limit_no_file;
	linted_ko err_writer = args->err_writer;
	linted_ko logger_writer = args->logger_writer;
	char const *const *argv = args->argv;
	bool use_seccomp = args->use_seccomp;
	char const *binary = args->binary;

	err = safe_dup2(logger_writer, LINTED_KO_STDOUT);
	if (err != 0)
		goto fail;

	err = safe_dup2(logger_writer, LINTED_KO_STDERR);
	if (err != 0)
		goto fail;

	if (limit_no_file >= 0) {
		struct rlimit const lim = {
		    .rlim_cur = limit_no_file,
		    .rlim_max = limit_no_file,
		};

		if (-1 == setrlimit(RLIMIT_NOFILE, &lim)) {
			err = errno;
			LINTED_ASSUME(err != 0);

			LINTED_ASSERT(err != EINVAL);
			goto fail;
		}
	}

	/* Do seccomp filter last of all */
	if (use_seccomp) {
		if (-1 == prctl(PR_SET_SECCOMP,
		                (unsigned long)SECCOMP_MODE_FILTER,
		                &default_filter, 0UL, 0UL)) {
			err = errno;
			LINTED_ASSUME(err != 0);

			LINTED_ASSERT(err != EINVAL);
			goto fail;
		}
	}

	execve(binary, (char *const *)argv, environ);
	err = errno;

fail : {
	linted_error xx = err;
	linted_io_write_all(err_writer, 0, &xx, sizeof xx);
}
	return EXIT_FAILURE;
}

LINTED_NO_SANITIZE_ADDRESS static linted_error
mount_directories(struct mount_args const *mount_args, size_t size)
{
	linted_error err;

	for (size_t ii = 0U; ii < size; ++ii) {
		struct mount_args const *arg = &mount_args[ii];
		char const *fsname = arg->fsname;
		char const *dir = arg->dir;
		char const *type = arg->type;
		char const *data = arg->data;
		bool mkdir_flag = arg->mkdir_flag;
		bool touch_flag = arg->touch_flag;
		bool nomount_flag = arg->nomount_flag;
		unsigned long mountflags = arg->mountflags;

		if (mkdir_flag) {
			err = linted_dir_create(0, LINTED_KO_CWD, dir,
			                        0U, S_IRWXU);
			if (err != 0)
				return err;
		} else if (touch_flag) {
			err = linted_file_create(0, LINTED_KO_CWD, dir,
			                         0U, S_IRWXU);
			if (err != 0)
				return err;
		}

		if (nomount_flag)
			continue;

		unsigned long aliasflags =
		    mountflags & (MS_BIND | MS_SHARED | MS_SLAVE);
		if (0 == aliasflags) {
			if (-1 ==
			    mount(fsname, dir, type, mountflags, data))
				return errno;
			continue;
		}

		if (-1 == mount(fsname, dir, type, aliasflags, 0))
			return errno;

		if (0 == data && 0 == (mountflags & ~aliasflags))
			continue;

		if (-1 == mount(fsname, dir, type,
		                MS_REMOUNT | mountflags, data))
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

	err = linted_ko_close(old_root);
	if (err != 0)
		return err;

	if (-1 == umount2(".", MNT_DETACH))
		return errno;

	if (-1 == chdir("/"))
		return errno;

	return 0;
}

enum { MKDIR,
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
       NOEXEC };

static char const *const mount_options[] =
    {[MKDIR] = "mkdir",        /**/
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
     0};

static linted_error
parse_mount_opts(char const *opts, bool *mkdir_flagp, bool *touch_flagp,
                 bool *nomount_flagp, unsigned long *mountflagsp,
                 char const **leftoversp)
{
	linted_error err;

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
	char *leftovers = 0;

	char *subopts_str;
	{
		char *xx;
		err = linted_str_dup(&xx, opts);
		if (err != 0)
			return err;
		subopts_str = xx;
	}

	char *subopts = subopts_str;
	char *value = 0;

	while (*subopts != '\0') {
		int token;
		{
			char *xx = subopts;
			char *yy = value;
			token = getsubopt(
			    &xx, (char *const *)mount_options, &yy);
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

	/*
	 * Due to a completely idiotic kernel bug (see
	 * https://bugzilla.kernel.org/show_bug.cgi?id=24912) using a
	 *recursive
	 * bind mount as readonly would fail completely silently and
	 *there is
	 * no way to workaround this.
	 *
	 * Even after working around by remounting it will fail for the
	 * recursive case. For example, /home directory that is
	 *recursively
	 * bind mounted as readonly and that has encrypted user
	 *directories as
	 * an example. The /home directory will be readonly but the user
	 * directory /home/user will not be.
	 */

	if (bind && (rec || shared || slave) &&
	    (readonly || !suid || !dev || !exec))
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

LINTED_NO_SANITIZE_ADDRESS static linted_error
set_child_subreaper(bool v)
{
	linted_error err;

	if (-1 == prctl(PR_SET_CHILD_SUBREAPER, (unsigned long)v, 0UL,
	                0UL, 0UL)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	return 0;
}

static linted_error set_no_new_privs(bool b)
{
	linted_error err;

	if (-1 == prctl(PR_SET_NO_NEW_PRIVS, (unsigned long)b, 0UL, 0UL,
	                0UL)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		LINTED_ASSERT(err != EINVAL);
		return err;
	}

	return 0;
}

static linted_error safe_dup2(int oldfd, int newfd)
{
	linted_error err;
	/*
	 * The state of a file descriptor after close gives an EINTR
	 * error is unspecified by POSIX so this function avoids the
	 * problem by simply blocking all signals.
	 */

	sigset_t sigset;

	/* First use the signal set for the full set */
	sigfillset(&sigset);

	/* Then reuse the signal set for the old set */

	err = pthread_sigmask(SIG_BLOCK, &sigset, &sigset);
	if (err != 0)
		return err;

	if (-1 == dup2(oldfd, newfd)) {
		err = errno;
		LINTED_ASSUME(err != 0);
	} else {
		err = 0;
	}

	linted_error mask_err =
	    pthread_sigmask(SIG_SETMASK, &sigset, 0);
	if (0 == err)
		err = mask_err;

	return err;
}

/* Most compilers can't handle the weirdness of vfork so contain it in
 * a safe abstraction.
 */
LINTED_NOINLINE LINTED_NOCLONE LINTED_NO_SANITIZE_ADDRESS static pid_t
safe_vfork(int (*volatile f)(void *), void *volatile arg)
{
	pid_t child = vfork();
	if (0 == child)
		_Exit(f(arg));
	return child;
}

LINTED_NO_SANITIZE_ADDRESS static int
my_pivot_root(char const *new_root, char const *put_old)
{
	return syscall(__NR_pivot_root, new_root, put_old);
}

static linted_error parse_int(char const *str, int *resultp)
{
	linted_error err = 0;

	char start = str[0U];

	if (isspace(start))
		return EINVAL;
	if ('+' == start)
		return EINVAL;
	if ('-' == start)
		return ERANGE;

	errno = 0;
	long yy;
	char *endptr;
	{
		char *xx;
		yy = strtol(str, &xx, 10);
		endptr = xx;
	}
	err = errno;
	if (err != 0)
		return err;

	if (*endptr != '\0')
		return EINVAL;
	if (yy > INT_MAX)
		return ERANGE;

	*resultp = yy;
	return 0;
}

#if defined __amd64__
#include "sandbox-amd64.c"
#elif defined __i386__
#include "sandbox-i386.c"
#else
#error No default seccomp filter has been defined for this architecture
#endif
