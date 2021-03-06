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

#include "lntd/dir.h"
#include "lntd/error.h"
#include "lntd/execveat.h"
#include "lntd/fifo.h"
#include "lntd/file.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/locale.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/path.h"
#include "lntd/prctl.h"
#include "lntd/start.h"
#include "lntd/str.h"
#include "lntd/util.h"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <fcntl.h>
#include <limits.h>
#include <mntent.h>
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
#include <wordexp.h>

#include <linux/seccomp.h>
#include <seccomp.h>

#ifndef CLONE_NEWCGROUP
#define CLONE_NEWCGROUP 0x02000000
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
       LIMIT_NO_FILE,
       LIMIT_LOCKS,
       LIMIT_MSGQUEUE,
       LIMIT_MEMLOCK,
       CHDIR,
       PRIORITY,
       TIMER_SLACK,
       CHROOTDIR,
       FSTAB,
       WAITER,
       SECCOMP_FILTER,
       SECCOMP_NATIVE,
       NEWCGROUP_ARG,
       NEWUSER_ARG,
       NEWPID_ARG,
       NEWIPC_ARG,
       NEWNET_ARG,
       NEWNS_ARG,
       NEWUTS_ARG,
       NUM_OPTIONS };

struct mount_args {
	char const *fsname;
	char *dir;
	char const *type;
	char const *data;

	unsigned long mountflags;

	bool mkdir_flag : 1U;
	bool touch_flag : 1U;
	bool nomount_flag : 1U;
};

static char const *const argstrs[NUM_OPTIONS] = {
        /**/[STOP_OPTIONS] = "--",
        /**/ [HELP] = "--help",
        /**/ [VERSION_OPTION] = "--version",
        /**/ [WAIT] = "--wait",
        /**/ [TRACEME] = "--traceme",
        /**/ [DROP_CAPS] = "--dropcaps",
        /**/ [NO_NEW_PRIVS] = "--nonewprivs",
        /**/ [LIMIT_NO_FILE] = "--limit-no-file",
        /**/ [LIMIT_LOCKS] = "--limit-locks",
        /**/ [LIMIT_MSGQUEUE] = "--limit-msgqueue",
        /**/ [LIMIT_MEMLOCK] = "--limit-memlock",
        /**/ [CHDIR] = "--chdir",
        /**/ [PRIORITY] = "--priority",
        /**/ [TIMER_SLACK] = "--timer-slack",
        /**/ [CHROOTDIR] = "--chrootdir",
        /**/ [FSTAB] = "--fstab",
        /**/ [WAITER] = "--waiter",
        /**/ [SECCOMP_FILTER] = "--seccomp-filter",
        /**/ [SECCOMP_NATIVE] = "--seccomp-arch-native",
        /**/ [NEWUSER_ARG] = "--clone-newuser",
        /**/ [NEWPID_ARG] = "--clone-newpid",
        /**/ [NEWIPC_ARG] = "--clone-newipc",
        /**/ [NEWNET_ARG] = "--clone-newnet",
        /**/ [NEWCGROUP_ARG] = "--clone-newcgroup",
        /**/ [NEWNS_ARG] = "--clone-newns",
        /**/ [NEWUTS_ARG] = "--clone-newuts"};

enum { OPT_TYPE_FLAG,
       OPT_TYPE_STRING,
};
union opt_value {
	bool flag;
	char const *string;
};

static uint_least8_t opt_types[NUM_OPTIONS] = {
        /**/[STOP_OPTIONS] = OPT_TYPE_FLAG,
        /**/ [HELP] = OPT_TYPE_FLAG,
        /**/ [VERSION_OPTION] = OPT_TYPE_FLAG,
        /**/ [WAIT] = OPT_TYPE_FLAG,
        /**/ [TRACEME] = OPT_TYPE_FLAG,
        /**/ [DROP_CAPS] = OPT_TYPE_FLAG,
        /**/ [NO_NEW_PRIVS] = OPT_TYPE_FLAG,
        /**/ [LIMIT_NO_FILE] = OPT_TYPE_STRING,
        /**/ [LIMIT_LOCKS] = OPT_TYPE_STRING,
        /**/ [LIMIT_MSGQUEUE] = OPT_TYPE_STRING,
        /**/ [LIMIT_MEMLOCK] = OPT_TYPE_STRING,
        /**/ [CHDIR] = OPT_TYPE_STRING,
        /**/ [PRIORITY] = OPT_TYPE_STRING,
        /**/ [TIMER_SLACK] = OPT_TYPE_STRING,
        /**/ [CHROOTDIR] = OPT_TYPE_STRING,
        /**/ [FSTAB] = OPT_TYPE_STRING,
        /**/ [WAITER] = OPT_TYPE_STRING,
        /**/ [SECCOMP_FILTER] = OPT_TYPE_STRING,
        /**/ [SECCOMP_NATIVE] = OPT_TYPE_FLAG,
        /**/ [NEWUSER_ARG] = OPT_TYPE_FLAG,
        /**/ [NEWPID_ARG] = OPT_TYPE_FLAG,
        /**/ [NEWIPC_ARG] = OPT_TYPE_FLAG,
        /**/ [NEWNET_ARG] = OPT_TYPE_FLAG,
        /**/ [NEWNS_ARG] = OPT_TYPE_FLAG,
        /**/ [NEWUTS_ARG] = OPT_TYPE_FLAG,
};

struct first_fork_args {
	char const *chdir_path;
	cap_t caps;
	struct mount_args *mount_args;
	size_t mount_args_size;
	char const *waiter;
	char const *waiter_base;
	char const *const *command;
	char const *binary;
	int_least64_t limit_no_file;
	scmp_filter_ctx *seccomp_context;
	lntd_ko err_writer;
	lntd_ko logger_reader;
	lntd_ko logger_writer;
	bool limit_no_file_set;
};

struct second_fork_args {
	char const *const *argv;
	char const *binary;
	int_least64_t limit_no_file;
	scmp_filter_ctx *seccomp_context;
	lntd_ko err_writer;
	lntd_ko logger_writer;
	bool limit_no_file_set;
};

static int first_fork_routine(void *arg);
static int second_fork_routine(void *arg);

static lntd_error do_help(lntd_ko ko, char const *process_name,
                          char const *package_name,
                          char const *package_url,
                          char const *package_bugreport);

static lntd_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                   bool *touch_flagp,
                                   bool *nomount_flagp,
                                   unsigned long *mountflagsp,
                                   char const **leftoversp);

static lntd_error set_id_maps(char const *uid_map, char const *gid_map);

static lntd_error mount_directories(struct mount_args const *mount_args,
                                    size_t size);

static lntd_error set_child_subreaper(bool v);

static lntd_error parse_int(char const *str, int *resultp);

static lntd_error safe_dup2(int oldfd, int newfd);
static pid_t safe_vfork(int (*f)(void *), void *args);
static int my_pivot_root(char const *new_root, char const *put_old);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-sandbox",
    .dont_init_signals = true};

union opt_value opt_values[NUM_OPTIONS] = {0};

static unsigned char lntd_start_main(char const *const process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err;

	size_t arguments_length = argc;

	char const *bad_option = 0;

	bool have_command = false;
	size_t command_start;

	bool need_version;
	bool need_help;

	bool wait;
	bool traceme;
	bool no_new_privs;
	bool drop_caps;

	bool clone_newcgroup;
	bool clone_newuser;
	bool clone_newpid;
	bool clone_newipc;
	bool clone_newnet;
	bool clone_newns;
	bool clone_newuts;

	bool seccomp_native;

	char const *limit_no_file_str;
	char const *limit_locks_str;
	char const *limit_msgqueue_str;
	char const *limit_memlock_str;

	char const *chdir_str;
	char const *priority_str;
	char const *timer_slack_str;
	char const *chrootdir_str;
	char const *fstab_str;
	char const *waiter_str;
	char const *seccomp_filter_str;

	for (size_t ii = 1U; ii < arguments_length; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LNTD_ARRAY_SIZE(argstrs);
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

		default:
			switch (opt_types[arg]) {
			case OPT_TYPE_FLAG:
				opt_values[arg].flag = true;
				break;

			case OPT_TYPE_STRING:
				++ii;
				if (ii >= arguments_length)
					goto exit_loop;
				opt_values[arg].string = argv[ii];
				break;
			}
			break;
		}
	}
exit_loop:
	need_version = opt_values[VERSION_OPTION].flag;
	need_help = opt_values[HELP].flag;

	wait = opt_values[WAIT].flag;
	traceme = opt_values[TRACEME].flag;
	no_new_privs = opt_values[NO_NEW_PRIVS].flag;
	drop_caps = opt_values[DROP_CAPS].flag;

	clone_newcgroup = opt_values[NEWCGROUP_ARG].flag;
	clone_newuser = opt_values[NEWUSER_ARG].flag;
	clone_newpid = opt_values[NEWPID_ARG].flag;
	clone_newipc = opt_values[NEWIPC_ARG].flag;
	clone_newnet = opt_values[NEWNET_ARG].flag;
	clone_newns = opt_values[NEWNS_ARG].flag;
	clone_newuts = opt_values[NEWUTS_ARG].flag;

	seccomp_native = opt_values[SECCOMP_NATIVE].flag;

	limit_no_file_str = opt_values[RLIMIT_NOFILE].string;
	limit_locks_str = opt_values[LIMIT_LOCKS].string;
	limit_msgqueue_str = opt_values[LIMIT_MSGQUEUE].string;
	limit_memlock_str = opt_values[LIMIT_MEMLOCK].string;
	chdir_str = opt_values[CHDIR].string;
	priority_str = opt_values[PRIORITY].string;
	timer_slack_str = opt_values[TIMER_SLACK].string;
	chrootdir_str = opt_values[CHROOTDIR].string;
	fstab_str = opt_values[FSTAB].string;
	waiter_str = opt_values[WAITER].string;
	seccomp_filter_str = opt_values[SECCOMP_FILTER].string;

	if (need_help) {
		do_help(LNTD_KO_STDOUT, process_name, PACKAGE_NAME,
		        PACKAGE_URL, PACKAGE_BUGREPORT);
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		lntd_log(LNTD_LOG_ERROR, "bad option: %s", bad_option);
		return EXIT_FAILURE;
	}

	if (need_version) {
		lntd_locale_version(LNTD_KO_STDOUT, PACKAGE_STRING,
		                    COPYRIGHT_YEAR);
		return EXIT_SUCCESS;
	}

	if (!have_command) {
		lntd_log(LNTD_LOG_ERROR, "need command");
		return EXIT_FAILURE;
	}

	if (0 == waiter_str) {
		lntd_log(LNTD_LOG_ERROR, "need waiter");
		return EXIT_FAILURE;
	}

	if ((fstab_str != 0 && 0 == chrootdir_str) ||
	    (0 == fstab_str && chrootdir_str != 0)) {
		lntd_log(
		    LNTD_LOG_ERROR,
		    "--chrootdir and --fstab are required together");
		return EXIT_FAILURE;
	}

	if (traceme) {
		/* Register with the parent */
		if (-1 == raise(SIGSTOP)) {
			lntd_log(LNTD_LOG_ERROR, "raise: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	char const *const *command = argv + 1U + command_start;
	size_t command_size = argc - (1U + command_start);
	char **command_dup;
	{
		void *xx;
		err = lntd_mem_alloc_array(&xx, command_size + 1U,
		                           sizeof command_dup[0U]);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_mem_alloc_array: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		command_dup = xx;
	}
	command_dup[command_size] = 0;
	for (size_t ii = 0U; ii < command_size; ++ii) {
		char const *c = command[ii];

		char *xx = 0;
		err = lntd_str_dup(&xx, c);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_str_dup: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		command_dup[ii] = xx;
	}

	char *waiter_base;
	{
		char *xx;
		err = lntd_path_base(&xx, waiter_str);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_path_base: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		waiter_base = xx;
	}

	char *command_base;
	{
		char const *c = command_dup[0U];
		char *xx;
		err = lntd_path_base(&xx, c);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_path_base: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		command_base = xx;
	}
	lntd_mem_free(command_dup[0U]);
	command_dup[0U] = command_base;

	char const *binary = command[0U];

	int_least64_t prio_val = 0;
	bool prio_val_set = priority_str != 0;
	if (prio_val_set) {
		int xx;
		err = parse_int(priority_str, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "parse_int: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		prio_val = xx;
	}

	int_least64_t timer_slack_val = 0;
	bool timer_slack_val_set = timer_slack_str != 0;
	if (timer_slack_val_set) {
		int xx;
		err = parse_int(timer_slack_str, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "parse_int: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		timer_slack_val = xx;
	}

	int_least64_t limit_no_file_val = 0;
	bool limit_no_file_val_set = limit_no_file_str != 0;
	if (limit_no_file_val_set) {
		int xx;
		err = parse_int(limit_no_file_str, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "parse_int: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		limit_no_file_val = xx;
	}

	int_least64_t limit_locks_val = 0;
	bool limit_locks_val_set = limit_locks_str != 0;
	if (limit_locks_val_set) {
		int xx;
		err = parse_int(limit_locks_str, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "parse_int: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		limit_locks_val = xx;
	}

	int_least64_t limit_msgqueue_val = 0;
	bool limit_msgqueue_val_set = limit_msgqueue_str != 0;
	if (limit_msgqueue_val_set) {
		int xx;
		err = parse_int(limit_msgqueue_str, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "parse_int: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		limit_msgqueue_val = xx;
	}

	int_least64_t limit_memlock_val = 0;
	bool limit_memlock_val_set = limit_memlock_str != 0;
	if (limit_memlock_val_set) {
		int xx;
		err = parse_int(limit_memlock_str, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "parse_int: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		limit_memlock_val = xx;
	}

	size_t mount_args_size = 0U;
	struct mount_args *mount_args = 0;
	if (fstab_str != 0) {
		FILE *fstab_file = fopen(fstab_str, "re");
		if (0 == fstab_file) {
			lntd_log(LNTD_LOG_ERROR, "fopen(%s): %s",
			         fstab_str, lntd_error_string(errno));
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
						lntd_log(
						    LNTD_LOG_ERROR,
						    "getlines: %s",
						    lntd_error_string(
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
			{
				wordexp_t xx;
				switch (wordexp(buf, &xx, WRDE_NOCMD)) {
				case WRDE_BADCHAR:
				case WRDE_CMDSUB:
				case WRDE_SYNTAX:
					err = EINVAL;
					break;

				case WRDE_NOSPACE:
					err = LNTD_ERROR_OUT_OF_MEMORY;
					break;
				}
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "wordexp(%s): %s", buf,
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				expr = xx;
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
					lntd_log(
					    LNTD_LOG_ERROR,
					    "parse_mount_opts: %s",
					    lntd_error_string(err));
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
				err = lntd_mem_realloc_array(
				    &xx, mount_args,
				    new_mount_args_size,
				    sizeof mount_args[0U]);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_mem_realloc_"
					    "array: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				mount_args = xx;
			}

			if (fsname != 0) {
				char *xx;
				err = lntd_str_dup(&xx, fsname);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_str_dup: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				fsname = xx;
			}

			if (dir != 0) {
				char *xx;
				err = lntd_str_dup(&xx, dir);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_str_dup: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				dir = xx;
			}

			if (type != 0) {
				char *xx;
				err = lntd_str_dup(&xx, type);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_str_dup: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				type = xx;
			}

			if (data != 0) {
				char *xx;
				err = lntd_str_dup(&xx, data);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_str_dup: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				data = xx;
			}

			char *dir_dup;
			{
				char *xx;
				err = lntd_str_dup(&xx, dir);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_str_dup: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				dir_dup = xx;
			}

			struct mount_args *new_mount_arg =
			    &mount_args[mount_args_size];
			new_mount_arg->fsname = fsname;
			new_mount_arg->dir = dir_dup;
			new_mount_arg->type = type;
			new_mount_arg->data = data;
			new_mount_arg->mountflags = mountflags;
			new_mount_arg->mkdir_flag = mkdir_flag;
			new_mount_arg->touch_flag = touch_flag;
			new_mount_arg->nomount_flag = nomount_flag;
			mount_args_size = new_mount_args_size;

		free_words : {
			wordexp_t xx = expr;
			wordfree(&xx);
		}
		}

		if (fclose(fstab_file) != 0) {
			lntd_log(LNTD_LOG_ERROR, "fclose: %s",
			         lntd_error_string(errno));
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
			lntd_log(LNTD_LOG_ERROR, "asprintf: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
		uid_map = xx;
	}

	{
		char *xx;
		if (-1 == asprintf(&xx, "%i %i 1\n", mapped_gid, gid)) {
			lntd_log(LNTD_LOG_ERROR, "asprintf: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
		gid_map = xx;
	}

	lntd_ko logger_reader;
	lntd_ko logger_writer;
	{
		int xx[2U];
		if (-1 == pipe2(xx, O_CLOEXEC)) {
			lntd_log(LNTD_LOG_ERROR, "pipe2: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
		logger_reader = xx[0U];
		logger_writer = xx[1U];
	}

	lntd_ko err_reader;
	lntd_ko err_writer;
	{
		lntd_ko xx;
		lntd_ko yy;
		err = lntd_fifo_pair(&xx, &yy, 0);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_fifo_pair: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		err_reader = xx;
		err_writer = yy;
	}

	cap_t caps = 0;
	if (drop_caps) {
		caps = cap_get_proc();
		if (0 == caps) {
			lntd_log(LNTD_LOG_ERROR, "cap_get_proc: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_EFFECTIVE)) {
			lntd_log(LNTD_LOG_ERROR, "cap_clear_flag: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_PERMITTED)) {
			lntd_log(LNTD_LOG_ERROR, "cap_clear_flag: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == cap_clear_flag(caps, CAP_INHERITABLE)) {
			lntd_log(LNTD_LOG_ERROR, "cap_clear_flag: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	scmp_filter_ctx *seccomp_context = 0;
	if (seccomp_native || seccomp_filter_str != 0) {
		seccomp_context = seccomp_init(SCMP_ACT_KILL);
		if (0 == seccomp_context) {
			lntd_log(LNTD_LOG_ERROR, "seccomp_init: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}

		if (0 == seccomp_filter_str) {
			seccomp_reset(seccomp_context, SCMP_ACT_ALLOW);
		} else {
			char const *start = seccomp_filter_str;
			for (;;) {
				char *end = strchr(start, ',');
				size_t size;
				if (0 == end) {
					size = strlen(start);
				} else {
					size = end - start;
				}

				char *syscall_str;
				{
					char *xx;
					err = lntd_str_dup_len(
					    &xx, start, size);
					if (err != 0) {
						lntd_log(
						    LNTD_LOG_ERROR,
						    "lntd_str_dup: %s",
						    lntd_error_string(
						        err));
						return EXIT_FAILURE;
					}
					syscall_str = xx;
				}
				start = end + 1U;

				int sysno =
				    seccomp_syscall_resolve_name(
				        syscall_str);
				if (__NR_SCMP_ERROR == sysno) {
					lntd_log(LNTD_LOG_ERROR,
					         "seccomp_syscall_"
					         "resolve_name: %s",
					         syscall_str);
					return EXIT_FAILURE;
				}

				lntd_mem_free(syscall_str);

				err = -seccomp_rule_add(seccomp_context,
				                        SCMP_ACT_ALLOW,
				                        sysno, 0);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "seccomp_rule_add: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}

				if (0 == end)
					break;
			}
		}
	}

	int clone_flags = 0;
	if (clone_newcgroup)
		clone_flags |= CLONE_NEWCGROUP;
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

	/* Only start actually dropping privileges now */
	if (timer_slack_val_set) {
		err = lntd_prctl_set_timerslack(timer_slack_val);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "prctl: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (prio_val_set) {
		if (-1 == setpriority(PRIO_PROCESS, 0, prio_val)) {
			lntd_log(LNTD_LOG_ERROR, "setpriority: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (no_new_privs) {
		/* Must appear before the seccomp filter */
		err = lntd_prctl_set_no_new_privs(true);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "prctl: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (clone_flags != 0) {
		if (-1 == unshare(clone_flags)) {
			lntd_log(LNTD_LOG_ERROR, "unshare: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (clone_newuser) {
		err = set_id_maps(uid_map, gid_map);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "set_id_maps: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (chrootdir_str != 0) {
		if (-1 == mount(chrootdir_str, chrootdir_str, 0,
		                MS_BIND, 0)) {
			lntd_log(LNTD_LOG_ERROR, "mount: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}

		if (-1 == chdir(chrootdir_str)) {
			lntd_log(LNTD_LOG_ERROR, "chdir: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (limit_locks_val_set) {
		struct rlimit const lim = {
		    .rlim_cur = limit_locks_val,
		    .rlim_max = limit_locks_val,
		};

		if (-1 == setrlimit(RLIMIT_LOCKS, &lim)) {
			lntd_log(LNTD_LOG_ERROR, "setrlimit: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (limit_msgqueue_val_set) {
		struct rlimit const lim = {
		    .rlim_cur = limit_msgqueue_val,
		    .rlim_max = limit_msgqueue_val,
		};

		if (-1 == setrlimit(RLIMIT_MSGQUEUE, &lim)) {
			lntd_log(LNTD_LOG_ERROR, "setrlimit: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (limit_memlock_val_set) {
		struct rlimit const lim = {
		    .rlim_cur = limit_memlock_val,
		    .rlim_max = limit_memlock_val,
		};

		if (-1 == setrlimit(RLIMIT_MEMLOCK, &lim)) {
			lntd_log(LNTD_LOG_ERROR, "setrlimit: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	pid_t child;
	{
		struct first_fork_args args = {
		    .err_writer = err_writer,
		    .limit_no_file = limit_no_file_val,
		    .limit_no_file_set = limit_no_file_val_set,
		    .logger_reader = logger_reader,
		    .logger_writer = logger_writer,
		    .chdir_path = chdir_str,
		    .caps = caps,
		    .mount_args = mount_args,
		    .mount_args_size = mount_args_size,
		    .seccomp_context = seccomp_context,
		    .waiter_base = waiter_base,
		    .waiter = waiter_str,
		    .command = (char const *const *)command_dup,
		    .binary = binary};
		child = safe_vfork(first_fork_routine, &args);
	}
	if (-1 == child) {
		lntd_log(LNTD_LOG_ERROR, "clone: %s",
		         lntd_error_string(errno));
		return EXIT_FAILURE;
	}

	lntd_ko_close(err_writer);

	{
		size_t xx;
		lntd_error yy;
		err = lntd_io_read_all(err_reader, &xx, &yy, sizeof yy);
		if (err != 0)
			goto close_err_reader;
		/* If bytes_read is zero then a succesful exec
		 * occured */
		if (xx == sizeof yy)
			err = yy;
	}
close_err_reader:
	lntd_ko_close(err_reader);

	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "spawning: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	if (!wait)
		return EXIT_SUCCESS;

	lntd_ko_close(LNTD_KO_STDIN);
	lntd_ko_close(LNTD_KO_STDOUT);

	for (;;) {
		int xx;
		switch (waitpid(child, &xx, 0)) {
		case -1:
			switch (errno) {
			case EINTR:
				continue;
			default:
				lntd_log(LNTD_LOG_ERROR, "waitpid: %s",
				         lntd_error_string(errno));
				return EXIT_FAILURE;
			}

		default:
			return EXIT_SUCCESS;
		}
	}
}

static lntd_error set_id_maps(char const *uid_map, char const *gid_map)
{
	lntd_error err;

	lntd_ko set_groups;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD,
		                   "/proc/thread-self/setgroups",
		                   LNTD_KO_WRONLY);
		if (err != 0)
			return err;
		set_groups = xx;
	}

	err = lntd_io_write_string(set_groups, 0, "deny");
	if (err != 0)
		return err;

	err = lntd_ko_close(set_groups);
	if (err != 0)
		return err;

	/* Note that writing to uid_map and gid_map will fail if the
	 * binary is not dumpable.  DON'T set the process dumpable and
	 * fail if the process is nondumpable as presumably the
	 * invoker of the process had good reasons to have the process
	 * nondumpable.
	 */
	lntd_ko uid_file;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD,
		                   "/proc/thread-self/uid_map",
		                   LNTD_KO_WRONLY);
		if (err != 0)
			return err;
		uid_file = xx;
	}

	err = lntd_io_write_string(uid_file, 0, uid_map);
	if (err != 0)
		return err;

	err = lntd_ko_close(uid_file);
	if (err != 0)
		return err;

	lntd_ko gid_file;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD,
		                   "/proc/thread-self/gid_map",
		                   LNTD_KO_WRONLY);
		if (err != 0)
			return err;
		gid_file = xx;
	}

	err = lntd_io_write_string(gid_file, 0, gid_map);
	if (err != 0)
		return err;

	err = lntd_ko_close(gid_file);
	if (err != 0)
		return err;

	return 0;
}

LNTD_NO_SANITIZE_ADDRESS static int first_fork_routine(void *void_args)
{
	lntd_error err = 0;

	struct first_fork_args const *first_fork_args = void_args;

	int_least64_t limit_no_file = first_fork_args->limit_no_file;
	bool limit_no_file_set = first_fork_args->limit_no_file_set;
	lntd_ko err_writer = first_fork_args->err_writer;
	lntd_ko logger_reader = first_fork_args->logger_reader;
	lntd_ko logger_writer = first_fork_args->logger_writer;
	char const *chdir_path = first_fork_args->chdir_path;
	cap_t caps = first_fork_args->caps;
	struct mount_args *mount_args = first_fork_args->mount_args;
	size_t mount_args_size = first_fork_args->mount_args_size;
	scmp_filter_ctx *seccomp_context =
	    first_fork_args->seccomp_context;
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
	 * - setsid(2) The Single UNIX ?? Specification, Version 2
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
	lntd_ko waiter_ko;
	if (mount_args_size > 0U) {
		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD, waiter, 0);
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
	 * Don't create stuff usable by other processes by default
	 */
	umask(0777);

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

	lntd_ko vfork_err_reader;
	lntd_ko vfork_err_writer;
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
	struct second_fork_args args = {
	    .err_writer = vfork_err_writer,
	    .limit_no_file = limit_no_file,
	    .limit_no_file_set = limit_no_file_set,
	    .logger_writer = logger_writer,
	    .binary = binary,
	    .argv = command,
	    .seccomp_context = seccomp_context};
	grand_child = safe_vfork(second_fork_routine, &args);
	if (-1 == grand_child) {
		err = errno;
		goto fail;
	}

	lntd_ko_close(vfork_err_writer);

	{
		size_t xx;
		lntd_error yy;
		err = lntd_io_read_all(vfork_err_reader, &xx, &yy,
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

	err = safe_dup2(logger_reader, LNTD_KO_STDIN);
	if (err != 0)
		goto fail;

	{
		char const *const arguments[] = {waiter_base, 0};
		if (mount_args_size > 0U) {
			err = lntd_execveat(waiter_ko, "",
			                    (char **)arguments, environ,
			                    AT_EMPTY_PATH);
		} else {
			execve(waiter, (char *const *)arguments,
			       environ);
			err = errno;
			LNTD_ASSUME(err != 0);
		}
	}

fail : {
	lntd_error xx = err;
	lntd_io_write_all(err_writer, 0, &xx, sizeof xx);
}
	return EXIT_FAILURE;
}

LNTD_NO_SANITIZE_ADDRESS static int second_fork_routine(void *arg)
{
	lntd_error err;

	struct second_fork_args const *args = arg;

	int_least64_t limit_no_file = args->limit_no_file;
	bool limit_no_file_set = args->limit_no_file_set;
	lntd_ko err_writer = args->err_writer;
	lntd_ko logger_writer = args->logger_writer;
	char const *const *argv = args->argv;
	scmp_filter_ctx seccomp_context = args->seccomp_context;
	char const *binary = args->binary;

	err = safe_dup2(logger_writer, LNTD_KO_STDOUT);
	if (err != 0)
		goto fail;

	err = safe_dup2(logger_writer, LNTD_KO_STDERR);
	if (err != 0)
		goto fail;

	if (limit_no_file_set) {
		struct rlimit const lim = {
		    .rlim_cur = limit_no_file,
		    .rlim_max = limit_no_file,
		};

		if (-1 == setrlimit(RLIMIT_NOFILE, &lim)) {
			err = errno;
			LNTD_ASSUME(err != 0);

			LNTD_ASSERT(err != EINVAL);
			goto fail;
		}
	}

	/* Do seccomp filter last of all */
	if (seccomp_context != 0) {
		err = -seccomp_load(seccomp_context);
		if (err != 0)
			goto fail;
	}

	execve(binary, (char *const *)argv, environ);
	err = errno;

fail : {
	lntd_error xx = err;
	lntd_io_write_all(err_writer, 0, &xx, sizeof xx);
}
	return EXIT_FAILURE;
}

LNTD_NO_SANITIZE_ADDRESS static lntd_error
mount_directories(struct mount_args const *mount_args, size_t size)
{
	lntd_error err;

	for (size_t ii = 0U; ii < size; ++ii) {
		struct mount_args const *arg = &mount_args[ii];
		char const *fsname = arg->fsname;
		char *dir = arg->dir;
		char const *type = arg->type;
		char const *data = arg->data;
		bool mkdir_flag = arg->mkdir_flag;
		bool touch_flag = arg->touch_flag;
		bool nomount_flag = arg->nomount_flag;
		unsigned long mountflags = arg->mountflags;

		if (mkdir_flag) {
			err = lntd_dir_create(0, LNTD_KO_CWD, dir, 0U,
			                      S_IRWXU);
			if (err != 0) {
				return err;
			}
		} else if (touch_flag) {
			/* Disgusting */
			char *xx = dir;
			if ('/' == *xx)
				++xx;
			for (;;) {
				xx = strchr(xx, '/');
				if (0 == xx)
					break;
				*xx = '\0';
				err = lntd_dir_create(0, LNTD_KO_CWD,
				                      dir, 0U, S_IRWXU);
				if (EEXIST == err)
					err = 0;
				if (err != 0)
					return err;
				*xx = '/';
				++xx;
			}

			err = lntd_file_create(0, LNTD_KO_CWD, dir, 0U,
			                       S_IRWXU);
			if (err != 0)
				return err;
		}

		if (nomount_flag)
			continue;

		unsigned long aliasflags =
		    mountflags & (MS_BIND | MS_SHARED | MS_SLAVE);
		if (0 == aliasflags) {
			if (-1 == mount(fsname, dir, type, mountflags,
			                data)) {
				err = errno;
				return err;
			}
			continue;
		}

		if (-1 == mount(fsname, dir, type, aliasflags, 0)) {
			err = errno;
			LNTD_ASSUME(err != 0);
			return err;
		}

		if (0 == data && 0 == (mountflags & ~aliasflags))
			continue;

		if (-1 == mount(fsname, dir, type,
		                MS_REMOUNT | mountflags, data)) {
			err = errno;
			LNTD_ASSUME(err != 0);
			return err;
		}
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

	err = lntd_ko_close(old_root);
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

static lntd_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                   bool *touch_flagp,
                                   bool *nomount_flagp,
                                   unsigned long *mountflagsp,
                                   char const **leftoversp)
{
	lntd_error err;

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
		err = lntd_str_dup(&xx, opts);
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
	lntd_mem_free(subopts_str);

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

LNTD_NO_SANITIZE_ADDRESS static lntd_error set_child_subreaper(bool v)
{
	lntd_error err;

	if (-1 == prctl(PR_SET_CHILD_SUBREAPER, (unsigned long)v, 0UL,
	                0UL, 0UL)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	return 0;
}

static lntd_error safe_dup2(int oldfd, int newfd)
{
	lntd_error err;
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
		LNTD_ASSUME(err != 0);
	} else {
		err = 0;
	}

	lntd_error mask_err = pthread_sigmask(SIG_SETMASK, &sigset, 0);
	if (0 == err)
		err = mask_err;

	return err;
}

/* Most compilers can't handle the weirdness of vfork so contain it in
 * a safe abstraction.
 */
LNTD_NOINLINE LNTD_NOCLONE LNTD_NO_SANITIZE_ADDRESS static pid_t
safe_vfork(int (*volatile f)(void *), void *volatile arg)
{
	pid_t child = vfork();
	if (0 == child)
		_Exit(f(arg));
	return child;
}

LNTD_NO_SANITIZE_ADDRESS static int my_pivot_root(char const *new_root,
                                                  char const *put_old)
{
	return syscall(__NR_pivot_root, new_root, put_old);
}

static lntd_error parse_int(char const *str, int *resultp)
{
	lntd_error err = 0;

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

static lntd_error do_help(lntd_ko ko, char const *process_name,
                          char const *package_name,
                          char const *package_url,
                          char const *package_bugreport)
{
	lntd_error err;

	err = lntd_io_write_string(ko, 0, "Usage: ");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, process_name);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, " [OPTIONS]\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "Play the game.\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, "Report bugs to <");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, package_bugreport);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, package_name);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, " home page: <");
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, package_url);
	if (err != 0)
		return err;

	err = lntd_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	return 0;
}
