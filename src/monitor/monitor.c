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

#include "linted/admin.h"
#include "linted/asynch.h"
#include "linted/conf.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <mntent.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/ptrace.h>

#define MAX_MANAGE_CONNECTIONS 10U

enum {
	WAITER,
	ADMIN_ACCEPTED_CONNECTION,
	ADMIN_READ_CONNECTION,
	ADMIN_WROTE_CONNECTION
};

enum {
	MAX_TASKS = ADMIN_READ_CONNECTION + MAX_MANAGE_CONNECTIONS
};

struct wait_service_task
{
	struct linted_asynch_task_waitid parent;
	struct linted_asynch_pool *pool;
	pid_t parent_process;
	bool time_to_exit : 1U;
};

struct conn;
struct conn_pool;

struct accepted_conn_task
{
	struct linted_admin_task_accept parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct linted_unit_db const *units;
};

struct read_conn_task
{
	struct linted_admin_task_recv_request parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct conn *conn;
	struct linted_unit_db const *units;
};

struct wrote_conn_task
{
	struct linted_admin_task_send_reply parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct conn *conn;
};

struct conn
{
	struct read_conn_task read_task;
	struct wrote_conn_task write_task;
	linted_ko ko;
	bool is_free : 1U;
};

static linted_error create_units(struct linted_unit_db **unitsp,
                                 struct linted_conf_db *conf_db);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

static linted_error activate_units(struct linted_unit_db *units, linted_ko cwd,
                                   char const *chrootdir);

static linted_error dispatch(struct linted_asynch_task *completed_task,
                             bool time_to_quit);

static linted_error on_process_wait(struct linted_asynch_task *task,
                                    bool time_to_quit);
static linted_error on_accepted_conn(struct linted_asynch_task *task,
                                     bool time_to_quit);
static linted_error on_read_conn(struct linted_asynch_task *task,
                                 bool time_to_quit);
static linted_error on_wrote_conn(struct linted_asynch_task *task,
                                  bool time_to_quit);

static linted_error ptrace_children(pid_t parent);

static linted_error conn_pool_create(struct conn_pool **poolp);
static linted_error conn_pool_destroy(struct conn_pool *pool);
static linted_error conn_add(struct conn_pool *pool, struct conn **connp);
static linted_error conn_remove(struct conn *conn, struct conn_pool *conn_pool);

static linted_error socket_activate(struct linted_unit_socket *unit);
static linted_error service_activate(struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir,
                                     struct linted_unit_db const *units);

static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path);
static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error str_from_strs(char const *const *strs, char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);
static linted_error long_from_cstring(char const *str, long *longp);

static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type);
static linted_error get_process_children(linted_ko *kop, pid_t pid);

static linted_error ptrace_interrupt(pid_t pid);
static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);
static linted_error ptrace_cont(pid_t pid, int signo);
static linted_error ptrace_listen(pid_t pid);
static linted_error ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo);

static linted_ko kos[1U];

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-monitor",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos
};

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	pid_t ppid = getppid();
	linted_admin admin = kos[0U];

	/**
	 * @TODO Make the monitor not rely upon being basically PID 1
	 * and recover from being restarted suddenly.
	 */

	char const *chrootdir = getenv("LINTED_CHROOT");
	char const *unit_path = getenv("LINTED_UNIT_PATH");

	if (NULL == chrootdir) {
		linted_io_write_format(
		    STDERR_FILENO, NULL,
		    "%s: LINTED_CHROOT is a required environment variable\n",
		    process_name);
		return EXIT_FAILURE;
	}

	if (NULL == unit_path) {
		linted_io_write_format(
		    STDERR_FILENO, NULL,
		    "%s: LINTED_UNIT_PATH is a required environment variable\n",
		    process_name);
		return EXIT_FAILURE;
	}

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

	if (-1 == chdir("/")) {
		perror("chdir");
		return EXIT_FAILURE;
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			goto exit_monitor;
		pool = xx;
	}

	struct wait_service_task waiter_task;
	struct accepted_conn_task accepted_conn_task;

	struct conn_pool *conn_pool;
	{
		struct conn_pool *xx;
		errnum = conn_pool_create(&xx);
		if (errnum != 0)
			goto drain_asynch_pool;
		conn_pool = xx;
	}

	struct linted_conf_db *conf_db;
	{
		struct linted_conf_db *xx;
		errnum = linted_conf_db_create_from_path(&xx, unit_path);
		if (errnum != 0)
			goto destroy_conn_pool;
		conf_db = xx;
	}

	struct linted_unit_db *units;
	{
		struct linted_unit_db *xx;
		errnum = create_units(&xx, conf_db);
		if (errnum != 0)
			goto destroy_confs;
		units = xx;
	}

	linted_admin_accept(LINTED_UPCAST(&accepted_conn_task),
	                    ADMIN_ACCEPTED_CONNECTION, admin);
	accepted_conn_task.pool = pool;
	accepted_conn_task.conn_pool = conn_pool;
	accepted_conn_task.units = units;

	linted_asynch_pool_submit(
	    pool,
	    LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&accepted_conn_task))));

	for (;;) {
		errnum = ptrace_seize(ppid, 0U);
		if (errnum != EPERM)
			break;

		sched_yield();
	}
	if (errnum != 0)
		goto destroy_confs;

	errnum = ptrace_interrupt(ppid);
	if (errnum != 0)
		goto destroy_confs;

	/**
	 * @todo Warn about unactivated units.
	 */
	errnum = activate_units(units, cwd, chrootdir);
	if (errnum != 0)
		goto kill_procs;

	linted_asynch_task_waitid(LINTED_UPCAST(&waiter_task), WAITER, P_ALL,
	                          -1, WEXITED);
	waiter_task.pool = pool;
	waiter_task.parent_process = ppid;
	waiter_task.time_to_exit = false;

	linted_asynch_pool_submit(pool,
	                          LINTED_UPCAST(LINTED_UPCAST(&waiter_task)));

	do {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		errnum = dispatch(completed_task, false);
		if (errnum != 0)
			goto kill_procs;
	} while (!waiter_task.time_to_exit);

kill_procs:
	for (size_t ii = 0U, size = linted_unit_db_size(units); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(units, ii);

		if (unit->type != UNIT_TYPE_SERVICE)
			continue;

		struct linted_unit_service *service = (void *)unit;

		if (service->pid <= 1)
			continue;

		if (-1 == kill(service->pid, SIGKILL)) {
			linted_error kill_errnum = errno;
			LINTED_ASSUME(kill_errnum != 0);
			if (kill_errnum != ESRCH) {
				assert(kill_errnum != EINVAL);
				assert(kill_errnum != EPERM);
				assert(false);
			}
		}
	}

	for (size_t ii = 0U, size = linted_unit_db_size(units); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(units, ii);

		if (unit->type != UNIT_TYPE_SOCKET)
			continue;

		struct linted_unit_socket *socket = (void *)unit;

		if (!socket->is_open)
			continue;

		linted_error close_errnum = linted_ko_close(socket->ko);
		if (0 == errnum)
			errnum = close_errnum;

		socket->is_open = false;
	}

	linted_unit_db_destroy(units);

destroy_confs:
	linted_conf_db_destroy(conf_db);

destroy_conn_pool : {
	linted_error close_errnum = conn_pool_destroy(conn_pool);
	if (0 == errnum)
		errnum = close_errnum;
}

drain_asynch_pool:
	linted_asynch_pool_stop(pool);

	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			if (EAGAIN == linted_asynch_pool_poll(pool, &xx))
				break;
			completed_task = xx;
		}

		linted_error dispatch_error = dispatch(completed_task, true);
		if (0 == errnum)
			errnum = dispatch_error;
	}

	{
		linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
		if (0 == errnum)
			errnum = destroy_errnum;

		/* Insure that the tasks are in proper scope until they are
		 * terminated */
		(void)waiter_task;
		(void)accepted_conn_task;
	}

exit_monitor:
	if (errnum != 0) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "could not run the game: %s\n",
		                       linted_error_string(errnum));

		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error create_units(struct linted_unit_db **unitsp,
                                 struct linted_conf_db *conf_db)
{
	linted_error errnum;

	struct linted_unit_db *units;
	{
		struct linted_unit_db *xx;
		errnum = linted_unit_db_create(&xx);
		if (errnum != 0)
			return errnum;
		units = xx;
	}

	size_t size = linted_conf_db_size(conf_db);
	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_unit *unit;
		{
			struct linted_unit *xx;
			errnum = linted_unit_db_add_unit(units, &xx);
			if (errnum != 0)
				goto destroy_units;
			unit = xx;
		}

		struct linted_conf *conf = linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		enum linted_unit_type unit_type;
		if (0 == strcmp(suffix, "socket")) {
			unit_type = UNIT_TYPE_SOCKET;
		} else if (0 == strcmp(suffix, "service")) {
			unit_type = UNIT_TYPE_SERVICE;
		} else {
			errnum = EINVAL;
			goto destroy_units;
		}

		char *unit_name = strndup(file_name, dot - file_name);
		if (NULL == unit_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto destroy_units;
		}

		unit->type = unit_type;
		unit->name = unit_name;

		switch (unit_type) {
		case UNIT_TYPE_SERVICE: {
			struct linted_unit_service *s = (void *)unit;
			s->pid = -1;

			errnum = service_create(s, conf);
			if (errnum != 0)
				goto destroy_units;
			break;
		}

		case UNIT_TYPE_SOCKET: {
			struct linted_unit_socket *s = (void *)unit;
			s->is_open = false;

			errnum = socket_create(s, conf);
			if (errnum != 0)
				goto destroy_units;
			break;
		}
		}
	}

	*unitsp = units;

	return errnum;

destroy_units:
	linted_unit_db_destroy(units);
	return errnum;
}

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf)
{
	linted_error errnum;

	char const *const *types = linted_conf_find(conf, "Service", "Type");
	char const *const *exec_start =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *no_new_privss =
	    linted_conf_find(conf, "Service", "NoNewPrivileges");
	char const *const *files =
	    linted_conf_find(conf, "Service", "X-Linted-Files");
	char const *const *fstabs =
	    linted_conf_find(conf, "Service", "X-Linted-Fstab");
	char const *const *chdir_paths =
	    linted_conf_find(conf, "Service", "X-Linted-Chdir");
	char const *const *env_whitelist =
	    linted_conf_find(conf, "Service", "X-Linted-Environment-Whitelist");

	char const *type;
	{
		char const *xx;
		errnum = str_from_strs(types, &xx);
		if (errnum != 0)
			return errnum;
		type = xx;
	}

	if (NULL == exec_start)
		return EINVAL;

	char const *no_new_privs;
	{
		char const *xx;
		errnum = str_from_strs(no_new_privss, &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs = xx;
	}

	char const *fstab;
	{
		char const *xx;
		errnum = str_from_strs(fstabs, &xx);
		if (errnum != 0)
			return errnum;
		fstab = xx;
	}

	char const *chdir_path;
	{
		char const *xx;
		errnum = str_from_strs(chdir_paths, &xx);
		if (errnum != 0)
			return errnum;
		chdir_path = xx;
	}

	if (NULL == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		return EINVAL;
	}

	bool no_new_privs_value = false;
	if (no_new_privs != NULL) {
		bool xx;
		errnum = bool_from_cstring(no_new_privs, &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs_value = xx;
	}

	unit->exec_start = exec_start;
	unit->no_new_privs = no_new_privs_value;
	unit->files = files;
	unit->fstab = fstab;
	unit->chdir_path = chdir_path;
	unit->env_whitelist = env_whitelist;

	return 0;
}

static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf)
{
	linted_error errnum;

	char const *const *paths =
	    linted_conf_find(conf, "Socket", "ListenMessageQueue");
	char const *const *maxmsgss =
	    linted_conf_find(conf, "Socket", "MessageQueueMaxMessages");
	char const *const *msgsizes =
	    linted_conf_find(conf, "Socket", "MessageQueueMessageSize");
	char const *const *temps =
	    linted_conf_find(conf, "Socket", "X-Linted-Temporary");

	char const *path;
	{
		char const *xx;
		errnum = str_from_strs(paths, &xx);
		if (errnum != 0)
			return errnum;
		path = xx;
	}

	char const *maxmsgs;
	{
		char const *xx;
		errnum = str_from_strs(maxmsgss, &xx);
		if (errnum != 0)
			return errnum;
		maxmsgs = xx;
	}

	char const *msgsize;
	{
		char const *xx;
		errnum = str_from_strs(msgsizes, &xx);
		if (errnum != 0)
			return errnum;
		msgsize = xx;
	}

	char const *temp;
	{
		char const *xx;
		errnum = str_from_strs(temps, &xx);
		if (errnum != 0)
			return errnum;
		temp = xx;
	}

	long maxmsgs_value;
	{
		long xx;
		errnum = long_from_cstring(maxmsgs, &xx);
		if (errnum != 0)
			return errnum;
		maxmsgs_value = xx;
	}

	long msgsize_value;
	{
		long xx;
		errnum = long_from_cstring(msgsize, &xx);
		if (errnum != 0)
			return errnum;
		msgsize_value = xx;
	}

	bool temp_value;
	{
		bool xx;
		errnum = bool_from_cstring(temp, &xx);
		if (errnum != 0)
			return errnum;
		temp_value = xx;
	}

	if (!temp_value)
		return EINVAL;

	unit->path = path;
	unit->maxmsgs = maxmsgs_value;
	unit->msgsize = msgsize_value;

	return 0;
}

static linted_error activate_units(struct linted_unit_db *units, linted_ko cwd,
                                   char const *chrootdir)
{
	linted_error errnum;

	for (size_t ii = 0U, size = linted_unit_db_size(units); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(units, ii);

		if (unit->type != UNIT_TYPE_SOCKET)
			continue;

		errnum = socket_activate((void *)unit);
		if (errnum != 0)
			return errnum;
	}

	for (size_t ii = 0U, size = linted_unit_db_size(units); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(units, ii);

		if (unit->type != UNIT_TYPE_SERVICE)
			continue;

		errnum = service_activate((void *)unit, cwd, chrootdir, units);
		if (errnum != 0)
			return errnum;
	}

	return 0;
}

static linted_error socket_activate(struct linted_unit_socket *unit)
{
	linted_error errnum;
	linted_ko ko;
	{
		linted_ko xx;
		errnum = linted_mq_create(&xx, unit->path, unit->maxmsgs,
		                          unit->msgsize, 0);
		if (errnum != 0)
			return errnum;
		ko = xx;
	}

	unit->ko = ko;
	unit->is_open = true;

	return 0;
}

enum {
	RDONLY,
	WRONLY
};

struct pair
{
	linted_ko ko;
	unsigned long options;
};

static char const *const file_options[] = {[RDONLY] = "rdonly",
	                                   [WRONLY] = "wronly", NULL };

static char const *const default_envvars[] = { "LANG", "USER", "LOGNAME",
	                                       "HOME", "SHELL",
	                                       "XDG_RUNTIME_DIR"
	                                       "XDG_SESSION_ID",
	                                       "XDG_SEAT", "TERM" };

static struct pair const defaults[] = { { STDIN_FILENO, LINTED_KO_RDONLY },
	                                { STDOUT_FILENO, LINTED_KO_WRONLY },
	                                { STDERR_FILENO, LINTED_KO_WRONLY } };

static linted_error service_activate(struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir,
                                     struct linted_unit_db const *units)
{
	linted_error errnum = 0;

	char const *service_name = unit->name;

	struct linted_unit_service *unit_service = (void *)unit;

	char const *const *exec_start = unit_service->exec_start;
	bool no_new_privs = unit_service->no_new_privs;
	char const *const *files = unit_service->files;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *env_whitelist = unit_service->env_whitelist;

	if (NULL == env_whitelist)
		env_whitelist = default_envvars;

	char **envvars;
	{
		char **xx;
		errnum = filter_envvars(&xx, env_whitelist);
		if (errnum != 0)
			return errnum;
		envvars = xx;
	}

	char *service_name_setting;
	{
		char *xx;
		if (-1 == asprintf(&xx, "LINTED_SERVICE=%s", service_name)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_envvars;
		}
		service_name_setting = xx;
	}

	{
		size_t envvars_size =
		    null_list_size((char const * const *)envvars);

		void *xx;
		errnum = linted_mem_realloc_array(
		    &xx, envvars, envvars_size + 2U, sizeof envvars[0U]);
		if (errnum != 0) {
			linted_mem_free(service_name_setting);
			goto free_envvars;
		}
		envvars = xx;

		envvars[envvars_size] = service_name_setting;
		envvars[envvars_size + 1U] = NULL;
	}

	struct linted_spawn_file_actions *file_actions;
	struct linted_spawn_attr *attr;

	{
		struct linted_spawn_file_actions *xx;
		errnum = linted_spawn_file_actions_init(&xx);
		if (errnum != 0)
			goto free_envvars;
		file_actions = xx;
	}

	{
		struct linted_spawn_attr *xx;
		errnum = linted_spawn_attr_init(&xx);
		if (errnum != 0)
			goto destroy_file_actions;
		attr = xx;
	}

	int clone_flags =
	    CLONE_NEWUSER | CLONE_NEWPID | CLONE_NEWIPC | CLONE_NEWNET;

	if (fstab != NULL)
		clone_flags |= CLONE_NEWNS;

	linted_spawn_attr_setname(attr, service_name);
	linted_spawn_attr_setdeparent(attr, true);
	linted_spawn_attr_setnonewprivs(attr, no_new_privs);
	linted_spawn_attr_setdropcaps(attr, true);
	linted_spawn_attr_setcloneflags(attr, clone_flags);
	if (chdir_path != NULL)
		linted_spawn_attr_setchdir(attr, chdir_path);

	if (fstab != NULL) {
		linted_spawn_attr_setchrootdir(attr, chrootdir);

		errnum = parse_fstab(attr, cwd, fstab);
		if (errnum != 0)
			goto destroy_attr;
	}

	linted_ko *proc_kos = NULL;
	size_t kos_opened = 0U;

	size_t files_size = 0U;
	if (files != NULL)
		files_size = null_list_size(files);

	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, sizeof proc_kos[0U],
		                                3U + files_size);
		if (errnum != 0)
			goto destroy_attr;
		proc_kos = xx;
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(defaults); ++ii) {
		struct pair const *pair = &defaults[ii];
		linted_ko ko = pair->ko;
		unsigned long options = pair->options;

		linted_ko copy_ko;
		{
			linted_ko xx;
			errnum = linted_ko_reopen(&xx, ko, options);
			if (errnum != 0)
				goto destroy_proc_kos;
			copy_ko = xx;
		}

		size_t old_kos_opened = kos_opened;
		kos_opened = old_kos_opened + 1U;

		proc_kos[old_kos_opened] = copy_ko;

		errnum = linted_spawn_file_actions_adddup2(
		    &file_actions, copy_ko, old_kos_opened);
		if (errnum != 0)
			goto destroy_proc_kos;
	}

	for (size_t ii = 0U; ii < files_size; ++ii) {
		char const *open_command = files[ii];

		if (strncmp(open_command, "OPEN:", strlen("OPEN:")) != 0) {
			errnum = EINVAL;
			goto destroy_proc_kos;
		}

		open_command = open_command + strlen("OPEN:");

		char *filenameend = strchr(open_command, ',');
		char *filename;
		if (NULL == filenameend) {
			filename = strdup(open_command);
		} else {
			filename =
			    strndup(open_command, filenameend - open_command);
		}
		if (NULL == filename) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto destroy_attr;
		}

		char *opts_buffer =
		    strdup(open_command + 1U + strlen(filename));
		if (NULL == opts_buffer) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_filename;
		}

		bool rdonly = false;
		bool wronly = false;

		char *opts = opts_buffer;
		char *value = NULL;
		while (*opts != '\0') {
			int token;
			{
				char *xx = opts;
				char *yy = value;
				token = getsubopt(
				    &xx, (char * const *)file_options, &yy);
				opts = xx;
				value = yy;
			}
			switch (token) {
			case RDONLY:
				rdonly = true;
				break;

			case WRONLY:
				wronly = true;
				break;

			default:
				errnum = EINVAL;
				goto free_opts_buffer;
			}
		}

	free_opts_buffer:
		linted_mem_free(opts_buffer);
		if (errnum != 0)
			goto free_filename;

		if (wronly && rdonly) {
			errnum = EINVAL;
			goto free_filename;
		}

		unsigned long flags = 0;

		if (rdonly)
			flags |= LINTED_KO_RDONLY;

		if (wronly)
			flags |= LINTED_KO_WRONLY;

		struct linted_unit const *socket_unit =
		    linted_unit_db_get_unit_by_name(units, filename);
		if (NULL == socket_unit) {
			errnum = EINVAL;
			goto free_filename;
		}

		struct linted_unit_socket const *socket = (void *)socket_unit;

		linted_ko ko;
		{
			linted_ko xx;
			errnum = linted_ko_reopen(&xx, socket->ko, flags);
			if (errnum != 0)
				goto free_filename;
			ko = xx;
		}

		proc_kos[kos_opened] = ko;

		size_t old_kos_opened = kos_opened;
		++kos_opened;

		errnum = linted_spawn_file_actions_adddup2(&file_actions, ko,
		                                           old_kos_opened);
		if (errnum != 0)
			goto free_filename;

	free_filename:
		linted_mem_free(filename);

		if (errnum != 0)
			goto destroy_proc_kos;
	}

	pid_t process;
	{
		pid_t xx;
		errnum =
		    linted_spawn(&xx, cwd, exec_start[0U], file_actions, attr,
		                 exec_start, (char const * const *)envvars);
		if (errnum != 0)
			goto destroy_attr;
		process = xx;
	}

	unit_service->pid = process;

destroy_proc_kos:
	for (size_t jj = 0; jj < kos_opened; ++jj)
		linted_ko_close(proc_kos[jj]);
	linted_mem_free(proc_kos);

destroy_attr:
	linted_spawn_attr_destroy(attr);

destroy_file_actions:
	linted_spawn_file_actions_destroy(file_actions);

free_envvars:
	for (char **envp = envvars; *envp != NULL; ++envp)
		linted_mem_free(*envp);
	linted_mem_free(envvars);

	return errnum;
}

static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path)
{
	linted_error errnum = 0;

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

		bool mkdir_flag;
		bool touch_flag;
		unsigned long mountflags;
		char const *data;
		if (NULL == opts) {
			mkdir_flag = false;
			touch_flag = false;
			mountflags = 0U;
			data = NULL;
		} else {
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

		errnum = linted_spawn_attr_setmount(attr, fsname, dir, type,
		                                    mkdir_flag, touch_flag,
		                                    mountflags, data);
		if (errnum != 0)
			goto close_file;
	}

close_file:
	if (endmntent(fstab) != 1 && 0 == errnum) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	return errnum;
}

enum {
	MKDIR,
	TOUCH,
	BIND,
	RBIND,
	RO,
	RW,
	SUID,
	NOSUID,
	NODEV,
	NOEXEC
};

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

	unsigned long mountflags = 0;

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

static linted_error dispatch(struct linted_asynch_task *task, bool time_to_quit)
{
	switch (task->task_action) {
	case WAITER:
		return on_process_wait(task, time_to_quit);

	case ADMIN_ACCEPTED_CONNECTION:
		return on_accepted_conn(task, time_to_quit);

	case ADMIN_READ_CONNECTION:
		return on_read_conn(task, time_to_quit);

	case ADMIN_WROTE_CONNECTION:
		return on_wrote_conn(task, time_to_quit);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_wait(struct linted_asynch_task *task,
                                    bool time_to_quit)
{
	if (time_to_quit)
		return 0;

	linted_error errnum = 0;

	if (task->errnum != 0)
		return 0;

	/**
	 * @todo Warn on bad process wait.
	 */

	struct wait_service_task *wait_service_task =
	    LINTED_DOWNCAST(struct wait_service_task, task);
	struct linted_asynch_pool *pool = wait_service_task->pool;
	pid_t parent_process = wait_service_task->parent_process;

	pid_t pid;
	int exit_status;
	int exit_code;
	{
		siginfo_t *exit_info = &LINTED_UPCAST(wait_service_task)->info;
		pid = exit_info->si_pid;
		exit_status = exit_info->si_status;
		exit_code = exit_info->si_code;
	}

	linted_asynch_pool_submit(pool, task);

	switch (exit_code) {
	case CLD_DUMPED:
	case CLD_KILLED:
		fprintf(stderr, "%i killed due to signal %s\n", pid,
		        strsignal(exit_status));
		break;

	case CLD_EXITED:
		fprintf(stderr, "%i exited with %i\n", pid, exit_status);
		break;

	case CLD_TRAPPED: {
		int event = exit_status >> 8U;
		switch (event) {
		/* Trap a signal */
		case 0: {
			if (exit_status != SIGCHLD)
				goto restart_process;

			int code;
			pid_t sandboxed_pid;
			int status;
			{
				siginfo_t info = { 0 };
				errnum = ptrace_getsiginfo(pid, &info);
				if (errnum != 0)
					goto restart_process;

				code = info.si_code;

				/* User generated signal */
				if (code < 0)
					goto restart_process;

				sandboxed_pid = info.si_pid;
				status = info.si_status;
			}

			switch (code) {
			case CLD_EXITED:
				fprintf(
				    stderr,
				    "process %i in sandbox %i exited with %i\n",
				    sandboxed_pid, pid, status);
				break;

			case CLD_DUMPED:
			case CLD_KILLED:
				fprintf(
				    stderr,
				    "process %i in sandbox %i killed by %s\n",
				    sandboxed_pid, pid, strsignal(status));
				break;

			case CLD_STOPPED:
				fprintf(
				    stderr,
				    "process %i in sandbox %i stopped by %s\n",
				    sandboxed_pid, pid, strsignal(status));
				break;

			default:
				LINTED_ASSUME_UNREACHABLE();
			}

		restart_process : {
			linted_error cont_errnum =
			    ptrace_cont(pid, exit_status);
			if (0 == errnum)
				errnum = cont_errnum;
			break;
		}
		}

		case PTRACE_EVENT_STOP: {
			int stop_sig = exit_status & 0xFF;

			fprintf(stderr, "ptracing %i!\n", pid);

			if (pid == parent_process)
				errnum = ptrace_children(parent_process);

			switch (stop_sig) {
			case SIGTRAP: {
				linted_error cont_errnum = ptrace_cont(pid, 0);
				if (0 == errnum)
					errnum = cont_errnum;
				break;
			}

			default: {
				linted_error cont_errnum = ptrace_listen(pid);
				if (0 == errnum)
					errnum = cont_errnum;
				break;
			}
			}
			break;
		}

		default:
			LINTED_ASSUME_UNREACHABLE();
		}
		break;
	}

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	return errnum;
}

static linted_error on_accepted_conn(struct linted_asynch_task *completed_task,
                                     bool time_to_quit)
{
	linted_error errnum;

	if ((errnum = completed_task->errnum) != 0)
		return errnum;

	struct accepted_conn_task *accepted_conn_task =
	    LINTED_DOWNCAST(struct accepted_conn_task, completed_task);

	struct linted_ko_task_accept *accept_task =
	    LINTED_UPCAST(LINTED_UPCAST(accepted_conn_task));

	struct linted_asynch_pool *pool = accepted_conn_task->pool;
	struct conn_pool *conn_pool = accepted_conn_task->conn_pool;

	struct linted_unit_db const *units = accepted_conn_task->units;

	linted_admin new_socket = accept_task->returned_ko;

	if (time_to_quit)
		return linted_ko_close(new_socket);

	linted_asynch_pool_submit(pool, completed_task);

	struct conn *conn;
	{
		struct conn *xx;
		errnum = conn_add(conn_pool, &xx);
		if (EAGAIN == errnum) {
			errnum = 0;
			goto close_new_socket;
		}

		if (errnum != 0)
			goto close_new_socket;
		conn = xx;
	}

	conn->ko = new_socket;

	linted_admin_recv_request(LINTED_UPCAST(&conn->read_task),
	                          ADMIN_READ_CONNECTION, new_socket);
	conn->read_task.pool = pool;
	conn->read_task.conn_pool = conn_pool;
	conn->read_task.conn = conn;
	conn->read_task.units = units;

	linted_asynch_pool_submit(pool, LINTED_UPCAST(LINTED_UPCAST(
	                                    LINTED_UPCAST(&conn->read_task))));
	return 0;

close_new_socket : {
	linted_error close_errnum = linted_ko_close(new_socket);
	if (0 == errnum)
		errnum = close_errnum;
}

	return errnum;
}

static linted_error on_read_conn(struct linted_asynch_task *task,
                                 bool time_to_quit)
{
	linted_error errnum;

	struct read_conn_task *read_conn_task =
	    LINTED_DOWNCAST(struct read_conn_task, task);

	struct linted_asynch_pool *pool = read_conn_task->pool;
	struct conn_pool *conn_pool = read_conn_task->conn_pool;
	struct conn *conn = read_conn_task->conn;
	struct linted_unit_db const *units = read_conn_task->units;

	if ((errnum = task->errnum) != 0) {
		/* The other end did something bad */
		errnum = conn_remove(conn, conn_pool);
		if (errnum != 0)
			return errnum;
		return 0;
	}

	if (time_to_quit)
		return 0;

	struct linted_admin_task_recv_request *task_recv =
	    LINTED_UPCAST(read_conn_task);
	struct linted_ko_task_read *task_read = LINTED_UPCAST(task_recv);

	linted_ko ko = task_read->ko;

	union linted_admin_request *request = &task_recv->request;
	union linted_admin_reply reply;

	switch (request->type) {
	case LINTED_ADMIN_STATUS: {
		struct linted_unit const *unit =
		    linted_unit_db_get_unit_by_name(units,
		                                    request->status.name);
		if (NULL == unit) {
			reply.status.is_up = false;
			break;
		}

		struct linted_unit_service *unit_service = (void *)unit;

		pid_t pid;
		switch (unit->type) {
		case UNIT_TYPE_SERVICE:
			pid = unit_service->pid;
			goto status_service_process;

		status_service_process:
			if (0 == kill(pid, 0)) {
				reply.status.is_up = true;
				break;
			}

			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			assert(errnum != EINVAL);
			switch (errnum) {
			case ESRCH:
				reply.status.is_up = false;
				break;

			default:
				goto conn_remove;
			}
			break;

		default:
			break;
		}
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_unit const *unit =
		    linted_unit_db_get_unit_by_name(units,
		                                    request->status.name);
		if (NULL == unit) {
			reply.status.is_up = false;
			break;
		}

		struct linted_unit_service *unit_service = (void *)unit;

		pid_t pid;
		switch (unit->type) {
		case UNIT_TYPE_SERVICE:
			pid = unit_service->pid;
			goto stop_service_process;

		stop_service_process:
			if (0 == kill(pid, SIGKILL)) {
				reply.stop.was_up = true;
				break;
			}

			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			assert(errnum != EINVAL);
			switch (errnum) {
			case ESRCH:
				reply.stop.was_up = false;
				break;

			default:
				goto conn_remove;
			}
			break;

		default:
			break;
		}
	}
	}

	linted_admin_send_reply(LINTED_UPCAST(&conn->write_task),
	                        ADMIN_WROTE_CONNECTION, ko, &reply);
	conn->write_task.pool = pool;
	conn->write_task.conn_pool = conn_pool;
	conn->write_task.conn = conn;

	linted_asynch_pool_submit(pool, LINTED_UPCAST(LINTED_UPCAST(
	                                    LINTED_UPCAST(&conn->write_task))));

	return 0;

conn_remove:
	conn_remove(conn, conn_pool);
	return errnum;
}

static linted_error on_wrote_conn(struct linted_asynch_task *task,
                                  bool time_to_quit)
{
	linted_error errnum;

	struct wrote_conn_task *wrote_conn_task =
	    LINTED_DOWNCAST(struct wrote_conn_task, task);
	struct conn_pool *conn_pool = wrote_conn_task->conn_pool;
	struct conn *conn = wrote_conn_task->conn;

	errnum = task->errnum;

	if (time_to_quit)
		return errnum;

	linted_error remove_errnum = conn_remove(conn, conn_pool);
	if (0 == errnum)
		errnum = remove_errnum;

	return errnum;
}

static linted_error ptrace_children(pid_t parent)
{
	linted_error errnum;

	linted_ko init_children;
	{
		linted_ko xx;
		errnum = get_process_children(&xx, parent);
		if (errnum != 0)
			return errnum;
		init_children = xx;
	}

	FILE *file = fdopen(init_children, "r");
	if (NULL == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(init_children);

		return errnum;
	}

	char *buf = NULL;
	size_t buf_size = 0U;

	for (;;) {
		{
			char *xx = buf;
			size_t yy = buf_size;

			errno = 0;
			ssize_t zz = getdelim(&xx, &yy, ' ', file);
			if (-1 == zz) {
				errnum = errno;
				if (0 == errnum)
					break;

				goto free_buf;
			}
			buf = xx;
			buf_size = yy;
		}

		pid_t child = strtol(buf, NULL, 10);
		if (getpid() == child)
			continue;

		errnum = ptrace_seize(child, 0U);

		/* Child already exited */
		if (EPERM == errnum)
			continue;

		if (errnum != 0)
			break;

		errnum = ptrace_interrupt(child);
		if (errnum != 0)
			break;
	}

free_buf:
	linted_mem_free(buf);

	fclose(file);

	return errnum;
}

struct conn_pool
{
	bool conns_free[MAX_MANAGE_CONNECTIONS];
	struct conn conns[MAX_MANAGE_CONNECTIONS];
	size_t count;
};

static linted_error conn_pool_create(struct conn_pool **poolp)
{
	linted_error errnum;

	struct conn_pool *pool;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *pool);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	pool->count = 0U;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(pool->conns); ++ii)
		pool->conns[ii].is_free = true;

	*poolp = pool;
	return 0;
}

static linted_error conn_pool_destroy(struct conn_pool *pool)
{
	linted_error errnum = 0;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(pool->conns); ++ii) {
		struct conn *const conn = &pool->conns[ii];
		if (!conn->is_free) {
			linted_error close_errnum = linted_ko_close(conn->ko);
			if (0 == errnum)
				errnum = close_errnum;
		}
	}

	linted_mem_free(pool);

	return errnum;
}

static linted_error conn_add(struct conn_pool *pool, struct conn **connp)
{
	if (pool->count >= MAX_MANAGE_CONNECTIONS)
		return EAGAIN;

	struct conn *conn;

	size_t ii = 0U;
	for (; ii < MAX_MANAGE_CONNECTIONS; ++ii) {
		conn = &pool->conns[ii];
		if (conn->is_free)
			goto got_space;
	}
	LINTED_ASSUME_UNREACHABLE();
got_space:
	++pool->count;
	conn->is_free = false;

	*connp = conn;

	return 0;
}

static linted_error conn_remove(struct conn *conn, struct conn_pool *pool)
{
	linted_ko ko = conn->ko;

	conn->is_free = true;
	--pool->count;

	return linted_ko_close(ko);
}

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	char **result_envvars;
	linted_error errnum;

	size_t allowed_envvars_size = null_list_size(allowed_envvars);

	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, allowed_envvars_size,
		                                sizeof result_envvars[0U]);
		if (errnum != 0)
			return errnum;
		result_envvars = xx;
	}

	size_t result_envvars_size = 0U;
	for (size_t ii = 0U; ii < allowed_envvars_size; ++ii) {
		char const *envvar_name = allowed_envvars[ii];

		char const *envvar_value = getenv(envvar_name);
		if (NULL == envvar_value)
			continue;

		++result_envvars_size;

		size_t envvar_name_length = strlen(envvar_name);
		size_t envvar_value_length = strlen(envvar_value);

		size_t assign_string_length =
		    envvar_name_length + 1U + envvar_value_length;

		char *assign_string;
		{
			void *xx;
			errnum =
			    linted_mem_alloc(&xx, assign_string_length + 1U);
			if (errnum != 0)
				goto free_result_envvars;
			assign_string = xx;
		}
		memcpy(assign_string, envvar_name, envvar_name_length);
		assign_string[envvar_name_length] = '=';
		memcpy(assign_string + envvar_name_length + 1U, envvar_value,
		       envvar_value_length);
		assign_string[assign_string_length] = '\0';

		result_envvars[result_envvars_size - 1U] = assign_string;
	}

	{
		void *xx;
		errnum = linted_mem_realloc_array(&xx, result_envvars,
		                                  result_envvars_size + 1U,
		                                  sizeof result_envvars[0U]);
		if (errnum != 0)
			goto free_result_envvars;
		result_envvars = xx;
	}
	result_envvars[result_envvars_size] = NULL;

	*result_envvarsp = result_envvars;

	return 0;

free_result_envvars:
	for (size_t ii = 0U; ii < result_envvars_size; ++ii)
		linted_mem_free(result_envvars[ii]);
	linted_mem_free(result_envvars);
	return errnum;
}

static size_t null_list_size(char const *const *list)
{
	for (size_t ii = 0U;; ++ii)
		if (NULL == list[ii])
			return ii;
}

static linted_error str_from_strs(char const *const *strs, char const **strp)
{
	char const *str;
	if (NULL == strs) {
		str = NULL;
	} else {
		str = strs[0U];

		if (strs[1U] != NULL)
			return EINVAL;
	}

	*strp = str;
	return 0;
}

static linted_error bool_from_cstring(char const *str, bool *boolp)
{
	static char const *const yes_strs[] = { "1", "yes", "true", "on" };
	static char const *const no_strs[] = { "0", "no", "false", "off" };

	bool result;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(yes_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = true;
			goto return_result;
		}
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(no_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = false;
			goto return_result;
		}
	}

	return EINVAL;

return_result:
	*boolp = result;
	return 0;
}

static linted_error long_from_cstring(char const *str, long *longp)
{
	size_t length = strlen(str);
	unsigned long position = 1U;

	if ('0' == str[0U] && length != 1U)
		return EINVAL;

	unsigned long total = 0U;
	for (; length > 0U; --length) {
		char const digit = str[length - 1U];

		if (digit < '0' || digit > '9')
			return EINVAL;

		unsigned long sum =
		    total + ((unsigned)(digit - '0')) * position;
		if (sum > LONG_MAX)
			return ERANGE;

		total = sum;

		unsigned long next_position = 10U * position;
		if (next_position > LONG_MAX)
			return ERANGE;
		position = next_position;
	}

	*longp = total;
	return 0;
}

static linted_error my_setmntentat(FILE **filep, linted_ko cwd,
                                   char const *filename, char const *type)
{
	linted_error errnum;

	char const *abspath;
	if (filename[0U] != '/') {
		char *xx;
		if (-1 == asprintf(&xx, "/proc/self/fd/%i/%s", cwd, filename)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		abspath = xx;
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

static linted_error get_process_children(linted_ko *kop, pid_t pid)
{
	char path[] = "/proc/XXXXXXXXXXXXXXXX/task/XXXXXXXXXXXXXXXXX";
	sprintf(path, "/proc/%i/task/%i/children", pid, pid);

	return linted_ko_open(kop, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
}

static linted_error ptrace_interrupt(pid_t pid)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_INTERRUPT, pid, (void *)NULL, (void *)NULL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
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

static linted_error ptrace_cont(pid_t pid, int signo)
{
	linted_error errnum;

	if (-1 ==
	    ptrace(PTRACE_CONT, pid, (void *)(intptr_t)signo, (void *)NULL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_listen(pid_t pid)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_LISTEN, pid, (void *)NULL, (void *)NULL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo)
{
	linted_error errnum;

	if (-1 ==
	    ptrace(PTRACE_GETSIGINFO, pid, (void *)NULL, (void *)siginfo)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}
