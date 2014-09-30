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
#include "linted/dir.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/mq.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <mntent.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <unistd.h>

#define MAX_MANAGE_CONNECTIONS 10U

enum { WAITER,
       ADMIN_ACCEPTED_CONNECTION,
       ADMIN_READ_CONNECTION,
       ADMIN_WROTE_CONNECTION };

enum { MAX_TASKS = ADMIN_READ_CONNECTION + MAX_MANAGE_CONNECTIONS };

enum unit_type { UNIT_TYPE_SOCKET, UNIT_TYPE_SERVICE };

struct unit_common
{
	enum unit_type type;
	char *name;
};

struct unit_service
{
	struct unit_common common;
	pid_t pid;
};

struct unit_socket
{
	struct unit_common common;
	linted_ko ko;
	bool is_open : 1U;
};

union unit
{
	struct unit_common common;
	struct unit_service service;
	struct unit_socket socket;
};

struct units
{
	size_t size;
	union unit list[];
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
	struct units const *units;
};

struct read_conn_task
{
	struct linted_admin_task_recv_request parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct conn *conn;
	struct units const *units;
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

struct pair
{
	linted_ko ko;
	unsigned long options;
};

static linted_error confs_from_path(char const *unit_path,
                                    struct linted_conf ***unitsp,
                                    size_t *sizep);
void confs_destroy(struct linted_conf **confs, size_t size);

static linted_error units_create(struct units **unitp,
                                 struct linted_conf **confs, size_t size);
static void units_destroy(struct units *units);
static linted_error units_activate(struct units *units,
                                   struct linted_conf **confs, linted_ko cwd,
                                   char const *chrootdir);
static union unit const *unit_for_name(struct units const *unit,
                                       const char *name);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_accepted_conn(struct linted_asynch_task *task);
static linted_error on_read_conn(struct linted_asynch_task *task);
static linted_error on_wrote_conn(struct linted_asynch_task *task);

linted_error ptrace_children(pid_t parent);

static linted_error dispatch_drainers(struct linted_asynch_task *task);

static linted_error drain_on_process_wait(struct linted_asynch_task *task);
static linted_error drain_on_accepted_conn(struct linted_asynch_task *task);
static linted_error drain_on_read_conn(struct linted_asynch_task *task);
static linted_error drain_on_wrote_conn(struct linted_asynch_task *task);

static linted_error conn_pool_create(struct conn_pool **poolp);
static linted_error conn_pool_destroy(struct conn_pool *pool);
static linted_error conn_add(struct conn_pool *pool, struct conn **connp);
static linted_error conn_remove(struct conn *conn, struct conn_pool *conn_pool);

static linted_error socket_create(linted_ko *kop, struct linted_conf *unit);
static linted_error service_spawn(pid_t *pidp, struct linted_conf *unit,
                                  linted_ko cwd, char const *chrootdir,
                                  struct units const *units);
static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path);
static linted_error parse_mount_opts(char const *opts, bool *mkdir_flagp,
                                     bool *touch_flagp,
                                     unsigned long *mountflagsp,
                                     char const **leftoversp);
static linted_error bool_from_cstring(char const *str, bool *boolp);
static linted_error long_from_cstring(char const *str, long *longp);
static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);

static linted_error ptrace_attach(pid_t pid);
static linted_error ptrace_cont(pid_t pid, int signo);

static linted_ko kos[1U];

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor",
    .kos_size = LINTED_ARRAY_SIZE(kos),
    .kos = kos};

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

	struct linted_conf **confs;
	size_t confs_size;
	{
		struct linted_conf **xx;
		size_t yy;
		errnum = confs_from_path(unit_path, &xx, &yy);
		if (errnum != 0)
			goto destroy_conn_pool;
		confs = xx;
		confs_size = yy;
	}

	struct units *units;
	{
		struct units *xx;
		errnum = units_create(&xx, confs, confs_size);
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
		errnum = ptrace_attach(ppid);
		if (errnum != EPERM)
			break;

		sched_yield();
	}

	if (errnum != 0)
		goto destroy_confs;

	/**
	 * @todo Warn about unactivated units.
	 */
	errnum = units_activate(units, confs, cwd, chrootdir);
	if (errnum != 0)
		goto kill_procs;

	linted_asynch_task_waitid(LINTED_UPCAST(&waiter_task), WAITER, P_ALL,
	                          -1, WSTOPPED);
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

		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto kill_procs;
	} while (!waiter_task.time_to_exit);

kill_procs:
	for (size_t ii = 0U; ii < units->size; ++ii) {
		union unit *unit = &units->list[ii];

		if (unit->common.type != UNIT_TYPE_SERVICE)
			continue;

		struct unit_service *service = &unit->service;

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

	for (size_t ii = 0U; ii < units->size; ++ii) {
		union unit *unit = &units->list[ii];

		if (unit->common.type != UNIT_TYPE_SOCKET)
			continue;

		struct unit_socket *socket = &unit->socket;

		if (!socket->is_open)
			continue;

		linted_error close_errnum = linted_ko_close(socket->ko);
		if (0 == errnum)
			errnum = close_errnum;

		socket->is_open = false;
	}

	units_destroy(units);

destroy_confs:
	confs_destroy(confs, confs_size);

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

		linted_error dispatch_error = dispatch_drainers(completed_task);
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

static linted_error confs_from_path(char const *unit_path,
                                    struct linted_conf ***unitsp, size_t *sizep)
{
	linted_ko errnum = 0;

	struct linted_conf **units = NULL;
	size_t units_size = 0U;

	char const *dirstart = unit_path;
	for (;;) {
		char const *dirend = strchr(dirstart, ':');

		char *dir_name;
		if (NULL == dirend) {
			dir_name = strdup(dirstart);
		} else {
			dir_name = strndup(dirstart, dirend - dirstart);
		}

		if (NULL == dir_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_units;
		}

		DIR *units_dir = opendir(dir_name);
		if (NULL == units_dir) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}

		linted_mem_free(dir_name);

		if (ENOENT == errnum) {
			errnum = 0;
			goto next_dir;
		}

		if (errnum != 0)
			goto free_units;

		size_t files_count = 0U;
		char **files = NULL;
		for (;;) {
			errno = 0;
			struct dirent const *entry = readdir(units_dir);
			if (NULL == entry) {
				errnum = errno;
				if (0 == errnum)
					break;

				goto free_file_names;
			}

			char const *name = entry->d_name;

			if (0 == strcmp(".", name))
				continue;

			if (0 == strcmp("..", name))
				continue;

			char *name_copy = strdup(name);
			if (NULL == name_copy) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_file_names;
			}

			size_t new_files_count = files_count + 1U;
			char **new_files;
			{
				void *xx;
				errnum = linted_mem_realloc_array(
				    &xx, files, new_files_count,
				    sizeof files[0U]);
				if (errnum != 0)
					goto free_file_name;
				new_files = xx;
			}
			new_files[files_count] = name_copy;

			files = new_files;
			files_count = new_files_count;

			if (errnum != 0) {
			free_file_name:
				linted_mem_free(name_copy);
				goto free_file_names;
			}
		}

		for (size_t ii = 0U; ii < files_count; ++ii) {
			char const *file_name = files[ii];

			linted_ko unit_fd =
			    openat(dirfd(units_dir), file_name, O_RDONLY);
			if (-1 == unit_fd) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_file_names;
			}

			FILE *unit_file = fdopen(unit_fd, "r");
			if (NULL == unit_file) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);

				close(unit_fd);

				goto free_file_names;
			}

			struct linted_conf *unit = NULL;
			{
				struct linted_conf *xx;
				errnum = linted_conf_parse_file(&xx, unit_file,
				                                file_name);
				if (errnum != 0)
					goto close_unit_file;
				unit = xx;
			}

		close_unit_file:
			if (EOF == fclose(unit_file)) {
				if (0 == errnum) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
				}
			}

			if (errnum != 0)
				goto free_unit;

			size_t new_units_size = units_size + 1U;
			struct linted_conf **new_units;
			{
				void *xx;
				errnum = linted_mem_realloc_array(
				    &xx, units, new_units_size,
				    sizeof units[0U]);
				if (errnum != 0)
					goto free_unit;
				new_units = xx;
			}
			new_units[units_size] = unit;

			units = new_units;
			units_size = new_units_size;

		free_unit:
			if (errnum != 0)
				linted_conf_put(unit);
		}

	free_file_names:
		for (size_t ii = 0U; ii < files_count; ++ii)
			linted_mem_free(files[ii]);
		linted_mem_free(files);

		if (-1 == closedir(units_dir)) {
			if (0 == errnum) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			}
		}
		if (errnum != 0)
			goto free_units;

	next_dir:
		if (NULL == dirend)
			break;

		dirstart = dirend + 1U;
	}

free_units:
	if (errnum != 0) {
		for (size_t ii = 0U; ii < units_size; ++ii)
			linted_conf_put(units[ii]);
		linted_mem_free(units);
	} else {
		*unitsp = units;
		*sizep = units_size;
	}

	return errnum;
}

void confs_destroy(struct linted_conf **confs, size_t size)
{
	for (size_t ii = 0U; ii < size; ++ii)
		linted_conf_put(confs[ii]);
	linted_mem_free(confs);
}

static linted_error units_create(struct units **unitsp,
                                 struct linted_conf **confs, size_t size)
{
	linted_error errnum;
	struct units *units;

	size_t mem_size = sizeof *units + size * sizeof units->list[0U];
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, mem_size);
		if (errnum != 0)
			return errnum;
		units = xx;
	}
	units->size = 0U;

	*unitsp = units;

	for (size_t ii = 0U; ii < size; ++ii) {
		union unit *unit = &units->list[ii];
		struct linted_conf *conf = confs[ii];

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		enum unit_type unit_type;
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

		unit->common.type = unit_type;
		unit->common.name = unit_name;

		switch (unit_type) {
		case UNIT_TYPE_SERVICE:
			unit->service.pid = -1;
			break;

		case UNIT_TYPE_SOCKET:
			unit->socket.is_open = false;
			break;
		}

		++units->size;
	}

	return 0;

destroy_units:
	units_destroy(units);

	return errnum;
}

static void units_destroy(struct units *units)
{
	size_t size = units->size;
	union unit *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii)
		linted_mem_free(list[ii].common.name);
	linted_mem_free(units);
}

static linted_error units_activate(struct units *units,
                                   struct linted_conf **confs, linted_ko cwd,
                                   char const *chrootdir)
{
	linted_error errnum;

	for (size_t ii = 0U; ii < units->size; ++ii) {
		struct linted_conf *conf = confs[ii];
		union unit *unit = &units->list[ii];

		if (unit->common.type != UNIT_TYPE_SOCKET)
			continue;

		linted_ko ko;
		{
			linted_ko xx;
			errnum = socket_create(&xx, conf);
			if (errnum != 0)
				return errnum;
			ko = xx;
		}

		unit->socket.ko = ko;
		unit->socket.is_open = true;
	}

	for (size_t ii = 0U; ii < units->size; ++ii) {
		struct linted_conf *conf = confs[ii];
		union unit *unit = &units->list[ii];

		if (unit->common.type != UNIT_TYPE_SERVICE)
			continue;

		struct unit_service *unit_service = &unit->service;

		pid_t process;
		{
			pid_t xx;
			errnum =
			    service_spawn(&xx, conf, cwd, chrootdir, units);
			if (errnum != 0)
				return errnum;

			process = xx;
		}

		unit_service->pid = process;
	}

	return 0;
}

static linted_error socket_create(linted_ko *kop, struct linted_conf *unit)
{
	linted_error errnum;

	char const *const *name =
	    linted_conf_find(unit, "Socket", "ListenMessageQueue");
	char const *const *maxmsgs =
	    linted_conf_find(unit, "Socket", "MessageQueueMaxMessages");
	char const *const *msgsize =
	    linted_conf_find(unit, "Socket", "MessageQueueMessageSize");
	char const *const *temp =
	    linted_conf_find(unit, "Socket", "X-Linted-Temporary");

	if (NULL == name || NULL == name[0U] || name[1U] != NULL)
		return EINVAL;

	if (NULL == maxmsgs || NULL == maxmsgs[0U] || maxmsgs[1U] != NULL)
		return EINVAL;

	if (NULL == msgsize || NULL == msgsize[0U] || msgsize[1U] != NULL)
		return EINVAL;

	if (NULL == temp || NULL == temp[0U] || temp[1U] != NULL)
		return EINVAL;

	long maxmsgs_value;
	{
		long xx;
		errnum = long_from_cstring(maxmsgs[0U], &xx);
		if (errnum != 0)
			return errnum;
		maxmsgs_value = xx;
	}

	long msgsize_value;
	{
		long xx;
		errnum = long_from_cstring(msgsize[0U], &xx);
		if (errnum != 0)
			return errnum;
		msgsize_value = xx;
	}

	linted_ko ko;
	{
		linted_ko xx;
		errnum = linted_mq_create(&xx, name[0U], maxmsgs_value,
		                          msgsize_value, 0);
		if (errnum != 0)
			return errnum;
		ko = xx;
	}

	*kop = ko;
	return 0;
}

enum { RDONLY, WRONLY };

static char const *const file_options[] = {[RDONLY] = "rdonly",
                                           [WRONLY] = "wronly", NULL};

static char const *const default_envvars[] = {"LANG", "USER", "LOGNAME", "HOME",
                                              "SHELL", "XDG_RUNTIME_DIR"
                                                       "XDG_SESSION_ID",
                                              "XDG_SEAT", "TERM"};

static struct pair const defaults[] = {{STDIN_FILENO, LINTED_KO_RDONLY},
                                       {STDOUT_FILENO, LINTED_KO_WRONLY},
                                       {STDERR_FILENO, LINTED_KO_WRONLY}};

static linted_error service_spawn(pid_t *pidp, struct linted_conf *conf,
                                  linted_ko cwd, char const *chrootdir,
                                  struct units const *units)
{
	linted_error errnum = 0;

	char const *const *type = linted_conf_find(conf, "Service", "Type");
	char const *const *exec_start =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *no_new_privs =
	    linted_conf_find(conf, "Service", "NoNewPrivileges");
	char const *const *files =
	    linted_conf_find(conf, "Service", "X-Linted-Files");
	char const *const *fstab =
	    linted_conf_find(conf, "Service", "X-Linted-Fstab");
	char const *const *chdir_path =
	    linted_conf_find(conf, "Service", "X-Linted-Chdir");
	char const *const *env_whitelist =
	    linted_conf_find(conf, "Service", "X-Linted-Environment-Whitelist");

	if (type != NULL && (NULL == type[0U] || type[1U] != NULL))
		return EINVAL;

	if (NULL == exec_start)
		return EINVAL;

	if (no_new_privs != NULL &&
	    (NULL == no_new_privs[0U] || no_new_privs[1U] != NULL))
		return EINVAL;

	if (fstab != NULL && (NULL == fstab[0U] || fstab[1U] != NULL))
		return EINVAL;

	if (chdir_path != NULL &&
	    (NULL == chdir_path[0U] || chdir_path[1U] != NULL))
		return EINVAL;

	if (NULL == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type[0U])) {
		/* simple type of service */
	} else {
		return EINVAL;
	}

	bool no_new_privs_value = false;
	if (no_new_privs != NULL) {
		bool xx;
		errnum = bool_from_cstring(no_new_privs[0U], &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs_value = xx;
	}

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

	char *service_name;
	{
		char *xx;
		if (-1 == asprintf(&xx, "LINTED_SERVICE=%s",
		                   linted_conf_peek_name(conf))) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_envvars;
		}
		service_name = xx;
	}

	{
		size_t envvars_size;
		for (size_t ii = 0U;; ++ii) {
			if (NULL == envvars[ii]) {
				envvars_size = ii;
				break;
			}
		}

		void *xx;
		errnum = linted_mem_realloc_array(
		    &xx, envvars, envvars_size + 2U, sizeof envvars[0U]);
		if (errnum != 0) {
			linted_mem_free(service_name);
			goto free_envvars;
		}
		envvars = xx;

		envvars[envvars_size] = service_name;
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

	linted_spawn_attr_setdeparent(attr, true);
	linted_spawn_attr_setnonewprivs(attr, no_new_privs_value);
	linted_spawn_attr_setdropcaps(attr, true);
	linted_spawn_attr_setcloneflags(attr, clone_flags);
	if (chdir_path != NULL)
		linted_spawn_attr_setchdir(attr, chdir_path[0U]);

	if (fstab != NULL) {
		linted_spawn_attr_setchrootdir(attr, chrootdir);

		errnum = parse_fstab(attr, cwd, fstab[0U]);
		if (errnum != 0)
			goto destroy_attr;
	}

	linted_ko *proc_kos = NULL;
	size_t kos_opened = 0U;

	size_t files_size = 0U;
	if (files != NULL) {
		for (size_t ii = 0U;; ++ii) {
			if (NULL == files[ii]) {
				files_size = ii;
				break;
			}
		}
	}

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
				    &xx, (char *const *)file_options, &yy);
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

		union unit const *unit = unit_for_name(units, filename);
		if (NULL == unit) {
			errnum = EINVAL;
			goto free_filename;
		}

		struct unit_socket const *socket = &unit->socket;

		linted_ko ko;
		{
			linted_ko xx;
			errnum = linted_ko_reopen(&xx, socket->ko, flags);
			if (errnum != 0)
				goto free_filename;
			ko = xx;
		}

		proc_kos[kos_opened] = ko;
		++kos_opened;

		errnum = linted_spawn_file_actions_adddup2(&file_actions, ko,
		                                           kos_opened - 1U);
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
		                 exec_start, (char const *const *)envvars);
		if (errnum != 0)
			goto destroy_attr;
		process = xx;
	}

	*pidp = process;

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

	char const *abspath;
	if (fstab_path[0U] != '/') {
		char *xx;
		if (-1 ==
		    asprintf(&xx, "/proc/self/fd/%i/%s", cwd, fstab_path)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		abspath = xx;
	} else {
		abspath = fstab_path;
	}

	FILE *fstab = setmntent(abspath, "re");
	errnum = errno;

	if (abspath != fstab_path)
		linted_mem_free((char *)abspath);

	if (NULL == fstab) {
		LINTED_ASSUME(errnum != 0);
		return errnum;
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
                                            NULL};

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
			    getsubopt(&xx, (char *const *)mount_options, &yy);
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

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (task->task_action) {
	case WAITER:
		return on_process_wait(task);

	case ADMIN_ACCEPTED_CONNECTION:
		return on_accepted_conn(task);

	case ADMIN_READ_CONNECTION:
		return on_read_conn(task);

	case ADMIN_WROTE_CONNECTION:
		return on_wrote_conn(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_wait(struct linted_asynch_task *task)
{
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
		raise(exit_status);
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;

	case CLD_EXITED:
		if (exit_status != 0) {
			errnum = exit_status;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		break;

	case CLD_TRAPPED: {
		int restart_signal = exit_status;

		/* Ignore SIGCHLD for example */
		if (exit_status != SIGSTOP)
			goto restart_init;

		restart_signal = 0;

		fprintf(stderr, "started tracing %i!\n", pid);

		if (pid == parent_process)
			errnum = ptrace_children(parent_process);

	restart_init : {
		linted_error cont_errnum = ptrace_cont(pid, restart_signal);
		if (0 == errnum)
			errnum = cont_errnum;
	} break;
	}

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	return errnum;
}

linted_error ptrace_children(pid_t parent)
{
	linted_error errnum;

	linted_dir init_children;
	{
		char path[] = "/proc/XXXXXXXXXXXXXXXX/task/XXXXXXXXXXXXXXXXX";
		sprintf(path, "/proc/%i/task/%i/children", parent, parent);

		linted_dir xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (errnum != 0)
			return errnum;
		init_children = xx;
	}

	FILE *init_children_file = fdopen(init_children, "r");
	if (NULL == init_children_file) {
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
			ssize_t zz =
			    getdelim(&xx, &yy, ' ', init_children_file);
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

		fprintf(stderr, "pid: %i\n", child);

		errnum = ptrace_attach(child);
		if (errnum != 0)
			break;
	}

free_buf:
	linted_mem_free(buf);

	fclose(init_children_file);

	return errnum;
}

static linted_error on_accepted_conn(struct linted_asynch_task *completed_task)
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

	struct units const *units = accepted_conn_task->units;

	linted_admin new_socket = accept_task->returned_ko;
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

static linted_error on_read_conn(struct linted_asynch_task *task)
{
	linted_error errnum;

	struct read_conn_task *read_conn_task =
	    LINTED_DOWNCAST(struct read_conn_task, task);

	struct linted_asynch_pool *pool = read_conn_task->pool;
	struct conn_pool *conn_pool = read_conn_task->conn_pool;
	struct conn *conn = read_conn_task->conn;
	struct units const *units = read_conn_task->units;

	if ((errnum = task->errnum) != 0) {
		/* The other end did something bad */
		errnum = conn_remove(conn, conn_pool);
		if (errnum != 0)
			return errnum;
		return 0;
	}

	struct linted_admin_task_recv_request *task_recv =
	    LINTED_UPCAST(read_conn_task);
	struct linted_ko_task_read *task_read = LINTED_UPCAST(task_recv);

	linted_ko ko = task_read->ko;

	union linted_admin_request *request = &task_recv->request;
	union linted_admin_reply reply;

	switch (request->type) {
	case LINTED_ADMIN_REBOOT:
		if (-1 == reboot(RB_POWER_OFF)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto conn_remove;
		}
		break;

	case LINTED_ADMIN_STATUS: {
		union unit const *unit =
		    unit_for_name(units, request->status.name);
		if (NULL == unit) {
			reply.status.is_up = false;
			break;
		}

		pid_t pid;
		switch (unit->common.type) {
		case UNIT_TYPE_SERVICE:
			pid = unit->service.pid;
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
		union unit const *unit =
		    unit_for_name(units, request->status.name);
		if (NULL == unit) {
			reply.status.is_up = false;
			break;
		}

		pid_t pid;
		switch (unit->common.type) {
		case UNIT_TYPE_SERVICE:
			pid = unit->service.pid;
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

static linted_error on_wrote_conn(struct linted_asynch_task *task)
{
	linted_error errnum;

	struct wrote_conn_task *wrote_conn_task =
	    LINTED_DOWNCAST(struct wrote_conn_task, task);
	struct conn_pool *conn_pool = wrote_conn_task->conn_pool;
	struct conn *conn = wrote_conn_task->conn;

	errnum = task->errnum;

	linted_error remove_errnum = conn_remove(conn, conn_pool);
	if (0 == errnum)
		errnum = remove_errnum;

	return errnum;
}

static linted_error dispatch_drainers(struct linted_asynch_task *task)
{
	switch (task->task_action) {
	case WAITER:
		return drain_on_process_wait(task);

	case ADMIN_ACCEPTED_CONNECTION:
		return drain_on_accepted_conn(task);

	case ADMIN_READ_CONNECTION:
		return drain_on_read_conn(task);

	case ADMIN_WROTE_CONNECTION:
		return drain_on_wrote_conn(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error drain_on_process_wait(struct linted_asynch_task *task)
{
	return 0;
}

static linted_error drain_on_accepted_conn(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct accepted_conn_task *accepted_conn_task =
	    LINTED_DOWNCAST(struct accepted_conn_task, task);

	struct linted_ko_task_accept *accept_task =
	    LINTED_UPCAST(LINTED_UPCAST(accepted_conn_task));

	linted_admin new_socket = accept_task->returned_ko;

	return linted_ko_close(new_socket);
}

static linted_error drain_on_read_conn(struct linted_asynch_task *task)
{
	return task->errnum;
}

static linted_error drain_on_wrote_conn(struct linted_asynch_task *task)
{
	return task->errnum;
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

static union unit const *unit_for_name(struct units const *units,
                                       const char *name)
{
	for (size_t ii = 0U; ii < units->size; ++ii) {
		union unit const *unit = &units->list[ii];

		if (0 == strncmp(unit->common.name, name, LINTED_UNIT_NAME_MAX))
			return unit;
	}

	return NULL;
}

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	size_t allowed_envvars_size;
	char **result_envvars;
	linted_error errnum;

	for (size_t ii = 0U;; ++ii) {
		if (NULL == allowed_envvars[ii]) {
			allowed_envvars_size = ii;
			break;
		}
	}

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

static linted_error bool_from_cstring(char const *str, bool *boolp)
{
	if (0 == strcmp(str, "1") || 0 == strcmp(str, "yes") ||
	    0 == strcmp(str, "true") || 0 == strcmp(str, "on")) {
		*boolp = true;
		return 0;
	} else if (0 == strcmp(str, "0") || 0 == strcmp(str, "no") ||
	           0 == strcmp(str, "false") || 0 == strcmp(str, "off")) {
		*boolp = false;
		return 0;
	} else {
		return EINVAL;
	}
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

		if ('0' <= digit && digit <= '9') {
			unsigned long sum =
			    total + ((unsigned)(digit - '0')) * position;
			if (sum > LONG_MAX)
				return ERANGE;

			total = sum;
		} else {
			return EINVAL;
		}

		unsigned long next_position = 10U * position;
		if (next_position > LONG_MAX)
			return ERANGE;
		position = next_position;
	}

	*longp = total;
	return 0;
}

static linted_error ptrace_attach(pid_t pid)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_ATTACH, pid, (void *)NULL, (void *)NULL)) {
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
	    ptrace(PTRACE_CONT, pid, (void *)(intptr_t) signo, (void *)NULL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}
