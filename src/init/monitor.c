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

#include "init_config.h"
#include "init.h"
#include "monitor.h"

#include "linted/asynch.h"
#include "linted/controller.h"
#include "linted/db.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/manager.h"
#include "linted/mem.h"
#include "linted/spawn.h"
#include "linted/updater.h"
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
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/prctl.h>
#include <wordexp.h>
#include <unistd.h>

#define BACKLOG 20U

#define MAX_MANAGE_CONNECTIONS 10U

#ifndef PR_SET_CHILD_SUBREAPER
#define PR_SET_CHILD_SUBREAPER 36UL
#endif

enum {
	WAITER,
	NEW_CONNECTIONS,
	READ_CONNECTION,
	WRITE_CONNECTION
};

enum service_type {
	SERVICE_TYPE_PROCESS,
	SERVICE_TYPE_FILE
};

enum {
	MAX_TASKS = READ_CONNECTION + MAX_MANAGE_CONNECTIONS
};

struct service_process
{
	enum service_type type;
	char const *name;
	pid_t pid;
};

struct service_file
{
	enum service_type type;
	char const *name;
	linted_ko ko;
	bool is_open : 1U;
};

union service
{
	enum service_type type;
	struct service_process process;
	struct service_file file;
};

struct services
{
	size_t size;
	union service list[];
};

struct wait_service_task
{
	struct linted_asynch_task_waitid parent;
	struct linted_asynch_pool *pool;
	pid_t halt_pid;
	bool time_to_exit : 1U;
};

struct conn;
struct conn_pool;

struct new_connection_task
{
	struct linted_manager_task_accept parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct services const *services;
};

struct read_conn_task
{
	struct linted_manager_task_recv_request parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct conn *conn;
	struct services const *services;
	union service_config const *config;
};

struct write_conn_task
{
	struct linted_manager_task_send_reply parent;
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
	struct conn *conn;
};

struct conn
{
	struct read_conn_task read_task;
	struct write_conn_task write_task;
	linted_ko ko;
	bool is_free : 1U;
};

enum unit_type {
	UNIT_TYPE_SOCKET,
	UNIT_TYPE_SERVICE
};

struct unit;
struct unit_section;
struct unit_setting;

static linted_error process_units_in_path(char const *unit_path,
                                          struct unit ***unitsp, size_t *sizep);

static linted_error bool_from_cstring(char const *str, bool *boolp);
static linted_error long_from_cstring(char const *str, long *longp);

static linted_error parse_unit_file(struct unit **unitp, FILE *unitfile,
                                    enum unit_type type, char const *name);

static linted_error unit_create(struct unit **unitp, enum unit_type type,
                                char const *name);

static void unit_put(struct unit *unit);

static enum unit_type unit_type(struct unit *unit);
static char const *unit_peek_name(struct unit *unit);

static char const *const *
unit_section_find(struct unit *unit, char const *section, char const *field);

static linted_error unit_add_section(struct unit *unit,
                                     struct unit_section **sectionp,
                                     char *section_name);

static linted_error unit_section_add_setting(struct unit_section *section,
                                             char *field,
                                             char const *const *value);

static linted_error spawn_process(pid_t *pidp, struct unit *unit, linted_ko cwd,
                                  char const *chrootdir,
                                  struct services const *services);

static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path);

static linted_error get_flags_and_data(char const *opts, bool *mkdir_flagp,
                                       bool *touch_flagp,
                                       unsigned long *mountflagsp,
                                       char const **leftoversp);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_new_connection(struct linted_asynch_task *task);
static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_read_connection(struct linted_asynch_task *task);
static linted_error on_write_connection(struct linted_asynch_task *task);

static linted_error dispatch_drainers(struct linted_asynch_task *task);

static linted_error drain_on_new_connection(struct linted_asynch_task *task);
static linted_error drain_on_process_wait(struct linted_asynch_task *task);
static linted_error drain_on_read_connection(struct linted_asynch_task *task);
static linted_error drain_on_write_connection(struct linted_asynch_task *task);

static linted_error check_db(linted_ko cwd);

static linted_error conn_pool_create(struct conn_pool **poolp);
static linted_error conn_pool_destroy(struct conn_pool *pool);
static linted_error conn_add(struct conn_pool *pool, struct conn **connp);
static linted_error conn_remove(struct conn *conn, struct conn_pool *conn_pool);

static union service const *service_for_name(struct services const *services,
                                             const char *name);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);

unsigned char linted_init_monitor(linted_ko cwd,
                                  struct linted_init_config const *init_config)
{
	linted_error errnum;

	char const *unit_path = init_config->unit_path;
	char const *chrootdir = init_config->chrootdir;

	static char const process_name[] = "monitor";

	/* The process monitor and manager */
	if (-1 ==
	    prctl(PR_SET_NAME, (unsigned long)process_name, 0UL, 0UL, 0UL)) {
		perror("prctl");
		return EXIT_FAILURE;
	}

	if (-1 ==
	    prctl(PR_SET_PDEATHSIG, (unsigned long)SIGKILL, 0UL, 0UL, 0UL)) {
		perror("prctl");
		return EXIT_FAILURE;
	}

	if (-1 == prctl(PR_SET_CHILD_SUBREAPER, 1UL, 0UL, 0UL, 0UL)) {
		perror("prctl");
		return EXIT_FAILURE;
	}

	/* Acquire socket before unsharing namespaces */
	linted_manager new_connections;
	{
		linted_manager xx;
		errnum = linted_manager_bind(&xx, BACKLOG, NULL, 0);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_manager_bind");
			return EXIT_FAILURE;
		}
		new_connections = xx;
	}

	{
		char buf[LINTED_MANAGER_PATH_MAX];
		size_t len;
		errnum = linted_manager_path(new_connections, buf, &len);
		if (errnum != 0)
			return errnum;

		linted_io_write_str(STDOUT_FILENO, NULL,
		                    LINTED_STR("LINTED_SOCKET="));
		linted_io_write_all(STDOUT_FILENO, NULL, buf, len);
		linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));
	}

	errnum = check_db(cwd);
	if (errnum != 0) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: database: %s\n", process_name,
		                       linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	struct unit **units;
	size_t units_size;
	{
		struct unit **xx;
		size_t yy;
		errnum = process_units_in_path(unit_path, &xx, &yy);
		if (errnum != 0) {
			errno = errnum;
			perror("process_units_in_path");
			return EXIT_FAILURE;
		}
		units = xx;
		units_size = yy;
	}

	struct services *services;
	{
		void *xx;
		errnum = linted_mem_alloc(
		    &xx,
		    sizeof *services + units_size * sizeof services->list[0U]);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_mem_alloc");
			return EXIT_FAILURE;
		}
		services = xx;
		services->size = units_size;
	}

	for (size_t ii = 0U; ii < units_size; ++ii) {
		union service *service = &services->list[ii];
		struct unit *unit = units[ii];
		switch (unit_type(unit)) {
		case UNIT_TYPE_SERVICE:
			service->type = SERVICE_TYPE_PROCESS;
			service->process.name = unit_peek_name(unit);
			service->process.pid = -1;
			break;

		case UNIT_TYPE_SOCKET:
			service->type = SERVICE_TYPE_FILE;
			service->file.name = unit_peek_name(unit);
			service->file.is_open = false;
			break;
		}
	}

	struct conn_pool *conn_pool;

	{
		struct conn_pool *xx;
		errnum = conn_pool_create(&xx);
		if (errnum != 0)
			return errnum;
		conn_pool = xx;
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	struct wait_service_task waiter_task;
	struct new_connection_task new_connection_task;

	linted_manager_accept(LINTED_UPCAST(&new_connection_task),
	                      NEW_CONNECTIONS, new_connections);
	new_connection_task.pool = pool;
	new_connection_task.conn_pool = conn_pool;
	new_connection_task.services = services;

	linted_asynch_pool_submit(
	    pool,
	    LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&new_connection_task))));

	for (size_t ii = 0U; ii < services->size; ++ii) {
		struct unit *unit = units[ii];
		if (unit_type(unit) != UNIT_TYPE_SOCKET)
			continue;

		char const *const *listen_message_queue =
		    unit_section_find(unit, "Socket", "ListenMessageQueue");
		char const *const *mq_max_messages = unit_section_find(
		    unit, "Socket", "MessageQueueMaxMessages");
		char const *const *mq_message_size = unit_section_find(
		    unit, "Socket", "MessageQueueMessageSize");
		char const *const *temporary =
		    unit_section_find(unit, "Socket", "X-Linted-Temporary");

		if (NULL == listen_message_queue ||
		    NULL == listen_message_queue[0U] ||
		    listen_message_queue[1U] != NULL)
			return EINVAL;

		if (NULL == mq_max_messages || NULL == mq_max_messages[0U] ||
		    mq_max_messages[1U] != NULL)
			return EINVAL;

		if (NULL == mq_message_size || NULL == mq_message_size[0U] ||
		    mq_message_size[1U] != NULL)
			return EINVAL;

		if (NULL == temporary || NULL == temporary[0U] ||
		    temporary[1U] != NULL)
			return EINVAL;

		long mq_max_messages_value;
		{
			long xx;
			errnum = long_from_cstring(mq_max_messages[0U], &xx);
			if (errnum != 0)
				return errnum;
			mq_max_messages_value = xx;
		}

		long mq_message_size_value;
		{
			long xx;
			errnum = long_from_cstring(mq_message_size[0U], &xx);
			if (errnum != 0)
				return errnum;
			mq_message_size_value = xx;
		}

		union service *service = &services->list[ii];

		linted_ko ko;
		{
			struct linted_mq_attr attr;
			attr.maxmsg = mq_max_messages_value;
			attr.msgsize = mq_message_size_value;
			errnum = linted_mq_create(&ko, listen_message_queue[0U],
			                          &attr, 0);
		}

		service->file.ko = ko;
		service->file.is_open = true;
	}

	pid_t halt_pid = -1;

	for (size_t ii = 0U; ii < services->size; ++ii) {
		struct unit *unit = units[ii];
		if (unit_type(unit) != UNIT_TYPE_SERVICE)
			continue;

		struct service_process *service = &services->list[ii].process;

		char const *const *halt_after_exit = unit_section_find(
		    unit, "Service", "X-Linted-Halt-After-Exit");

		if (halt_after_exit != NULL && (NULL == halt_after_exit[0U] ||
		                                halt_after_exit[1U] != NULL))
			return EINVAL;

		bool halt_after_exit_value = false;
		if (halt_after_exit != NULL) {
			bool xx;
			errnum = bool_from_cstring(halt_after_exit[0U], &xx);
			if (errnum != 0)
				return errnum;
			halt_after_exit_value = xx;
		}

		pid_t process;
		{
			pid_t xx;
			errnum =
			    spawn_process(&xx, unit, cwd, chrootdir, services);
			if (errnum != 0)
				goto exit_services;
			process = xx;
		}

		service->pid = process;

		if (halt_after_exit_value)
			halt_pid = process;
	}

	linted_asynch_task_waitid(LINTED_UPCAST(&waiter_task), WAITER, P_ALL,
	                          -1, WEXITED);
	waiter_task.pool = pool;
	waiter_task.halt_pid = halt_pid;
	waiter_task.time_to_exit = false;

	linted_asynch_pool_submit(pool,
	                          LINTED_UPCAST(LINTED_UPCAST(&waiter_task)));

	while (!waiter_task.time_to_exit) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto drain_dispatches;
	}

drain_dispatches:
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
		linted_error close_errnum = conn_pool_destroy(conn_pool);
		if (0 == errnum)
			errnum = close_errnum;
	}

exit_services:
	if (-1 == kill(-1, SIGKILL)) {
		linted_error kill_errnum = errno;
		LINTED_ASSUME(kill_errnum != 0);
		if (kill_errnum != ESRCH) {
			assert(kill_errnum != EINVAL);
			assert(kill_errnum != EPERM);
			LINTED_ASSUME_UNREACHABLE();
		}
	}

	for (size_t ii = 0U; ii < services->size; ++ii) {
		union service *service = &services->list[ii];

		if (service->type != SERVICE_TYPE_FILE)
			continue;

		struct service_file *file = &service->file;
		if (file->is_open) {
			if (file->ko == STDERR_FILENO)
				continue;

			linted_error close_errnum = linted_ko_close(file->ko);
			if (0 == errnum)
				errnum = close_errnum;
		}
		file->is_open = false;
	}

	{
		linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
		if (0 == errnum)
			errnum = destroy_errnum;

		/* Insure that the tasks are in proper scope until they are
		 * terminated */
		(void)waiter_task;
		(void)new_connection_task;
	}

	{
		linted_error close_errnum = linted_ko_close(new_connections);
		if (0 == errnum)
			errnum = close_errnum;
	}

	for (size_t ii = 0U; ii < units_size; ++ii)
		unit_put(units[ii]);
	free(units);

	if (errnum != 0) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "could not run the game: %s\n",
		                       linted_error_string(errnum));

		return EXIT_FAILURE;
	}

	if (linted_ko_close(STDERR_FILENO) != 0)
		/* Sadly, this is all we can do */
		return EXIT_FAILURE;

	return EXIT_SUCCESS;
}

static linted_error spawn_process(pid_t *pidp, struct unit *unit, linted_ko cwd,
                                  char const *chrootdir,
                                  struct services const *services)
{
	linted_error errnum = 0;

	char const *const *type = unit_section_find(unit, "Service", "Type");
	char const *const *exec_start =
	    unit_section_find(unit, "Service", "ExecStart");
	char const *const *files =
	    unit_section_find(unit, "Service", "X-Linted-Files");
	char const *const *fstab =
	    unit_section_find(unit, "Service", "X-Linted-Fstab");
	char const *const *environment_whitelist = unit_section_find(
	    unit, "Service", "X-Linted-Environment-Whitelist");

	if (type != NULL && (NULL == type[0U] || type[1U] != NULL))
		return EINVAL;

	if (NULL == exec_start)
		return EINVAL;

	if (fstab != NULL && (NULL == fstab[0U] || fstab[1U] != NULL))
		return EINVAL;

	if (NULL == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type[0U])) {
		/* simple type of service */
	} else {
		return EINVAL;
	}

	char *default_envvars[] = { NULL };
	char **envvars = default_envvars;
	if (environment_whitelist != NULL) {
		char **xx;
		errnum = filter_envvars(&xx, environment_whitelist);
		if (errnum != 0)
			return errnum;
		envvars = xx;
	}

	char const *chdir_path = "/var";
	linted_ko dirko = cwd;
	int clone_flags = CLONE_NEWNS | CLONE_NEWIPC | CLONE_NEWNET;
	char const *const *environment = (char const * const *)envvars;
	char const *const *arguments = (char const * const *)exec_start;
	char const *path = exec_start[0U];

	struct linted_spawn_file_actions *file_actions;
	struct linted_spawn_attr *attr;

	{
		struct linted_spawn_file_actions *xx;
		errnum = linted_spawn_file_actions_init(&xx);
		if (errnum != 0)
			return errnum;
		file_actions = xx;
	}

	{
		struct linted_spawn_attr *xx;
		errnum = linted_spawn_attr_init(&xx);
		if (errnum != 0)
			goto destroy_file_actions;
		attr = xx;
	}

	if (chdir_path != NULL)
		linted_spawn_attr_setcloneflags(attr, clone_flags);

	linted_spawn_attr_drop_caps(attr);

	if (chdir_path != NULL)
		linted_spawn_attr_setchdir(attr, chdir_path);

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

	struct
	{
		linted_ko ko;
		unsigned long options;
	} std[] = { { STDIN_FILENO, LINTED_KO_RDONLY },
		    { STDOUT_FILENO, LINTED_KO_WRONLY },
		    { STDERR_FILENO, LINTED_KO_WRONLY } };

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(std); ++ii) {
		linted_ko ko;
		{
			linted_ko xx;
			errnum =
			    linted_ko_reopen(&xx, std[ii].ko, std[ii].options);
			if (errnum != 0)
				goto destroy_proc_kos;
			ko = xx;
		}

		proc_kos[kos_opened] = ko;
		++kos_opened;

		errnum = linted_spawn_file_actions_adddup2(&file_actions, ko,
		                                           kos_opened - 1U);
		if (errnum != 0)
			goto destroy_proc_kos;
	}

	if (files != NULL) {
		for (size_t ii = 0U; ii < files_size; ++ii) {
			char const *open_command = files[ii];

			if (strncmp(open_command, "OPEN:", strlen("OPEN:")) !=
			    0) {
				errnum = EINVAL;
				goto destroy_proc_kos;
			}

			open_command = open_command + strlen("OPEN:");

			char *filenameend = strchr(open_command, ',');
			char *filename;
			if (NULL == filenameend) {
				filename = strdup(open_command);
			} else {
				filename = strndup(open_command,
				                   filenameend - open_command);
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

			enum {
				RDONLY,
				WRONLY
			};

			static char const *const tokens[] =
			    {[RDONLY] = "rdonly", [WRONLY] = "wronly", NULL };

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
					    &xx, (char * const *)tokens, &yy);
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
			free(opts_buffer);
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

			union service const *service =
			    service_for_name(services, filename);
			if (NULL == service) {
				errnum = EINVAL;
				goto free_filename;
			}

			struct service_file const *file = &service->file;

			linted_ko ko;
			{
				linted_ko xx;
				errnum = linted_ko_reopen(&xx, file->ko, flags);
				if (errnum != 0)
					goto free_filename;
				ko = xx;
			}

			proc_kos[kos_opened] = ko;
			++kos_opened;

			errnum = linted_spawn_file_actions_adddup2(
			    &file_actions, ko, kos_opened - 1U);
			if (errnum != 0)
				goto free_filename;

		free_filename:
			free(filename);

			if (errnum != 0)
				goto destroy_proc_kos;
		}
	}

	pid_t process;
	{
		pid_t xx;
		errnum = linted_spawn(&xx, dirko, path, file_actions, attr,
		                      (char **)arguments, (char **)environment);
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

	if (abspath != fstab_path) {
		linted_mem_free((char *)abspath);
	}

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

		if (0 == strcmp("none", fsname)) {
			fsname = NULL;
		}

		if (0 == strcmp("none", opts)) {
			opts = NULL;
		}

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
			errnum = get_flags_and_data(opts, &xx, &yy, &zz, &ww);
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

static linted_error get_flags_and_data(char const *opts, bool *mkdir_flagp,
                                       bool *touch_flagp,
                                       unsigned long *mountflagsp,
                                       char const **leftoversp)
{
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

	linted_error errnum;

	static char const *const tokens[] =
	    {[MKDIR] = "mkdir",    [TOUCH] = "touch",        [BIND] = "bind",
	     [RBIND] = "rbind",    [RO] = MNTOPT_RO,         [RW] = MNTOPT_RW,
	     [SUID] = MNTOPT_SUID, [NOSUID] = MNTOPT_NOSUID, [NODEV] = "nodev",
	     [NOEXEC] = "noexec",  NULL };
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
			token = getsubopt(&xx, (char * const *)tokens, &yy);
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
	case NEW_CONNECTIONS:
		return on_new_connection(task);

	case WAITER:
		return on_process_wait(task);

	case READ_CONNECTION:
		return on_read_connection(task);

	case WRITE_CONNECTION:
		return on_write_connection(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_wait(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0) {
		return errnum;
	}

	struct wait_service_task *wait_service_task =
	    LINTED_DOWNCAST(struct wait_service_task, task);

	struct linted_asynch_pool *pool = wait_service_task->pool;
	pid_t halt_pid = wait_service_task->halt_pid;

	int exit_status;
	int exit_code;
	pid_t pid;
	{
		siginfo_t *exit_info = &LINTED_UPCAST(wait_service_task)->info;
		exit_status = exit_info->si_status;
		exit_code = exit_info->si_code;
		pid = exit_info->si_pid;
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
		goto process_exited;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	return 0;

process_exited:
	if (pid == halt_pid)
		wait_service_task->time_to_exit = true;

	return 0;
}

static linted_error on_new_connection(struct linted_asynch_task *completed_task)
{
	linted_error errnum;

	if ((errnum = completed_task->errnum) != 0)
		return errnum;

	struct new_connection_task *new_connection_task =
	    LINTED_DOWNCAST(struct new_connection_task, completed_task);

	struct linted_ko_task_accept *accept_task =
	    LINTED_UPCAST(LINTED_UPCAST(new_connection_task));

	struct linted_asynch_pool *pool = new_connection_task->pool;
	struct conn_pool *conn_pool = new_connection_task->conn_pool;

	struct services const *services = new_connection_task->services;

	linted_manager new_socket = accept_task->returned_ko;
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

	linted_manager_recv_request(LINTED_UPCAST(&conn->read_task),
	                            READ_CONNECTION, new_socket);
	conn->read_task.pool = pool;
	conn->read_task.conn_pool = conn_pool;
	conn->read_task.conn = conn;
	conn->read_task.services = services;

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

static linted_error on_read_connection(struct linted_asynch_task *task)
{
	linted_error errnum;

	struct read_conn_task *read_conn_task =
	    LINTED_DOWNCAST(struct read_conn_task, task);

	struct linted_asynch_pool *pool = read_conn_task->pool;
	struct conn_pool *conn_pool = read_conn_task->conn_pool;
	struct conn *conn = read_conn_task->conn;
	struct services const *services = read_conn_task->services;

	if ((errnum = task->errnum) != 0) {
		/* The other end did something bad */
		errnum = conn_remove(conn, conn_pool);
		if (errnum != 0)
			return errnum;
		return 0;
	}

	struct linted_manager_task_recv_request *task_recv =
	    LINTED_UPCAST(read_conn_task);
	struct linted_ko_task_read *task_read = LINTED_UPCAST(task_recv);

	linted_ko ko = task_read->ko;

	union linted_manager_request *request = &task_recv->request;
	union linted_manager_reply reply;

	switch (request->type) {
	case LINTED_MANAGER_REBOOT:
		if (-1 == reboot(RB_POWER_OFF)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto conn_remove;
		}
		break;

	case LINTED_MANAGER_STATUS: {
		union service const *service =
		    service_for_name(services, request->status.service_name);
		if (NULL == service) {
			reply.status.is_up = false;
			break;
		}

		pid_t pid;
		switch (service->type) {
		case SERVICE_TYPE_PROCESS:
			pid = service->process.pid;
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

	case LINTED_MANAGER_STOP: {
		union service const *service =
		    service_for_name(services, request->status.service_name);
		if (NULL == service) {
			reply.status.is_up = false;
			break;
		}

		pid_t pid;
		switch (service->type) {
		case SERVICE_TYPE_PROCESS:
			pid = service->process.pid;
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

	linted_manager_send_reply(LINTED_UPCAST(&conn->write_task),
	                          WRITE_CONNECTION, ko, &reply);
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

static linted_error on_write_connection(struct linted_asynch_task *task)
{
	linted_error errnum;

	struct write_conn_task *write_conn_task =
	    LINTED_DOWNCAST(struct write_conn_task, task);
	struct conn_pool *conn_pool = write_conn_task->conn_pool;
	struct conn *conn = write_conn_task->conn;

	errnum = task->errnum;

	linted_error remove_errnum = conn_remove(conn, conn_pool);
	if (0 == errnum)
		errnum = remove_errnum;

	return errnum;
}

static linted_error dispatch_drainers(struct linted_asynch_task *task)
{
	switch (task->task_action) {
	case NEW_CONNECTIONS:
		return drain_on_new_connection(task);

	case WAITER:
		return drain_on_process_wait(task);

	case READ_CONNECTION:
		return drain_on_read_connection(task);

	case WRITE_CONNECTION:
		return drain_on_write_connection(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error drain_on_new_connection(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct new_connection_task *new_connection_task =
	    LINTED_DOWNCAST(struct new_connection_task, task);

	struct linted_ko_task_accept *accept_task =
	    LINTED_UPCAST(LINTED_UPCAST(new_connection_task));

	linted_manager new_socket = accept_task->returned_ko;

	return linted_ko_close(new_socket);
}

static linted_error drain_on_process_wait(struct linted_asynch_task *task)
{
	return task->errnum;
}

static linted_error drain_on_read_connection(struct linted_asynch_task *task)
{
	return task->errnum;
}

static linted_error drain_on_write_connection(struct linted_asynch_task *task)
{
	return task->errnum;
}

static linted_error check_db(linted_ko cwd)
{
	enum {
		TMP_WRITE_FINISHED
	};

	linted_error errnum;

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, 1U);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	linted_db my_db;
	{
		/**
		 * @todo Place the database according to the XDG base
		 *       directory specification.
		 */
		linted_db xx;
		errnum = linted_db_open(&xx, cwd, "linted-db", LINTED_DB_CREAT);
		if (errnum != 0)
			goto destroy_pool;
		my_db = xx;
	}

	{
		linted_ko tmp;
		char *path;
		{
			linted_ko xx;
			char *yy;
			errnum = linted_db_temp_file(my_db, &xx, &yy);
			if (errnum != 0)
				goto close_db;
			tmp = xx;
			path = yy;
		}

		static char const hello[] = "Hello anybody!";
		char const *data = hello;
		size_t data_size = sizeof hello - 1U;

		struct linted_ko_task_write write_task;

		linted_ko_task_write(&write_task, TMP_WRITE_FINISHED, tmp, data,
		                     data_size);
		linted_asynch_pool_submit(pool, LINTED_UPCAST(&write_task));

		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		if ((errnum = completed_task->errnum) != 0)
			goto close_tmp;

		switch (completed_task->task_action) {
		case TMP_WRITE_FINISHED:
			goto done_writing;

		default:
			LINTED_ASSUME_UNREACHABLE();
		}

	done_writing:
		errnum = linted_db_temp_send(my_db, path, "hello");
		if (errnum != 0)
			goto close_tmp;

	close_tmp:
		linted_mem_free(path);

		linted_error close_errnum = linted_ko_close(tmp);
		if (0 == errnum)
			errnum = close_errnum;
	}

close_db : {
	linted_error close_errnum = linted_db_close(my_db);
	if (0 == errnum)
		errnum = close_errnum;
}

destroy_pool:
	linted_asynch_pool_stop(pool);

	linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
	if (0 == errnum)
		errnum = destroy_errnum;

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

static union service const *service_for_name(struct services const *services,
                                             const char *name)
{
	for (size_t ii = 0U; ii < services->size; ++ii) {
		union service const *service = &services->list[ii];
		switch (service->type) {
		case SERVICE_TYPE_PROCESS:
			if (0 == strncmp(service->process.name, name,
			                 LINTED_SERVICE_NAME_MAX))
				return service;
			break;

		case SERVICE_TYPE_FILE:
			if (0 == strncmp(service->file.name, name,
			                 LINTED_SERVICE_NAME_MAX))
				return service;
			break;
		}
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

static linted_error process_units_in_path(char const *unit_path,
                                          struct unit ***unitsp, size_t *sizep)
{
	linted_ko errnum = 0;

	struct unit **units = NULL;
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

		free(dir_name);

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
			char **new_files =
			    realloc(files, new_files_count * sizeof files[0U]);
			if (NULL == new_files) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_file_name;
			}
			new_files[files_count] = name_copy;

			files = new_files;
			files_count = new_files_count;

			if (errnum != 0) {
			free_file_name:
				free(name_copy);
				goto free_file_names;
			}
		}

		for (size_t ii = 0U; ii < files_count; ++ii) {
			char const *file_name = files[ii];

			char const *dot = strchr(file_name, '.');
			if (NULL == dot) {
				errnum = EINVAL;
				goto free_file_names;
			}

			char *unit_name = strndup(file_name, dot - file_name);
			if (NULL == unit_name) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_file_names;
			}

			char const *suffix = dot + 1U;

			enum unit_type unit_type;
			if (0 == strcmp(suffix, "socket")) {
				unit_type = UNIT_TYPE_SOCKET;
			} else if (0 == strcmp(suffix, "service")) {
				unit_type = UNIT_TYPE_SERVICE;
			} else {
				errnum = EINVAL;
				goto free_unit_name;
			}

			linted_ko unit_fd =
			    openat(dirfd(units_dir), file_name, O_RDONLY);
			if (-1 == unit_fd) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_unit_name;
			}

			FILE *unit_file = fdopen(unit_fd, "r");
			if (NULL == unit_file) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);

				close(unit_fd);

				goto free_unit_name;
			}

			struct unit *unit = NULL;
			{
				struct unit *xx;
				errnum = parse_unit_file(&xx, unit_file,
				                         unit_type, unit_name);
				if (errnum != 0) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
					goto close_unit_file;
				}
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
			struct unit **new_units =
			    realloc(units, new_units_size * sizeof units[0U]);
			if (NULL == new_units) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_unit;
			}
			new_units[units_size] = unit;

			units = new_units;
			units_size = new_units_size;

		free_unit:
			if (errnum != 0)
				unit_put(unit);

		free_unit_name:
			free(unit_name);
		}

	free_file_names:
		for (size_t ii = 0U; ii < files_count; ++ii)
			free(files[ii]);
		free(files);

		if (-1 == closedir(units_dir)) {
			if (0 == errnum) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			}
		}
		if (errnum != 0)
			goto free_units;

		if (NULL == dirend)
			break;

		dirstart = dirend + 1U;
	}

free_units:
	if (errnum != 0) {
		for (size_t ii = 0U; ii < units_size; ++ii)
			unit_put(units[ii]);
		free(units);
	} else {
		*unitsp = units;
		*sizep = units_size;
	}

	return errnum;
}

static linted_error parse_unit_file(struct unit **unitp, FILE *unit_file,
                                    enum unit_type type, char const *name)
{
	linted_error errnum = 0;

	char *line_buffer = NULL;
	size_t line_capacity = 0U;

	struct unit *unit;
	{
		struct unit *xx;
		errnum = unit_create(&xx, type, name);
		if (errnum != 0)
			return errnum;
		unit = xx;
	}

	struct unit_section *current_section = NULL;

	for (;;) {
		size_t line_size;
		{
			char *xx = line_buffer;
			size_t yy = line_capacity;
			errno = 0;
			ssize_t zz = getline(&xx, &yy, unit_file);
			if (-1 == zz) {
				errnum = errno;
				/* May be 0 to indicate end of line */
				break;
			}
			line_buffer = xx;
			line_capacity = yy;
			line_size = zz;
		}
		if (0U == line_size)
			break;

		if ('\n' == line_buffer[line_size - 1U])
			--line_size;

		/* Ignore empty lines */
		if (0U == line_size)
			continue;

		switch (line_buffer[0U]) {
		/* Ignore comments */
		case ';':
		case '#':
			continue;

		/* A section start */
		case '[': {
			if (line_buffer[line_size - 1U] != ']') {
				errnum = EINVAL;
				goto free_line_buffer;
			}

			char *section_name = malloc(line_size - 1U);
			if (NULL == section_name) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_line_buffer;
			}
			section_name[line_size - 2U] = '\0';
			memcpy(section_name, line_buffer + 1U, line_size - 2U);

			{
				struct unit_section *xx;
				errnum =
				    unit_add_section(unit, &xx, section_name);
				if (0 == errnum)
					current_section = xx;
			}

			if (errnum != 0) {
				free(section_name);
				goto free_line_buffer;
			}
			break;
		}

		default: {
			if (NULL == current_section) {
				errnum = EINVAL;
				goto free_line_buffer;
			}

			bool has_equals_sign = false;
			size_t equals_position;
			for (size_t ii = 0U; ii < line_size; ++ii) {
				if ('=' == line_buffer[ii]) {
					has_equals_sign = true;
					equals_position = ii;
					break;
				}
			}

			if (!has_equals_sign) {
				errnum = EINVAL;
				goto free_line_buffer;
			}

			size_t field_len = equals_position;
			size_t value_offset = field_len + 1U;
			size_t value_len = line_size - value_offset;

			char *field = malloc(field_len + 1U);
			if (NULL == field) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_line_buffer;
			}
			memcpy(field, line_buffer, field_len);
			field[field_len] = '\0';

			char *value = malloc(value_len + 1U);
			if (NULL == value) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				goto free_field;
			}
			memcpy(value, line_buffer + value_offset, value_len);
			value[value_len] = '\0';

			wordexp_t expr;
			switch (wordexp(value, &expr, WRDE_NOCMD)) {
			case WRDE_BADCHAR:
			case WRDE_CMDSUB:
			case WRDE_SYNTAX:
				errnum = EINVAL;
				break;

			case WRDE_NOSPACE:
				errnum = ENOMEM;
				break;
			}

			free(value);

			if (errnum != 0)
				goto free_field;

			errnum = unit_section_add_setting(
			    current_section, field,
			    (char const * const *)expr.we_wordv);

			wordfree(&expr);

		free_field:
			if (errnum != 0)
				free(field);

			if (errnum != 0)
				goto free_line_buffer;
		}
		}
	}

free_line_buffer:
	free(line_buffer);

	if (errnum != 0)
		unit_put(unit);

	if (0 == errnum)
		*unitp = unit;

	return errnum;
}

struct unit_section_bucket
{
	size_t sections_size;
	struct unit_section *sections;
};

#define SECTION_BUCKETS_SIZE 1024U

struct unit
{
	char *name;
	enum unit_type type;
	unsigned long refcount;
	struct unit_section_bucket buckets[SECTION_BUCKETS_SIZE];
};

struct unit_setting_buckets;

#define SETTING_BUCKETS_SIZE 1024U

struct unit_setting_bucket
{
	size_t settings_size;
	struct unit_setting *settings;
};

struct unit_section
{
	unsigned long refcount;
	char *name;
	struct unit_setting_bucket buckets[SETTING_BUCKETS_SIZE];
};

struct unit_setting
{
	char *field;
	char **value;
};

static size_t string_hash(char const *str);

static linted_error unit_create(struct unit **unitp, enum unit_type type,
                                char const *name)
{
	linted_error errnum = 0;
	struct unit *unit;

	char *name_copy = strdup(name);
	if (NULL == name_copy) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	unit = malloc(sizeof *unit);
	if (NULL == unit) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_name_copy;
	}

	unit->type = type;
	unit->name = name_copy;
	unit->refcount = 1;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct unit_section_bucket *bucket = &unit->buckets[ii];

		bucket->sections_size = 0U;
		bucket->sections = NULL;
	}

free_name_copy:
	if (errnum != 0) {
		free(name_copy);
		return errnum;
	}

	*unitp = unit;

	return 0;
}

static void unit_put(struct unit *unit)
{
	if (--unit->refcount != 0)
		return;

	for (size_t ii = 0U; ii < SECTION_BUCKETS_SIZE; ++ii) {
		struct unit_section_bucket const *bucket = &unit->buckets[ii];

		for (size_t jj = 0U; jj < bucket->sections_size; ++jj) {
			struct unit_section const *section =
			    &bucket->sections[jj];

			for (size_t kk = 0U; kk < SETTING_BUCKETS_SIZE; ++kk) {
				struct unit_setting_bucket const *
				setting_bucket = &section->buckets[kk];

				for (size_t ww = 0U;
				     ww < setting_bucket->settings_size; ++ww) {
					free(
					    setting_bucket->settings[ww].field);
					free(
					    setting_bucket->settings[ww].value);
				}

				free(setting_bucket->settings);
			}
		}

		free(bucket->sections);
	}
	free(unit->name);
	free(unit);
}

static enum unit_type unit_type(struct unit *unit)
{
	return unit->type;
}

static char const *unit_peek_name(struct unit *unit)
{
	return unit->name;
}

static linted_error unit_add_section(struct unit *unit,
                                     struct unit_section **sectionp,
                                     char *section_name)
{
	linted_error errnum;

	struct unit_section_bucket *buckets = unit->buckets;

	struct unit_section_bucket *bucket =
	    &buckets[string_hash(section_name) % SECTION_BUCKETS_SIZE];

	size_t sections_size = bucket->sections_size;
	struct unit_section *sections = bucket->sections;

	bool have_found_field = false;
	size_t found_field;
	for (size_t ii = 0U; ii < sections_size; ++ii) {
		if (0 == strcmp(sections[ii].name, section_name)) {
			have_found_field = true;
			found_field = ii;
			break;
		}
	}

	if (have_found_field) {
		free(section_name);
		*sectionp = &sections[found_field];
	} else {
		size_t new_sections_size = sections_size + 1U;
		struct unit_section *new_sections =
		    realloc(sections, new_sections_size * sizeof sections[0U]);
		if (NULL == new_sections) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}

		struct unit_section *new_section = &new_sections[sections_size];

		new_section->name = section_name;

		for (size_t ii = 0U; ii < SETTING_BUCKETS_SIZE; ++ii) {
			new_section->buckets[ii].settings_size = 0U;
			new_section->buckets[ii].settings = NULL;
		}

		bucket->sections_size = new_sections_size;
		bucket->sections = new_sections;

		*sectionp = new_section;
	}

	return 0;
}

static char const *const *
unit_section_find(struct unit *unit, char const *section, char const *field)
{
	struct unit_section *found_section;

	{
		struct unit_section_bucket *buckets = unit->buckets;
		struct unit_section_bucket *bucket =
		    &buckets[string_hash(section) % SECTION_BUCKETS_SIZE];

		size_t sections_size = bucket->sections_size;
		struct unit_section *sections = bucket->sections;

		bool have_found_section = false;
		for (size_t ii = 0U; ii < sections_size; ++ii) {
			if (0 == strcmp(sections[ii].name, section)) {
				have_found_section = true;
				found_section = &sections[ii];
			}
		}

		if (!have_found_section)
			return NULL;
	}

	struct unit_setting_bucket *buckets = found_section->buckets;
	struct unit_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct unit_setting *settings = bucket->settings;

	struct unit_setting *found_setting;
	bool have_found_setting = false;
	for (size_t ii = 0U; ii < settings_size; ++ii) {
		if (0 == strcmp(settings[ii].field, field)) {
			have_found_setting = true;
			found_setting = &settings[ii];
		}
	}

	if (!have_found_setting)
		return NULL;

	return (char const * const *)found_setting->value;
}

static linted_error unit_section_add_setting(struct unit_section *section,
                                             char *field,
                                             char const *const *value)
{
	linted_error errnum;

	struct unit_setting_bucket *buckets = section->buckets;

	struct unit_setting_bucket *bucket =
	    &buckets[string_hash(field) % SETTING_BUCKETS_SIZE];

	size_t settings_size = bucket->settings_size;
	struct unit_setting *settings = bucket->settings;

	bool have_found_field = false;
	size_t found_field;
	for (size_t ii = 0U; ii < settings_size; ++ii) {
		if (0 == strcmp(settings[ii].field, field)) {
			have_found_field = true;
			found_field = ii;
			break;
		}
	}

	if (have_found_field) {
		if (NULL == value[0U]) {
			free(field);
			free(settings[found_field].field);

			char **values = settings[found_field].value;

			for (size_t ii = 0U; values[ii] != NULL; ++ii)
				free(values[ii]);

			free(values);

			bucket->settings_size = settings_size - 1U;
			memcpy(bucket->settings + found_field,
			       buckets->settings + found_field + 1U,
			       (settings_size - 1U - found_field) *
			           sizeof bucket->settings[0U]);
		} else {
			char **old_value = settings[found_field].value;

			size_t old_value_len;
			for (size_t ii = 0U;; ++ii) {
				if (NULL == old_value[ii]) {
					old_value_len = ii;
					break;
				}
			}

			size_t value_len;
			for (size_t ii = 0U;; ++ii) {
				if (NULL == value[ii]) {
					value_len = ii;
					break;
				}
			}

			size_t new_value_len = old_value_len + value_len;

			char **new_value =
			    malloc((new_value_len + 1U) * sizeof value[0U]);
			if (NULL == new_value) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				return errnum;
			}

			for (size_t ii = 0U; ii < old_value_len; ++ii)
				new_value[ii] = old_value[ii];

			for (size_t ii = 0U; ii < value_len; ++ii) {
				char *copy = strdup(value[ii]);
				if (NULL == copy) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
					for (; ii != 0; --ii)
						free(new_value[ii - 1U]);

					free(new_value);
					return errnum;
				}
				new_value[old_value_len + ii] = copy;
			}

			new_value[new_value_len] = NULL;

			free(settings[found_field].field);
			free(old_value);

			settings[found_field].field = field;
			settings[found_field].value = new_value;
		}
	} else {
		if (NULL == value[0U]) {
			free(field);
		} else {
			size_t new_settings_size = settings_size + 1U;
			struct unit_setting *new_settings = realloc(
			    settings, new_settings_size * sizeof settings[0U]);
			if (NULL == new_settings) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
				return errnum;
			}

			size_t value_len;
			for (size_t ii = 0U;; ++ii) {
				if (NULL == value[ii]) {
					value_len = ii;
					break;
				}
			}

			char **value_copy =
			    malloc((value_len + 1U) * sizeof value[0U]);
			for (size_t ii = 0U; ii < value_len; ++ii)
				value_copy[ii] = strdup(value[ii]);
			value_copy[value_len] = NULL;

			new_settings[settings_size].field = field;
			new_settings[settings_size].value = value_copy;

			bucket->settings_size = new_settings_size;
			bucket->settings = new_settings;
		}
	}

	return 0;
}

static size_t string_hash(char const *str)
{
	size_t hash = 0U;
	for (size_t ii = 0U; str[ii] != '\0'; ++ii)
		hash = hash * 31 + str[ii];
	return hash;
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
