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
#include "linted/admin.h"
#include "linted/async.h"
#include "linted/conf.h"
#include "linted/dir.h"
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/fifo.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/path.h"
#include "linted/pid.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

#if defined HAVE_POSIX_API
#include "linted/prctl.h"
#include "linted/ptrace.h"
#endif

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <fcntl.h>
#include <libgen.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined HAVE_POSIX_API
#include <sys/wait.h>
#endif

#if defined HAVE_POSIX_API
#ifndef PTRACE_EVENT_STOP
#define PTRACE_EVENT_STOP 128
#endif
#endif

enum { WAITID,
       SIGNAL_WAIT,
       ADMIN_IN_READ,
       ADMIN_OUT_WRITE,
       KILL_READ,
       MAX_TASKS };

struct monitor {
	char const *process_name;
	char const *sandbox;
	char const *waiter;
	struct linted_admin_in_task_read *read_task;
	struct linted_admin_out_task_write *write_task;
	struct linted_async_pool *pool;
	struct linted_unit_db *unit_db;
	linted_admin_in admin_in;
	linted_admin_out admin_out;
	linted_pid manager_pid;
	linted_ko cwd;
	bool time_to_exit : 1U;
};

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      linted_ko cwd, char const *path);
static linted_error add_unit_dir_to_db(struct linted_conf_db *db,
                                       linted_ko cwd,
                                       char const *dir_name);
static linted_error create_unit_db(struct linted_unit_db **unit_dbp,
                                   struct linted_conf_db *conf_db,
                                   char const *sandbox,
                                   char const *waiter);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf,
                                   char const *sandbox,
                                   char const *waiter);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

static linted_error activate_unit_db(char const *process_name,
                                     struct linted_unit_db *unit_db,
                                     linted_pid manager_pid,
                                     linted_ko cwd);

static linted_error dispatch(struct monitor *monitor,
                             struct linted_async_task *completed_task);

static linted_error on_signal(struct monitor *monitor,
                              struct linted_async_task *task);
static linted_error on_process_wait(struct monitor *monitor,
                                    struct linted_async_task *task);
static linted_error on_admin_in_read(struct monitor *monitor,
                                     struct linted_async_task *task);
static linted_error on_admin_out_write(struct monitor *monitor,
                                       struct linted_async_task *task);
static linted_error on_kill_read(struct monitor *monitor,
                                 struct linted_async_task *task);

static linted_error
on_status_request(linted_pid manager_pid,
                  struct linted_admin_status_request const *request,
                  struct linted_admin_status_reply *reply);
static linted_error
on_stop_request(linted_pid manager_pid,
                struct linted_admin_stop_request const *request,
                struct linted_admin_stop_reply *reply);

static linted_error on_child_stopped(char const *process_name,
                                     linted_pid pid);

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, linted_pid pid,
                                     int exit_status,
                                     linted_pid manager_pid,
                                     linted_ko cwd,
                                     struct linted_unit_db *unit_db);
static linted_error on_child_signaled(char const *process_name,
                                      linted_pid pid, int exit_status);
static linted_error on_child_about_to_clone(linted_pid pid);
static linted_error
on_child_about_to_exit(char const *process_name, bool time_to_exit,
                       linted_pid pid, linted_pid manager_pid,
                       linted_ko cwd, struct linted_unit_db *unit_db);
static linted_error
on_child_linted_ptrace_event_stopped(char const *process_name,
                                     linted_pid pid, int exit_status);
static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit,
                                     linted_pid manager_pid,
                                     linted_ko cwd, bool check);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error str_from_strs(char const *const *strs,
                                  char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);

static linted_error service_children_terminate(linted_pid pid);

static linted_error pid_is_child_of(linted_pid parent, linted_pid child,
                                    bool *isp);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor", 0};

static char const *const default_envvars[] = {
    "MALLOC_CHECK_", "MALLOC_PERTURB_", "MANAGERPID", "USER", "LOGNAME",
    "HOME", "SHELL", "XDG_RUNTIME_DIR"
                     "XDG_SESSION_ID",
    "XDG_SEAT", "TERM", "LD_DEBUG", "LD_DEBUG_OUTPUT", 0};

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err;

	err = linted_prctl_set_death_sig(SIGKILL);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_prctl_set_death_sig: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	char const *manager_pid_str;
	{
		char *xx;
		err = linted_environment_get("MANAGERPID", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		manager_pid_str = xx;
	}

	char const *unit_path;
	{
		char *xx;
		err = linted_environment_get("LINTED_UNIT_PATH", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		unit_path = xx;
	}

	char const *sandbox;
	{
		char *xx;
		err = linted_environment_get("LINTED_SANDBOX", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		sandbox = xx;
	}

	char const *waiter;
	{
		char *xx;
		err = linted_environment_get("LINTED_WAITER", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		waiter = xx;
	}

	if (0 == manager_pid_str) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "MANAGERPID");
		return EXIT_FAILURE;
	}

	if (0 == unit_path) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_UNIT_PATH");
		return EXIT_FAILURE;
	}

	if (0 == sandbox) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_SANDBOX");
		return EXIT_FAILURE;
	}

	if (0 == waiter) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_WAITER");
		return EXIT_FAILURE;
	}

	linted_pid manager_pid;
	{
		linted_pid yy;
		err = linted_pid_from_str(manager_pid_str, &yy);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_pid_from_str: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		manager_pid = yy;
	}

	char *package_runtime_dir_path;
	{
		char *xx;
		err = linted_path_package_runtime_dir(&xx);
		if (err != 0) {
			linted_log(
			    LINTED_LOG_ERROR,
			    "linted_path_package_runtime_dir: %s",
			    linted_error_string(err));
			return EXIT_FAILURE;
		}
		package_runtime_dir_path = xx;
	}

	char *package_data_home_path;
	{
		char *xx;
		err = linted_path_package_data_home(&xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_path_package_data_home: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		package_data_home_path = xx;
	}

	char *process_runtime_dir_path;
	{
		char *xx;
		err = linted_str_format(&xx, "%s/%" PRIuMAX,
		                        package_runtime_dir_path,
		                        (uintmax_t)manager_pid);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_str_format: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		process_runtime_dir_path = xx;
	}

	char *process_data_home_path;
	{
		char *xx;
		err = linted_str_format(&xx, "%s/%" PRIuMAX,
		                        package_data_home_path,
		                        (uintmax_t)manager_pid);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_str_format: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		process_data_home_path = xx;
	}

	err = linted_dir_create(0, LINTED_KO_CWD,
	                        package_runtime_dir_path, 0U, S_IRWXU);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	err = linted_dir_create(0, LINTED_KO_CWD,
	                        package_data_home_path, 0U, S_IRWXU);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	err = linted_dir_create(0, LINTED_KO_CWD,
	                        process_runtime_dir_path, 0U, S_IRWXU);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	err = linted_dir_create(0, LINTED_KO_CWD,
	                        process_data_home_path, 0U, S_IRWXU);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	linted_ko cwd;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, ".",
		                     LINTED_KO_DIRECTORY);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	err = linted_ko_change_directory(process_runtime_dir_path);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_ko_change_directory: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	err = linted_ko_symlink(process_data_home_path, "var");
	if (err != 0) {
		if (errno != EEXIST) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_symlink: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	linted_admin_in admin_in;
	{
		linted_admin_in xx;
		err = linted_fifo_create(&xx, LINTED_KO_CWD, "admin-in",
		                         LINTED_FIFO_RDWR,
		                         S_IRUSR | S_IWUSR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_fifo_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		admin_in = xx;
	}

	linted_admin_out admin_out;
	{
		linted_admin_out xx;
		err = linted_fifo_create(&xx, LINTED_KO_CWD,
		                         "admin-out", LINTED_FIFO_RDWR,
		                         S_IRUSR | S_IWUSR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_fifo_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		admin_out = xx;
	}

	linted_fifo kill_fifo;
	{
		linted_admin_out xx;
		err = linted_fifo_create(&xx, LINTED_KO_CWD, "kill",
		                         LINTED_FIFO_RDWR,
		                         S_IRUSR | S_IWUSR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_fifo_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		kill_fifo = xx;
	}

	struct linted_async_pool *pool;
	{
		struct linted_async_pool *xx;
		err = linted_async_pool_create(&xx, MAX_TASKS);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_async_pool_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	struct linted_signal_task_wait *signal_wait_task;
	struct linted_pid_task_waitid *sandbox_task;
	struct linted_admin_in_task_read *admin_in_read_task;
	struct linted_admin_out_task_write *write_task;
	struct linted_io_task_read *kill_read_task;

	{
		struct linted_signal_task_wait *xx;
		err = linted_signal_task_wait_create(&xx, 0);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_io_task_read_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		signal_wait_task = xx;
	}

	{
		struct linted_pid_task_waitid *xx;
		err = linted_pid_task_waitid_create(&xx, 0);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_pid_task_waitid_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		sandbox_task = xx;
	}

	{
		struct linted_admin_in_task_read *xx;
		err = linted_admin_in_task_read_create(&xx, 0);
		if (err != 0) {
			linted_log(
			    LINTED_LOG_ERROR,
			    "linted_admin_in_task_read_create: %s",
			    linted_error_string(err));
			return EXIT_FAILURE;
		}
		admin_in_read_task = xx;
	}

	{
		struct linted_admin_out_task_write *xx;
		err = linted_admin_out_task_write_create(&xx, 0);
		if (err != 0) {
			linted_log(
			    LINTED_LOG_ERROR,
			    "linted_admin_out_task_write_create: %s",
			    linted_error_string(err));
			return EXIT_FAILURE;
		}
		write_task = xx;
	}

	{
		struct linted_io_task_read *xx;
		err = linted_io_task_read_create(&xx, 0);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_io_task_read_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		kill_read_task = xx;
	}

	struct linted_conf_db *conf_db;
	{
		struct linted_conf_db *xx;
		err = conf_db_from_path(&xx, cwd, unit_path);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "conf_db_from_path(%s): %s",
			           unit_path, linted_error_string(err));
			return EXIT_FAILURE;
		}
		conf_db = xx;
	}

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		err = create_unit_db(&xx, conf_db, sandbox, waiter);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "create_unit_db: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		unit_db = xx;
	}

	/**
	 * @todo Warn about unactivated unit_db.
	 */
	err = activate_unit_db(process_name, unit_db, manager_pid, cwd);
	if (err != 0)
		goto kill_procs;

	static struct monitor monitor = {0};

	monitor.admin_in = admin_in;
	monitor.admin_out = admin_out;
	monitor.cwd = cwd;
	monitor.manager_pid = manager_pid;
	monitor.pool = pool;
	monitor.process_name = process_name;
	monitor.read_task = admin_in_read_task;
	monitor.sandbox = sandbox;
	monitor.time_to_exit = false;
	monitor.unit_db = unit_db;
	monitor.waiter = waiter;
	monitor.write_task = write_task;

	linted_signal_task_wait_prepare(
	    signal_wait_task,
	    (union linted_async_ck){.u64 = SIGNAL_WAIT});
	linted_async_pool_submit(
	    pool, linted_signal_task_wait_to_async(signal_wait_task));

	linted_admin_in_task_read_prepare(
	    admin_in_read_task,
	    (union linted_async_ck){.u64 = ADMIN_IN_READ}, admin_in);
	linted_async_pool_submit(
	    pool,
	    linted_admin_in_task_read_to_async(admin_in_read_task));

	linted_pid_task_waitid_prepare(
	    sandbox_task, (union linted_async_ck){.u64 = WAITID}, P_ALL,
	    -1, WEXITED | WSTOPPED);
	linted_async_pool_submit(
	    pool, linted_pid_task_waitid_to_async(sandbox_task));

	static char dummy;

	linted_io_task_read_prepare(
	    kill_read_task, (union linted_async_ck){.u64 = KILL_READ},
	    kill_fifo, &dummy, sizeof dummy);
	linted_async_pool_submit(
	    pool, linted_io_task_read_to_async(kill_read_task));

	linted_signal_listen_to_sighup();
	linted_signal_listen_to_sigint();
	linted_signal_listen_to_sigquit();
	linted_signal_listen_to_sigterm();

	for (;;) {
		struct linted_async_task *completed_task;
		{
			struct linted_async_task *xx;
			linted_async_pool_wait(pool, &xx);
			completed_task = xx;
		}

		err = dispatch(&monitor, completed_task);
		if (err != 0)
			goto cancel_tasks;
	}

cancel_tasks:
	if (LINTED_ERROR_CANCELLED == err)
		err = 0;

	linted_async_task_cancel(
	    linted_pid_task_waitid_to_async(sandbox_task));
	linted_async_task_cancel(
	    linted_admin_in_task_read_to_async(admin_in_read_task));

	for (;;) {
		struct linted_async_task *completed_task;
		{
			struct linted_async_task *xx;
			if (LINTED_ERROR_AGAIN ==
			    linted_async_pool_poll(pool, &xx))
				break;
			completed_task = xx;
		}

		linted_error dispatch_error =
		    dispatch(&monitor, completed_task);
		if (0 == err)
			err = dispatch_error;
	}

kill_procs:
	linted_unit_db_destroy(unit_db);

	linted_conf_db_destroy(conf_db);

	linted_error destroy_err = linted_async_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;

	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)sandbox_task;
	(void)admin_in_read_task;

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error create_unit_db(struct linted_unit_db **unit_dbp,
                                   struct linted_conf_db *conf_db,
                                   char const *sandbox,
                                   char const *waiter)
{
	linted_error err;

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		err = linted_unit_db_create(&xx);
		if (err != 0)
			return err;
		unit_db = xx;
	}

	size_t size = linted_conf_db_size(conf_db);
	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_unit *unit;
		{
			struct linted_unit *xx;
			err = linted_unit_db_add_unit(unit_db, &xx);
			if (err != 0)
				goto destroy_unit_db;
			unit = xx;
		}

		struct linted_conf *conf =
		    linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		linted_unit_type unit_type;
		if (0 == strcmp(suffix, "socket")) {
			unit_type = LINTED_UNIT_TYPE_SOCKET;
		} else if (0 == strcmp(suffix, "service")) {
			unit_type = LINTED_UNIT_TYPE_SERVICE;
		} else {
			err = LINTED_ERROR_INVALID_PARAMETER;
			goto destroy_unit_db;
		}

		char *unit_name;
		{
			char *xx;
			err = linted_str_dup_len(&xx, file_name,
			                         dot - file_name);
			if (err != 0)
				goto destroy_unit_db;
			unit_name = xx;
		}

		unit->type = unit_type;
		unit->name = unit_name;

		switch (unit_type) {
		case LINTED_UNIT_TYPE_SERVICE: {
			struct linted_unit_service *s = (void *)unit;

			err = service_create(s, conf, sandbox, waiter);
			if (err != 0)
				goto destroy_unit_db;
			break;
		}

		case LINTED_UNIT_TYPE_SOCKET: {
			struct linted_unit_socket *s = (void *)unit;

			err = socket_create(s, conf);
			if (err != 0)
				goto destroy_unit_db;
			break;
		}
		}
	}

	*unit_dbp = unit_db;

	return err;

destroy_unit_db:
	linted_unit_db_destroy(unit_db);
	return err;
}

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf,
                                   char const *sandbox,
                                   char const *waiter)
{
	linted_error err;

	char const *const *types =
	    linted_conf_find(conf, "Service", "Type");
	char const *const *exec_start =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *no_new_privss =
	    linted_conf_find(conf, "Service", "NoNewPrivileges");
	char const *const *chdir_paths =
	    linted_conf_find(conf, "Service", "WorkingDirectory");
	char const *const *fstabs =
	    linted_conf_find(conf, "Service", "X-LintedFstab");
	char const *const *env_whitelist = linted_conf_find(
	    conf, "Service", "X-LintedEnvironmentWhitelist");
	char const *const *clone_flags =
	    linted_conf_find(conf, "Service", "X-LintedCloneFlags");

	char const *type;
	{
		char const *xx;
		err = str_from_strs(types, &xx);
		if (err != 0)
			return err;
		type = xx;
	}

	if (0 == exec_start)
		return LINTED_ERROR_INVALID_PARAMETER;

	char const *no_new_privs;
	{
		char const *xx;
		err = str_from_strs(no_new_privss, &xx);
		if (err != 0)
			return err;
		no_new_privs = xx;
	}

	char const *fstab;
	{
		char const *xx;
		err = str_from_strs(fstabs, &xx);
		if (err != 0)
			return err;
		fstab = xx;
	}

	char const *chdir_path;
	{
		char const *xx;
		err = str_from_strs(chdir_paths, &xx);
		if (err != 0)
			return err;
		chdir_path = xx;
	}

	bool clone_newuser = false;
	bool clone_newpid = false;
	bool clone_newipc = false;
	bool clone_newnet = false;
	bool clone_newns = false;
	bool clone_newuts = false;
	if (clone_flags != 0) {
		for (size_t ii = 0U; clone_flags[ii] != 0; ++ii) {
			char const *flag = clone_flags[ii];
			if (0 == strcmp("CLONE_NEWUSER", flag)) {
				clone_newuser = true;
			} else if (0 == strcmp("CLONE_NEWPID", flag)) {
				clone_newpid = true;
			} else if (0 == strcmp("CLONE_NEWIPC", flag)) {
				clone_newipc = true;
			} else if (0 == strcmp("CLONE_NEWNET", flag)) {
				clone_newnet = true;
			} else if (0 == strcmp("CLONE_NEWNS", flag)) {
				clone_newns = true;
			} else if (0 == strcmp("CLONE_NEWUTS", flag)) {
				clone_newuts = true;
			} else {
				return LINTED_ERROR_INVALID_PARAMETER;
			}
		}
	}

	if (0 == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		return LINTED_ERROR_INVALID_PARAMETER;
	}

	bool no_new_privs_value = false;
	if (no_new_privs != 0) {
		bool xx;
		err = bool_from_cstring(no_new_privs, &xx);
		if (err != 0)
			return err;
		no_new_privs_value = xx;
	}

	linted_sched_priority current_priority;
	{
		linted_sched_priority xx;
		err = linted_sched_getpriority(&xx);
		if (err != 0)
			return err;
		current_priority = xx;
	}

	unit->sandbox = sandbox;
	unit->waiter = waiter;

	unit->exec_start = exec_start;
	unit->no_new_privs = no_new_privs_value;
	unit->fstab = fstab;
	unit->chdir_path = chdir_path;
	unit->env_whitelist = env_whitelist;

	unit->priority = current_priority + 1;
	unit->has_priority = true;

	unit->clone_newuser = clone_newuser;
	unit->clone_newpid = clone_newpid;
	unit->clone_newipc = clone_newipc;
	unit->clone_newnet = clone_newnet;
	unit->clone_newns = clone_newns;
	unit->clone_newuts = clone_newuts;

	return 0;
}

static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf)
{
	linted_error err;

	char const *const *listen_dirs =
	    linted_conf_find(conf, "Socket", "ListenDirectory");

	char const *const *listen_files =
	    linted_conf_find(conf, "Socket", "ListenFile");

	char const *const *listen_fifos =
	    linted_conf_find(conf, "Socket", "ListenFIFO");

	char const *const *fifo_sizes =
	    linted_conf_find(conf, "Socket", "PipeSize");

	char const *listen_dir;
	{
		char const *xx;
		err = str_from_strs(listen_dirs, &xx);
		if (err != 0)
			return err;
		listen_dir = xx;
	}

	char const *listen_file;
	{
		char const *xx;
		err = str_from_strs(listen_files, &xx);
		if (err != 0)
			return err;
		listen_file = xx;
	}

	char const *listen_fifo;
	{
		char const *xx;
		err = str_from_strs(listen_fifos, &xx);
		if (err != 0)
			return err;
		listen_fifo = xx;
	}

	char const *fifo_size;
	{
		char const *xx;
		err = str_from_strs(fifo_sizes, &xx);
		if (err != 0)
			return err;
		fifo_size = xx;
	}

	linted_unit_socket_type socket_type;
	char const *path = 0;

	if (listen_dir != 0) {
		socket_type = LINTED_UNIT_SOCKET_TYPE_DIR;
		path = listen_dir;
	}

	if (listen_file != 0) {
		if (path != 0)
			return LINTED_ERROR_INVALID_PARAMETER;
		socket_type = LINTED_UNIT_SOCKET_TYPE_FILE;
		path = listen_file;
	}

	if (listen_fifo != 0) {
		if (path != 0)
			return LINTED_ERROR_INVALID_PARAMETER;
		socket_type = LINTED_UNIT_SOCKET_TYPE_FIFO;
		path = listen_fifo;
	}

	if (0 == path)
		return LINTED_ERROR_INVALID_PARAMETER;

	int fifo_size_value = -1;
	if (fifo_size != 0) {
		if (0 == listen_fifo)
			return LINTED_ERROR_INVALID_PARAMETER;
		fifo_size_value = atoi(fifo_size);
	}

	switch (socket_type) {
	case LINTED_UNIT_SOCKET_TYPE_DIR:
	case LINTED_UNIT_SOCKET_TYPE_FILE:
		break;

	case LINTED_UNIT_SOCKET_TYPE_FIFO:
		unit->fifo_size = fifo_size_value;
		break;
	}

	unit->type = socket_type;
	unit->path = path;

	return 0;
}

static linted_error activate_unit_db(char const *process_name,
                                     struct linted_unit_db *unit_db,
                                     linted_pid manager_pid,
                                     linted_ko cwd)
{
	linted_error err;

	size_t db_size = linted_unit_db_size(unit_db);

	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct linted_unit *unit =
		    linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SOCKET)
			continue;

		err = linted_unit_socket_activate((void *)unit);
		if (err != 0)
			return err;
	}

	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct linted_unit *unit =
		    linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		err = service_activate(process_name, unit, manager_pid,
		                       cwd, true);
		if (err != 0)
			return err;
	}

	return 0;
}

struct my_option {
	char const *name;
	char const *value;
	bool flag : 1U;
};

static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit,
                                     linted_pid manager_pid,
                                     linted_ko cwd, bool check)
{
	linted_error err = 0;

	char const *unit_name = unit->name;

	struct linted_unit_service *unit_service = (void *)unit;

	if (!check)
		goto spawn_service;

	linted_pid child;
	{
		linted_pid xx;
		err = linted_unit_pid(&xx, manager_pid, unit_name);
		if (err != 0)
			goto service_not_found;
		child = xx;
	}

	linted_io_write_format(
	    LINTED_KO_STDERR, 0, "%s: ptracing %" PRIiMAX " %s\n",
	    process_name, (intmax_t)child, unit_name);

	return linted_ptrace_seize(child, PTRACE_O_TRACEEXIT);

service_not_found:
	if (err != ESRCH)
		return err;
spawn_service:
	;
	char const *const *exec_start = unit_service->exec_start;
	bool no_new_privs = unit_service->no_new_privs;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *env_whitelist = unit_service->env_whitelist;

	char const *sandbox = unit_service->sandbox;
	char const *waiter = unit_service->waiter;

	bool has_priority = unit_service->has_priority;

	bool clone_newuser = unit_service->clone_newuser;
	bool clone_newpid = unit_service->clone_newpid;
	bool clone_newipc = unit_service->clone_newipc;
	bool clone_newnet = unit_service->clone_newnet;
	bool clone_newns = unit_service->clone_newns;
	bool clone_newuts = unit_service->clone_newuts;

	linted_sched_priority priority;
	if (has_priority)
		priority = unit_service->priority;

	if (fstab != 0) {
		linted_ko name_dir;
		{
			linted_ko xx;
			err = linted_dir_create(&xx, LINTED_KO_CWD,
			                        unit_name, 0U, S_IRWXU);
			if (err != 0)
				return err;
			name_dir = xx;
		}

		err = linted_dir_create(0, name_dir, "chroot", 0U,
		                        S_IRWXU);

		linted_ko_close(name_dir);

		if (err != 0)
			return err;
	}

	if (fstab != 0 && !clone_newns)
		return LINTED_ERROR_INVALID_PARAMETER;

	bool drop_caps = true;

	/* Favor other processes over this process hierarchy.  Only
	 * superuser may lower priorities so this is not
	 * stoppable. This also makes the process hierarchy nicer for
	 * the OOM killer.
	 */
	char *prio_str = 0;
	if (has_priority) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)priority);
		if (err != 0)
			return err;
		prio_str = xx;
	}

	char *chrootdir;
	{
		char *xx;
		err = linted_str_format(&xx, "%s/chroot", unit_name);
		if (err != 0)
			goto free_prio_str;
		chrootdir = xx;
	}

	char **envvars;
	{
		char **xx;
		err = filter_envvars(&xx, env_whitelist);
		if (err != 0)
			goto free_chrootdir;
		envvars = xx;
	}

	char *service_name_setting;
	{
		char *xx;
		err = linted_str_format(&xx, "LINTED_SERVICE=%s",
		                        unit_name);
		if (err != 0)
			goto free_envvars;
		service_name_setting = xx;
	}
	size_t envvars_size =
	    null_list_size((char const *const *)envvars);
	size_t new_size = envvars_size + 2U;
	{
		void *xx;
		err = linted_mem_realloc_array(&xx, envvars, new_size,
		                               sizeof envvars[0U]);
		if (err != 0)
			goto envvar_allocate_failed;
		envvars = xx;
		goto envvar_allocate_succeeded;
	}
envvar_allocate_failed:
	linted_mem_free(service_name_setting);
	goto free_envvars;

envvar_allocate_succeeded:
	envvars[envvars_size] = service_name_setting;
	envvars[envvars_size + 1U] = 0;

	char *sandbox_dup;
	{
		char *xx;
		err = linted_str_dup(&xx, sandbox);
		if (err != 0)
			goto free_envvars;
		sandbox_dup = xx;
	}
	char *sandbox_base = basename(sandbox_dup);

	size_t exec_start_size =
	    null_list_size((char const *const *)exec_start);

	char const **args;
	size_t num_options;
	size_t args_size;
	{
		struct my_option const options[] = {
		    {"--traceme", 0, true},
		    {"--waiter", waiter, waiter != 0},
		    {"--chrootdir", chrootdir, fstab != 0},
		    {"--fstab", fstab, fstab != 0},
		    {"--nonewprivs", 0, no_new_privs},
		    {"--dropcaps", 0, drop_caps},
		    {"--chdir", chdir_path, chdir_path != 0},
		    {"--priority", prio_str, true},
		    {"--clone-newuser", 0, clone_newuser},
		    {"--clone-newpid", 0, clone_newpid},
		    {"--clone-newipc", 0, clone_newipc},
		    {"--clone-newnet", 0, clone_newnet},
		    {"--clone-newns", 0, clone_newns},
		    {"--clone-newuts", 0, clone_newuts}};

		num_options = 0U;
		for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(options);
		     ++ii) {
			struct my_option const *option = &options[ii];

			char const *value = option->value;
			bool flag = option->flag;

			if (!flag)
				continue;

			++num_options;
			if (value != 0)
				++num_options;
		}

		args_size = 1U + num_options + 1U + exec_start_size;
		{
			void *xx;
			err = linted_mem_alloc_array(
			    &xx, args_size + 1U, sizeof exec_start[0U]);
			if (err != 0)
				goto free_sandbox_dup;
			args = xx;
		}
		args[0U] = sandbox_base;

		size_t ix = 1U;
		for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(options);
		     ++ii) {
			struct my_option option = options[ii];

			char const *name = option.name;
			char const *value = option.value;
			bool flag = option.flag;

			if (!flag)
				continue;

			args[ix++] = name;
			if (value != 0)
				args[ix++] = value;
		}
	}

	args[1U + num_options] = "--";
	for (size_t ii = 0U; ii < exec_start_size; ++ii)
		args[1U + num_options + 1U + ii] = exec_start[ii];
	args[args_size] = 0;

	err = linted_spawn(0, cwd, sandbox, 0, 0, args,
	                   (char const *const *)envvars);

	linted_mem_free(args);

free_sandbox_dup:
	linted_mem_free(sandbox_dup);

free_envvars:
	for (char **envp = envvars;;) {
		char *env = *envp;
		if (0 == env)
			break;

		linted_mem_free(env);

		++envp;
	}
	linted_mem_free(envvars);

free_chrootdir:
	linted_mem_free(chrootdir);

free_prio_str:
	linted_mem_free(prio_str);

	return err;
}

static linted_error dispatch(struct monitor *monitor,
                             struct linted_async_task *task)
{
	switch (linted_async_task_ck(task).u64) {
	case WAITID:
		return on_process_wait(monitor, task);

	case SIGNAL_WAIT:
		return on_signal(monitor, task);

	case ADMIN_IN_READ:
		return on_admin_in_read(monitor, task);

	case ADMIN_OUT_WRITE:
		return on_admin_out_write(monitor, task);

	case KILL_READ:
		return on_kill_read(monitor, task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_wait(struct monitor *monitor,
                                    struct linted_async_task *task)
{
	struct linted_async_pool *pool = monitor->pool;

	char const *process_name = monitor->process_name;
	linted_pid manager_pid = monitor->manager_pid;
	linted_ko cwd = monitor->cwd;
	struct linted_unit_db *unit_db = monitor->unit_db;
	bool time_to_exit = monitor->time_to_exit;

	linted_error err = 0;

	err = linted_async_task_err(task);
	if (LINTED_ERROR_CANCELLED == err)
		return 0;
	if (ECHILD == err)
		return LINTED_ERROR_CANCELLED;
	if (err != 0)
		return err;

	struct linted_pid_task_waitid *sandbox_task =
	    linted_pid_task_waitid_from_async(task);

	linted_pid pid;
	int exit_status;
	int exit_code;
	{
		siginfo_t exit_info;
		linted_pid_task_waitid_info(sandbox_task, &exit_info);
		pid = exit_info.si_pid;
		exit_status = exit_info.si_status;
		exit_code = exit_info.si_code;
	}

	switch (exit_code) {
	case CLD_DUMPED:
	case CLD_KILLED:
	case CLD_EXITED:
		/* Do nothing */
		break;

	case CLD_STOPPED:
		err = on_child_stopped(process_name, pid);
		break;

	case CLD_TRAPPED:
		err = on_child_trapped(process_name, time_to_exit, pid,
		                       exit_status, manager_pid, cwd,
		                       unit_db);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	linted_async_pool_submit(pool, task);

	return err;
}

static linted_error on_signal(struct monitor *monitor,
                              struct linted_async_task *task)
{
	struct linted_unit_db *unit_db = monitor->unit_db;
	struct linted_async_pool *pool = monitor->pool;
	linted_pid manager_pid = monitor->manager_pid;

	linted_error err = 0;

	err = linted_async_task_err(task);
	if (LINTED_ERROR_CANCELLED == err)
		return 0;
	if (err != 0)
		return err;

	struct linted_signal_task_wait *wait_task =
	    linted_signal_task_wait_from_async(task);

	int signo = linted_signal_task_wait_signo(wait_task);

	size_t db_size = linted_unit_db_size(unit_db);
	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct linted_unit *unit =
		    linted_unit_db_get_unit(unit_db, ii);

		char const *name = unit->name;
		linted_unit_type type = unit->type;

		if (type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		linted_pid pid;
		{
			linted_pid xx;
			linted_error pid_err =
			    linted_unit_pid(&xx, manager_pid, name);
			if (ESRCH == pid_err)
				continue;
			if (pid_err != 0) {
				if (0 == err)
					err = pid_err;
				continue;
			}
			pid = xx;
		}
		linted_error kill_err = linted_pid_kill(pid, signo);
		if (kill_err != ESRCH) {
			LINTED_ASSERT(kill_err != EINVAL);
			LINTED_ASSERT(kill_err != EPERM);
			LINTED_ASSERT(0 == err);
		}
	}

	monitor->time_to_exit = true;

	linted_async_pool_submit(pool, task);

	return 0;
}

static linted_error on_admin_in_read(struct monitor *monitor,
                                     struct linted_async_task *task)
{
	struct linted_async_pool *pool = monitor->pool;
	struct linted_admin_out_task_write *write_task =
	    monitor->write_task;
	linted_pid manager_pid = monitor->manager_pid;

	linted_ko admin_out = monitor->admin_out;

	linted_error err;

	err = linted_async_task_err(task);

	/* Assume the other end did something bad and don't exit with
	 * an error. */
	if (err != 0)
		return 0;

	struct linted_admin_in_task_read *admin_in_read_task =
	    linted_admin_in_task_read_from_async(task);

	union linted_admin_request request;
	{
		union linted_admin_request xx;
		linted_admin_in_task_read_request(admin_in_read_task,
		                                  &xx);
		request = xx;
	}

	union linted_admin_reply reply;
	switch (request.type) {
	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_request xx = request.status;
		struct linted_admin_status_reply yy = {0};
		err = on_status_request(manager_pid, &xx, &yy);
		reply.status = yy;
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_request xx = request.stop;
		struct linted_admin_stop_reply yy = {0};
		err = on_stop_request(manager_pid, &xx, &yy);
		reply.stop = yy;
		break;
	}

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	{
		union linted_admin_reply xx = reply;
		linted_admin_out_task_write_prepare(
		    write_task,
		    (union linted_async_ck){.u64 = ADMIN_OUT_WRITE},
		    admin_out, &xx);
	}
	linted_async_pool_submit(
	    pool, linted_admin_out_task_write_to_async(write_task));

	return err;
}

static linted_error on_admin_out_write(struct monitor *monitor,
                                       struct linted_async_task *task)
{
	struct linted_async_pool *pool = monitor->pool;
	struct linted_admin_in_task_read *read_task =
	    monitor->read_task;
	linted_admin_in admin_in = monitor->admin_in;

	linted_error err = linted_async_task_err(task);
	if (err != 0)
		return 0;

	linted_admin_in_task_read_prepare(
	    read_task, (union linted_async_ck){.u64 = ADMIN_IN_READ},
	    admin_in);
	linted_async_pool_submit(
	    pool, linted_admin_in_task_read_to_async(read_task));

	return 0;
}

static linted_error on_kill_read(struct monitor *monitor,
                                 struct linted_async_task *task)
{
	struct linted_unit_db *unit_db = monitor->unit_db;
	struct linted_async_pool *pool = monitor->pool;
	linted_pid manager_pid = monitor->manager_pid;

	linted_error err = 0;

	err = linted_async_task_err(task);
	if (LINTED_ERROR_CANCELLED == err)
		return 0;
	if (err != 0)
		return err;

	size_t db_size = linted_unit_db_size(unit_db);
	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct linted_unit *unit =
		    linted_unit_db_get_unit(unit_db, ii);

		char const *name = unit->name;
		linted_unit_type type = unit->type;

		if (type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		linted_pid pid;
		{
			linted_pid xx;
			linted_error pid_err =
			    linted_unit_pid(&xx, manager_pid, name);
			if (ESRCH == pid_err)
				continue;
			if (pid_err != 0) {
				if (0 == err)
					err = pid_err;
				continue;
			}
			pid = xx;
		}
		linted_error kill_err = linted_pid_kill(pid, SIGTERM);
		if (kill_err != ESRCH) {
			LINTED_ASSERT(kill_err != EINVAL);
			LINTED_ASSERT(kill_err != EPERM);
			LINTED_ASSERT(0 == kill_err);
		}
	}

	monitor->time_to_exit = true;

	linted_async_pool_submit(pool, task);

	return 0;
}

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, linted_pid pid,
                                     int exit_status,
                                     linted_pid manager_pid,
                                     linted_ko cwd,
                                     struct linted_unit_db *unit_db)
{
	int event = exit_status >> 8U;
	exit_status = exit_status & 0xFF;
	switch (event) {
	case 0:
		return on_child_signaled(process_name, pid,
		                         exit_status);

	case PTRACE_EVENT_EXIT:
		return on_child_about_to_exit(
		    process_name, time_to_exit, pid, manager_pid, cwd,
		    unit_db);

	case PTRACE_EVENT_STOP:
		return on_child_linted_ptrace_event_stopped(
		    process_name, pid, exit_status);

	case PTRACE_EVENT_VFORK:
	case PTRACE_EVENT_FORK:
	case PTRACE_EVENT_CLONE:
		return on_child_about_to_clone(pid);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_child_stopped(char const *process_name,
                                     linted_pid pid)
{
	linted_error err = 0;

	linted_error seize_err = linted_ptrace_seize(
	    pid, PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK |
	             PTRACE_O_TRACEVFORK);
	if (0 == err)
		err = seize_err;

	linted_error kill_err = linted_pid_kill(pid, SIGCONT);
	if (0 == err)
		err = kill_err;

	return err;
}

static linted_error on_child_signaled(char const *process_name,
                                      linted_pid pid, int signo)
{
	return linted_ptrace_cont(pid, signo);
}

static linted_error
on_child_linted_ptrace_event_stopped(char const *process_name,
                                     linted_pid pid, int exit_status)
{
	linted_error err = 0;

	linted_pid self = linted_pid_get_pid();

	bool is_child;
	{
		bool xx;
		err = pid_is_child_of(self, pid, &xx);
		if (err != 0)
			goto continue_process;
		is_child = xx;
	}

	if (is_child)
		goto continue_process;

	{
		char name[LINTED_UNIT_NAME_MAX + 1U];
		err = linted_unit_name(pid, name);
		if (err != 0)
			return err;

		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: ptracing %" PRIiMAX " %s\n",
		                       process_name, (intmax_t)pid,
		                       name);
	}

	err = linted_ptrace_setoptions(pid, PTRACE_O_TRACEEXIT);

continue_process:
	;
	linted_error cont_err = linted_ptrace_cont(pid, 0);
	if (0 == err)
		err = cont_err;

	return err;
}

static linted_error
on_child_about_to_exit(char const *process_name, bool time_to_exit,
                       linted_pid pid, linted_pid manager_pid,
                       linted_ko cwd, struct linted_unit_db *unit_db)
{

	linted_error err = 0;
	struct linted_unit *unit = 0;

	err = service_children_terminate(pid);
	if (err != 0)
		goto detach_from_process;

	unsigned long status;
	{
		unsigned long xx;
		err = linted_ptrace_geteventmsg(pid, &xx);
		if (err != 0)
			goto detach_from_process;
		status = xx;
	}

	{
		char name[LINTED_UNIT_NAME_MAX + 1U];
		err = linted_unit_name(pid, name);
		if (err != 0)
			goto detach_from_process;

		if (WIFEXITED(status)) {
			int exit_status = WEXITSTATUS(status);

			linted_io_write_format(
			    LINTED_KO_STDERR, 0,
			    "%s: %s: exited with %i\n", process_name,
			    name, exit_status);

		} else if (WIFSIGNALED(status)) {
			int signo = WTERMSIG(status);

			char const *str = linted_signal_string(signo);
			if (0 == str)
				str = "unknown signal";

			linted_io_write_format(LINTED_KO_STDERR, 0,
			                       "%s: %s: killed by %s\n",
			                       process_name, name, str);
		} else {
			LINTED_ASSUME_UNREACHABLE();
		}

		unit = linted_unit_db_get_unit_by_name(unit_db, name);
	}

detach_from_process:
	err = linted_ptrace_detach(pid, 0);
	if (err != 0)
		return err;

	if (time_to_exit)
		return err;

	if (0 == unit)
		return err;

	err = service_activate(process_name, unit, manager_pid, cwd,
	                       false);

	return err;
}

/* A sandbox creator is creating a sandbox */
static linted_error on_child_about_to_clone(linted_pid pid)
{
	return linted_ptrace_detach(pid, 0);
}

static linted_error
on_status_request(linted_pid manager_pid,
                  struct linted_admin_status_request const *request,
                  struct linted_admin_status_reply *reply)
{
	linted_error err = 0;
	bool is_up;

	char const *unit_name = request->name;
	size_t name_size = request->size;

	{
		char xx[LINTED_UNIT_NAME_MAX + 1U] = {0};
		memcpy(xx, unit_name, name_size);

		err = linted_unit_pid(0, manager_pid, xx);
	}
	if (err != 0) {
		err = 0;
		is_up = false;
		goto reply;
	}

	/* Note that a process can be a zombie or stuck in an
	 * uninterruptable (unkillable as well) state even if it is
	 * found.  Not sure if it is possible to give a useful status
	 * report then. */
	is_up = true;

reply:
	if (err != 0)
		return err;

	reply->type = LINTED_ADMIN_STATUS;
	reply->is_up = is_up;
	return 0;
}

static linted_error
on_stop_request(linted_pid manager_pid,
                struct linted_admin_stop_request const *request,
                struct linted_admin_stop_reply *reply)
{
	linted_error err = 0;
	bool was_up;

	char const *unit_name = request->name;
	size_t name_size = request->size;

	linted_pid pid;
	{
		linted_pid xx;
		char yy[LINTED_UNIT_NAME_MAX + 1U] = {0};
		memcpy(yy, unit_name, name_size);

		err = linted_unit_pid(&xx, manager_pid, yy);
		if (err != 0)
			goto pid_find_failure;
		pid = xx;
		goto found_pid;
	}
pid_find_failure:
	err = 0;
	was_up = false;
	goto reply;

found_pid:
	err = linted_pid_terminate(pid);
	LINTED_ASSERT(err != EINVAL);
	if (err != 0) {
		if (ESRCH == err)
			err = 0;

		was_up = false;
	} else {
		was_up = true;
	}

reply:
	if (err != 0)
		return err;

	reply->type = LINTED_ADMIN_STOP;
	reply->was_up = was_up;
	return 0;
}

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      linted_ko cwd, char const *path)
{
	linted_error err = 0;

	struct linted_conf_db *db;
	{
		struct linted_conf_db *xx;
		err = linted_conf_db_create(&xx);
		if (err != 0)
			return err;
		db = xx;
	}

	char const *dirstart = path;
	for (;;) {
		char const *dirend = strchr(dirstart, ':');

		size_t len;
		if (0 == dirend) {
			len = strlen(dirstart);
		} else {
			len = dirend - dirstart;
		}

		char *dir_name;
		{
			char *xx;
			err = linted_str_dup_len(&xx, dirstart, len);
			if (err != 0)
				goto free_units;
			dir_name = xx;
		}

		err = add_unit_dir_to_db(db, cwd, dir_name);

		linted_mem_free(dir_name);

		if (err != 0)
			goto free_units;

		if (0 == dirend)
			break;

		dirstart = dirend + 1U;
	}

free_units:
	if (err != 0) {
		linted_conf_db_destroy(db);

		return err;
	}

	*dbp = db;

	return 0;
}

static linted_error add_unit_dir_to_db(struct linted_conf_db *db,
                                       linted_ko cwd,
                                       char const *dir_name)
{
	linted_error err;

	linted_ko units_ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, cwd, dir_name,
		                     LINTED_KO_DIRECTORY);
		/* Just treat as an empty directory */
		if (ENOENT == err)
			return 0;
		if (err != 0)
			return err;
		units_ko = xx;
	}

	DIR *units_dir = fdopendir(units_ko);
	if (0 == units_dir) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(units_ko);
	}

	if (ENOENT == err)
		return 0;

	if (err != 0)
		return err;

	size_t files_count = 0U;
	char **files = 0;
	for (;;) {
		errno = 0;
		struct dirent const *entry = readdir(units_dir);
		if (0 == entry) {
			err = errno;
			if (0 == err)
				break;

			goto free_file_names;
		}

		char const *name = entry->d_name;

		if (0 == strcmp(".", name))
			continue;

		if (0 == strcmp("..", name))
			continue;

		char *name_copy;
		{
			char *xx;
			err = linted_str_dup(&xx, name);
			if (err != 0)
				goto free_file_names;
			name_copy = xx;
		}

		size_t new_files_count = files_count + 1U;
		char **new_files;
		{
			void *xx;
			err = linted_mem_realloc_array(
			    &xx, files, new_files_count,
			    sizeof files[0U]);
			if (err != 0)
				goto free_file_name;
			new_files = xx;
		}
		new_files[files_count] = name_copy;

		files = new_files;
		files_count = new_files_count;

		if (err != 0) {
		free_file_name:
			linted_mem_free(name_copy);
			goto free_file_names;
		}
	}

	for (size_t ii = 0U; ii < files_count; ++ii) {
		char *file_name = files[ii];

		linted_unit_type unit_type;
		{
			char const *dot = strchr(file_name, '.');

			char const *suffix = dot + 1U;

			if (0 == strcmp(suffix, "socket")) {
				unit_type = LINTED_UNIT_TYPE_SOCKET;
			} else if (0 == strcmp(suffix, "service")) {
				unit_type = LINTED_UNIT_TYPE_SERVICE;
			} else {
				err = LINTED_ERROR_INVALID_PARAMETER;
				goto free_file_names;
			}
		}
		struct linted_conf *conf = 0;
		{
			struct linted_conf *xx;
			err = linted_conf_create(&xx, file_name);
			if (err != 0)
				goto close_unit_file;
			conf = xx;
		}

		files[ii] = 0;

		switch (unit_type) {
		case LINTED_UNIT_TYPE_SOCKET:
			/* Okay but we have no defaults for this */
			break;

		case LINTED_UNIT_TYPE_SERVICE: {
			char *section_name;
			{
				char *xx;
				err = linted_str_dup(&xx, "Service");
				if (err != 0)
					goto close_unit_file;
				section_name = xx;
			}

			char *env_whitelist;
			{
				char *xx;
				err = linted_str_dup(
				    &xx,
				    "X-LintedEnvironmentWhitelist");
				if (err != 0) {
					linted_mem_free(section_name);
					goto close_unit_file;
				}
				env_whitelist = xx;
			}

			linted_conf_section service;
			{
				linted_conf_section xx;
				err = linted_conf_add_section(
				    conf, &xx, section_name);
				if (err != 0) {
					linted_mem_free(env_whitelist);
					linted_mem_free(section_name);
					goto close_unit_file;
				}
				service = xx;
			}

			err = linted_conf_add_setting(conf, service,
			                              env_whitelist,
			                              default_envvars);
			if (err != 0)
				goto close_unit_file;
			break;
		}
		}

		err = linted_conf_parse_file(conf, units_ko, file_name);

	close_unit_file:
		if (err != 0)
			goto free_unit;

		err = linted_conf_db_add_conf(db, conf);

	free_unit:
		if (err != 0)
			linted_conf_put(conf);
	}

free_file_names:
	for (size_t ii = 0U; ii < files_count; ++ii)
		linted_mem_free(files[ii]);
	linted_mem_free(files);

	if (-1 == closedir(units_dir)) {
		if (0 == err) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}
	}

	return err;
}

static linted_error service_children_terminate(linted_pid pid)
{
	linted_error err = 0;

	linted_pid *children;
	size_t len;
	{
		linted_pid *xx;
		size_t yy;
		err = linted_pid_children(pid, &xx, &yy);
		if (err != 0)
			return err;
		children = xx;
		len = yy;
	}

	for (size_t ii = 0U; ii < len; ++ii) {
		err = linted_pid_terminate(children[ii]);
		if (err != 0)
			goto free_children;
	}

free_children:
	linted_mem_free(children);

	return err;
}

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	char **result_envvars;
	linted_error err;

	size_t allowed_envvars_size = null_list_size(allowed_envvars);

	{
		void *xx;
		err = linted_mem_alloc_array(&xx,
		                             allowed_envvars_size + 1U,
		                             sizeof result_envvars[0U]);
		if (err != 0)
			return err;
		result_envvars = xx;
	}

	size_t result_envvars_size = 0U;
	for (size_t ii = 0U; ii < allowed_envvars_size; ++ii) {
		char const *envvar_name = allowed_envvars[ii];

		char *envvar_value;
		{
			char *xx;
			err = linted_environment_get(envvar_name, &xx);
			if (err != 0)
				goto free_result_envvars;
			envvar_value = xx;
		}
		if (0 == envvar_value)
			continue;

		++result_envvars_size;

		err = linted_str_format(
		    &result_envvars[result_envvars_size - 1U], "%s=%s",
		    envvar_name, envvar_value);

		linted_mem_free(envvar_value);

		if (err != 0)
			goto free_result_envvars;
	}
	result_envvars[result_envvars_size] = 0;

	*result_envvarsp = result_envvars;

	return 0;

free_result_envvars:
	for (size_t ii = 0U; ii < result_envvars_size; ++ii)
		linted_mem_free(result_envvars[ii]);
	linted_mem_free(result_envvars);
	return err;
}

static size_t null_list_size(char const *const *list)
{
	for (size_t ii = 0U;; ++ii)
		if (0 == list[ii])
			return ii;
}

static linted_error str_from_strs(char const *const *strs,
                                  char const **strp)
{
	char const *str;
	if (0 == strs) {
		str = 0;
	} else {
		str = strs[0U];

		if (strs[1U] != 0)
			return LINTED_ERROR_INVALID_PARAMETER;
	}

	*strp = str;
	return 0;
}

static linted_error bool_from_cstring(char const *str, bool *boolp)
{
	static char const *const yes_strs[] = {"1", "yes", "true",
	                                       "on"};
	static char const *const no_strs[] = {"0", "no", "false",
	                                      "off"};

	bool result;

	if (0 == str)
		return LINTED_ERROR_INVALID_PARAMETER;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(yes_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = true;
			goto return_result;
		}
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(no_strs); ++ii) {
		if (0 == strcmp(str, no_strs[ii])) {
			result = false;
			goto return_result;
		}
	}

	return LINTED_ERROR_INVALID_PARAMETER;

return_result:
	*boolp = result;
	return 0;
}

static linted_error pid_is_child_of(linted_pid parent, linted_pid child,
                                    bool *isp)
{
	linted_error err;

	linted_pid ppid;
	{
		struct linted_pid_stat xx;
		err = linted_pid_stat(child, &xx);
		if (err != 0)
			return err;
		ppid = xx.ppid;
	}

	*isp = ppid == parent;

	return err;
}
