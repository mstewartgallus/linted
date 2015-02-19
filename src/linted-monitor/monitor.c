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
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/fifo.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <inttypes.h>
#include <libgen.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if defined HAVE_POSIX_API
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#endif

#include <sys/stat.h>
#include <unistd.h>

enum { WAITID,
       SIGNAL_WAIT,
       ADMIN_IN_READ,
       ADMIN_OUT_WRITE,
       KILL_READ,
       MAX_TASKS };

struct signal_wait_data
{
	bool *time_to_exit;
	struct linted_unit_db *unit_db;
	struct linted_asynch_pool *pool;
	pid_t manager_pid;
};

struct wait_service_data
{
	char const *process_name;
	struct linted_asynch_pool *pool;
	char const *sandbox;
	char const *waiter;
	struct linted_unit_db *unit_db;
	bool *time_to_exit;
	pid_t manager_pid;
	linted_ko cwd;
};

struct admin_in_read_data
{
	struct linted_asynch_pool *pool;
	struct linted_admin_out_task_write *write_task;
	linted_admin_out admin_out;
	pid_t manager_pid;
};

struct admin_out_write_data
{
	struct linted_asynch_pool *pool;
	struct linted_admin_in_task_read *read_task;
	linted_admin_in admin_in;
};

struct kill_read_data
{
	bool *time_to_exit;
	struct linted_unit_db *unit_db;
	struct linted_asynch_pool *pool;
	pid_t manager_pid;
};

static unsigned char monitor_start(char const *process_name, size_t argc,
                                   char const *const argv[]);

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      char const *path);
static linted_error create_unit_db(struct linted_unit_db **unit_dbp,
                                   struct linted_conf_db *conf_db,
                                   char const *sandbox, char const *waiter);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf,
                                   char const *sandbox, char const *waiter);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

static linted_error activate_unit_db(char const *process_name,
                                     struct linted_unit_db *unit_db,
                                     pid_t manager_pid, linted_ko cwd);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_signal(struct linted_asynch_task *task);
static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_admin_in_read(struct linted_asynch_task *task);
static linted_error on_admin_out_write(struct linted_asynch_task *task);
static linted_error on_kill_read(struct linted_asynch_task *task);

static linted_error on_status_request(pid_t manager_pid,
                                      union linted_admin_request const *request,
                                      union linted_admin_reply *reply);
static linted_error on_stop_request(pid_t manager_pid,
                                    union linted_admin_request const *request,
                                    union linted_admin_reply *reply);

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, pid_t pid,
                                     int exit_status, pid_t manager_pid,
                                     linted_ko cwd,
                                     struct linted_unit_db *unit_db);
static linted_error on_child_signaled(char const *process_name, pid_t pid,
                                      int signo);
static linted_error on_child_about_to_clone(pid_t pid);
static linted_error on_child_about_to_exit(char const *process_name,
                                           bool time_to_exit, pid_t pid,
                                           pid_t manager_pid, linted_ko cwd,
                                           struct linted_unit_db *unit_db);
static linted_error socket_activate(struct linted_unit_socket *unit);
static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit,
                                     pid_t manager_pid, linted_ko cwd,
                                     bool check);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error str_from_strs(char const *const *strs, char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);

static linted_error service_children_terminate(pid_t pid);

static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp);

static linted_error set_death_sig(int signum);

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);
static linted_error ptrace_cont(pid_t pid, int signo);
static linted_error ptrace_detach(pid_t pid, int signo);
static linted_error ptrace_setoptions(pid_t pid, unsigned options);
static linted_error ptrace_geteventmsg(pid_t pid, unsigned long *msg);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor", .start = monitor_start};

static unsigned char monitor_start(char const *process_name, size_t argc,
                                   char const *const argv[])
{
	linted_error errnum;

	errnum = set_death_sig(SIGKILL);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "set_death_sig: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	char const *manager_pid_str;
	{
		char *xx;
		errnum = linted_environment_get("MANAGERPID", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		manager_pid_str = xx;
	}

	char const *unit_path;
	{
		char *xx;
		errnum = linted_environment_get("LINTED_UNIT_PATH", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		unit_path = xx;
	}

	char const *sandbox;
	{
		char *xx;
		errnum = linted_environment_get("LINTED_SANDBOX", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		sandbox = xx;
	}

	char const *waiter;
	{
		char *xx;
		errnum = linted_environment_get("LINTED_WAITER", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		waiter = xx;
	}

	char const *data_dir_path;
	{
		char *xx;
		errnum = linted_environment_get("XDG_DATA_HOME", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		data_dir_path = xx;
	}

	char const *runtime_dir_path;
	{
		char *xx;
		errnum = linted_environment_get("XDG_RUNTIME_DIR", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		runtime_dir_path = xx;
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

	/**
	 * @todo Use fallbacks for missing XDG environment variables.
	 */
	if (0 == runtime_dir_path) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "XDG_RUNTIME_HOME");
		return EXIT_FAILURE;
	}

	if (0 == data_dir_path) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "XDG_DATA_HOME");
		return EXIT_FAILURE;
	}

	pid_t manager_pid = strtol(manager_pid_str, 0, 10);

	char *package_runtime_dir_path;
	{
		char *xx;
		if (-1 ==
		    asprintf(&xx, "%s/%s", runtime_dir_path, PACKAGE_TARNAME)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		package_runtime_dir_path = xx;
	}

	char *package_data_dir_path;
	{
		char *xx;
		if (-1 ==
		    asprintf(&xx, "%s/%s", data_dir_path, PACKAGE_TARNAME)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		package_data_dir_path = xx;
	}

	char *process_runtime_dir_path;
	{
		char *xx;
		if (-1 == asprintf(&xx, "%s/%" PRIuMAX,
		                   package_runtime_dir_path,
		                   (uintmax_t)manager_pid)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		process_runtime_dir_path = xx;
	}

	char *process_data_dir_path;
	{
		char *xx;
		if (-1 == asprintf(&xx, "%s/%" PRIuMAX, package_data_dir_path,
		                   (uintmax_t)manager_pid)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		process_data_dir_path = xx;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, package_runtime_dir_path,
	                           0U, S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, package_data_dir_path, 0U,
	                           S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, process_runtime_dir_path,
	                           0U, S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, process_data_dir_path, 0U,
	                           S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	linted_ko cwd;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, ".",
		                        LINTED_KO_DIRECTORY);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_ko_open: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	if (-1 == chdir(process_runtime_dir_path)) {
		linted_log(LINTED_LOG_ERROR, "chdir: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	if (-1 == symlink(process_data_dir_path, "var")) {
		if (errno != EEXIST) {
			linted_log(LINTED_LOG_ERROR, "symlink: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	linted_admin_in admin_in;
	{
		linted_admin_in xx;
		errnum =
		    linted_fifo_create(&xx, LINTED_KO_CWD, "admin-in",
		                       LINTED_FIFO_RDWR, S_IRUSR | S_IWUSR);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_fifo_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		admin_in = xx;
	}

	linted_admin_out admin_out;
	{
		linted_admin_out xx;
		errnum =
		    linted_fifo_create(&xx, LINTED_KO_CWD, "admin-out",
		                       LINTED_FIFO_RDWR, S_IRUSR | S_IWUSR);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_fifo_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		admin_out = xx;
	}

	linted_fifo kill_fifo;
	{
		linted_admin_out xx;
		errnum =
		    linted_fifo_create(&xx, LINTED_KO_CWD, "kill",
		                       LINTED_FIFO_RDWR, S_IRUSR | S_IWUSR);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_fifo_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		kill_fifo = xx;
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_asynch_pool_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	bool time_to_exit = false;

	struct signal_wait_data signal_wait_data;
	struct wait_service_data sandbox_data;
	struct admin_in_read_data admin_in_read_data;
	struct admin_out_write_data admin_out_write_data;
	struct kill_read_data kill_read_data;

	struct linted_signal_task_wait *signal_wait_task;
	struct linted_pid_task_waitid *sandbox_task;
	struct linted_admin_in_task_read *admin_in_read_task;
	struct linted_admin_out_task_write *write_task;
	struct linted_io_task_read *kill_read_task;

	{
		struct linted_signal_task_wait *xx;
		errnum = linted_signal_task_wait_create(&xx, &signal_wait_data);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_io_task_read_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		signal_wait_task = xx;
	}

	{
		struct linted_pid_task_waitid *xx;
		errnum = linted_pid_task_waitid_create(&xx, &sandbox_data);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_pid_task_waitid_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		sandbox_task = xx;
	}

	{
		struct linted_admin_in_task_read *xx;
		errnum =
		    linted_admin_in_task_read_create(&xx, &admin_in_read_data);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_admin_in_task_read_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		admin_in_read_task = xx;
	}

	{
		struct linted_admin_out_task_write *xx;
		errnum = linted_admin_out_task_write_create(
		    &xx, &admin_out_write_data);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_admin_out_task_write_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		write_task = xx;
	}

	{
		struct linted_io_task_read *xx;
		errnum = linted_io_task_read_create(&xx, &kill_read_data);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_io_task_read_create: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		kill_read_task = xx;
	}

	struct linted_conf_db *conf_db;
	{
		struct linted_conf_db *xx;
		errnum = conf_db_from_path(&xx, unit_path);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "conf_db_from_path: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		conf_db = xx;
	}

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		errnum = create_unit_db(&xx, conf_db, sandbox, waiter);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "create_unit_db: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		unit_db = xx;
	}

	/**
	 * @todo Warn about unactivated unit_db.
	 */
	errnum = activate_unit_db(process_name, unit_db, manager_pid, cwd);
	if (errnum != 0)
		goto kill_procs;

	signal_wait_data.time_to_exit = &time_to_exit;
	signal_wait_data.unit_db = unit_db;
	signal_wait_data.pool = pool;
	signal_wait_data.manager_pid = manager_pid;

	sandbox_data.process_name = process_name;
	sandbox_data.pool = pool;
	sandbox_data.manager_pid = manager_pid;
	sandbox_data.cwd = cwd;
	sandbox_data.sandbox = sandbox;
	sandbox_data.waiter = waiter;
	sandbox_data.unit_db = unit_db;
	sandbox_data.time_to_exit = &time_to_exit;

	admin_in_read_data.pool = pool;
	admin_in_read_data.manager_pid = manager_pid;
	admin_in_read_data.write_task = write_task;
	admin_in_read_data.admin_out = admin_out;

	admin_out_write_data.pool = pool;
	admin_out_write_data.read_task = admin_in_read_task;
	admin_out_write_data.admin_in = admin_in;

	kill_read_data.time_to_exit = &time_to_exit;
	kill_read_data.unit_db = unit_db;
	kill_read_data.pool = pool;
	kill_read_data.manager_pid = manager_pid;

	errnum = linted_signal_listen_to_sighup();
	if (errnum != 0)
		goto kill_procs;

	errnum = linted_signal_listen_to_sigint();
	if (errnum != 0)
		goto kill_procs;

	errnum = linted_signal_listen_to_sigquit();
	if (errnum != 0)
		goto kill_procs;

	errnum = linted_signal_listen_to_sigterm();
	if (errnum != 0)
		goto kill_procs;

	linted_signal_task_wait_prepare(signal_wait_task, SIGNAL_WAIT);
	linted_asynch_pool_submit(
	    pool, linted_signal_task_wait_to_asynch(signal_wait_task));

	linted_admin_in_task_read_prepare(admin_in_read_task, ADMIN_IN_READ,
	                                  admin_in);
	linted_asynch_pool_submit(
	    pool, linted_admin_in_task_read_to_asynch(admin_in_read_task));

	linted_pid_task_waitid_prepare(sandbox_task, WAITID, P_ALL, -1,
	                               WEXITED);
	linted_asynch_pool_submit(
	    pool, linted_pid_task_waitid_to_asynch(sandbox_task));

	static char dummy;

	linted_io_task_read_prepare(kill_read_task, KILL_READ, kill_fifo,
	                            &dummy, sizeof dummy);
	linted_asynch_pool_submit(
	    pool, linted_io_task_read_to_asynch(kill_read_task));

	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto cancel_tasks;
	}

cancel_tasks:
	if (LINTED_ERROR_CANCELLED == errnum)
		errnum = 0;

	linted_asynch_task_cancel(
	    linted_pid_task_waitid_to_asynch(sandbox_task));
	linted_asynch_task_cancel(
	    linted_admin_in_task_read_to_asynch(admin_in_read_task));

	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			if (LINTED_ERROR_AGAIN ==
			    linted_asynch_pool_poll(pool, &xx))
				break;
			completed_task = xx;
		}

		linted_error dispatch_error = dispatch(completed_task);
		if (0 == errnum)
			errnum = dispatch_error;
	}

kill_procs:
	linted_unit_db_destroy(unit_db);

	linted_conf_db_destroy(conf_db);

	{
		linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
		if (0 == errnum)
			errnum = destroy_errnum;
	}

	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)sandbox_task;
	(void)admin_in_read_task;

	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "%s", linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error create_unit_db(struct linted_unit_db **unit_dbp,
                                   struct linted_conf_db *conf_db,
                                   char const *sandbox, char const *waiter)
{
	linted_error errnum;

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		errnum = linted_unit_db_create(&xx);
		if (errnum != 0)
			return errnum;
		unit_db = xx;
	}

	size_t size = linted_conf_db_size(conf_db);
	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_unit *unit;
		{
			struct linted_unit *xx;
			errnum = linted_unit_db_add_unit(unit_db, &xx);
			if (errnum != 0)
				goto destroy_unit_db;
			unit = xx;
		}

		struct linted_conf *conf = linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		linted_unit_type unit_type;
		if (0 == strcmp(suffix, "socket")) {
			unit_type = LINTED_UNIT_TYPE_SOCKET;
		} else if (0 == strcmp(suffix, "service")) {
			unit_type = LINTED_UNIT_TYPE_SERVICE;
		} else {
			errnum = LINTED_ERROR_INVALID_PARAMETER;
			goto destroy_unit_db;
		}

		char *unit_name = strndup(file_name, dot - file_name);
		if (0 == unit_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto destroy_unit_db;
		}

		unit->type = unit_type;
		unit->name = unit_name;

		switch (unit_type) {
		case LINTED_UNIT_TYPE_SERVICE: {
			struct linted_unit_service *s = (void *)unit;

			errnum = service_create(s, conf, sandbox, waiter);
			if (errnum != 0)
				goto destroy_unit_db;
			break;
		}

		case LINTED_UNIT_TYPE_SOCKET: {
			struct linted_unit_socket *s = (void *)unit;
			s->is_open = false;

			errnum = socket_create(s, conf);
			if (errnum != 0)
				goto destroy_unit_db;
			break;
		}
		}
	}

	*unit_dbp = unit_db;

	return errnum;

destroy_unit_db:
	linted_unit_db_destroy(unit_db);
	return errnum;
}

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf,
                                   char const *sandbox, char const *waiter)
{
	linted_error errnum;

	char const *const *types = linted_conf_find(conf, "Service", "Type");
	char const *const *exec_start =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *no_new_privss =
	    linted_conf_find(conf, "Service", "NoNewPrivileges");
	char const *const *chdir_paths =
	    linted_conf_find(conf, "Service", "WorkingDirectory");
	char const *const *fstabs =
	    linted_conf_find(conf, "Service", "X-LintedFstab");
	char const *const *env_whitelist =
	    linted_conf_find(conf, "Service", "X-LintedEnvironmentWhitelist");
	char const *const *clone_flags =
	    linted_conf_find(conf, "Service", "X-LintedCloneFlags");

	char const *type;
	{
		char const *xx;
		errnum = str_from_strs(types, &xx);
		if (errnum != 0)
			return errnum;
		type = xx;
	}

	if (0 == exec_start)
		return LINTED_ERROR_INVALID_PARAMETER;

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
		errnum = bool_from_cstring(no_new_privs, &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs_value = xx;
	}

	unit->sandbox = sandbox;
	unit->waiter = waiter;

	unit->exec_start = exec_start;
	unit->no_new_privs = no_new_privs_value;
	unit->fstab = fstab;
	unit->chdir_path = chdir_path;
	unit->env_whitelist = env_whitelist;

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
	linted_error errnum;

	char const *const *listen_dirs =
	    linted_conf_find(conf, "Socket", "ListenDirectory");

	char const *const *listen_files =
	    linted_conf_find(conf, "Socket", "ListenFile");

	char const *const *listen_fifos =
	    linted_conf_find(conf, "Socket", "ListenFifo");

	char const *listen_dir;
	{
		char const *xx;
		errnum = str_from_strs(listen_dirs, &xx);
		if (errnum != 0)
			return errnum;
		listen_dir = xx;
	}

	char const *listen_file;
	{
		char const *xx;
		errnum = str_from_strs(listen_files, &xx);
		if (errnum != 0)
			return errnum;
		listen_file = xx;
	}

	char const *listen_fifo;
	{
		char const *xx;
		errnum = str_from_strs(listen_fifos, &xx);
		if (errnum != 0)
			return errnum;
		listen_fifo = xx;
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

	switch (socket_type) {
	case LINTED_UNIT_SOCKET_TYPE_DIR:
	case LINTED_UNIT_SOCKET_TYPE_FILE:
	case LINTED_UNIT_SOCKET_TYPE_FIFO:
		break;
	}

	unit->type = socket_type;
	unit->path = path;

	return 0;
}

static linted_error activate_unit_db(char const *process_name,
                                     struct linted_unit_db *unit_db,
                                     pid_t manager_pid, linted_ko cwd)
{
	linted_error errnum;

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SOCKET)
			continue;

		errnum = socket_activate((void *)unit);
		if (errnum != 0)
			return errnum;
	}

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		errnum = service_activate(process_name, unit, manager_pid, cwd,
		                          true);
		if (errnum != 0)
			return errnum;
	}

	return 0;
}

static linted_error socket_activate(struct linted_unit_socket *unit)
{
	linted_error errnum;

	switch (unit->type) {
	case LINTED_UNIT_SOCKET_TYPE_DIR:
		errnum = linted_dir_create(0, LINTED_KO_CWD, unit->path, 0U,
		                           S_IRWXU);
		if (errnum != 0)
			return errnum;
		unit->is_open = false;
		break;

	case LINTED_UNIT_SOCKET_TYPE_FILE:
		errnum = linted_file_create(0, LINTED_KO_CWD, unit->path, 0U,
		                            S_IRWXU);
		if (errnum != 0)
			return errnum;
		unit->is_open = false;
		break;

	case LINTED_UNIT_SOCKET_TYPE_FIFO:
		errnum = linted_fifo_create(0, LINTED_KO_CWD, unit->path, 0U,
		                            S_IRWXU);
		if (errnum != 0)
			return errnum;
		unit->is_open = false;
		break;
	}

	return 0;
}

struct option
{
	char const *name;
	char const *value;
	bool flag : 1U;
};

static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit,
                                     pid_t manager_pid, linted_ko cwd,
                                     bool check)
{
	linted_error errnum = 0;

	char const *name = unit->name;

	struct linted_unit_service *unit_service = (void *)unit;

	if (!check)
		goto spawn_service;

	pid_t child;
	{
		pid_t xx;
		errnum = linted_unit_pid(&xx, manager_pid, name);
		if (errnum != 0)
			goto service_not_found;
		child = xx;
	}

	linted_io_write_format(LINTED_KO_STDERR, 0,
	                       "%s: ptracing %" PRIiMAX " %s\n", process_name,
	                       (intmax_t)child, name);

	return ptrace_seize(child, PTRACE_O_TRACEEXIT);

service_not_found:
	if (errnum != ESRCH)
		return errnum;
spawn_service:
	;
	char const *const *exec_start = unit_service->exec_start;
	bool no_new_privs = unit_service->no_new_privs;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *env_whitelist = unit_service->env_whitelist;

	char const *sandbox = unit_service->sandbox;
	char const *waiter = unit_service->waiter;

	bool clone_newuser = unit_service->clone_newuser;
	bool clone_newpid = unit_service->clone_newpid;
	bool clone_newipc = unit_service->clone_newipc;
	bool clone_newnet = unit_service->clone_newnet;
	bool clone_newns = unit_service->clone_newns;
	bool clone_newuts = unit_service->clone_newuts;

	if (fstab != 0) {
		linted_ko name_dir;
		{
			linted_ko xx;
			errnum = linted_dir_create(&xx, LINTED_KO_CWD, name, 0U,
			                           S_IRWXU);
			if (errnum != 0)
				return errnum;
			name_dir = xx;
		}

		errnum = linted_dir_create(0, name_dir, "chroot", 0U, S_IRWXU);

		linted_ko_close(name_dir);

		if (errnum != 0)
			return errnum;
	}

	if (fstab != 0 && !clone_newns)
		return LINTED_ERROR_INVALID_PARAMETER;

	bool drop_caps = true;

	linted_sched_priority current_priority;
	{
		linted_sched_priority xx;
		errnum = linted_sched_getpriority(&xx);
		if (errnum != 0)
			return errnum;
		current_priority = xx;
	}

	/* Favor other processes over this process hierarchy.  Only
	 * superuser may lower priorities so this is not
	 * stoppable. This also makes the process hierarchy nicer for
	 * the OOM killer.
	 */
	char prio_str[LINTED_NUMBER_TYPE_STRING_SIZE(linted_sched_priority) +
	              1U];
	sprintf(prio_str, "%" PRIiMAX, (intmax_t)current_priority + 1);

	char *chrootdir;
	{
		char *xx;
		if (-1 == asprintf(&xx, "%s/chroot", name)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		chrootdir = xx;
	}

	char **envvars;
	{
		char **xx;
		errnum = filter_envvars(&xx, env_whitelist);
		if (errnum != 0)
			goto free_chrootdir;
		envvars = xx;
	}

	char *service_name_setting;
	{
		char *xx;
		if (-1 == asprintf(&xx, "LINTED_SERVICE=%s", name))
			goto service_asprintf_failed;
		service_name_setting = xx;
		goto service_asprintf_succeeded;
	}
service_asprintf_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);
	goto free_envvars;

service_asprintf_succeeded:
	;
	size_t envvars_size = null_list_size((char const *const *)envvars);
	size_t new_size = envvars_size + 2U;
	{
		void *xx;
		errnum = linted_mem_realloc_array(&xx, envvars, new_size,
		                                  sizeof envvars[0U]);
		if (errnum != 0)
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

	char *sandbox_dup = strdup(sandbox);
	if (0 == sandbox_dup) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_envvars;
	}
	char *sandbox_base = basename(sandbox_dup);

	size_t exec_start_size =
	    null_list_size((char const *const *)exec_start);

	struct option options[] = {{"--traceme", 0, true},
	                           {"--waiter", waiter, waiter != 0},
	                           {"--chrootdir", chrootdir, fstab != 0},
	                           {"--fstab", fstab, fstab != 0},
	                           {"--nonewprivs", 0, no_new_privs},
	                           {"--dropcaps", 0, drop_caps},
	                           {"--chdir", chdir_path, chdir_path != 0},
	                           {"--priority", prio_str, prio_str != 0},
	                           {"--clone-newuser", 0, clone_newuser},
	                           {"--clone-newpid", 0, clone_newpid},
	                           {"--clone-newipc", 0, clone_newipc},
	                           {"--clone-newnet", 0, clone_newnet},
	                           {"--clone-newns", 0, clone_newns},
	                           {"--clone-newuts", 0, clone_newuts}};

	size_t num_options = 0U;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(options); ++ii) {
		struct option option = options[ii];
		if (!option.flag)
			continue;

		++num_options;
		if (option.value != 0)
			++num_options;
	}

	char const **args;
	size_t args_size = 1U + num_options + 1U + exec_start_size;
	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, args_size + 1U,
		                                sizeof exec_start[0U]);
		if (errnum != 0)
			goto free_sandbox_dup;
		args = xx;
	}
	args[0U] = sandbox_base;

	size_t ix = 1U;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(options); ++ii) {
		struct option option = options[ii];
		if (!option.flag)
			continue;

		args[ix++] = option.name;
		if (option.value != 0)
			args[ix++] = option.value;
	}

	args[1U + num_options] = "--";
	for (size_t ii = 0U; ii < exec_start_size; ++ii)
		args[1U + num_options + 1U + ii] = exec_start[ii];
	args[args_size] = 0;

	errnum = linted_spawn(0, cwd, sandbox, 0, 0, args,
	                      (char const *const *)envvars);

	/* Let the child be leaked, we'll get the wait later */

	linted_mem_free(args);

free_sandbox_dup:
	linted_mem_free(sandbox_dup);

free_envvars:
	for (char **envp = envvars; *envp != 0; ++envp)
		linted_mem_free(*envp);
	linted_mem_free(envvars);

free_chrootdir:
	linted_mem_free(chrootdir);

	return errnum;
}

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (linted_asynch_task_action(task)) {
	case WAITID:
		return on_process_wait(task);

	case SIGNAL_WAIT:
		return on_signal(task);

	case ADMIN_IN_READ:
		return on_admin_in_read(task);

	case ADMIN_OUT_WRITE:
		return on_admin_out_write(task);

	case KILL_READ:
		return on_kill_read(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_wait(struct linted_asynch_task *task)
{
	linted_error errnum = 0;

	errnum = linted_asynch_task_errnum(task);
	if (LINTED_ERROR_CANCELLED == errnum)
		return 0;
	if (ECHILD == errnum)
		return LINTED_ERROR_CANCELLED;
	if (errnum != 0)
		return errnum;

	struct linted_pid_task_waitid *sandbox_task =
	    linted_pid_task_waitid_from_asynch(task);
	struct wait_service_data *wait_service_data =
	    linted_pid_task_waitid_data(sandbox_task);
	struct linted_asynch_pool *pool = wait_service_data->pool;

	char const *process_name = wait_service_data->process_name;
	pid_t manager_pid = wait_service_data->manager_pid;
	linted_ko cwd = wait_service_data->cwd;
	struct linted_unit_db *unit_db = wait_service_data->unit_db;
	bool time_to_exit = *wait_service_data->time_to_exit;

	pid_t pid;
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

	case CLD_TRAPPED:
		errnum =
		    on_child_trapped(process_name, time_to_exit, pid,
		                     exit_status, manager_pid, cwd, unit_db);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	linted_asynch_pool_submit(pool, task);

	return errnum;
}

static linted_error on_signal(struct linted_asynch_task *task)
{
	linted_error errnum = 0;

	errnum = linted_asynch_task_errnum(task);
	if (LINTED_ERROR_CANCELLED == errnum)
		return 0;
	if (errnum != 0)
		return errnum;

	struct linted_signal_task_wait *wait_task =
	    linted_signal_task_wait_from_asynch(task);
	struct signal_wait_data *data = linted_signal_task_wait_data(wait_task);
	bool *time_to_exit = data->time_to_exit;
	struct linted_unit_db *unit_db = data->unit_db;
	struct linted_asynch_pool *pool = data->pool;
	pid_t manager_pid = data->manager_pid;
	int signo = linted_signal_task_wait_signo(wait_task);

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		pid_t pid;
		{
			pid_t xx;
			linted_error pid_errnum =
			    linted_unit_pid(&xx, manager_pid, unit->name);
			if (ESRCH == pid_errnum)
				continue;
			if (pid_errnum != 0) {
				if (0 == errnum)
					errnum = pid_errnum;
				continue;
			}
			pid = xx;
		}
		linted_error kill_errnum = linted_pid_kill(pid, signo);
		if (kill_errnum != ESRCH) {
			assert(kill_errnum != EINVAL);
			assert(kill_errnum != EPERM);
			assert(0 == errnum);
		}
	}

	*time_to_exit = true;

	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_admin_in_read(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);

	/* Assume the other end did something bad and don't exit with
	 * an error. */
	if (errnum != 0)
		return 0;

	struct linted_admin_in_task_read *admin_in_read_task =
	    linted_admin_in_task_read_from_asynch(task);
	struct admin_in_read_data *admin_in_read_data =
	    linted_admin_in_task_read_data(admin_in_read_task);

	struct linted_asynch_pool *pool = admin_in_read_data->pool;
	struct linted_admin_out_task_write *write_task =
	    admin_in_read_data->write_task;
	pid_t manager_pid = admin_in_read_data->manager_pid;

	linted_ko admin_out = admin_in_read_data->admin_out;

	union linted_admin_request const *request =
	    linted_admin_in_task_read_request(admin_in_read_task);
	union linted_admin_reply reply;

	switch (request->type) {
	case LINTED_ADMIN_STATUS:
		errnum = on_status_request(manager_pid, request, &reply);
		break;

	case LINTED_ADMIN_STOP:
		errnum = on_stop_request(manager_pid, request, &reply);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	linted_admin_out_task_write_prepare(write_task, ADMIN_OUT_WRITE,
	                                    admin_out, &reply);
	linted_asynch_pool_submit(
	    pool, linted_admin_out_task_write_to_asynch(write_task));

	return errnum;
}

static linted_error on_admin_out_write(struct linted_asynch_task *task)
{
	linted_error errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return 0;

	struct linted_admin_out_task_write *admin_out_write_task =
	    linted_admin_out_task_write_from_asynch(task);
	struct admin_out_write_data *admin_out_write_data =
	    linted_admin_out_task_write_data(admin_out_write_task);

	struct linted_asynch_pool *pool = admin_out_write_data->pool;
	struct linted_admin_in_task_read *read_task =
	    admin_out_write_data->read_task;
	linted_admin_in admin_in = admin_out_write_data->admin_in;

	linted_admin_in_task_read_prepare(read_task, ADMIN_IN_READ, admin_in);
	linted_asynch_pool_submit(
	    pool, linted_admin_in_task_read_to_asynch(read_task));

	return 0;
}

static linted_error on_kill_read(struct linted_asynch_task *task)
{
	linted_error errnum = 0;

	errnum = linted_asynch_task_errnum(task);
	if (LINTED_ERROR_CANCELLED == errnum)
		return 0;
	if (errnum != 0)
		return errnum;

	struct linted_io_task_read *kill_read_task =
	    linted_io_task_read_from_asynch(task);
	struct kill_read_data *kill_read_data =
	    linted_io_task_read_data(kill_read_task);
	bool *time_to_exit = kill_read_data->time_to_exit;
	struct linted_unit_db *unit_db = kill_read_data->unit_db;
	struct linted_asynch_pool *pool = kill_read_data->pool;
	pid_t manager_pid = kill_read_data->manager_pid;

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		pid_t pid;
		{
			pid_t xx;
			linted_error pid_errnum =
			    linted_unit_pid(&xx, manager_pid, unit->name);
			if (ESRCH == pid_errnum)
				continue;
			if (pid_errnum != 0) {
				if (0 == errnum)
					errnum = pid_errnum;
				continue;
			}
			pid = xx;
		}
		linted_error kill_errnum = linted_pid_kill(pid, SIGTERM);
		if (kill_errnum != ESRCH) {
			assert(kill_errnum != EINVAL);
			assert(kill_errnum != EPERM);
			assert(0 == errnum);
		}
	}

	*time_to_exit = true;

	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, pid_t pid,
                                     int exit_status, pid_t manager_pid,
                                     linted_ko cwd,
                                     struct linted_unit_db *unit_db)
{
	int event = exit_status >> 8U;
	switch (event) {
	case 0:
		return on_child_signaled(process_name, pid, exit_status);

	case PTRACE_EVENT_EXIT:
		return on_child_about_to_exit(process_name, time_to_exit, pid,
		                              manager_pid, cwd, unit_db);

	case PTRACE_EVENT_VFORK:
	case PTRACE_EVENT_FORK:
	case PTRACE_EVENT_CLONE:
		return on_child_about_to_clone(pid);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_child_signaled(char const *process_name, pid_t pid,
                                      int signo)
{
	linted_error errnum = 0;

	switch (signo) {
	default:
		break;

	case SIGSTOP: {
		signo = 0;

		pid_t self = getpid();
		bool is;
		{
			bool xx;
			errnum = pid_is_child_of(self, pid, &xx);
			if (errnum != 0)
				break;
			is = xx;
		}
		if (is) {
			errnum = ptrace_setoptions(
			    pid, PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK |
			             PTRACE_O_TRACEVFORK);
			break;
		}

		char name[LINTED_UNIT_NAME_MAX + 1U];
		errnum = linted_unit_name(pid, name);
		if (errnum != 0)
			return errnum;

		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: ptracing %" PRIiMAX " %s\n",
		                       process_name, (intmax_t)pid, name);

		errnum = ptrace_setoptions(pid, PTRACE_O_TRACEEXIT);
		break;
	}

	case SIGTRAP:
		signo = 0;
		break;
	}

	linted_error cont_errnum = ptrace_cont(pid, signo);
	if (0 == errnum)
		errnum = cont_errnum;

	return errnum;
}

static linted_error on_child_about_to_exit(char const *process_name,
                                           bool time_to_exit, pid_t pid,
                                           pid_t manager_pid, linted_ko cwd,
                                           struct linted_unit_db *unit_db)
{

	linted_error errnum = 0;
	struct linted_unit *unit = 0;

	errnum = service_children_terminate(pid);
	if (errnum != 0)
		goto detach_from_process;

	unsigned long status;
	{
		unsigned long xx;
		errnum = ptrace_geteventmsg(pid, &xx);
		if (errnum != 0)
			goto detach_from_process;
		status = xx;
	}

	char name[LINTED_UNIT_NAME_MAX + 1U];
	errnum = linted_unit_name(pid, name);
	if (errnum != 0)
		goto detach_from_process;

	if (WIFEXITED(status)) {
		int exit_status = WEXITSTATUS(status);

		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: %s: exited with %i\n", process_name,
		                       name, exit_status);

	} else if (WIFSIGNALED(status)) {
		int signo = WTERMSIG(status);

		char const *str = linted_signal_string(signo);
		if (0 == str)
			str = "unknown signal";

		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: %s: killed by %s\n", process_name,
		                       name, str);
	} else {
		LINTED_ASSUME_UNREACHABLE();
	}

	unit = linted_unit_db_get_unit_by_name(unit_db, name);

detach_from_process:
	errnum = ptrace_detach(pid, 0);
	if (errnum != 0)
		return errnum;

	if (time_to_exit)
		return errnum;

	if (0 == unit)
		return errnum;

	errnum = service_activate(process_name, unit, manager_pid, cwd, false);

	return errnum;
}

/* A sandbox creator is creating a sandbox */
static linted_error on_child_about_to_clone(pid_t pid)
{
	return ptrace_detach(pid, 0);
}

static linted_error on_status_request(pid_t manager_pid,
                                      union linted_admin_request const *request,
                                      union linted_admin_reply *reply)
{
	linted_error errnum = 0;
	bool is_up;

	errnum = linted_unit_pid(0, manager_pid, request->status.name);
	if (errnum != 0) {
		errnum = 0;
		is_up = false;
		goto reply;
	}

	/* Note that a process can be a zombie or stuck in an
	 * uninterruptable (unkillable as well) state even if it is
	 * found.  Not sure if it is possible to give a useful status
	 * report then. */
	is_up = true;

reply:
	if (errnum != 0)
		return errnum;

	reply->status.is_up = is_up;
	return 0;
}

static linted_error on_stop_request(pid_t manager_pid,
                                    union linted_admin_request const *request,
                                    union linted_admin_reply *reply)
{
	linted_error errnum = 0;
	bool was_up;

	pid_t pid;
	{
		pid_t xx;
		errnum =
		    linted_unit_pid(&xx, manager_pid, request->status.name);
		if (errnum != 0)
			goto pid_find_failure;
		pid = xx;
		goto found_pid;
	}
pid_find_failure:
	errnum = 0;
	was_up = false;
	goto reply;

found_pid:
	errnum = linted_pid_terminate(pid);
	assert(errnum != EINVAL);
	if (errnum != 0) {
		if (ESRCH == errnum)
			errnum = 0;

		was_up = false;
	} else {
		was_up = true;
	}

reply:
	if (errnum != 0)
		return errnum;

	reply->stop.was_up = was_up;
	return 0;
}

static char const *default_envvars[] = {
    "MANAGERPID", "USER", "LOGNAME", "HOME", "SHELL", "XDG_RUNTIME_DIR"
                                                      "XDG_SESSION_ID",
    "XDG_SEAT", "TERM", "LD_DEBUG", "LD_DEBUG_OUTPUT", 0};

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      char const *path)
{
	linted_error errnum = 0;

	struct linted_conf_db *db;
	{
		struct linted_conf_db *xx;
		errnum = linted_conf_db_create(&xx);
		if (errnum != 0)
			return errnum;
		db = xx;
	}

	char const *dirstart = path;
	for (;;) {
		char const *dirend = strchr(dirstart, ':');

		char *dir_name;
		if (0 == dirend) {
			dir_name = strdup(dirstart);
		} else {
			dir_name = strndup(dirstart, dirend - dirstart);
		}

		if (0 == dir_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_units;
		}

		DIR *units_dir = opendir(dir_name);
		if (0 == units_dir) {
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

		linted_ko dirko = dirfd(units_dir);

		size_t files_count = 0U;
		char **files = 0;
		for (;;) {
			errno = 0;
			struct dirent const *entry = readdir(units_dir);
			if (0 == entry) {
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
			if (0 == name_copy) {
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

			linted_ko unit_fd;
			{
				linted_ko xx;
				errnum = linted_ko_open(&xx, dirko, file_name,
				                        LINTED_KO_RDONLY);
				if (errnum != 0)
					goto free_file_names;
				unit_fd = xx;
			}

			FILE *unit_file = fdopen(unit_fd, "r");
			if (0 == unit_file) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);

				linted_ko_close(unit_fd);

				goto free_file_names;
			}

			struct linted_conf *conf = 0;
			{
				struct linted_conf *xx;
				errnum = linted_conf_create(&xx, file_name);
				if (errnum != 0)
					goto close_unit_file;
				conf = xx;
			}

			char const *dot = strchr(file_name, '.');

			char const *suffix = dot + 1U;

			if (0 == strcmp(suffix, "socket")) {
				/* Okay but we have no defaults for this */
			} else if (0 == strcmp(suffix, "service")) {
				char *section_name = strdup("Service");
				if (0 == section_name) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
					goto close_unit_file;
				}

				char *env_whitelist =
				    strdup("X-LintedEnvironmentWhitelist");
				if (0 == env_whitelist) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
					linted_mem_free(section_name);
					goto close_unit_file;
				}

				linted_conf_section service;
				{
					linted_conf_section xx;
					errnum = linted_conf_add_section(
					    conf, &xx, section_name);
					if (errnum != 0) {
						linted_mem_free(env_whitelist);
						linted_mem_free(section_name);
						goto close_unit_file;
					}
					service = xx;
				}

				errnum = linted_conf_add_setting(
				    conf, service, env_whitelist,
				    default_envvars);
				if (errnum != 0)
					goto close_unit_file;

			} else {
				errnum = LINTED_ERROR_INVALID_PARAMETER;
				goto close_unit_file;
			}

			errnum = linted_conf_parse_file(conf, unit_file);

		close_unit_file:
			if (EOF == fclose(unit_file)) {
				if (0 == errnum) {
					errnum = errno;
					LINTED_ASSUME(errnum != 0);
				}
			}

			if (errnum != 0)
				goto free_unit;

			errnum = linted_conf_db_add_conf(db, conf);

		free_unit:
			if (errnum != 0)
				linted_conf_put(conf);
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
		if (0 == dirend)
			break;

		dirstart = dirend + 1U;
	}

free_units:
	if (errnum != 0) {
		linted_conf_db_destroy(db);

		return errnum;
	}

	*dbp = db;

	return 0;
}

static linted_error service_children_terminate(pid_t pid)
{
	linted_error errnum = 0;

	pid_t *children;
	size_t len;
	{
		pid_t *xx;
		size_t yy;
		errnum = linted_pid_children(pid, &xx, &yy);
		if (errnum != 0)
			return errnum;
		children = xx;
		len = yy;
	}

	for (size_t ii = 0U; ii < len; ++ii) {
		errnum = linted_pid_terminate(children[ii]);
		if (errnum != 0)
			goto free_children;
	}

free_children:
	linted_mem_free(children);

	return errnum;
}

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	char **result_envvars;
	linted_error errnum;

	size_t allowed_envvars_size = null_list_size(allowed_envvars);

	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, allowed_envvars_size + 1U,
		                                sizeof result_envvars[0U]);
		if (errnum != 0)
			return errnum;
		result_envvars = xx;
	}

	size_t result_envvars_size = 0U;
	for (size_t ii = 0U; ii < allowed_envvars_size; ++ii) {
		char const *envvar_name = allowed_envvars[ii];

		char *envvar_value;
		{
			char *xx;
			errnum = linted_environment_get(envvar_name, &xx);
			if (errnum != 0)
				goto free_result_envvars;
			envvar_value = xx;
		}
		if (0 == envvar_value)
			continue;

		++result_envvars_size;

		if (-1 == asprintf(&result_envvars[result_envvars_size - 1U],
		                   "%s=%s", envvar_name, envvar_value)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}

		linted_mem_free(envvar_value);

		if (errnum != 0)
			goto free_result_envvars;
	}
	result_envvars[result_envvars_size] = 0;

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
		if (0 == list[ii])
			return ii;
}

static linted_error str_from_strs(char const *const *strs, char const **strp)
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
	static char const *const yes_strs[] = {"1", "yes", "true", "on"};
	static char const *const no_strs[] = {"0", "no", "false", "off"};

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

static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp)
{
	linted_error errnum;

	pid_t ppid;
	{
		struct linted_pid_stat buf;

		errnum = linted_pid_stat(child, &buf);
		if (errnum != 0)
			return errnum;

		ppid = buf.ppid;
	}

	*isp = ppid == parent;

	return errnum;
}

static linted_error set_death_sig(int signum)
{
	linted_error errnum;

	if (-1 ==
	    prctl(PR_SET_PDEATHSIG, (unsigned long)signum, 0UL, 0UL, 0UL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_detach(pid_t pid, int signo)
{
	linted_error errnum;

	if (-1 ==
	    ptrace(PTRACE_DETACH, pid, (void *)0, (void *)(intptr_t) signo)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_setoptions(pid_t pid, unsigned options)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_SETOPTIONS, pid, (void *)0,
	                 (void *)(uintptr_t) options)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_geteventmsg(pid_t pid, unsigned long *msg)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_GETEVENTMSG, pid, (void *)0, (void *)msg)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_SEIZE, pid, (void *)0, (void *)options)) {
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
	    ptrace(PTRACE_CONT, pid, (void *)0, (void *)(intptr_t) signo)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}
