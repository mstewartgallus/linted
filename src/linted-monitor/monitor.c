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
#include "linted/file.h"
#include "linted/fifo.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/pool.h"
#include "linted/signal.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <arpa/inet.h>
#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <libgen.h>
#include <limits.h>
#include <locale.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/ptrace.h>

#define BACKLOG 20U
#define MAX_MANAGE_CONNECTIONS 10U

enum {
	WAITID,
	SIGWAITINFO,
	ADMIN_ACCEPTED_CONNECTION,
	ADMIN_READ_CONNECTION,
	ADMIN_WROTE_CONNECTION
};

enum { MAX_TASKS = ADMIN_READ_CONNECTION + MAX_MANAGE_CONNECTIONS };

struct wait_service_data
{
	char const *process_name;
	struct linted_asynch_pool *pool;
	char const *sandbox;
	char const *waiter;
	sigset_t const *orig_mask;
	struct linted_unit_db *unit_db;
	bool *time_to_exit;
	linted_ko cwd;
};

struct sigwait_data
{
	bool *time_to_exit;
	struct linted_unit_db *unit_db;
};

struct conn;

struct accepted_conn_data
{
	struct linted_asynch_pool *pool;
	struct conn_pool *conn_pool;
};

struct read_conn_data
{
	struct linted_asynch_pool *pool;
	struct conn *conn;
};

struct wrote_conn_data
{
	struct linted_asynch_pool *pool;
	struct conn *conn;
};

struct conn
{
	struct linted_pool_node node;
	struct read_conn_data read_data;
	struct wrote_conn_data write_data;
	struct linted_admin_task_recv_request *read_task;
	struct linted_admin_task_send_reply *write_task;
	linted_ko ko;
};

struct conn_pool;

#define SERVICE_NAME_MAX 32U

#define COMM_MAX 16U
struct pid_stat
{
	pid_t pid;
	char comm[COMM_MAX + 1U];
	char state;
	int ppid;
	int pgrp;
	int session;
	int tty_nr;
	int tpgid;
	unsigned flags;
	unsigned long minflt;
	unsigned long cminflt;
	unsigned long majflt;
	unsigned long cmajflt;
	unsigned long utime;
	unsigned long stime;
	long cutime;
	long cstime;
	long priority;
	long nice;
	long num_threads;
	long itrealvalue;
	unsigned long long starttime;
	unsigned long vsize;
	long rss;
	unsigned long rsslim;
	unsigned long startcode;
	unsigned long endcode;
	unsigned long startstack;
	unsigned long kstkesp;
	unsigned long kstkeip;
	unsigned long signal;
	unsigned long blocked;
	unsigned long sigignore;
	unsigned long sigcatch;
	unsigned long wchan;
	unsigned long nswap;
	unsigned long cnswap;
	int exit_signal;
	int processor;
	unsigned rt_priority;
	unsigned policy;
	unsigned long long delayacct_blkio_ticks;
	unsigned long guest_time;
	long cguest_time;
};

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      char const *path);
static linted_error create_unit_db(struct linted_unit_db **unit_dbp,
                                   struct linted_conf_db *conf_db,
                                   sigset_t const *orig_mask,
                                   char const *sandbox, char const *waiter);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf,
                                   sigset_t const *orig_mask,
                                   char const *sandbox, char const *waiter);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

static linted_error activate_unit_db(char const *process_name,
                                     struct linted_unit_db *unit_db,
                                     linted_ko cwd);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_sigwaitinfo(struct linted_asynch_task *task);
static linted_error on_accepted_conn(struct linted_asynch_task *task);
static linted_error on_read_conn(struct linted_asynch_task *task);
static linted_error on_wrote_conn(struct linted_asynch_task *task);

static linted_error on_status_request(union linted_admin_request const *request,
                                      union linted_admin_reply *reply);
static linted_error on_stop_request(union linted_admin_request const *request,
                                    union linted_admin_reply *reply);

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, pid_t pid,
                                     int exit_status, linted_ko cwd,
                                     struct linted_unit_db *unit_db);
static linted_error on_child_signaled(char const *process_name, pid_t pid,
                                      int signo);
static linted_error on_child_about_to_clone(pid_t pid);
static linted_error on_child_about_to_exit(char const *process_name,
                                           bool time_to_exit, pid_t pid,
                                           linted_ko cwd,
                                           struct linted_unit_db *unit_db);
static linted_error socket_activate(struct linted_unit_socket *unit);
static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit, linted_ko cwd,
                                     bool check);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error str_from_strs(char const *const *strs, char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);

static linted_error kill_pid_children(pid_t pid, int signo);

static linted_error service_name(pid_t pid,
                                 char name[static SERVICE_NAME_MAX + 1U]);
static linted_error service_pid(pid_t *pidp, char const *name);

static linted_error conn_pool_create(struct conn_pool **poolp);
static void conn_pool_destroy(struct conn_pool *pool);

static linted_error conn_insert(struct conn_pool *pool, struct conn **connp,
                                linted_ko ko);
static void conn_discard(struct conn *conn);

static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp);
static linted_error pid_stat(pid_t pid, struct pid_stat *buf);
static linted_error pid_children(pid_t pid, pid_t **childrenp, size_t *lenp);

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);
static linted_error ptrace_cont(pid_t pid, int signo);
static linted_error ptrace_detach(pid_t pid, int signo);
static linted_error ptrace_setoptions(pid_t pid, unsigned options);
static linted_error ptrace_geteventmsg(pid_t pid, unsigned long *msg);

static linted_error set_death_sig(int signum);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-monitor"
};

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	if (0 == setlocale(LC_ALL, "")) {
		linted_log(LINTED_LOG_ERR, "setlocale: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	sigset_t orig_mask;

	errnum = set_death_sig(SIGKILL);
	if (errnum != 0)
		goto exit_monitor;

	sigset_t exit_signals;
	sigemptyset(&exit_signals);
	sigaddset(&exit_signals, SIGHUP);
	sigaddset(&exit_signals, SIGINT);
	sigaddset(&exit_signals, SIGQUIT);
	sigaddset(&exit_signals, SIGTERM);

	/* Block signals before spawning threads */
	errnum = pthread_sigmask(SIG_BLOCK, &exit_signals, &orig_mask);
	if (errnum != 0)
		goto exit_monitor;

	char const *unit_path = getenv("LINTED_UNIT_PATH");
	char const *sandbox = getenv("LINTED_SANDBOX");
	char const *waiter = getenv("LINTED_WAITER");
	char const *data_dir_path = getenv("XDG_DATA_HOME");
	char const *runtime_dir_path = getenv("XDG_RUNTIME_DIR");

	if (0 == unit_path) {
		linted_log(LINTED_LOG_ERR,
		           "%s is a required environment variable",
		           "LINTED_UNIT_PATH");
		return EXIT_FAILURE;
	}

	if (0 == sandbox) {
		linted_log(LINTED_LOG_ERR,
		           "%s is a required environment variable",
		           "LINTED_SANDBOX");
		return EXIT_FAILURE;
	}

	if (0 == waiter) {
		linted_log(LINTED_LOG_ERR,
		           "%s is a required environment variable",
		           "LINTED_WAITER");
		return EXIT_FAILURE;
	}

	/**
	 * @todo Use fallbacks for missing XDG environment variables.
	 */
	if (0 == runtime_dir_path) {
		linted_log(LINTED_LOG_ERR,
		           "%s is a required environment variable",
		           "XDG_RUNTIME_HOME");
		return EXIT_FAILURE;
	}

	if (0 == data_dir_path) {
		linted_log(LINTED_LOG_ERR,
		           "%s is a required environment variable",
		           "XDG_DATA_HOME");
		return EXIT_FAILURE;
	}

	pid_t parent = getppid();

	char *package_runtime_dir_path;
	{
		char *xx;
		if (-1 ==
		    asprintf(&xx, "%s/%s", runtime_dir_path, PACKAGE_TARNAME)) {
			linted_log(LINTED_LOG_ERR, "asprintf: %s",
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
			linted_log(LINTED_LOG_ERR, "asprintf: %s",
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
		                   (uintmax_t)parent)) {
			linted_log(LINTED_LOG_ERR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		process_runtime_dir_path = xx;
	}

	char *process_data_dir_path;
	{
		char *xx;
		if (-1 == asprintf(&xx, "%s/%" PRIuMAX, package_data_dir_path,
		                   (uintmax_t)parent)) {
			linted_log(LINTED_LOG_ERR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		process_data_dir_path = xx;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, package_runtime_dir_path,
	                           0U, S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, package_data_dir_path, 0U,
	                           S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, process_runtime_dir_path,
	                           0U, S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	errnum = linted_dir_create(0, LINTED_KO_CWD, process_data_dir_path, 0U,
	                           S_IRWXU);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERR, "linted_dir_create: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	linted_ko cwd;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, ".",
		                        LINTED_KO_DIRECTORY);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERR, "linted_ko_open: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	if (-1 == chdir(process_runtime_dir_path)) {
		linted_log(LINTED_LOG_ERR, "chdir: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	if (-1 == symlink(process_data_dir_path, "var")) {
		if (errno != EEXIST) {
			linted_log(LINTED_LOG_ERR, "symlink: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

retry_bind:
	;
	linted_admin admin;
	{
		linted_admin xx;
		errnum = linted_admin_bind(&xx, BACKLOG, "admin-socket",
		                           strlen("admin-socket"));
		if (EADDRINUSE == errnum) {
			if (-1 == unlink("admin-socket")) {
				linted_log(LINTED_LOG_ERR, "unlink: %s",
				           linted_error_string(errno));
				return EXIT_FAILURE;
			}
			goto retry_bind;
		}
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERR, "linted_admin_bind: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		admin = xx;
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			goto exit_monitor;
		pool = xx;
	}

	bool time_to_exit = false;

	struct sigwait_data sigwait_data;
	struct wait_service_data sandbox_data;
	struct accepted_conn_data accepted_conn_data;

	struct linted_pid_task_waitid *sandbox_task;
	struct linted_signal_task_sigwaitinfo *sigwait_task;
	struct linted_admin_task_accept *accepted_conn_task;

	{
		struct linted_pid_task_waitid *xx;
		errnum = linted_pid_task_waitid_create(&xx, &sandbox_data);
		if (errnum != 0)
			goto destroy_pool;
		sandbox_task = xx;
	}

	{
		struct linted_signal_task_sigwaitinfo *xx;
		errnum =
		    linted_signal_task_sigwaitinfo_create(&xx, &sigwait_data);
		if (errnum != 0)
			goto destroy_pool;
		sigwait_task = xx;
	}

	{
		struct linted_admin_task_accept *xx;
		errnum =
		    linted_admin_task_accept_create(&xx, &accepted_conn_data);
		if (errnum != 0)
			goto destroy_pool;
		accepted_conn_task = xx;
	}

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
		errnum = conf_db_from_path(&xx, unit_path);
		if (errnum != 0)
			goto destroy_conn_pool;
		conf_db = xx;
	}

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		errnum =
		    create_unit_db(&xx, conf_db, &orig_mask, sandbox, waiter);
		if (errnum != 0)
			goto destroy_confs;
		unit_db = xx;
	}

	linted_signal_task_sigwaitinfo_prepare(sigwait_task, SIGWAITINFO,
	                                       &exit_signals);
	sigwait_data.time_to_exit = &time_to_exit;
	sigwait_data.unit_db = unit_db;

	linted_asynch_pool_submit(
	    pool, linted_signal_task_sigwaitinfo_to_asynch(sigwait_task));

	linted_admin_task_accept_prepare(accepted_conn_task,
	                                 ADMIN_ACCEPTED_CONNECTION, admin);
	accepted_conn_data.pool = pool;
	accepted_conn_data.conn_pool = conn_pool;

	linted_asynch_pool_submit(
	    pool, linted_admin_task_accept_to_asynch(accepted_conn_task));

	/**
	 * @todo Warn about unactivated unit_db.
	 */
	errnum = activate_unit_db(process_name, unit_db, cwd);
	if (errnum != 0)
		goto kill_procs;

	linted_pid_task_waitid_prepare(sandbox_task, WAITID, P_ALL, -1,
	                               WEXITED);
	sandbox_data.process_name = process_name;
	sandbox_data.pool = pool;
	sandbox_data.cwd = cwd;
	sandbox_data.sandbox = sandbox;
	sandbox_data.waiter = waiter;
	sandbox_data.unit_db = unit_db;
	sandbox_data.orig_mask = &orig_mask;
	sandbox_data.time_to_exit = &time_to_exit;

	linted_asynch_pool_submit(
	    pool, linted_pid_task_waitid_to_asynch(sandbox_task));

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
	if (ECANCELED == errnum)
		errnum = 0;

	linted_asynch_task_cancel(
	    linted_pid_task_waitid_to_asynch(sandbox_task));
	linted_asynch_task_cancel(
	    linted_admin_task_accept_to_asynch(accepted_conn_task));

kill_procs:
	linted_unit_db_destroy(unit_db);

destroy_confs:
	linted_conf_db_destroy(conf_db);

destroy_conn_pool:
	conn_pool_destroy(conn_pool);

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

		linted_error dispatch_error = dispatch(completed_task);
		if (0 == errnum)
			errnum = dispatch_error;
	}

destroy_pool : {
	linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
	if (0 == errnum)
		errnum = destroy_errnum;

	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)sandbox_task;
	(void)accepted_conn_task;
}

exit_monitor:
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERR, "could not run the game: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error create_unit_db(struct linted_unit_db **unit_dbp,
                                   struct linted_conf_db *conf_db,
                                   sigset_t const *orig_mask,
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
			errnum = EINVAL;
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

			errnum =
			    service_create(s, conf, orig_mask, sandbox, waiter);
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
                                   sigset_t const *orig_mask,
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
				return EINVAL;
			}
		}
	}

	if (0 == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		return EINVAL;
	}

	bool no_new_privs_value = false;
	if (no_new_privs != 0) {
		bool xx;
		errnum = bool_from_cstring(no_new_privs, &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs_value = xx;
	}

	unit->sigmask = orig_mask;
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
			return EINVAL;
		socket_type = LINTED_UNIT_SOCKET_TYPE_FILE;
		path = listen_file;
	}

	if (listen_fifo != 0) {
		if (path != 0)
			return EINVAL;
		socket_type = LINTED_UNIT_SOCKET_TYPE_FIFO;
		path = listen_fifo;
	}

	if (0 == path)
		return EINVAL;

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
                                     linted_ko cwd)
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

		errnum = service_activate(process_name, unit, cwd, true);
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
                                     struct linted_unit *unit, linted_ko cwd,
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
		errnum = service_pid(&xx, name);
		if (errnum != 0)
			goto service_not_found;
		child = xx;
	}

	fprintf(stderr, "%s: ptracing %i %s\n", process_name, child, name);

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

	sigset_t const *sigmask = unit_service->sigmask;
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
		return EINVAL;

	bool drop_caps = true;
	int priority;

	/* Favor other processes over this process hierarchy.  Only
	 * superuser may lower priorities so this is not
	 * stoppable. This also makes the process hierarchy nicer for
	 * the OOM killer.
	 */
	errno = 0;
	int current_priority = getpriority(PRIO_PROCESS, 0);
	if (-1 == current_priority) {
		errnum = errno;
		if (errnum != 0)
			return errnum;
	}
	priority = current_priority + 1;

	char prio_str[LINTED_NUMBER_TYPE_STRING_SIZE(int)+1U];
	sprintf(prio_str, "%i", priority);

	pid_t ppid = getppid();

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

	char *root_setting;
	{
		char *xx;
		if (-1 ==
		    asprintf(&xx, "MANAGERPID=%" PRIuMAX "", (uintmax_t)ppid))
			goto manager_asprintf_failed;
		root_setting = xx;
		goto manager_asprintf_succeeded;
	}
manager_asprintf_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);
	goto free_envvars;

manager_asprintf_succeeded:
	;
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
	linted_mem_free(root_setting);
	goto free_envvars;

service_asprintf_succeeded:
	;
	size_t envvars_size = null_list_size((char const * const *)envvars);
	size_t new_size = envvars_size + 3U;
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
	linted_mem_free(root_setting);
	goto free_envvars;

envvar_allocate_succeeded:
	envvars[envvars_size] = root_setting;
	envvars[envvars_size + 1U] = service_name_setting;
	envvars[envvars_size + 2U] = 0;

	char *sandbox_dup = strdup(sandbox);
	if (0 == sandbox_dup) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto free_envvars;
	}
	char *sandbox_base = basename(sandbox_dup);

	size_t exec_start_size =
	    null_list_size((char const * const *)exec_start);

	struct option options[] = { { "--traceme", 0, true },
		                    { "--waiter", waiter, waiter != 0 },
		                    { "--chrootdir", chrootdir, fstab != 0 },
		                    { "--fstab", fstab, fstab != 0 },
		                    { "--nonewprivs", 0, no_new_privs },
		                    { "--dropcaps", 0, drop_caps },
		                    { "--chdir", chdir_path, chdir_path != 0 },
		                    { "--priority", prio_str, prio_str != 0 },
		                    { "--clone-newuser", 0, clone_newuser },
		                    { "--clone-newpid", 0, clone_newpid },
		                    { "--clone-newipc", 0, clone_newipc },
		                    { "--clone-newnet", 0, clone_newnet },
		                    { "--clone-newns", 0, clone_newns },
		                    { "--clone-newuts", 0, clone_newuts } };

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

	struct linted_spawn_file_actions *file_actions;
	struct linted_spawn_attr *attr;

	{
		struct linted_spawn_file_actions *xx;
		errnum = linted_spawn_file_actions_init(&xx);
		if (errnum != 0)
			goto free_args;
		file_actions = xx;
	}

	{
		struct linted_spawn_attr *xx;
		errnum = linted_spawn_attr_init(&xx);
		if (errnum != 0)
			goto destroy_file_actions;
		attr = xx;
	}

	linted_spawn_attr_setmask(attr, sigmask);

	linted_ko *proc_kos = 0;
	size_t kos_opened = 0U;

	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, 3U, sizeof proc_kos[0U]);
		if (errnum != 0)
			goto destroy_attr;
		proc_kos = xx;
	}

	for (size_t ii = 0U; ii < 3U; ++ii) {
		linted_ko null;
		{
			linted_ko xx;
			errnum = linted_ko_open(&xx, LINTED_KO_CWD, "/dev/null",
			                        LINTED_KO_RDWR);
			if (errnum != 0)
				goto destroy_proc_kos;
			null = xx;
		}

		size_t old_kos_opened = kos_opened;
		kos_opened = old_kos_opened + 1U;

		proc_kos[old_kos_opened] = null;

		errnum = linted_spawn_file_actions_adddup2(&file_actions, null,
		                                           old_kos_opened);
		if (errnum != 0)
			goto destroy_proc_kos;
	}

	errnum = linted_spawn(0, cwd, sandbox, file_actions, attr, args,
	                      (char const * const *)envvars);
	if (errnum != 0)
		goto destroy_proc_kos;

/* Let the child be leaked, we'll get the wait later */

destroy_proc_kos:
	for (size_t jj = 0; jj < kos_opened; ++jj)
		linted_ko_close(proc_kos[jj]);
	linted_mem_free(proc_kos);

destroy_attr:
	linted_spawn_attr_destroy(attr);

destroy_file_actions:
	linted_spawn_file_actions_destroy(file_actions);

free_args:
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

	case SIGWAITINFO:
		return on_sigwaitinfo(task);

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

	errnum = linted_asynch_task_errnum(task);
	if (ECANCELED == errnum)
		return 0;
	if (ECHILD == errnum)
		return ECANCELED;
	if (errnum != 0)
		return errnum;

	struct linted_pid_task_waitid *sandbox_task =
	    linted_pid_task_waitid_from_asynch(task);
	struct wait_service_data *wait_service_data =
	    linted_pid_task_waitid_data(sandbox_task);
	struct linted_asynch_pool *pool = wait_service_data->pool;

	char const *process_name = wait_service_data->process_name;
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
		errnum = on_child_trapped(process_name, time_to_exit, pid,
		                          exit_status, cwd, unit_db);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	linted_asynch_pool_submit(pool, task);

	return errnum;
}

static linted_error on_sigwaitinfo(struct linted_asynch_task *task)
{
	linted_error errnum = 0;

	errnum = linted_asynch_task_errnum(task);
	if (ECANCELED == errnum)
		return 0;
	if (errnum != 0)
		return errnum;

	struct linted_signal_task_sigwaitinfo *sigwait_task =
	    linted_signal_task_sigwaitinfo_from_asynch(task);
	struct sigwait_data *sigwait_data =
	    linted_signal_task_sigwaitinfo_data(sigwait_task);
	bool *time_to_exit = sigwait_data->time_to_exit;
	struct linted_unit_db *unit_db = sigwait_data->unit_db;

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != LINTED_UNIT_TYPE_SERVICE)
			continue;

		pid_t pid;
		{
			pid_t xx;
			linted_error pid_errnum = service_pid(&xx, unit->name);
			if (ESRCH == pid_errnum)
				continue;
			if (pid_errnum != 0) {
				if (0 == errnum)
					errnum = pid_errnum;
				continue;
			}
			pid = xx;
		}
		if (-1 == kill(pid, SIGTERM)) {
			linted_error kill_errnum = errno;
			LINTED_ASSUME(kill_errnum != 0);
			if (kill_errnum != ESRCH) {
				assert(kill_errnum != EINVAL);
				assert(kill_errnum != EPERM);
				assert(0 == errnum);
			}
		}
	}

	*time_to_exit = true;

	return 0;
}

static linted_error on_accepted_conn(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
	if (ECANCELED == errnum)
		return 0;
	if (errnum != 0)
		return errnum;

	struct linted_admin_task_accept *accepted_conn_task =
	    linted_admin_task_accept_from_asynch(task);
	struct accepted_conn_data *accepted_conn_data =
	    linted_admin_task_accept_data(accepted_conn_task);

	struct linted_asynch_pool *pool = accepted_conn_data->pool;
	struct conn_pool *conn_pool = accepted_conn_data->conn_pool;

	linted_admin new_socket =
	    linted_admin_task_accept_returned_ko(accepted_conn_task);

	linted_asynch_pool_submit(pool, task);

	struct conn *conn;
	{
		struct conn *xx;
		if (conn_insert(conn_pool, &xx, new_socket) != 0)
			goto close_new_socket;
		conn = xx;
	}

	struct linted_admin_task_recv_request *read_task;
	{
		struct linted_admin_task_recv_request *xx;
		errnum = linted_admin_task_recv_request_create(
		    &xx, &conn->read_data);
		if (errnum != 0)
			goto close_new_socket;
		read_task = xx;
	}

	struct linted_admin_task_send_reply *write_task;
	{
		struct linted_admin_task_send_reply *xx;
		errnum =
		    linted_admin_task_send_reply_create(&xx, &conn->write_data);
		if (errnum != 0)
			goto close_new_socket;
		write_task = xx;
	}

	conn->read_task = read_task;
	conn->write_task = write_task;

	conn->read_data.pool = pool;
	conn->read_data.conn = conn;
	linted_admin_task_recv_request_prepare(read_task, ADMIN_READ_CONNECTION,
	                                       new_socket);
	linted_asynch_pool_submit(
	    pool, linted_admin_task_recv_request_to_asynch(conn->read_task));
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

	errnum = linted_asynch_task_errnum(task);

	struct linted_admin_task_recv_request *read_conn_task =
	    linted_admin_task_recv_request_from_asynch(task);
	struct read_conn_data *read_conn_data =
	    linted_admin_task_recv_request_data(read_conn_task);

	struct linted_asynch_pool *pool = read_conn_data->pool;
	struct conn *conn = read_conn_data->conn;

	if (errnum != 0)
		goto conn_remove;

	linted_ko ko = linted_admin_task_recv_request_ko(read_conn_task);

	union linted_admin_request const *request =
	    linted_admin_task_recv_request_request(read_conn_task);
	union linted_admin_reply reply;

	switch (request->type) {
	case LINTED_ADMIN_STATUS:
		errnum = on_status_request(request, &reply);
		break;

	case LINTED_ADMIN_STOP:
		errnum = on_stop_request(request, &reply);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
conn_remove:
	/* Assume the other end did something bad and don't exit with
	 * an error (unless conn_remove screws up.) */
	if (errnum != 0) {
		conn_discard(conn);

		if (ECANCELED == errnum)
			return 0;

		return 0;
	}

	linted_admin_task_send_reply_prepare(
	    conn->write_task, ADMIN_WROTE_CONNECTION, ko, &reply);
	conn->write_data.pool = pool;
	conn->write_data.conn = conn;

	linted_asynch_pool_submit(
	    pool, linted_admin_task_send_reply_to_asynch(conn->write_task));
	return 0;
}

static linted_error on_wrote_conn(struct linted_asynch_task *task)
{
	linted_error errnum = linted_asynch_task_errnum(task);

	struct linted_admin_task_send_reply *wrote_conn_task =
	    linted_admin_task_send_reply_from_asynch(task);
	struct wrote_conn_data *wrote_conn_data =
	    linted_admin_task_send_reply_data(wrote_conn_task);

	struct conn *conn = wrote_conn_data->conn;

	conn_discard(conn);

	if (ECANCELED == errnum)
		return 0;

	return errnum;
}

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, pid_t pid,
                                     int exit_status, linted_ko cwd,
                                     struct linted_unit_db *unit_db)
{
	int event = exit_status >> 8U;
	switch (event) {
	case 0:
		return on_child_signaled(process_name, pid, exit_status);

	case PTRACE_EVENT_EXIT:
		return on_child_about_to_exit(process_name, time_to_exit, pid,
		                              cwd, unit_db);

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

		char name[SERVICE_NAME_MAX + 1U];
		errnum = service_name(pid, name);
		if (errnum != 0)
			return errnum;

		fprintf(stderr, "%s: ptracing %" PRIuMAX " %s\n", process_name,
		        (uintmax_t)pid, name);

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
                                           linted_ko cwd,
                                           struct linted_unit_db *unit_db)
{

	linted_error errnum = 0;
	struct linted_unit *unit = 0;

	errnum = kill_pid_children(pid, SIGKILL);
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

	char name[SERVICE_NAME_MAX + 1U];
	errnum = service_name(pid, name);
	if (errnum != 0)
		goto detach_from_process;

	if (WIFEXITED(status)) {
		int exit_status = WEXITSTATUS(status);

		fprintf(stderr, "%s: %s: exited with %i\n", process_name, name,
		        exit_status);

	} else if (WIFSIGNALED(status)) {
		int signo = WTERMSIG(status);

		fprintf(stderr, "%s: %s: killed by %s\n", process_name, name,
		        strsignal(signo));
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

	errnum = service_activate(process_name, unit, cwd, false);

	return errnum;
}

/* A sandbox creator is creating a sandbox */
static linted_error on_child_about_to_clone(pid_t pid)
{
	return ptrace_detach(pid, 0);
}

static linted_error on_status_request(union linted_admin_request const *request,
                                      union linted_admin_reply *reply)
{
	linted_error errnum = 0;
	bool is_up;

	pid_t pid;
	{
		pid_t xx;
		errnum = service_pid(&xx, request->status.name);
		if (errnum != 0)
			goto pid_find_failure;
		pid = xx;
		goto found_pid;
	}
pid_find_failure:
	errnum = 0;
	is_up = false;
	goto reply;

found_pid:
	if (-1 == kill(pid, 0)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(errnum != EINVAL);
		if (ESRCH == errnum)
			errnum = 0;

		is_up = false;
	} else {
		is_up = true;
	}

reply:
	if (errnum != 0)
		return errnum;

	reply->status.is_up = is_up;
	return 0;
}

static linted_error on_stop_request(union linted_admin_request const *request,
                                    union linted_admin_reply *reply)
{
	linted_error errnum = 0;
	bool was_up;

	pid_t pid;
	{
		pid_t xx;
		errnum = service_pid(&xx, request->status.name);
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
	if (-1 == kill(pid, SIGKILL)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		assert(errnum != EINVAL);
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

static char const *default_envvars[] = { "USER", "LOGNAME", "HOME", "SHELL",
	                                 "XDG_RUNTIME_DIR"
	                                 "XDG_SESSION_ID",
	                                 "XDG_SEAT", "TERM", "LD_DEBUG",
	                                 "LD_DEBUG_OUTPUT", 0 };

static linted_error conf_db_from_path(struct linted_conf_db **dbp,
                                      char const *path)
{
	linted_ko errnum = 0;

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

				struct linted_conf_section *service;
				{
					struct linted_conf_section *xx;
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
				    service, env_whitelist, default_envvars);
				if (errnum != 0)
					goto close_unit_file;

			} else {
				errnum = EINVAL;
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

static linted_error service_name(pid_t pid,
                                 char name[static SERVICE_NAME_MAX + 1U])
{
	linted_error errnum;

	memset(name, 0, SERVICE_NAME_MAX + 1U);

	char path[sizeof "/proc/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/environ" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/environ", (uintmax_t)pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	linted_ko ko;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
		if (errnum != 0)
			return errnum;
		ko = xx;
	}

	FILE *file = fdopen(ko, "r");
	if (0 == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(ko);

		return errnum;
	}

	/* Get the buffer all at once to avoid raciness. */
	char *buf = 0;
	size_t buf_size = 0U;

	{
		char *xx = buf;
		size_t yy = buf_size;

		errno = 0;
		ssize_t zz = getline(&xx, &yy, file);
		if (-1 == zz) {
			errnum = errno;
			/* May be zero */
			goto close_file;
		}
		buf = xx;
	}

close_file:
	if (EOF == fclose(file)) {
		if (0 == errnum) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

	if (0 == buf)
		return errnum;

	char *iter = buf;
	for (;;) {
		if (0 == strncmp("LINTED_SERVICE=", iter,
		                 strlen("LINTED_SERVICE="))) {
			strncpy(name, iter + strlen("LINTED_SERVICE="),
			        SERVICE_NAME_MAX);
			break;
		}
		iter = strchr(iter, '\0');
		if (0 == iter) {
			errnum = EINVAL;
			break;
		}
		++iter;
		if ('\0' == *iter)
			break;
		if ('\n' == *iter)
			break;
	}

	linted_mem_free(buf);

	return errnum;
}

static linted_error service_pid(pid_t *pidp, char const *name)
{
	linted_error errnum = 0;

	pid_t ppid = getppid();

	pid_t *children;
	size_t len;
	{
		pid_t *xx;
		size_t yy;
		errnum = pid_children(ppid, &xx, &yy);
		if (errnum != 0)
			return errnum;
		children = xx;
		len = yy;
	}
	if (0U == len)
		return ESRCH;

	pid_t pid = -1;
	for (size_t ii = 0U; ii < len; ++ii) {
		pid_t child = children[ii];

		char other_name[SERVICE_NAME_MAX + 1U];
		errnum = service_name(child, other_name);
		if (errnum != 0)
			goto free_buf;

		if (0 == strcmp(name, other_name)) {
			pid = child;
			break;
		}
	}

free_buf:
	linted_mem_free(children);

	if (errnum != 0)
		return errnum;

	if (-1 == pid)
		return ESRCH;

	*pidp = pid;

	return 0;
}

static linted_error kill_pid_children(pid_t pid, int signo)
{
	linted_error errnum = 0;

	pid_t *children;
	size_t len;
	{
		pid_t *xx;
		size_t yy;
		errnum = pid_children(pid, &xx, &yy);
		if (errnum != 0)
			return errnum;
		children = xx;
		len = yy;
	}

	for (size_t ii = 0U; ii < len; ++ii) {
		if (-1 == kill(children[ii], signo)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_children;
		}
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
		if (0 == envvar_value)
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

	if (0 == str)
		return EINVAL;

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

	return EINVAL;

return_result:
	*boolp = result;
	return 0;
}

static linted_error conn_pool_create(struct conn_pool **poolp)
{
	linted_error errnum;

	struct linted_pool *pool;
	{
		struct linted_pool *xx;
		errnum = linted_pool_create(&xx);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	*poolp = (void *)pool;

	return 0;
}

static void conn_pool_destroy(struct conn_pool *pool)
{
	struct linted_pool *poolx = (void *)pool;
	for (;;) {
		struct conn *conn;
		{
			struct linted_pool_node *xx;
			if (EAGAIN == linted_pool_remove_node(poolx, &xx))
				break;
			conn = (void *)xx;
		}
		linted_ko_close(conn->ko);
		linted_mem_free(conn);
	}

	linted_pool_destroy(poolx);
}

static linted_error conn_insert(struct conn_pool *pool, struct conn **connp,
                                linted_ko ko)
{
	linted_error errnum;

	struct conn *conn;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *conn);
		if (errnum != 0)
			return errnum;
		conn = xx;
	}
	linted_pool_node_create(&conn->node);
	conn->ko = ko;

	linted_pool_insert_node((void *)pool, &conn->node);

	*connp = conn;

	return errnum;
}

static void conn_discard(struct conn *conn)
{
	linted_pool_node_discard(&conn->node);
	linted_ko_close(conn->ko);
	linted_mem_free(conn);
}

static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp)
{
	linted_error errnum;

	pid_t ppid;
	{
		struct pid_stat buf;

		errnum = pid_stat(child, &buf);
		if (errnum != 0)
			return errnum;

		ppid = buf.ppid;
	}

	*isp = ppid == parent;

	return errnum;
}

static linted_error pid_stat(pid_t pid, struct pid_stat *buf)
{
	linted_error errnum = 0;

	char path[sizeof "/proc/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/stat" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/stat", (uintmax_t)pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	linted_ko stat_ko;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
		if (errnum != 0)
			return errnum;
		stat_ko = xx;
	}

	FILE *file = fdopen(stat_ko, "r");
	if (0 == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(stat_ko);

		return errnum;
	}

	memset(buf, 0, sizeof *buf);

	char *line;
	{
		char *xx = 0;
		size_t yy = 0U;
		if (-1 == getline(&xx, &yy, file)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto close_file;
		}
		line = xx;
	}

	/* If some fields are missing just leave them to be zero */
	if (EOF == sscanf(line, "%d (" /* pid */
	                  ,
	                  &buf->pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_file;
	}

	/* Avoid troubles with processes that have names like ':-) 0 1
	 * 2 3 4 5'. procps-ng takes a different approach involving
	 * limits on the possible size of a name that I'm not actually
	 * sure works. */

	char *start = strchr(line, '(') + 1U;
	char *end = strrchr(line, ')');
	memset(buf->comm, 0, sizeof buf->comm);
	memcpy(buf->comm, start, end - start);

	if (EOF ==
	    sscanf(end, ")\n"
	                "%c\n"   /* state */
	                "%d\n"   /* ppid */
	                "%d\n"   /* pgrp */
	                "%d\n"   /* session */
	                "%d\n"   /* tty_nr */
	                "%d\n"   /* tpgid */
	                "%u\n"   /* flags */
	                "%lu\n"  /* minflt */
	                "%lu\n"  /* cminflt */
	                "%lu\n"  /* majflt */
	                "%lu\n"  /* cmajflt */
	                "%lu\n"  /* utime */
	                "%lu\n"  /* stime */
	                "%ld\n"  /* cutime */
	                "%ld\n"  /* cstime */
	                "%ld\n"  /* priority */
	                "%ld\n"  /* nice */
	                "%ld\n"  /* num_threads */
	                "%ld\n"  /* itrealvalue */
	                "%llu\n" /* starttime */
	                "%lu\n"  /* vsize */
	                "%ld\n"  /* rss */
	                "%lu\n"  /* rsslim */
	                "%lu\n"  /* startcode */
	                "%lu\n"  /* endcode */
	                "%lu\n"  /* startstack */
	                "%lu\n"  /* kstkesp */
	                "%lu\n"  /* kstkeip */
	                "%lu\n"  /* signal */
	                "%lu\n"  /* blocked */
	                "%lu\n"  /* sigignore */
	                "%lu\n"  /* sigcatch */
	                "%lu\n"  /* wchan */
	                "%lu\n"  /* nswap */
	                "%lu\n"  /* cnswap */
	                "%d\n"   /* exit_signal */
	                "%d\n"   /* processor */
	                "%u\n"   /* rt_priority */
	                "%u\n"   /* policy */
	                "%llu\n" /* delayacct_blkio_ticks */
	                "%lu\n"  /* guest_time */
	                "%ld\n"  /* cguest_time */
	           ,
	           &buf->state, &buf->ppid, &buf->pgrp, &buf->session,
	           &buf->tty_nr, &buf->tpgid, &buf->flags, &buf->minflt,
	           &buf->cminflt, &buf->majflt, &buf->cmajflt, &buf->utime,
	           &buf->stime, &buf->cutime, &buf->cstime, &buf->priority,
	           &buf->nice, &buf->num_threads, &buf->itrealvalue,
	           &buf->starttime, &buf->vsize, &buf->rss, &buf->rsslim,
	           &buf->startcode, &buf->endcode, &buf->startstack,
	           &buf->kstkesp, &buf->kstkeip, &buf->signal, &buf->blocked,
	           &buf->sigignore, &buf->sigcatch, &buf->wchan, &buf->nswap,
	           &buf->cnswap, &buf->exit_signal, &buf->processor,
	           &buf->rt_priority, &buf->policy, &buf->delayacct_blkio_ticks,
	           &buf->guest_time, &buf->cguest_time)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto close_file;
	}

close_file:
	if (EOF == fclose(file)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return errnum;
}

static linted_error pid_children(pid_t pid, pid_t **childrenp, size_t *lenp)
{
	linted_error errnum;

	char path[sizeof "/proc/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/task/" - 1U + LINTED_NUMBER_TYPE_STRING_SIZE(pid_t) +
	          sizeof "/children" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/task/%" PRIuMAX "/children",
	                  (uintmax_t)pid, (uintmax_t)pid)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	linted_ko children_ko;
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
		if (errnum != 0)
			return errnum;
		children_ko = xx;
	}

	FILE *file = fdopen(children_ko, "r");
	if (0 == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(children_ko);

		return errnum;
	}

	/* Get the child all at once to avoid raciness. */
	char *buf = 0;

	{
		char *xx = buf;
		size_t yy = 0U;

		errno = 0;
		ssize_t zz = getline(&xx, &yy, file);
		if (-1 == zz) {
			errnum = errno;
			/* May be zero */
			goto set_childrenp;
		}
		buf = xx;
	}

set_childrenp:
	if (EOF == fclose(file)) {
		if (0 == errnum) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

	if (errnum != 0) {
		linted_mem_free(buf);
		return errnum;
	}

	size_t ii = 0U;
	char const *start = buf;
	pid_t *children = 0;

	if (0 == buf)
		goto finish;

	for (;;) {
		errno = 0;
		pid_t child = strtol(start, 0, 10);
		errnum = errno;
		if (errnum != 0)
			goto free_buf;

		{
			void *xx;
			errnum = linted_mem_realloc_array(
			    &xx, children, ii + 1U, sizeof children[0U]);
			if (errnum != 0) {
				linted_mem_free(children);
				linted_mem_free(buf);
				return errnum;
			}
			children = xx;
		}
		children[ii] = child;
		++ii;

		start = strchr(start, ' ');
		if (0 == start)
			break;
		if ('\n' == *start)
			break;
		if ('\0' == *start)
			break;
		++start;
		if ('\n' == *start)
			break;
		if ('\0' == *start)
			break;
	}

free_buf:
	linted_mem_free(buf);

finish:
	*lenp = ii;
	*childrenp = children;

	return errnum;
}

static linted_error ptrace_detach(pid_t pid, int signo)
{
	linted_error errnum;

	if (-1 ==
	    ptrace(PTRACE_DETACH, pid, (void *)0, (void *)(intptr_t)signo)) {
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
	                 (void *)(uintptr_t)options)) {
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
	    ptrace(PTRACE_CONT, pid, (void *)0, (void *)(intptr_t)signo)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
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
