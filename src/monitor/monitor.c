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
#include "linted/pool.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
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
	struct linted_asynch_pool *pool;
	linted_ko cwd;
	char const *chrootdir;
	char const *sandbox;
	char const *waiter;
	sigset_t const *orig_mask;
	struct linted_unit_db *unit_db;
	bool *time_to_exit;
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

#define COMM_MAX 16U

struct pidstat
{
	pid_t pid;
	char comm[1U + COMM_MAX + 1U + 1U];
	char state;
	pid_t ppid;
	pid_t pgrp;
	pid_t session;
	int tty_nr;
	pid_t tpgid;
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

static linted_error activate_unit_db(struct linted_unit_db *unit_db,
                                     linted_ko cwd, char const *chrootdir,
                                     char const *sandbox, char const *waiter,
                                     sigset_t const *orig_mask);

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

static linted_error on_child_exited(pid_t pid, int exit_status);
static linted_error on_child_trapped(bool time_to_exit, pid_t pid,
                                     int exit_status, linted_ko cwd,
                                     struct linted_unit_db *unit_db,
                                     char const *chrootdir);
static linted_error on_child_signaled(pid_t pid, int signo);
static linted_error on_child_about_to_clone(pid_t pid);
static linted_error on_child_about_to_exit(bool time_to_exit, pid_t pid,
                                           linted_ko cwd,
                                           struct linted_unit_db *unit_db,
                                           char const *chrootdir);
static linted_error socket_activate(struct linted_unit_socket *unit);
static linted_error service_activate(struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir,
                                     struct linted_unit_db *unit_db);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error str_from_strs(char const *const *strs, char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);
static linted_error long_from_cstring(char const *str, long *longp);

static linted_error pid_children(linted_ko *kop, pid_t pid);
static linted_error kill_pid_children(pid_t pid, int signo);

static linted_error pid_of_service(pid_t *pidp, char const *name);

static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp);

static linted_error pid_comm(pid_t pid, char comm[static COMM_MAX + 1U]);
static linted_error pid_stat(pid_t pid, struct pidstat *buf);

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);
static linted_error ptrace_cont(pid_t pid, int signo);
static linted_error ptrace_detach(pid_t pid, int signo);
static linted_error ptrace_setoptions(pid_t pid, unsigned options);
static linted_error ptrace_geteventmsg(pid_t pid, unsigned long *msg);
static linted_error ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo);

static linted_error set_death_sig(int signum);

static linted_error conn_pool_create(struct conn_pool **poolp);
static void conn_pool_destroy(struct conn_pool *pool);

static linted_error conn_insert(struct conn_pool *pool, struct conn **connp,
                                linted_ko ko);
static void conn_discard(struct conn *conn);

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

	linted_dir private_run_dir = kos[0U];

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

	char const *chrootdir = getenv("LINTED_CHROOT");
	char const *unit_path = getenv("LINTED_UNIT_PATH");
	char const *sandbox = getenv("LINTED_SANDBOX");
	char const *waiter = getenv("LINTED_WAITER");

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

	if (NULL == sandbox) {
		linted_io_write_format(
		    STDERR_FILENO, NULL,
		    "%s: LINTED_SANDBOX is a required environment variable\n",
		    process_name);
		return EXIT_FAILURE;
	}

	if (NULL == waiter) {
		linted_io_write_format(
		    STDERR_FILENO, NULL,
		    "%s: LINTED_waiter is a required environment variable\n",
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

	if (-1 == fchdir(private_run_dir)) {
		perror("fchdir");
		return EXIT_FAILURE;
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
				perror("unlink");
				return EXIT_FAILURE;
			}
			goto retry_bind;
		}
		if (errnum != 0) {
			errno = errnum;
			perror("linted_admin_bind");
			return EXIT_FAILURE;
		}
		admin = xx;
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

	bool time_to_exit = false;

	struct sigwait_data sigwait_data;
	struct wait_service_data sandbox_data;
	struct accepted_conn_data accepted_conn_data;

	struct linted_asynch_task_waitid *sandbox_task;
	struct linted_asynch_task_sigwaitinfo *sigwait_task;
	struct linted_admin_task_accept *accepted_conn_task;

	{
		struct linted_asynch_task_waitid *xx;
		errnum = linted_asynch_task_waitid_create(&xx, &sandbox_data);
		if (errnum != 0)
			goto destroy_pool;
		sandbox_task = xx;
	}

	{
		struct linted_asynch_task_sigwaitinfo *xx;
		errnum =
		    linted_asynch_task_sigwaitinfo_create(&xx, &sigwait_data);
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
		errnum = linted_conf_db_create_from_path(&xx, unit_path);
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

	linted_asynch_task_sigwaitinfo_prepare(sigwait_task, SIGWAITINFO,
	                                       &exit_signals);
	sigwait_data.time_to_exit = &time_to_exit;
	sigwait_data.unit_db = unit_db;

	linted_asynch_pool_submit(
	    pool, linted_asynch_task_sigwaitinfo_to_asynch(sigwait_task));

	linted_admin_task_accept_prepare(accepted_conn_task,
	                                 ADMIN_ACCEPTED_CONNECTION, admin);
	accepted_conn_data.pool = pool;
	accepted_conn_data.conn_pool = conn_pool;

	linted_asynch_pool_submit(
	    pool, linted_admin_task_accept_to_asynch(accepted_conn_task));

	/**
	 * @todo Warn about unactivated unit_db.
	 */
	errnum = activate_unit_db(unit_db, cwd, chrootdir, sandbox, waiter,
	                          &orig_mask);
	if (errnum != 0)
		goto kill_procs;

	linted_asynch_task_waitid_prepare(sandbox_task, WAITID, P_ALL, -1,
	                                  WEXITED);
	sandbox_data.pool = pool;
	sandbox_data.cwd = cwd;
	sandbox_data.chrootdir = chrootdir;
	sandbox_data.sandbox = sandbox;
	sandbox_data.waiter = waiter;
	sandbox_data.unit_db = unit_db;
	sandbox_data.orig_mask = &orig_mask;
	sandbox_data.time_to_exit = &time_to_exit;

	linted_asynch_pool_submit(
	    pool, linted_asynch_task_waitid_to_asynch(sandbox_task));

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
	    linted_asynch_task_waitid_to_asynch(sandbox_task));
	linted_asynch_task_cancel(
	    linted_admin_task_accept_to_asynch(accepted_conn_task));

kill_procs:
	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

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
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "could not run the game: %s\n",
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

		enum linted_unit_type unit_type;
		if (0 == strcmp(suffix, "socket")) {
			unit_type = UNIT_TYPE_SOCKET;
		} else if (0 == strcmp(suffix, "service")) {
			unit_type = UNIT_TYPE_SERVICE;
		} else {
			errnum = EINVAL;
			goto destroy_unit_db;
		}

		char *unit_name = strndup(file_name, dot - file_name);
		if (NULL == unit_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto destroy_unit_db;
		}

		unit->type = unit_type;
		unit->name = unit_name;

		switch (unit_type) {
		case UNIT_TYPE_SERVICE: {
			struct linted_unit_service *s = (void *)unit;

			errnum =
			    service_create(s, conf, orig_mask, sandbox, waiter);
			if (errnum != 0)
				goto destroy_unit_db;
			break;
		}

		case UNIT_TYPE_SOCKET: {
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
	char const *const *files =
	    linted_conf_find(conf, "Service", "X-LintedFiles");
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

	bool clone_newuser = false;
	bool clone_newpid = false;
	bool clone_newipc = false;
	bool clone_newnet = false;
	bool clone_newns = false;
	bool clone_newuts = false;
	if (clone_flags != NULL) {
		for (size_t ii = 0U; clone_flags[ii] != NULL; ++ii) {
			if (0 == strcmp("CLONE_NEWUSER", clone_flags[ii])) {
				clone_newuser = true;
			} else if (0 ==
			           strcmp("CLONE_NEWPID", clone_flags[ii])) {
				clone_newpid = true;
			} else if (0 ==
			           strcmp("CLONE_NEWIPC", clone_flags[ii])) {
				clone_newipc = true;
			} else if (0 ==
			           strcmp("CLONE_NEWNET", clone_flags[ii])) {
				clone_newnet = true;
			} else if (0 ==
			           strcmp("CLONE_NEWNS", clone_flags[ii])) {
				clone_newns = true;
			} else if (0 ==
			           strcmp("CLONE_NEWUTS", clone_flags[ii])) {
				clone_newuts = true;
			} else {
				return EINVAL;
			}
		}
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

	unit->sigmask = orig_mask;
	unit->sandbox = sandbox;
	unit->waiter = waiter;

	unit->exec_start = exec_start;
	unit->no_new_privs = no_new_privs_value;
	unit->files = files;
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

	char const *const *paths =
	    linted_conf_find(conf, "Socket", "ListenMessageQueue");
	char const *const *maxmsgss =
	    linted_conf_find(conf, "Socket", "MessageQueueMaxMessages");
	char const *const *msgsizes =
	    linted_conf_find(conf, "Socket", "MessageQueueMessageSize");
	char const *const *temps =
	    linted_conf_find(conf, "Socket", "X-LintedTemporary");

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

static linted_error activate_unit_db(struct linted_unit_db *unit_db,
                                     linted_ko cwd, char const *chrootdir,
                                     char const *sandbox, char const *waiter,
                                     sigset_t const *orig_mask)
{
	linted_error errnum;

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != UNIT_TYPE_SOCKET)
			continue;

		errnum = socket_activate((void *)unit);
		if (errnum != 0)
			return errnum;
	}

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != UNIT_TYPE_SERVICE)
			continue;

		errnum = service_activate(unit, cwd, chrootdir, unit_db);
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

enum { RDONLY, WRONLY };

struct pair
{
	unsigned long flags;
	linted_ko ko;
};

struct option
{
	bool flag;
	char const *name;
	char const *value;
};

static char const *const file_flags[] = {[RDONLY] = "rdonly",
	                                 [WRONLY] = "wronly", NULL };

static char const *const default_envvars[] = { "LANG", "USER", "LOGNAME",
	                                       "HOME", "SHELL",
	                                       "XDG_RUNTIME_DIR"
	                                       "XDG_SESSION_ID",
	                                       "XDG_SEAT", "TERM" };

static struct pair const defaults[] = { { LINTED_KO_RDONLY, STDIN_FILENO },
	                                { LINTED_KO_WRONLY, STDOUT_FILENO },
	                                { LINTED_KO_WRONLY, STDERR_FILENO } };

static linted_error service_activate(struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir,
                                     struct linted_unit_db *unit_db)
{
	linted_error errnum = 0;

	char const *service_name = unit->name;

	struct linted_unit_service *unit_service = (void *)unit;

	pid_t child;
	{
		pid_t xx;
		errnum = pid_of_service(&xx, service_name);
		if (errnum != 0)
			goto service_not_found;
		child = xx;
	}

	fprintf(stderr, "ptracing service %s: %i\n", service_name, child);

	return ptrace_seize(child, PTRACE_O_TRACEEXIT);

service_not_found:
	if (errnum != ESRCH)
		return errnum;

	char const *const *exec_start = unit_service->exec_start;
	bool no_new_privs = unit_service->no_new_privs;
	char const *const *files = unit_service->files;
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

	if (fstab != NULL && !clone_newns)
		return EINVAL;

	if (NULL == env_whitelist)
		env_whitelist = default_envvars;

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

	char prio_str[] = "XXXXXXXXXXXXXXXXX";
	sprintf(prio_str, "%i", priority);

	pid_t ppid = getppid();

	char **envvars;
	{
		char **xx;
		errnum = filter_envvars(&xx, env_whitelist);
		if (errnum != 0)
			return errnum;
		envvars = xx;
	}

	char *root_setting;
	{
		char *xx;
		if (-1 == asprintf(&xx, "MANAGERPID=%i", ppid))
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
		if (-1 == asprintf(&xx, "LINTED_SERVICE=%s", service_name))
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
	envvars[envvars_size + 2U] = NULL;

	size_t files_size = 0U;
	if (files != NULL)
		files_size = null_list_size(files);

	size_t exec_start_size =
	    null_list_size((char const * const *)exec_start);

	struct option options[] = {
		{ true, "--traceme", NULL },
		{ waiter != NULL, "--waiter", waiter },
		{ fstab != NULL, "--chrootdir", chrootdir },
		{ fstab != NULL, "--fstab", fstab },
		{ no_new_privs, "--nonewprivs", NULL },
		{ drop_caps, "--dropcaps", NULL },
		{ chdir_path != NULL, "--chdir", chdir_path },
		{ prio_str != NULL, "--priority", prio_str },
		{ clone_newuser, "--clone-newuser", NULL },
		{ clone_newpid, "--clone-newpid", NULL },
		{ clone_newipc, "--clone-newipc", NULL },
		{ clone_newnet, "--clone-newnet", NULL },
		{ clone_newns, "--clone-newns", NULL },
		{ clone_newuts, "--clone-newuts", NULL }
	};

	size_t num_options = 0U;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(options); ++ii) {
		struct option option = options[ii];
		if (!option.flag)
			continue;

		++num_options;
		if (option.value != NULL)
			++num_options;
	}

	char const **args;
	size_t args_size = 1U + num_options + 1U + exec_start_size;
	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, args_size + 1U,
		                                sizeof exec_start[0U]);
		if (errnum != 0)
			goto free_envvars;
		args = xx;
	}
	args[0U] = sandbox;

	size_t ix = 1U;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(options); ++ii) {
		struct option option = options[ii];
		if (!option.flag)
			continue;

		args[ix++] = option.name;
		if (option.value != NULL)
			args[ix++] = option.value;
	}

	args[1U + num_options] = "--";
	for (size_t ii = 0U; ii < exec_start_size; ++ii)
		args[1U + num_options + 1U + ii] = exec_start[ii];
	args[args_size] = NULL;

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

	linted_ko *proc_kos = NULL;
	size_t kos_opened = 0U;

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
		unsigned long flags = pair->flags;

		linted_ko copy_ko;
		{
			linted_ko xx;
			errnum = linted_ko_reopen(&xx, ko, flags);
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
				    &xx, (char * const *)file_flags, &yy);
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
		    linted_unit_db_get_unit_by_name(unit_db, filename);
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

	errnum = linted_spawn(NULL, cwd, args[0U], file_actions, attr, args,
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

free_envvars:
	for (char **envp = envvars; *envp != NULL; ++envp)
		linted_mem_free(*envp);
	linted_mem_free(envvars);

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

	struct linted_asynch_task_waitid *sandbox_task =
	    linted_asynch_task_waitid_from_asynch(task);
	struct wait_service_data *wait_service_data =
	    linted_asynch_task_waitid_data(sandbox_task);
	struct linted_asynch_pool *pool = wait_service_data->pool;

	linted_ko cwd = wait_service_data->cwd;
	char const *chrootdir = wait_service_data->chrootdir;
	struct linted_unit_db *unit_db = wait_service_data->unit_db;
	bool time_to_exit = *wait_service_data->time_to_exit;

	pid_t pid;
	int exit_status;
	int exit_code;
	{
		siginfo_t exit_info;
		linted_asynch_task_waitid_info(sandbox_task, &exit_info);
		pid = exit_info.si_pid;
		exit_status = exit_info.si_status;
		exit_code = exit_info.si_code;
	}

	switch (exit_code) {
	case CLD_DUMPED:
	case CLD_KILLED:
	case CLD_EXITED:
		errnum = on_child_exited(pid, exit_status);
		break;

	case CLD_TRAPPED:
		errnum = on_child_trapped(time_to_exit, pid, exit_status, cwd,
		                          unit_db, chrootdir);
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

	struct linted_asynch_task_sigwaitinfo *sigwait_task =
	    linted_asynch_task_sigwaitinfo_from_asynch(task);
	struct sigwait_data *sigwait_data =
	    linted_asynch_task_sigwaitinfo_data(sigwait_task);
	bool *time_to_exit = sigwait_data->time_to_exit;
	struct linted_unit_db *unit_db = sigwait_data->unit_db;

	for (size_t ii = 0U, size = linted_unit_db_size(unit_db); ii < size;
	     ++ii) {
		struct linted_unit *unit = linted_unit_db_get_unit(unit_db, ii);

		if (unit->type != UNIT_TYPE_SERVICE)
			continue;

		pid_t pid;
		{
			pid_t xx;
			linted_error pid_errnum =
			    pid_of_service(&xx, unit->name);
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
				assert(false);
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

	linted_admin_task_recv_request_prepare(
	    conn->read_task, ADMIN_READ_CONNECTION, new_socket);
	conn->read_data.pool = pool;
	conn->read_data.conn = conn;

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

static linted_error on_child_exited(pid_t pid, int exit_status)
{
	return 0;
}

static linted_error on_child_trapped(bool time_to_exit, pid_t pid,
                                     int exit_status, linted_ko cwd,
                                     struct linted_unit_db *unit_db,
                                     char const *chrootdir)
{
	int event = exit_status >> 8U;
	switch (event) {
	case 0:
		return on_child_signaled(pid, exit_status);

	case PTRACE_EVENT_EXIT:
		return on_child_about_to_exit(time_to_exit, pid, cwd, unit_db,
		                              chrootdir);

	case PTRACE_EVENT_VFORK:
	case PTRACE_EVENT_FORK:
	case PTRACE_EVENT_CLONE:
		return on_child_about_to_clone(pid);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_child_signaled(pid_t pid, int signo)
{
	linted_error errnum = 0;

	switch (signo) {
	default:
		goto restart_process;

	case SIGSTOP: {
		signo = 0;

		bool is;
		{
			bool xx;
			errnum = pid_is_child_of(getpid(), pid, &xx);
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

		char service_name[COMM_MAX + 16U];

		errnum = pid_comm(pid, service_name);
		if (errnum != 0)
			return errnum;

		fprintf(stderr, "ptracing service %s: %i\n", service_name, pid);

		errnum = ptrace_setoptions(pid, PTRACE_O_TRACEEXIT);
		break;
	}

	case SIGTRAP:
		signo = 0;
		break;

	case SIGHUP:
	case SIGINT:
	case SIGQUIT:
	case SIGTERM:
		/* Propagate to the rest of the sandbox */
		errnum = kill_pid_children(pid, signo);
		break;

	case SIGCHLD: {
		int code;
		pid_t sandboxed_pid;
		int status;

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

		switch (code) {
		case CLD_EXITED:
			fprintf(stderr,
			        "process %i in sandbox %i exited with %i\n",
			        sandboxed_pid, pid, status);
			break;

		case CLD_DUMPED:
		case CLD_KILLED:
			fprintf(stderr,
			        "process %i in sandbox %i killed by %s\n",
			        sandboxed_pid, pid, strsignal(status));
			break;

		case CLD_STOPPED:
			fprintf(stderr,
			        "process %i in sandbox %i stopped by %s\n",
			        sandboxed_pid, pid, strsignal(status));
			break;

		case CLD_CONTINUED:
			fprintf(stderr,
			        "process %i in sandbox %i continued by %s\n",
			        sandboxed_pid, pid, strsignal(status));
			break;

		default:
			LINTED_ASSUME_UNREACHABLE();
		}
		break;
	}
	}

restart_process:
	;
	linted_error cont_errnum = ptrace_cont(pid, signo);
	if (0 == errnum)
		errnum = cont_errnum;

	return errnum;
}

static linted_error on_child_about_to_exit(bool time_to_exit, pid_t pid,
                                           linted_ko cwd,
                                           struct linted_unit_db *unit_db,
                                           char const *chrootdir)
{

	linted_error errnum = 0;
	struct linted_unit *unit = NULL;

	errnum = kill_pid_children(pid, SIGKILL);
	if (errnum != 0)
		goto detach_from_process;

	bool is;
	{
		bool xx;
		errnum = pid_is_child_of(getpid(), pid, &xx);
		if (errnum != 0)
			goto detach_from_process;
		is = xx;
	}
	if (is)
		goto detach_from_process;

	unsigned long status;
	{
		unsigned long xx;
		errnum = ptrace_geteventmsg(pid, &xx);
		if (errnum != 0)
			goto detach_from_process;
		status = xx;
	}

	char service_name[COMM_MAX + 1U];

	errnum = pid_comm(pid, service_name);
	if (errnum != 0)
		goto detach_from_process;

	if (WIFEXITED(status)) {
		int exit_status = WEXITSTATUS(status);

		fprintf(stderr, "%s %i exited with %i\n", service_name, pid,
		        exit_status);

	} else if (WIFSIGNALED(status)) {
		int signo = WTERMSIG(status);

		fprintf(stderr, "%s %i killed by %s\n", service_name, pid,
		        strsignal(signo));
	} else {
		LINTED_ASSUME_UNREACHABLE();
	}

	unit = linted_unit_db_get_unit_by_name(unit_db, service_name);

detach_from_process:
	errnum = ptrace_detach(pid, 0);
	if (errnum != 0)
		return errnum;

	if (time_to_exit)
		return 0;

	if (NULL == unit)
		return 0;

	errnum = service_activate(unit, cwd, chrootdir, unit_db);

	return errnum;
}

/* A sandbox creator is creating a sandbox */
static linted_error on_child_about_to_clone(pid_t pid)
{
	linted_error errnum = 0;

	linted_error detach_errnum = ptrace_detach(pid, 0);
	if (0 == errnum)
		errnum = detach_errnum;

	return errnum;
}

static linted_error on_status_request(union linted_admin_request const *request,
                                      union linted_admin_reply *reply)
{
	linted_error errnum = 0;
	bool is_up;

	pid_t pid;
	{
		pid_t xx;
		errnum = pid_of_service(&xx, request->status.name);
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
		errnum = pid_of_service(&xx, request->status.name);
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

static linted_error pid_of_service(pid_t *pidp, char const *name)
{
	linted_error errnum = 0;

	linted_ko init_children;
	{
		linted_ko xx;
		errnum = pid_children(&xx, getppid());
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

	pid_t pid;
	for (;;) {
		{
			char *xx = buf;
			size_t yy = buf_size;

			errno = 0;
			ssize_t zz = getdelim(&xx, &yy, ' ', file);
			if (-1 == zz)
				goto getdelim_failed;
			buf = xx;
			buf_size = yy;
			goto getdelim_succeeded;
		}
	getdelim_failed:
		errnum = errno;
		if (0 == errnum)
			errnum = ESRCH;
		goto free_buf;

	getdelim_succeeded:
		;
		pid_t child = strtol(buf, NULL, 10);

		char service_name[COMM_MAX + 16U];

		errnum = pid_comm(child, service_name);
		if (errnum != 0)
			return errnum;

		if (0 == strcmp(service_name, name)) {
			errnum = 0;
			pid = child;
			break;
		}
	}
	*pidp = pid;

free_buf:
	linted_mem_free(buf);

	fclose(file);

	return errnum;
}

static linted_error kill_pid_children(pid_t pid, int signo)
{
	linted_error errnum = 0;

	linted_ko children;
	{
		linted_ko xx;
		errnum = pid_children(&xx, pid);
		if (errnum != 0)
			return errnum;
		children = xx;
	}

	FILE *file = fdopen(children, "r");
	if (NULL == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(children);

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
			if (-1 == zz)
				goto getdelim_failed;
			buf = xx;
			buf_size = yy;
			goto getdelim_succeeded;
		}
	getdelim_failed:
		errnum = errno;
		goto free_buf;

	getdelim_succeeded:
		;
		pid_t child = strtol(buf, NULL, 10);

		if (-1 == kill(child, signo)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_buf;
		}
	}

free_buf:
	linted_mem_free(buf);

	if (EOF == fclose(file)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

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

	if (NULL == str)
		return EINVAL;

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

	if (NULL == str)
		return EINVAL;

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

static linted_error pid_children(linted_ko *kop, pid_t pid)
{
	linted_error errnum;
	{
		char path[] = "/proc/XXXXXXXXXXXXXXXX/task/XXXXXXXXXXXXXXXXX";
		sprintf(path, "/proc/%i/task/%i/children", pid, pid);
		errnum =
		    linted_ko_open(kop, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
	}
	if (ENOENT == errnum)
		return ESRCH;
	return errnum;
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
	struct pidstat buf;

	errnum = pid_stat(child, &buf);
	if (errnum != 0)
		return errnum;

	*isp = buf.ppid == parent;

	return errnum;
}

static linted_error pid_comm(pid_t pid, char comm[static COMM_MAX + 1U])
{
	linted_error errnum;

	linted_ko ko;
	{
		linted_ko xx;
		char path[] = "/proc/XXXXXXXXXXXXXXXX";
		sprintf(path, "/proc/%i/comm", pid);
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		ko = xx;
	}
	if (ENOENT == errnum)
		return ESRCH;
	if (errnum != 0)
		return errnum;

	FILE *file = fdopen(ko, "r");
	if (NULL == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(ko);

		return errnum;
	}

	int nr = fscanf(file, "%16s", comm);
	if (EOF == nr) {
		errnum = errno;
		goto close_file;
	}
	if (nr < 1) {
		errnum = ENOSYS;
		goto close_file;
	}

close_file:
	if (EOF == fclose(file)) {
		if (0 == errnum)
			errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	return errnum;
}

static linted_error pid_stat(pid_t pid, struct pidstat *buf)
{
	linted_error errnum = 0;

	linted_ko stat_ko;
	{
		linted_ko xx;
		char path[] = "/proc/XXXXXXXXXXXXXXXX/stat";
		sprintf(path, "/proc/%i/stat", pid);
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (errnum != 0)
			return errnum;
		stat_ko = xx;
	}

	FILE *file = fdopen(stat_ko, "r");
	if (NULL == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(stat_ko);

		return errnum;
	}

	memset(buf, 0, sizeof *buf);

	int num_match = fscanf(
	    file, "%d "   // pid
	          "%s "   // comm
	          "%c "   // state
	          "%d "   // ppid
	          "%d "   // pgrp
	          "%d "   // session
	          "%d "   // tty_nr
	          "%d "   // tpgid
	          "%u "   // flags
	          "%lu "  // minflt
	          "%lu "  // cminflt
	          "%lu "  // majflt
	          "%lu "  // cmajflt
	          "%lu "  // utime
	          "%lu "  // stime
	          "%ld "  // cutime
	          "%ld "  // cstime
	          "%ld "  // priority
	          "%ld "  // nice
	          "%ld "  // num_threads
	          "%ld "  // itrealvalue
	          "%llu " // starttime
	          "%lu "  // vsize
	          "%ld "  // rss
	          "%lu "  // rsslim
	          "%lu "  // startcode
	          "%lu "  // endcode
	          "%lu "  // startstack
	          "%lu "  // kstkesp
	          "%lu "  // kstkeip
	          "%lu "  // signal
	          "%lu "  // blocked
	          "%lu "  // sigignore
	          "%lu "  // sigcatch
	          "%lu "  // wchan
	          "%lu "  // nswap
	          "%lu "  // cnswap
	          "%d "   // exit_signal
	          "%d "   // processor
	          "%u "   // rt_priority
	          "%u "   // policy
	          "%llu " // delayacct_blkio_ticks
	          "%lu "  // guest_time
	          "%ld"   // cguest_time
	    ,
	    &buf->pid, buf->comm, &buf->state, &buf->ppid, &buf->pgrp,
	    &buf->session, &buf->tty_nr, &buf->tpgid, &buf->flags, &buf->minflt,
	    &buf->cminflt, &buf->majflt, &buf->cmajflt, &buf->utime,
	    &buf->stime, &buf->cutime, &buf->cstime, &buf->priority, &buf->nice,
	    &buf->num_threads, &buf->itrealvalue, &buf->starttime, &buf->vsize,
	    &buf->rss, &buf->rsslim, &buf->startcode, &buf->endcode,
	    &buf->startstack, &buf->kstkesp, &buf->kstkeip, &buf->signal,
	    &buf->blocked, &buf->sigignore, &buf->sigcatch, &buf->wchan,
	    &buf->nswap, &buf->cnswap, &buf->exit_signal, &buf->processor,
	    &buf->rt_priority, &buf->policy, &buf->delayacct_blkio_ticks,
	    &buf->guest_time, &buf->cguest_time);
	if (EOF == num_match) {
		errnum = errno;
		if (0 == errnum)
			errnum = ENOSYS;
		goto close_file;
	}

close_file:
	if (EOF == fclose(file))
		return errno;

	return errnum;
}

static linted_error ptrace_detach(pid_t pid, int signo)
{
	linted_error errnum;

	if (-1 ==
	    ptrace(PTRACE_DETACH, pid, (void *)NULL, (void *)(intptr_t)signo)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

static linted_error ptrace_setoptions(pid_t pid, unsigned options)
{
	linted_error errnum;

	if (-1 == ptrace(PTRACE_SETOPTIONS, pid, (void *)NULL,
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

	if (-1 == ptrace(PTRACE_GETEVENTMSG, pid, (void *)NULL, (void *)msg)) {
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
	    ptrace(PTRACE_CONT, pid, (void *)NULL, (void *)(intptr_t)signo)) {
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
