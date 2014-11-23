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
#include <mntent.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/ptrace.h>
#include <linux/seccomp.h>

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
	pid_t parent_process;
	linted_ko cwd;
	char const *chrootdir;
	char const *sandbox;
	sigset_t const *orig_mask;
	struct linted_unit_db *unit_db;
	bool *time_to_exit;
};

struct sigwait_data
{
	bool *time_to_exit;
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

static linted_error create_units(struct linted_unit_db **unitsp,
                                 struct linted_conf_db *conf_db);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

static linted_error activate_units(struct linted_unit_db *units, linted_ko cwd,
                                   char const *chrootdir, char const *sandbox,
                                   sigset_t const *orig_mask);

static linted_error dispatch(struct linted_asynch_task *completed_task,
                             bool time_to_quit);

static linted_error on_process_wait(struct linted_asynch_task *task,
                                    bool time_to_quit);
static linted_error on_sigwaitinfo(struct linted_asynch_task *task,
                                   bool time_to_quit);
static linted_error on_accepted_conn(struct linted_asynch_task *task,
                                     bool time_to_quit);
static linted_error on_read_conn(struct linted_asynch_task *task,
                                 bool time_to_quit);
static linted_error on_wrote_conn(struct linted_asynch_task *task,
                                  bool time_to_quit);
static linted_error on_status_request(union linted_admin_request const *request,
                                      union linted_admin_reply *reply);
static linted_error on_stop_request(union linted_admin_request const *request,
                                    union linted_admin_reply *reply);

static linted_error on_process_trap(pid_t ppid, pid_t pid, int exit_status);
static linted_error on_process_sigtrap(pid_t pid, int exit_status);
static linted_error on_event_stop(pid_t pid, int exit_status);

static linted_error socket_activate(struct linted_unit_socket *unit);
static linted_error service_activate(struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir, char const *sandbox,
                                     sigset_t const *orig_mask,
                                     struct linted_unit_db *units);

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

static linted_error pid_for_service_name(pid_t *pidp, char const *name);
static linted_error get_process_service_name(pid_t pid, char **service_namep);
static linted_error get_process_comm(linted_ko *kop, pid_t pid);
static linted_error get_process_children(linted_ko *kop, pid_t pid);

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);
static linted_error ptrace_cont(pid_t pid, int signo);
static linted_error ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo);

static linted_error conn_pool_create(struct conn_pool **poolp);
static void conn_pool_destroy(struct conn_pool *pool);

static linted_error conn_insert(struct conn_pool *pool, struct conn **connp,
                                linted_ko ko);
static void conn_discard(struct conn *conn);

static linted_ko kos[1U];
static struct sock_fprog const default_filter;

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-monitor",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos
};

static void ignore(int signo)
{
}

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum;

	pid_t ppid = getppid();
	linted_dir private_run_dir = kos[0U];

	sigset_t orig_mask;

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

	{
		struct sigaction action = { 0 };
		action.sa_handler = ignore;
		action.sa_flags = SA_RESTART | SA_NODEFER | SA_NOCLDSTOP;
		sigemptyset(&action.sa_mask);
		sigaction(SIGCHLD, &action, NULL);
	}

	char const *chrootdir = getenv("LINTED_CHROOT");
	char const *unit_path = getenv("LINTED_UNIT_PATH");
	char const *sandbox = getenv("LINTED_SANDBOX");

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

	linted_asynch_task_sigwaitinfo_prepare(sigwait_task, SIGWAITINFO,
	                                       &exit_signals);
	sigwait_data.time_to_exit = &time_to_exit;

	linted_asynch_pool_submit(
	    pool, linted_asynch_task_sigwaitinfo_to_asynch(sigwait_task));

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

	linted_admin_task_accept_prepare(accepted_conn_task,
	                                 ADMIN_ACCEPTED_CONNECTION, admin);
	accepted_conn_data.pool = pool;
	accepted_conn_data.conn_pool = conn_pool;

	linted_asynch_pool_submit(
	    pool, linted_admin_task_accept_to_asynch(accepted_conn_task));

	for (;;) {
		errnum = ptrace_seize(ppid, 0U);
		if (errnum != EPERM)
			break;

		sched_yield();
	}
	if (errnum != 0)
		goto destroy_confs;

	/**
	 * @todo Warn about unactivated units.
	 */
	errnum = activate_units(units, cwd, chrootdir, sandbox, &orig_mask);
	if (errnum != 0)
		goto kill_procs;

	linted_asynch_task_waitid_prepare(sandbox_task, WAITID, P_ALL, -1,
	                                  WEXITED | WNOWAIT);
	sandbox_data.pool = pool;
	sandbox_data.parent_process = ppid;
	sandbox_data.cwd = cwd;
	sandbox_data.chrootdir = chrootdir;
	sandbox_data.sandbox = sandbox;
	sandbox_data.unit_db = units;
	sandbox_data.orig_mask = &orig_mask;
	sandbox_data.time_to_exit = &time_to_exit;

	linted_asynch_pool_submit(
	    pool, linted_asynch_task_waitid_to_asynch(sandbox_task));

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
	} while (!time_to_exit);

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

		linted_error dispatch_error = dispatch(completed_task, true);
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

static linted_error activate_units(struct linted_unit_db *units, linted_ko cwd,
                                   char const *chrootdir, char const *sandbox,
                                   sigset_t const *orig_mask)
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

		errnum = service_activate(unit, cwd, chrootdir, sandbox,
		                          orig_mask, units);
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
	unsigned long options;
	linted_ko ko;
};

static char const *const file_options[] = {[RDONLY] = "rdonly",
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
                                     char const *chrootdir, char const *sandbox,
                                     sigset_t const *orig_mask,
                                     struct linted_unit_db *units)
{
	linted_error errnum = 0;

	char const *service_name = unit->name;

	struct linted_unit_service *unit_service = (void *)unit;

	pid_t child;
	{
		pid_t xx;
		errnum = pid_for_service_name(&xx, service_name);
		if (errnum != 0)
			goto service_not_found;
		child = xx;
	}
	unit_service->pid = child;
	goto ptrace_child;

service_not_found:
	if (errnum != ESRCH)
		return errnum;

	char const *const *exec_start = unit_service->exec_start;
	bool no_new_privs = unit_service->no_new_privs;
	char const *const *files = unit_service->files;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *env_whitelist = unit_service->env_whitelist;

	bool clone_newuser = unit_service->clone_newuser;
	bool clone_newpid = unit_service->clone_newpid;
	bool clone_newipc = unit_service->clone_newipc;
	bool clone_newnet = unit_service->clone_newnet;
	bool clone_newns = unit_service->clone_newns;

	if (fstab != NULL && !clone_newns)
		return EINVAL;

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

	pid_t ppid = getppid();
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

	size_t exec_start_size =
	    null_list_size((char const * const *)exec_start);
	size_t args_size = exec_start_size + 2U;
	char const **args;
	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, args_size + 1U,
		                                sizeof exec_start[0U]);
		if (errnum != 0)
			goto free_envvars;
		args = xx;
	}
	args[0U] = sandbox;
	args[1U] = "--";
	for (size_t ii = 0U; ii < exec_start_size; ++ii)
		args[ii + 2U] = exec_start[ii];
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

	int clone_flags = 0;
	if (clone_newuser)
		clone_flags |= CLONE_NEWUSER;
	if (clone_newpid)
		clone_flags |= CLONE_NEWPID;
	if (clone_newipc)
		clone_flags |= CLONE_NEWIPC;
	if (clone_newnet)
		clone_flags |= CLONE_NEWNET;
	if (clone_newns)
		clone_flags |= CLONE_NEWNS;

	/* Favor other processes over this process hierarchy.  Only
	 * superuser may lower priorities so this is not
	 * stoppable. This also makes the process hierarchy nicer for
	 * the OOM killer.
	 */
	errno = 0;
	int priority = getpriority(PRIO_PROCESS, 0);
	if (-1 == priority) {
		errnum = errno;
		if (errnum != 0)
			goto destroy_attr;
	}

	linted_spawn_attr_setmask(attr, orig_mask);
	linted_spawn_attr_setpriority(attr, priority + 1);
	linted_spawn_attr_setfilter(attr, &default_filter);
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

	{
		pid_t xx;
		errnum = linted_spawn(&xx, cwd, args[0U], file_actions, attr,
		                      args, (char const * const *)envvars);
		if (errnum != 0)
			goto destroy_proc_kos;
		child = xx;
	}

	unit_service->pid = child;

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

ptrace_child:
	if (errnum != 0)
		return errnum;

	fprintf(stderr, "ptracing service %s: %i\n", service_name, child);

	return ptrace_seize(child, 0);
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

static linted_error dispatch(struct linted_asynch_task *task, bool time_to_quit)
{
	switch (linted_asynch_task_action(task)) {
	case WAITID:
		return on_process_wait(task, time_to_quit);

	case SIGWAITINFO:
		return on_sigwaitinfo(task, time_to_quit);

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
	linted_error errnum = 0;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	if (time_to_quit)
		return 0;

	struct linted_asynch_task_waitid *sandbox_task =
	    linted_asynch_task_waitid_from_asynch(task);
	struct wait_service_data *wait_service_data =
	    linted_asynch_task_waitid_data(sandbox_task);
	struct linted_asynch_pool *pool = wait_service_data->pool;
	pid_t ppid = wait_service_data->parent_process;

	linted_ko cwd = wait_service_data->cwd;
	char const *chrootdir = wait_service_data->chrootdir;
	char const *sandbox = wait_service_data->sandbox;
	sigset_t const *orig_mask = wait_service_data->orig_mask;
	struct linted_unit_db *unit_db = wait_service_data->unit_db;

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

	bool was_signal = false;
	switch (exit_code) {
	case CLD_DUMPED:
	case CLD_KILLED:
		was_signal = true;

	case CLD_EXITED: {
		char *service_name;
		{
			char *xx;
			errnum = get_process_service_name(pid, &xx);
			if (errnum != 0)
				return errnum;
			service_name = xx;
		}

		if (was_signal) {
			fprintf(stderr, "%s %i killed due to signal %s\n",
			        service_name, pid, strsignal(exit_status));
		} else {
			fprintf(stderr, "%s %i exited with %i\n", service_name,
			        pid, exit_status);
		}

		struct linted_unit *unit =
		    linted_unit_db_get_unit_by_name(unit_db, service_name);

		linted_mem_free(service_name);

		/* This has to be done first so that service_activate
		 * isn't stupid and thinks that the zombie is still
		 * living. */
		do {
			int wait_status;
			{
				siginfo_t info;
				wait_status =
				    waitid(P_PID, pid, &info, WEXITED);
			}
			if (-1 == wait_status) {
				errnum = errno;
				LINTED_ASSUME(errnum != 0);
			} else {
				errnum = 0;
			}
		} while (EINTR == errnum);
		if (errnum != 0)
			return errnum;

		errnum = service_activate(unit, cwd, chrootdir, sandbox,
		                          orig_mask, unit_db);
		break;
	}

	case CLD_TRAPPED:
		errnum = on_process_trap(ppid, pid, exit_status);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	linted_asynch_pool_submit(pool, task);

	return errnum;
}

static linted_error on_sigwaitinfo(struct linted_asynch_task *task,
                                   bool time_to_quit)
{
	linted_error errnum = 0;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	if (time_to_quit)
		return 0;

	struct linted_asynch_task_sigwaitinfo *sigwait_task =
	    linted_asynch_task_sigwaitinfo_from_asynch(task);
	struct sigwait_data *sigwait_data =
	    linted_asynch_task_sigwaitinfo_data(sigwait_task);
	bool *time_to_exit = sigwait_data->time_to_exit;

	*time_to_exit = true;

	return 0;
}

static linted_error on_process_trap(pid_t ppid, pid_t pid, int exit_status)
{
	int event = exit_status >> 8U;
	switch (event) {
	case 0:
		return on_process_sigtrap(pid, exit_status);

	/* Even if we never use PTRACE_O_TRACECLONE,
	 * PTRACE_O_TRACEFORK, PTRACE_O_TRACEVFORK, or
	 * PTRACE_INTERRUPT we still get this event when a ptraced
	 * process is sent SIGCONT.
	 */
	case PTRACE_EVENT_STOP:
		return on_event_stop(pid, exit_status);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_sigtrap(pid_t pid, int exit_status)
{
	linted_error errnum = 0;

	do {
		int wait_status;
		{
			siginfo_t info;
			wait_status = waitid(P_PID, pid, &info, WEXITED);
		}
		if (-1 == wait_status) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		} else {
			errnum = 0;
		}
	} while (EINTR == errnum);

	if (exit_status != SIGCHLD)
		goto restart_process;

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
		fprintf(stderr, "process %i in sandbox %i exited with %i\n",
		        sandboxed_pid, pid, status);
		break;

	case CLD_DUMPED:
	case CLD_KILLED:
		fprintf(stderr, "process %i in sandbox %i killed by %s\n",
		        sandboxed_pid, pid, strsignal(status));
		break;

	case CLD_STOPPED:
		fprintf(stderr, "process %i in sandbox %i stopped by %s\n",
		        sandboxed_pid, pid, strsignal(status));
		break;

	case CLD_CONTINUED:
		fprintf(stderr, "process %i in sandbox %i continued by %s\n",
		        sandboxed_pid, pid, strsignal(status));
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

restart_process:
	;
	linted_error cont_errnum = ptrace_cont(pid, exit_status);
	if (0 == errnum)
		errnum = cont_errnum;
	return errnum;
}

static linted_error on_event_stop(pid_t pid, int exit_status)
{
	linted_error errnum = 0;

	do {
		int wait_status;
		{
			siginfo_t info;
			wait_status = waitid(P_PID, pid, &info, WEXITED);
		}
		if (-1 == wait_status) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		} else {
			errnum = 0;
		}
	} while (EINTR == errnum);

	linted_error cont_errnum = ptrace_cont(pid, exit_status);
	if (0 == errnum)
		errnum = cont_errnum;
	return errnum;
}

static linted_error on_accepted_conn(struct linted_asynch_task *task,
                                     bool time_to_quit)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
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

	if (time_to_quit)
		goto close_new_socket;

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

static linted_error on_read_conn(struct linted_asynch_task *task,
                                 bool time_to_quit)
{
	linted_error errnum;

	struct linted_admin_task_recv_request *read_conn_task =
	    linted_admin_task_recv_request_from_asynch(task);
	struct read_conn_data *read_conn_data =
	    linted_admin_task_recv_request_data(read_conn_task);

	struct linted_asynch_pool *pool = read_conn_data->pool;
	struct conn *conn = read_conn_data->conn;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		goto conn_remove;

	if (time_to_quit)
		return 0;

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
		return errnum;
	}

	linted_admin_task_send_reply_prepare(
	    conn->write_task, ADMIN_WROTE_CONNECTION, ko, &reply);
	conn->write_data.pool = pool;
	conn->write_data.conn = conn;

	linted_asynch_pool_submit(
	    pool, linted_admin_task_send_reply_to_asynch(conn->write_task));
	return 0;
}

static linted_error on_status_request(union linted_admin_request const *request,
                                      union linted_admin_reply *reply)
{
	linted_error errnum = 0;
	bool is_up;

	pid_t pid;
	{
		pid_t xx;
		errnum = pid_for_service_name(&xx, request->status.name);
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
		errnum = pid_for_service_name(&xx, request->status.name);
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

static linted_error on_wrote_conn(struct linted_asynch_task *task,
                                  bool time_to_quit)
{
	struct linted_admin_task_send_reply *wrote_conn_task =
	    linted_admin_task_send_reply_from_asynch(task);
	struct wrote_conn_data *wrote_conn_data =
	    linted_admin_task_send_reply_data(wrote_conn_task);

	struct conn *conn = wrote_conn_data->conn;

	linted_error errnum = linted_asynch_task_errnum(task);
	;

	conn_discard(conn);

	return errnum;
}

static linted_error get_process_service_name(pid_t pid, char **service_namep)
{
	linted_error errnum;

	linted_ko env;
	{
		linted_ko xx;
		errnum = get_process_comm(&xx, pid);
		if (errnum != 0)
			return errnum;
		env = xx;
	}

	FILE *file = fdopen(env, "r");
	if (NULL == file) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);

		linted_ko_close(env);

		return errnum;
	}

	ssize_t bytes;
	char *buf = NULL;
	{
		char *xx = NULL;
		size_t yy = 0U;
		errno = 0;
		bytes = getline(&xx, &yy, file);
		if (-1 == bytes)
			goto getline_failed;
		buf = xx;
		goto getline_succeeded;
	}
getline_failed:
	errnum = errno;
	if (errnum != 0)
		goto close_file;
	bytes = 1U;

getline_succeeded:
	/* Truncate out the newline character and handle a NULL buf */
	{
		void *xx;
		errnum = linted_mem_realloc(&xx, buf, bytes);
		if (errnum != 0)
			goto close_file;
		buf = xx;
	}

	buf[bytes - 1U] = '\0';

close_file:
	if (EOF == fclose(file)) {
		if (0 == errnum)
			errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	if (errnum != 0) {
		linted_mem_free(buf);
		return errnum;
	}

	*service_namep = buf;

	return errnum;
}

static linted_error pid_for_service_name(pid_t *pidp, char const *name)
{
	linted_error errnum = 0;

	linted_ko init_children;
	{
		linted_ko xx;
		errnum = get_process_children(&xx, getppid());
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

		char *service_name;
		{
			char *xx;
			errnum = get_process_service_name(child, &xx);
			if (ESRCH == errnum)
				continue;
			if (errnum != 0)
				goto free_buf;
			service_name = xx;
		}

		if (0 == strcmp(service_name, name)) {
			pid = child;
			break;
		}

		linted_mem_free(service_name);
	}

	*pidp = pid;

free_buf:
	linted_mem_free(buf);

	fclose(file);

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

static linted_error get_process_comm(linted_ko *kop, pid_t pid)
{
	linted_error errnum;
	{
		char path[] = "/proc/XXXXXXXXXXXXXXXX";
		sprintf(path, "/proc/%i/comm", pid);
		errnum =
		    linted_ko_open(kop, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
	}
	if (ENOENT == errnum)
		return ESRCH;
	return errnum;
}

static linted_error get_process_children(linted_ko *kop, pid_t pid)
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

#if defined __amd64__
#include "monitor-amd64.c"
#elif defined __i386__
#include "monitor-i386.c"
#else
#error No default seccomp filter has been defined for this architecture
#endif
