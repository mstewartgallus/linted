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
#include <string.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <linux/ptrace.h>

#define BACKLOG 20U

enum {
	WAITID,
	SIGWAITINFO,
	MAX_TASKS
};

struct wait_service_data
{
	char const *process_name;
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

#define SERVICE_NAME_MAX 32U

#define COMM_MAX 16U
struct pidstat
{
	pid_t pid;
	char comm[1U + COMM_MAX + 1U + 1U];
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
                                     linted_ko cwd, char const *chrootdir,
                                     char const *sandbox, char const *waiter,
                                     sigset_t const *orig_mask);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_sigwaitinfo(struct linted_asynch_task *task);

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, pid_t pid,
                                     int exit_status, linted_ko cwd,
                                     struct linted_unit_db *unit_db,
                                     char const *chrootdir);
static linted_error on_child_signaled(char const *process_name, pid_t pid,
                                      int signo);
static linted_error on_child_about_to_clone(pid_t pid);
static linted_error on_child_about_to_exit(char const *process_name,
                                           bool time_to_exit, pid_t pid,
                                           linted_ko cwd,
                                           struct linted_unit_db *unit_db,
                                           char const *chrootdir);
static linted_error socket_activate(struct linted_unit_socket *unit);
static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir,
                                     struct linted_unit_db *unit_db,
                                     bool check);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error str_from_strs(char const *const *strs, char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);
static linted_error long_from_cstring(char const *str, long *longp);

static linted_error kill_pid_children(pid_t pid, int signo);

static linted_error service_name(pid_t pid,
                                 char name[static SERVICE_NAME_MAX + 1U]);

static linted_error pid_of_service(pid_t *pidp, char const *name);
static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp);

static linted_error pid_stat(pid_t pid, struct pidstat *buf);

static linted_error pid_children(pid_t pid, char **childrenp);

static linted_error ptrace_seize(pid_t pid, uint_fast32_t options);
static linted_error ptrace_cont(pid_t pid, int signo);
static linted_error ptrace_detach(pid_t pid, int signo);
static linted_error ptrace_setoptions(pid_t pid, unsigned options);
static linted_error ptrace_geteventmsg(pid_t pid, unsigned long *msg);
static linted_error ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo);

static linted_error set_death_sig(int signum);

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

	struct linted_asynch_task_waitid *sandbox_task;
	struct linted_asynch_task_sigwaitinfo *sigwait_task;

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

	struct linted_conf_db *conf_db;
	{
		struct linted_conf_db *xx;
		errnum = linted_conf_db_create_from_path(&xx, unit_path);
		if (errnum != 0)
			goto drain_asynch_pool;
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

	/**
	 * @todo Warn about unactivated unit_db.
	 */
	errnum = activate_unit_db(process_name, unit_db, cwd, chrootdir,
	                          sandbox, waiter, &orig_mask);
	if (errnum != 0)
		goto kill_procs;

	linted_asynch_task_waitid_prepare(sandbox_task, WAITID, P_ALL, -1,
	                                  WEXITED);
	sandbox_data.process_name = process_name;
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
}

exit_monitor:
	if (errnum != 0) {
		linted_io_write_format(
		    STDERR_FILENO, NULL, "%s: could not run the game: %s\n",
		    process_name, linted_error_string(errnum));

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
	char const *const *chdir_paths =
	    linted_conf_find(conf, "Service", "WorkingDirectory");
	char const *const *files =
	    linted_conf_find(conf, "Service", "X-LintedFiles");
	char const *const *env_whitelist =
	    linted_conf_find(conf, "Service", "X-LintedEnvironmentWhitelist");

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

	unit->sigmask = orig_mask;
	unit->sandbox = sandbox;
	unit->waiter = waiter;

	unit->exec_start = exec_start;
	unit->files = files;
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

static linted_error activate_unit_db(char const *process_name,
                                     struct linted_unit_db *unit_db,
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

		errnum = service_activate(process_name, unit, cwd, chrootdir,
		                          unit_db, true);
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

static linted_error service_activate(char const *process_name,
                                     struct linted_unit *unit, linted_ko cwd,
                                     char const *chrootdir,
                                     struct linted_unit_db *unit_db, bool check)
{
	linted_error errnum = 0;

	char const *name = unit->name;

	struct linted_unit_service *unit_service = (void *)unit;

	if (!check)
		goto spawn_service;

	pid_t child;
	{
		pid_t xx;
		errnum = pid_of_service(&xx, name);
		if (errnum != 0)
			goto service_not_found;
		child = xx;
	}

	fprintf(stderr, "%s: %s: starting to ptrace\n", process_name, name);

	return ptrace_seize(child, PTRACE_O_TRACEEXIT);

service_not_found:
	if (errnum != ESRCH)
		return errnum;
spawn_service:
	;
	char const *const *exec_start = unit_service->exec_start;
	char const *const *files = unit_service->files;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *env_whitelist = unit_service->env_whitelist;

	sigset_t const *sigmask = unit_service->sigmask;
	char const *sandbox = unit_service->sandbox;
	char const *waiter = unit_service->waiter;

	if (NULL == env_whitelist)
		env_whitelist = default_envvars;

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
	envvars[envvars_size + 2U] = NULL;

	size_t files_size = 0U;
	if (files != NULL)
		files_size = null_list_size(files);

	size_t exec_start_size =
	    null_list_size((char const * const *)exec_start);

	struct option options[] = {
		{ true, "--traceme", NULL },
		{ waiter != NULL, "--waiter", waiter },
		{ chdir_path != NULL, "--chdir", chdir_path }
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

	char const *process_name = wait_service_data->process_name;
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
		/* Do nothing */
		break;

	case CLD_TRAPPED:
		errnum = on_child_trapped(process_name, time_to_exit, pid,
		                          exit_status, cwd, unit_db, chrootdir);
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

static linted_error on_child_trapped(char const *process_name,
                                     bool time_to_exit, pid_t pid,
                                     int exit_status, linted_ko cwd,
                                     struct linted_unit_db *unit_db,
                                     char const *chrootdir)
{
	int event = exit_status >> 8U;
	switch (event) {
	case 0:
		return on_child_signaled(process_name, pid, exit_status);

	case PTRACE_EVENT_EXIT:
		return on_child_about_to_exit(process_name, time_to_exit, pid,
		                              cwd, unit_db, chrootdir);

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
		goto restart_process;

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

		fprintf(stderr, "%s: %s: starting to ptrace\n", process_name,
		        name);

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

		char name[SERVICE_NAME_MAX + 1U];
		errnum = service_name(pid, name);
		if (errnum != 0)
			goto restart_process;

		switch (code) {
		case CLD_EXITED:
			fprintf(stderr, "%s: %s: process %i exited with %i\n",
			        process_name, name, sandboxed_pid, status);
			break;

		case CLD_DUMPED:
		case CLD_KILLED:
			fprintf(stderr, "%s: %s: process %i killed by %s\n",
			        process_name, name, sandboxed_pid,
			        strsignal(status));
			break;

		case CLD_STOPPED:
			fprintf(stderr, "%s: %s: process %i stopped by %s\n",
			        process_name, name, sandboxed_pid,
			        strsignal(status));
			break;

		case CLD_CONTINUED:
			fprintf(stderr, "%s: %s: process %i continued by %s\n",
			        process_name, name, sandboxed_pid,
			        strsignal(status));
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

static linted_error on_child_about_to_exit(char const *process_name,
                                           bool time_to_exit, pid_t pid,
                                           linted_ko cwd,
                                           struct linted_unit_db *unit_db,
                                           char const *chrootdir)
{

	linted_error errnum = 0;
	struct linted_unit *unit = NULL;

	errnum = kill_pid_children(pid, SIGKILL);
	if (errnum != 0)
		goto detach_from_process;

	pid_t self = getpid();
	{
		bool is;
		errnum = pid_is_child_of(self, pid, &is);
		if (errnum != 0)
			goto detach_from_process;
		if (is)
			goto detach_from_process;
	}

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

	if (NULL == unit)
		return errnum;

	errnum = service_activate(process_name, unit, cwd, chrootdir, unit_db,
	                          false);

	return errnum;
}

/* A sandbox creator is creating a sandbox */
static linted_error on_child_about_to_clone(pid_t pid)
{
	return ptrace_detach(pid, 0);
}

static linted_error pid_of_service(pid_t *pidp, char const *name)
{
	linted_error errnum = 0;

	pid_t ppid = getppid();

	char *buf;
	{
		char *xx;
		errnum = pid_children(ppid, &xx);
		if (errnum != 0)
			return errnum;
		buf = xx;
	}
	if (NULL == buf)
		return errnum;

	pid_t pid = -1;
	char const *start = buf;
	for (;;) {
		errno = 0;
		pid_t child = strtol(start, NULL, 10);
		errnum = errno;
		if (errnum != 0)
			goto free_buf;

		char other_name[SERVICE_NAME_MAX + 1U];
		errnum = service_name(child, other_name);
		if (errnum != 0)
			goto free_buf;

		if (strcmp(name, other_name) != 0)
			goto move_on;

		errnum = 0;
		pid = child;
		break;
	move_on:
		start = strchr(start, ' ');
		if (NULL == start)
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
	if (-1 == pid)
		errnum = ESRCH;

	*pidp = pid;

free_buf:
	linted_mem_free(buf);

	return errnum;
}

static linted_error service_name(pid_t pid,
                                 char name[static SERVICE_NAME_MAX + 1U])
{
	linted_error errnum;

	memset(name, 0, SERVICE_NAME_MAX + 1U);

	linted_ko ko;
	{
		linted_ko xx;
		char path[] = "/proc/XXXXXXXXXXXXXXXX/environ";
		sprintf(path, "/proc/%i/environ", pid);
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

	/* Get the child all at once to avoid raciness. */
	char *buf = NULL;
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
		buf_size = yy;
	}

close_file:
	if (EOF == fclose(file)) {
		if (0 == errnum) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

	if (NULL == buf)
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
		if (NULL == iter) {
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

static linted_error kill_pid_children(pid_t pid, int signo)
{
	linted_error errnum = 0;

	char *buf;
	{
		char *xx;
		errnum = pid_children(pid, &xx);
		if (errnum != 0)
			return errnum;
		buf = xx;
	}
	if (NULL == buf)
		return errnum;

	char const *start = buf;
	for (;;) {
		errno = 0;
		pid_t child = strtol(start, NULL, 10);
		errnum = errno;
		if (errnum != 0)
			goto free_buf;

		if (-1 == kill(child, signo)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto free_buf;
		}

		start = strchr(start, ' ');
		if (NULL == start)
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

static linted_error pid_children(pid_t pid, char **childrenp)
{
	linted_error errnum;
	linted_ko children;

	{
		linted_ko xx;
		char path[] = "/proc/XXXXXXXXXXXXXXXX/task/XXXXXXXXXXXXXXXXX";
		sprintf(path, "/proc/%i/task/%i/children", pid, pid);
		errnum =
		    linted_ko_open(&xx, LINTED_KO_CWD, path, LINTED_KO_RDONLY);
		if (ENOENT == errnum)
			return ESRCH;
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

	/* Get the child all at once to avoid raciness. */
	char *buf = NULL;
	size_t buf_size = 0U;

	{
		char *xx = buf;
		size_t yy = buf_size;

		errno = 0;
		ssize_t zz = getline(&xx, &yy, file);
		if (-1 == zz) {
			errnum = errno;
			/* May be zero */
			goto set_childrenp;
		}
		buf = xx;
		buf_size = yy;
	}

set_childrenp:
	*childrenp = buf;

	if (EOF == fclose(file)) {
		if (0 == errnum) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

	return errnum;
}

static linted_error pid_is_child_of(pid_t parent, pid_t child, bool *isp)
{
	linted_error errnum;

	pid_t ppid;
	{
		struct pidstat buf;

		errnum = pid_stat(child, &buf);
		if (errnum != 0)
			return errnum;

		ppid = buf.ppid;
	}

	*isp = ppid == parent;

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

	char *line;
	{
		char *xx = NULL;
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

	char *start = strchr(line, '(');
	char *end = strrchr(line, ')');
	memcpy(&buf->comm, start, end - start);

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
