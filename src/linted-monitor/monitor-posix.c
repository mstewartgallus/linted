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
#include "linted/ptrace.h"
#endif

#include <ctype.h>
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

enum { SIGNAL_WAIT,
       ADMIN_IN_READ,
       ADMIN_OUT_WRITE,
       KILL_READ,
       MAX_TASKS };

struct monitor {
	char const *process_name;
	char const *unit_path;
	char const *startup;
	char const *sandbox;
	char const *waiter;
	struct linted_admin_in_task_recv *read_task;
	struct linted_admin_out_task_send *write_task;
	struct linted_signal_task_wait *signal_wait_task;
	struct linted_io_task_read *kill_read_task;
	struct linted_async_pool *pool;
	struct linted_unit_db *unit_db;
	linted_ko kill_fifo;
	linted_admin_in admin_in;
	linted_admin_out admin_out;
	linted_pid manager_pid;
	linted_ko cwd;
	bool time_to_exit : 1U;
};

static linted_error
monitor_init(struct monitor *monitor, linted_ko admin_in,
             linted_ko admin_out, linted_ko kill_fifo, linted_ko cwd,
             linted_pid manager_pid, struct linted_async_pool *pool,
             char const *process_name, char const *unit_path,
             char const *startup, char const *sandbox,
             char const *waiter);

static linted_error monitor_destroy(struct monitor *monitor);

static linted_error monitor_start(struct monitor *monitor);
static linted_error monitor_stop(struct monitor *monitor);

static linted_error dispatch(struct monitor *monitor,
                             struct linted_async_task *completed_task);

static linted_error
monitor_monitor_monitor_on_signal(struct monitor *monitor,
                                  struct linted_async_task *task);
static linted_error
monitor_on_admin_in_read(struct monitor *monitor,
                         struct linted_async_task *task);
static linted_error
monitor_on_admin_out_write(struct monitor *monitor,
                           struct linted_async_task *task);
static linted_error
monitor_on_kill_read(struct monitor *monitor,
                     struct linted_async_task *task);

static linted_error on_sigchld(struct monitor *monitor);
static linted_error on_death_sig(struct monitor *monitor, int signo);
static linted_error
on_add_unit(struct monitor *monitor,
            struct linted_admin_add_unit_request const *request,
            struct linted_admin_add_unit_reply *reply);
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

static linted_error
on_child_trapped(struct monitor *monitor, char const *process_name,
                 bool time_to_exit, linted_pid pid, int exit_status,
                 linted_pid manager_pid, linted_ko cwd,
                 struct linted_unit_db *unit_db);
static linted_error on_child_signaled(char const *process_name,
                                      linted_pid pid, int exit_status);
static linted_error on_child_about_to_clone(linted_pid pid);
static linted_error
on_child_about_to_exit(struct monitor *monitor,
                       char const *process_name, bool time_to_exit,
                       linted_pid pid, linted_pid manager_pid,
                       linted_ko cwd, struct linted_unit_db *unit_db);
static linted_error
on_child_linted_ptrace_event_stopped(char const *process_name,
                                     linted_pid pid, int exit_status);

static linted_error service_activate(struct monitor *monitor,
                                     struct linted_unit *unit,
                                     bool check);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);
static size_t null_list_size(char const *const *list);

static linted_error service_children_terminate(linted_pid pid);

static linted_error pid_is_child_of(linted_pid parent, linted_pid child,
                                    bool *isp);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor",
    .dont_fork_thread = true};

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err;

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

	char const *startup;
	{
		char *xx;
		err = linted_environment_get("LINTED_STARTUP", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		startup = xx;
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

	if (0 == startup) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_STARTUP");
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

	static struct monitor monitor = {0};

	err = monitor_init(&monitor, admin_in, admin_out, kill_fifo,
	                   cwd, manager_pid, pool, process_name,
	                   unit_path, startup, sandbox, waiter);
	if (err != 0)
		goto destroy_pool;

	err = monitor_start(&monitor);
	if (err != 0)
		goto destroy_monitor;

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

	monitor_stop(&monitor);

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

destroy_monitor:
	monitor_destroy(&monitor);

destroy_pool:
	;
	linted_error destroy_err = linted_async_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error
monitor_init(struct monitor *monitor, linted_ko admin_in,
             linted_ko admin_out, linted_ko kill_fifo, linted_ko cwd,
             linted_pid manager_pid, struct linted_async_pool *pool,
             char const *process_name, char const *unit_path,
             char const *startup, char const *sandbox,
             char const *waiter)
{
	linted_error err = 0;

	struct linted_signal_task_wait *signal_wait_task;
	{
		struct linted_signal_task_wait *xx;
		err = linted_signal_task_wait_create(&xx, 0);
		if (err != 0)
			return err;
		signal_wait_task = xx;
	}

	struct linted_admin_in_task_recv *admin_in_read_task;
	{
		struct linted_admin_in_task_recv *xx;
		err = linted_admin_in_task_recv_create(&xx, 0);
		if (err != 0)
			goto destroy_signal_wait_task;
		admin_in_read_task = xx;
	}

	struct linted_admin_out_task_send *write_task;
	{
		struct linted_admin_out_task_send *xx;
		err = linted_admin_out_task_send_create(&xx, 0);
		if (err != 0)
			goto destroy_admin_in_read_task;
		write_task = xx;
	}

	struct linted_io_task_read *kill_read_task;
	{
		struct linted_io_task_read *xx;
		err = linted_io_task_read_create(&xx, 0);
		if (err != 0)
			goto destroy_write_task;
		kill_read_task = xx;
	}

	struct linted_unit_db *unit_db;
	{
		struct linted_unit_db *xx;
		err = linted_unit_db_create(&xx);
		if (err != 0)
			goto destroy_kill_read_task;
		unit_db = xx;
	}

	monitor->admin_in = admin_in;
	monitor->admin_out = admin_out;
	monitor->kill_fifo = kill_fifo;
	monitor->cwd = cwd;
	monitor->unit_db = unit_db;
	monitor->manager_pid = manager_pid;
	monitor->pool = pool;
	monitor->signal_wait_task = signal_wait_task;
	monitor->kill_read_task = kill_read_task;
	monitor->read_task = admin_in_read_task;
	monitor->write_task = write_task;
	monitor->process_name = process_name;
	monitor->startup = startup;
	monitor->unit_path = unit_path;
	monitor->sandbox = sandbox;
	monitor->waiter = waiter;
	monitor->time_to_exit = false;

	return 0;

destroy_kill_read_task:
	linted_io_task_read_destroy(kill_read_task);

destroy_write_task:
	linted_admin_out_task_send_destroy(write_task);

destroy_admin_in_read_task:
	linted_admin_in_task_recv_destroy(admin_in_read_task);

destroy_signal_wait_task:
	linted_signal_task_wait_destroy(signal_wait_task);

	return err;
}

static linted_error monitor_destroy(struct monitor *monitor)
{
	return 0;
}

static linted_error monitor_start(struct monitor *monitor)
{
	linted_error err = 0;

	linted_ko cwd = monitor->cwd;
	char const *unit_path = monitor->unit_path;
	char const *startup = monitor->startup;

	struct linted_async_pool *pool = monitor->pool;

	struct linted_admin_in_task_recv *admin_in_read_task =
	    monitor->read_task;
	struct linted_signal_task_wait *signal_wait_task =
	    monitor->signal_wait_task;
	struct linted_io_task_read *kill_read_task =
	    monitor->kill_read_task;

	linted_ko admin_in = monitor->admin_in;
	linted_ko kill_fifo = monitor->kill_fifo;

	linted_signal_listen_to_sigchld();

	linted_signal_task_wait_prepare(
	    signal_wait_task,
	    (union linted_async_ck){.u64 = SIGNAL_WAIT});
	linted_async_pool_submit(
	    pool, linted_signal_task_wait_to_async(signal_wait_task));

	linted_admin_in_task_recv_prepare(
	    admin_in_read_task,
	    (union linted_async_ck){.u64 = ADMIN_IN_READ}, admin_in);
	linted_async_pool_submit(
	    pool,
	    linted_admin_in_task_recv_to_async(admin_in_read_task));

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

	struct linted_spawn_attr *attr;
	{
		struct linted_spawn_attr *xx;
		err = linted_spawn_attr_init(&xx);
		if (err != 0)
			return err;
		attr = xx;
	}

	linted_spawn_attr_set_die_on_parent_death(attr);

	linted_pid startup_pid;
	{
		linted_pid xx;
		char const *const arguments[] = {
		    startup, "admin-in", "admin-out", unit_path, 0};
		err = linted_spawn(&xx, cwd, startup, 0, attr,
		                   arguments, 0);
		if (err != 0)
			return err;
		startup_pid = xx;
	}
	linted_spawn_attr_destroy(attr);

	return 0;
}

static linted_error monitor_stop(struct monitor *monitor)
{
	struct linted_admin_in_task_recv *read_task =
	    monitor->read_task;
	struct linted_admin_out_task_send *write_task =
	    monitor->write_task;
	struct linted_signal_task_wait *signal_wait_task =
	    monitor->signal_wait_task;
	struct linted_io_task_read *kill_task_read =
	    monitor->kill_read_task;

	linted_async_task_cancel(
	    linted_admin_in_task_recv_to_async(read_task));
	linted_async_task_cancel(
	    linted_admin_out_task_send_to_async(write_task));
	linted_async_task_cancel(
	    linted_signal_task_wait_to_async(signal_wait_task));
	linted_async_task_cancel(
	    linted_io_task_read_to_async(kill_task_read));

	return 0;
}

static linted_error dispatch(struct monitor *monitor,
                             struct linted_async_task *task)
{
	switch (linted_async_task_ck(task).u64) {
	case SIGNAL_WAIT:
		return monitor_monitor_monitor_on_signal(monitor, task);

	case ADMIN_IN_READ:
		return monitor_on_admin_in_read(monitor, task);

	case ADMIN_OUT_WRITE:
		return monitor_on_admin_out_write(monitor, task);

	case KILL_READ:
		return monitor_on_kill_read(monitor, task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error
monitor_monitor_monitor_on_signal(struct monitor *monitor,
                                  struct linted_async_task *task)
{
	struct linted_async_pool *pool = monitor->pool;

	linted_error err = 0;

	err = linted_async_task_err(task);
	if (LINTED_ERROR_CANCELLED == err)
		return 0;
	if (err != 0)
		return err;

	struct linted_signal_task_wait *wait_task =
	    linted_signal_task_wait_from_async(task);

	int signo = linted_signal_task_wait_signo(wait_task);

	switch (signo) {
	case SIGCHLD:
		err = on_sigchld(monitor);
		if (err != 0)
			return err;
		break;

	case SIGHUP:
	case SIGQUIT:
	case SIGINT:
	case SIGTERM:
		err = on_death_sig(monitor, signo);
		if (err != 0)
			return err;
		break;
	}

	linted_async_pool_submit(pool, task);

	return 0;
}

static linted_error
monitor_on_admin_in_read(struct monitor *monitor,
                         struct linted_async_task *task)
{
	linted_error err = 0;

	struct linted_async_pool *pool = monitor->pool;
	struct linted_admin_out_task_send *write_task =
	    monitor->write_task;
	linted_pid manager_pid = monitor->manager_pid;
	linted_ko admin_out = monitor->admin_out;

	err = linted_async_task_err(task);

	/* Assume the other end did something bad and don't exit with
	 * an error. */
	if (err != 0)
		return 0;

	struct linted_admin_in_task_recv *admin_in_read_task =
	    linted_admin_in_task_recv_from_async(task);

	struct linted_admin_request *request;
	{
		struct linted_admin_request *xx;
		err = linted_admin_in_task_recv_request(
		    &xx, admin_in_read_task);
		if (err != 0)
			return err;
		request = xx;
	}

	union linted_admin_reply reply;
	switch (request->x.type) {
	case LINTED_ADMIN_ADD_UNIT: {
		struct linted_admin_add_unit_reply yy = {0};
		err = on_add_unit(monitor, &request->x.add_unit, &yy);
		reply.add_unit = yy;
		break;
	}

	case LINTED_ADMIN_STATUS: {
		struct linted_admin_status_reply yy = {0};
		err = on_status_request(manager_pid, &request->x.status,
		                        &yy);
		reply.status = yy;
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_stop_reply yy = {0};
		err =
		    on_stop_request(manager_pid, &request->x.stop, &yy);
		reply.stop = yy;
		break;
	}

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
	linted_admin_request_free(request);

	{
		union linted_admin_reply xx = reply;
		linted_admin_out_task_send_prepare(
		    write_task,
		    (union linted_async_ck){.u64 = ADMIN_OUT_WRITE},
		    admin_out, &xx);
	}
	linted_async_pool_submit(
	    pool, linted_admin_out_task_send_to_async(write_task));

	return err;
}

static linted_error
monitor_on_admin_out_write(struct monitor *monitor,
                           struct linted_async_task *task)
{
	struct linted_async_pool *pool = monitor->pool;
	struct linted_admin_in_task_recv *read_task =
	    monitor->read_task;
	linted_admin_in admin_in = monitor->admin_in;

	linted_error err = linted_async_task_err(task);
	if (err != 0)
		return 0;

	linted_admin_in_task_recv_prepare(
	    read_task, (union linted_async_ck){.u64 = ADMIN_IN_READ},
	    admin_in);
	linted_async_pool_submit(
	    pool, linted_admin_in_task_recv_to_async(read_task));

	return 0;
}

static linted_error monitor_on_kill_read(struct monitor *monitor,
                                         struct linted_async_task *task)
{
	linted_error err = 0;

	struct linted_async_pool *pool = monitor->pool;
	struct linted_unit_db *unit_db = monitor->unit_db;
	linted_pid manager_pid = monitor->manager_pid;

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
			LINTED_ASSERT(kill_err !=
			              LINTED_ERROR_INVALID_PARAMETER);
			LINTED_ASSERT(kill_err !=
			              LINTED_ERROR_PERMISSION);
			LINTED_ASSERT(0 == kill_err);
		}
	}

	monitor->time_to_exit = true;

	linted_async_pool_submit(pool, task);

	return 0;
}

static linted_error on_sigchld(struct monitor *monitor)
{
	char const *process_name = monitor->process_name;
	linted_pid manager_pid = monitor->manager_pid;
	linted_ko cwd = monitor->cwd;
	struct linted_unit_db *unit_db = monitor->unit_db;
	bool time_to_exit = monitor->time_to_exit;

	linted_error err = 0;

	siginfo_t info;
	for (;;) {
		info.si_pid = 0;
		int wait_status = waitid(P_ALL, -1, &info,
		                         WEXITED | WSTOPPED | WNOHANG);
		if (-1 == wait_status) {
			err = errno;
			LINTED_ASSUME(err != 0);
			if (ECHILD == err)
				return LINTED_ERROR_CANCELLED;
			return err;
		}

		linted_pid pid = info.si_pid;
		if (0 == pid)
			break;

		int exit_status = info.si_status;
		int exit_code = info.si_code;

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
			err = on_child_trapped(
			    monitor, process_name, time_to_exit, pid,
			    exit_status, manager_pid, cwd, unit_db);
			break;

		default:
			LINTED_ASSUME_UNREACHABLE();
		}
		if (err != 0)
			return err;
	}

	return 0;
}

static linted_error on_death_sig(struct monitor *monitor, int signo)
{
	struct linted_unit_db *unit_db = monitor->unit_db;
	linted_pid manager_pid = monitor->manager_pid;

	linted_error err = 0;

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
			LINTED_ASSERT(kill_err !=
			              LINTED_ERROR_INVALID_PARAMETER);
			LINTED_ASSERT(kill_err !=
			              LINTED_ERROR_PERMISSION);
			LINTED_ASSERT(0 == err);
		}
	}

	monitor->time_to_exit = true;

	return 0;
}

static linted_error
on_child_trapped(struct monitor *monitor, char const *process_name,
                 bool time_to_exit, linted_pid pid, int exit_status,
                 linted_pid manager_pid, linted_ko cwd,
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
		    monitor, process_name, time_to_exit, pid,
		    manager_pid, cwd, unit_db);

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
	linted_error err = 0;

	if (signo != SIGCHLD)
		goto continue_process;

	siginfo_t info = {0};
	err = linted_ptrace_getsiginfo(pid, &info);
	if (err != 0)
		return err;

	int child_code = info.si_code;
	char const *code_str;
	bool is_signal = true;
	switch (child_code) {
	case CLD_EXITED:
		code_str = "exited";
		is_signal = false;
		break;

	case CLD_KILLED:
		code_str = "killed";
		break;

	case CLD_DUMPED:
		code_str = "dumped";
		break;

	default:
		code_str = "unknown";
		break;
	}

	linted_pid child_pid = info.si_pid;
	int child_signo = info.si_signo;
	int child_status = info.si_status;
	int child_errno = info.si_errno;
	uid_t child_uid = info.si_uid;
	clock_t child_utime = info.si_utime;
	clock_t child_stime = info.si_stime;

	linted_io_write_format(
	    LINTED_KO_STDERR, 0, "child exited!\n"
	                         "\tsignal: %s\n"
	                         "\terrno: %" PRIuMAX "\n"
	                         "\tcode: %s\n"
	                         "\tpid: %" PRIuMAX "\n"
	                         "\tuid: %" PRIuMAX "\n",
	    linted_signal_string(child_signo), (uintmax_t)child_errno,
	    code_str, (uintmax_t)child_pid, (uintmax_t)child_uid);

	if (is_signal) {
		linted_io_write_format(
		    LINTED_KO_STDERR, 0, "\tstatus: %s\n",
		    linted_signal_string(child_status));
	} else {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "\tstatus: %" PRIuMAX "\n",
		                       (uintmax_t)child_status);
	}

	linted_io_write_format(
	    LINTED_KO_STDERR, 0, "\tutime: %" PRIuMAX "\n"
	                         "\tstime: %" PRIuMAX "\n",
	    (uintmax_t)child_utime, (uintmax_t)child_stime);

continue_process:
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
on_child_about_to_exit(struct monitor *monitor,
                       char const *process_name, bool time_to_exit,
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

	err = service_activate(monitor, unit, false);

	return err;
}

/* A sandbox creator is creating a sandbox */
static linted_error on_child_about_to_clone(linted_pid pid)
{
	return linted_ptrace_detach(pid, 0);
}

static linted_error
on_add_unit(struct monitor *monitor,
            struct linted_admin_add_unit_request const *request,
            struct linted_admin_add_unit_reply *reply)
{
	linted_error err = 0;

	struct linted_unit_db *unit_db = monitor->unit_db;

	char const *unit_name = request->name;
	char const *unit_fstab = request->fstab;
	char const *unit_chdir_path = request->chdir_path;
	char const *const *unit_command = request->command;
	char const *const *unit_env_whitelist = request->env_whitelist;

	bool has_priority = request->has_priority;
	bool has_limit_no_file = request->has_limit_no_file;
	bool has_limit_msgqueue = request->has_limit_msgqueue;
	bool has_limit_locks = request->has_limit_locks;

	bool clone_newuser = request->clone_newuser;
	bool clone_newpid = request->clone_newpid;
	bool clone_newipc = request->clone_newipc;
	bool clone_newnet = request->clone_newnet;
	bool clone_newns = request->clone_newns;
	bool clone_newuts = request->clone_newuts;

	bool no_new_privs = request->no_new_privs;

	char *name;
	{
		char *xx;
		err = linted_str_dup(&xx, unit_name);
		if (err != 0)
			return err;
		name = xx;
	}

	char *fstab;
	if (0 == strcmp("", unit_fstab)) {
		fstab = 0;
	} else {
		char *xx;
		err = linted_str_dup(&xx, unit_fstab);
		if (err != 0)
			return err;
		fstab = xx;
	}

	char *chdir_path;
	if (0 == strcmp("", unit_chdir_path)) {
		chdir_path = 0;
	} else {
		char *xx;
		err = linted_str_dup(&xx, unit_chdir_path);
		if (err != 0)
			return err;
		chdir_path = xx;
	}

	size_t command_size = null_list_size(unit_command);

	char **command;
	{
		void *xx;
		err = linted_mem_alloc_array(&xx, command_size + 1U,
		                             sizeof command[0U]);
		if (err != 0)
			goto free_name;
		command = xx;
	}

	for (size_t ii = 0U; ii < command_size; ++ii) {
		err = linted_str_dup(&command[ii], unit_command[ii]);
		if (err != 0) {
			for (size_t jj = 0U; jj < ii; ++jj)
				linted_mem_free(command[jj]);
			linted_mem_free(command);
			goto free_name;
		}
	}
	command[command_size] = 0;

	size_t env_whitelist_size = null_list_size(unit_env_whitelist);

	char **env_whitelist;
	{
		void *xx;
		err =
		    linted_mem_alloc_array(&xx, env_whitelist_size + 1U,
		                           sizeof env_whitelist[0U]);
		if (err != 0)
			goto free_name;
		env_whitelist = xx;
	}

	for (size_t ii = 0U; ii < env_whitelist_size; ++ii) {
		err = linted_str_dup(&env_whitelist[ii],
		                     unit_env_whitelist[ii]);
		if (err != 0) {
			for (size_t jj = 0U; jj < ii; ++jj)
				linted_mem_free(env_whitelist[jj]);
			linted_mem_free(env_whitelist);
			goto free_name;
		}
	}
	env_whitelist[env_whitelist_size] = 0;

	struct linted_unit *unit;
	{
		struct linted_unit *xx;
		err = linted_unit_db_add_unit(unit_db, &xx);
		if (err != 0) {
			linted_mem_free(name);
			return err;
		}
		unit = xx;
	}

	unit->type = LINTED_UNIT_TYPE_SERVICE;
	unit->name = name;

	struct linted_unit_service *unit_service = (void *)unit;

	unit_service->command = (char const *const *)command;
	unit_service->fstab = fstab;
	unit_service->chdir_path = chdir_path;
	unit_service->env_whitelist =
	    (char const *const *)env_whitelist;

	unit_service->priority = -1;
	unit_service->limit_no_file = -1;
	unit_service->limit_msgqueue = -1;
	unit_service->limit_locks = -1;

	/* These aren't fully implemented yet */
	unit_service->has_priority = false;
	unit_service->has_limit_no_file = false;
	unit_service->has_limit_locks = false;
	unit_service->has_limit_msgqueue = false;

	unit_service->clone_newuser = clone_newuser;
	unit_service->clone_newpid = clone_newpid;
	unit_service->clone_newipc = clone_newipc;
	unit_service->clone_newnet = clone_newnet;
	unit_service->clone_newns = clone_newns;
	unit_service->clone_newuts = clone_newuts;

	unit_service->no_new_privs = no_new_privs;

	err = service_activate(monitor, unit, false);

	reply->type = LINTED_ADMIN_ADD_UNIT;
	return err;

free_name:
	linted_mem_free(name);

	return err;
}

static linted_error
on_status_request(linted_pid manager_pid,
                  struct linted_admin_status_request const *request,
                  struct linted_admin_status_reply *reply)
{
	linted_error err = 0;
	bool is_up;

	char const *unit_name = request->name;

	err = linted_unit_pid(0, manager_pid, unit_name);
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

	linted_pid pid;
	{
		linted_pid xx;
		err = linted_unit_pid(&xx, manager_pid, unit_name);
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
	LINTED_ASSERT(err != LINTED_ERROR_INVALID_PARAMETER);
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

extern char **environ;

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	char **result_envvars;
	linted_error err;

	if (0 == allowed_envvars) {
		size_t size =
		    null_list_size((char const *const *)environ);

		{
			void *xx;
			err = linted_mem_alloc_array(
			    &xx, size + 1U, sizeof result_envvars[0U]);
			if (err != 0)
				return err;
			result_envvars = xx;
		}

		for (size_t ii = 0U; ii < size; ++ii) {
			err = linted_str_dup(&result_envvars[ii],
			                     environ[ii]);
			if (err != 0) {
				for (size_t jj = 0; jj <= ii; ++jj) {
					linted_mem_free(
					    result_envvars[jj]);
				}
				linted_mem_free(result_envvars);
				return err;
			}
		}
		result_envvars[size] = 0;
		*result_envvarsp = result_envvars;
		return 0;
	}

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

struct my_option {
	char const *name;
	char const *value;
	bool flag : 1U;
};

static linted_error service_activate(struct monitor *monitor,
                                     struct linted_unit *unit,
                                     bool check)
{
	linted_error err = 0;

	char const *process_name = monitor->process_name;
	char const *sandbox = monitor->sandbox;
	char const *waiter = monitor->waiter;
	linted_pid manager_pid = monitor->manager_pid;
	linted_ko cwd = monitor->cwd;

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
	char const *const *command = unit_service->command;
	bool no_new_privs = unit_service->no_new_privs;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;
	char const *const *env_whitelist = unit_service->env_whitelist;

	bool has_priority = unit_service->has_priority;
	bool has_limit_no_file = unit_service->has_limit_no_file;
	bool has_limit_msgqueue = unit_service->has_limit_msgqueue;
	bool has_limit_locks = unit_service->has_limit_locks;

	bool clone_newuser = unit_service->clone_newuser;
	bool clone_newpid = unit_service->clone_newpid;
	bool clone_newipc = unit_service->clone_newipc;
	bool clone_newnet = unit_service->clone_newnet;
	bool clone_newns = unit_service->clone_newns;
	bool clone_newuts = unit_service->clone_newuts;

	linted_sched_priority priority;
	if (has_priority)
		priority = unit_service->priority;

	int limit_no_file;
	if (has_limit_no_file)
		limit_no_file = unit_service->limit_no_file;

	int limit_msgqueue;
	if (has_limit_msgqueue)
		limit_msgqueue = unit_service->limit_msgqueue;

	int limit_locks;
	if (has_limit_locks)
		limit_locks = unit_service->limit_locks;

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

	char *limit_no_file_str = 0;
	if (has_limit_no_file) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)limit_no_file);
		if (err != 0)
			return err;
		limit_no_file_str = xx;
	}

	char *limit_msgqueue_str = 0;
	if (has_limit_msgqueue) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)limit_msgqueue);
		if (err != 0)
			goto free_no_file_str;
		limit_msgqueue_str = xx;
	}

	char *limit_locks_str = 0;
	if (has_limit_locks) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)limit_locks);
		if (err != 0)
			goto free_limit_msgqueue_str;
		limit_locks_str = xx;
	}

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
			goto free_limit_locks_str;
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

	size_t command_size =
	    null_list_size((char const *const *)command);

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
		    {"--limit-no-file", limit_no_file_str,
		     has_limit_no_file},
		    {"--limit-msgqueue", limit_msgqueue_str,
		     has_limit_msgqueue},
		    {"--limit-locks", limit_locks_str, has_limit_locks},
		    {"--dropcaps", 0, drop_caps},
		    {"--chdir", chdir_path, chdir_path != 0},
		    {"--priority", prio_str, has_priority},
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

		args_size = 1U + num_options + 1U + command_size;
		{
			void *xx;
			err = linted_mem_alloc_array(
			    &xx, args_size + 1U, sizeof command[0U]);
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
	for (size_t ii = 0U; ii < command_size; ++ii)
		args[1U + num_options + 1U + ii] = command[ii];
	args[args_size] = 0;

	struct linted_spawn_attr *attr;
	{
		struct linted_spawn_attr *xx;
		err = linted_spawn_attr_init(&xx);
		if (err != 0)
			goto free_args;
		attr = xx;
	}

	linted_spawn_attr_set_die_on_parent_death(attr);

	err = linted_spawn(0, cwd, sandbox, 0, attr, args,
	                   (char const *const *)envvars);

	linted_spawn_attr_destroy(attr);

free_args:
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

free_limit_locks_str:
	linted_mem_free(limit_locks_str);

free_limit_msgqueue_str:
	linted_mem_free(limit_msgqueue_str);

free_no_file_str:
	linted_mem_free(limit_no_file_str);

	return err;
}
