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
#define _GNU_SOURCE

#include "config.h"

#include "lntd/admin.h"
#include "lntd/async.h"
#include "lntd/dir.h"
#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/file.h"
#include "lntd/fifo.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/path.h"
#include "lntd/proc.h"
#include "lntd/ptrace.h"
#include "lntd/sched.h"
#include "lntd/signal.h"
#include "lntd/spawn.h"
#include "lntd/start.h"
#include "lntd/str.h"
#include "lntd/unit.h"
#include "lntd/util.h"

#include <errno.h>
#include <inttypes.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include <time.h>

#if defined HAVE_POSIX_API
#include <sys/ptrace.h>
#include <sys/wait.h>
#endif

#ifndef PTRACE_EVENT_STOP
#define PTRACE_EVENT_STOP 128
#endif

enum { MANAGERPID,
       LNTD_STARTUP,
       LNTD_SANDBOX,
       LNTD_WAITER,
};

static char const *const required_envs[] =
    {[MANAGERPID] = "MANAGERPID", [LNTD_STARTUP] = "LINTED_STARTUP",
     [LNTD_SANDBOX] = "LINTED_SANDBOX",
     [LNTD_WAITER] = "LINTED_WAITER"};

static char const *const allowed_syscalls;

enum { SIGNAL_WAIT,
       ADMIN_IN_READ,
       ADMIN_OUT_WRITE,
       KILL_READ,
       MAX_TASKS };

struct monitor {
	char const *process_name;
	char const *startup;
	char const *sandbox;
	char const *waiter;
	struct lntd_admin_in_task_recv *read_task;
	struct lntd_admin_out_task_send *write_task;
	struct lntd_signal_task_wait *signal_wait_task;
	struct lntd_io_task_read *kill_read_task;
	struct lntd_async_pool *pool;
	struct lntd_unit_db *unit_db;
	lntd_proc manager_pid;
	lntd_ko kill_fifo;
	lntd_admin_in admin_in;
	lntd_admin_out admin_out;
	lntd_ko cwd;
	bool time_to_exit : 1U;
};

static lntd_error
monitor_init(struct monitor *monitor, lntd_ko admin_in,
             lntd_ko admin_out, lntd_ko kill_fifo, lntd_ko cwd,
             lntd_proc manager_pid, struct lntd_async_pool *pool,
             char const *process_name, char const *startup,
             char const *sandbox, char const *waiter);

static lntd_error monitor_destroy(struct monitor *monitor);

static lntd_error monitor_start(struct monitor *monitor);
static lntd_error monitor_stop(struct monitor *monitor);

static lntd_error dispatch(struct monitor *monitor,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_error err);

static lntd_error
monitor_on_signal(struct monitor *monitor,
                  struct lntd_signal_task_wait *signal_wait_task,
                  lntd_error err);
static lntd_error
monitor_on_admin_in_read(struct monitor *monitor,
                         struct lntd_admin_in_task_recv *read_task,
                         lntd_error err);
static lntd_error
monitor_on_admin_out_write(struct monitor *monitor,
                           struct lntd_admin_out_task_send *write_task,
                           lntd_error err);
static lntd_error
monitor_on_kill_read(struct monitor *monitor,
                     struct lntd_io_task_read *kill_read_task,
                     lntd_error err);

static lntd_error on_sigchld(struct monitor *monitor);
static lntd_error on_death_sig(struct monitor *monitor, int signo);

static lntd_error
on_add_unit(struct monitor *monitor,
            struct lntd_admin_request_add_unit const *request,
            struct lntd_admin_reply_add_unit *reply);
static lntd_error
on_add_socket(struct monitor *monitor,
              struct lntd_admin_request_add_socket const *request,
              struct lntd_admin_reply_add_socket *reply);

static lntd_error
on_status_request(lntd_proc manager_pid,
                  struct lntd_admin_request_status const *request,
                  struct lntd_admin_reply_status *reply);
static lntd_error
on_stop_request(lntd_proc manager_pid,
                struct lntd_admin_request_stop const *request,
                struct lntd_admin_reply_stop *reply);

static lntd_error on_child_stopped(char const *process_name,
                                   lntd_proc pid);

static lntd_error on_child_trapped(struct monitor *monitor,
                                   lntd_proc pid, int exit_status,
                                   struct lntd_unit_db *unit_db);
static lntd_error on_child_signaled(char const *process_name,
                                    lntd_proc pid, int exit_status);
static lntd_error on_child_about_to_clone(lntd_proc pid);
static lntd_error on_child_about_to_exit(struct monitor *monitor,
                                         bool time_to_exit,
                                         lntd_proc pid,
                                         struct lntd_unit_db *unit_db);
static lntd_error
on_child_ptrace_event_stopped(char const *process_name, lntd_proc pid,
                              int exit_status);

static lntd_error service_activate(struct monitor *monitor,
                                   struct lntd_unit *unit, bool check);
lntd_error socket_activate(struct lntd_unit_socket *unit);

static size_t null_list_size(char const *const *list);

static lntd_error service_children_terminate(lntd_proc pid);

static lntd_error pid_is_child_of(lntd_proc parent, lntd_proc child,
                                  bool *isp);
static lntd_error dup_array(char const *const *strs, size_t strs_size,
                            char ***strsp);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor"};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	char const *manager_pid_str;
	char const *startup;
	char const *sandbox;
	char const *waiter;
	{
		char *envs[LNTD_ARRAY_SIZE(required_envs)];
		for (size_t ii = 0U;
		     ii < LNTD_ARRAY_SIZE(required_envs); ++ii) {
			char const *req = required_envs[ii];
			char *value;
			{
				char *xx;
				err = lntd_env_get(req, &xx);
				if (err != 0) {
					lntd_log(
					    LNTD_LOG_ERROR,
					    "lntd_env_get: %s",
					    lntd_error_string(err));
					return EXIT_FAILURE;
				}
				value = xx;
			}
			if (0 == value) {
				lntd_log(LNTD_LOG_ERROR,
				         "%s is a required "
				         "environment variable",
				         req);
				return EXIT_FAILURE;
			}
			envs[ii] = value;
		}
		manager_pid_str = envs[MANAGERPID];
		startup = envs[LNTD_STARTUP];
		sandbox = envs[LNTD_SANDBOX];
		waiter = envs[LNTD_WAITER];
	}

	lntd_proc manager_pid;
	{
		lntd_proc yy;
		err = lntd_proc_from_str(manager_pid_str, &yy);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_proc_from_str: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		manager_pid = yy;
	}

	char *package_runtime_dir_path;
	{
		char *xx;
		err = lntd_path_package_runtime_dir(&xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_path_package_runtime_dir: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		package_runtime_dir_path = xx;
	}

	char *package_data_home_path;
	{
		char *xx;
		err = lntd_path_package_data_home(&xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_path_package_data_home: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		package_data_home_path = xx;
	}

	char *process_runtime_dir_path;
	{
		char *xx;
		err = lntd_str_format(&xx, "%s/%" PRIuMAX,
		                      package_runtime_dir_path,
		                      (uintmax_t)manager_pid);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_str_format: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		process_runtime_dir_path = xx;
	}

	char *process_data_home_path;
	{
		char *xx;
		err = lntd_str_format(&xx, "%s/%" PRIuMAX,
		                      package_data_home_path,
		                      (uintmax_t)manager_pid);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_str_format: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		process_data_home_path = xx;
	}

	err = lntd_dir_create(0, LNTD_KO_CWD, package_runtime_dir_path,
	                      0U, S_IRWXU);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_dir_create: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	err = lntd_dir_create(0, LNTD_KO_CWD, package_data_home_path,
	                      0U, S_IRWXU);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_dir_create: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	err = lntd_dir_create(0, LNTD_KO_CWD, process_runtime_dir_path,
	                      0U, S_IRWXU);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_dir_create: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	err = lntd_dir_create(0, LNTD_KO_CWD, process_data_home_path,
	                      0U, S_IRWXU);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_dir_create: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	lntd_ko cwd;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, ".",
		                   LNTD_KO_DIRECTORY);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_ko_open: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		cwd = xx;
	}

	err = lntd_ko_change_directory(process_runtime_dir_path);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_ko_change_directory: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	err = lntd_ko_symlink(process_data_home_path, "var");
	if (err != 0) {
		if (errno != EEXIST) {
			lntd_log(LNTD_LOG_ERROR, "lntd_ko_symlink: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	lntd_admin_in admin_in;
	{
		lntd_admin_in xx;
		err =
		    lntd_fifo_create(&xx, LNTD_KO_CWD, "admin-in",
		                     LNTD_FIFO_RDWR, S_IRUSR | S_IWUSR);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_fifo_create: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		admin_in = xx;
	}

	lntd_admin_out admin_out;
	{
		lntd_admin_out xx;
		err =
		    lntd_fifo_create(&xx, LNTD_KO_CWD, "admin-out",
		                     LNTD_FIFO_RDWR, S_IRUSR | S_IWUSR);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_fifo_create: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		admin_out = xx;
	}

	lntd_fifo kill_fifo;
	{
		lntd_admin_out xx;
		err =
		    lntd_fifo_create(&xx, LNTD_KO_CWD, "kill",
		                     LNTD_FIFO_RDWR, S_IRUSR | S_IWUSR);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_fifo_create: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		kill_fifo = xx;
	}

	struct lntd_async_pool *pool;
	{
		struct lntd_async_pool *xx;
		err = lntd_async_pool_create(&xx, MAX_TASKS);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_async_pool_create: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	static struct monitor monitor = {0};

	err = monitor_init(&monitor, admin_in, admin_out, kill_fifo,
	                   cwd, manager_pid, pool, process_name,
	                   startup, sandbox, waiter);
	if (err != 0)
		goto destroy_pool;

	err = monitor_start(&monitor);
	if (err != 0)
		goto destroy_monitor;

	for (;;) {
		struct lntd_async_result result;
		{
			struct lntd_async_result xx;
			lntd_async_pool_wait(pool, &xx);
			result = xx;
		}

		err = dispatch(&monitor, result.task_ck,
		               result.userstate, result.err);
		if (err != 0)
			goto cancel_tasks;
	}

cancel_tasks:
	if (LNTD_ERROR_CANCELLED == err)
		err = 0;

	monitor_stop(&monitor);

	for (;;) {
		struct lntd_async_result result;
		{
			struct lntd_async_result xx;
			if (LNTD_ERROR_AGAIN ==
			    lntd_async_pool_poll(pool, &xx))
				break;
			result = xx;
		}

		lntd_error dispatch_error =
		    dispatch(&monitor, result.task_ck, result.userstate,
		             result.err);
		if (0 == err)
			err = dispatch_error;
	}
	if (LNTD_ERROR_CANCELLED == err)
		err = 0;

destroy_monitor:
	monitor_destroy(&monitor);

destroy_pool:
	;
	lntd_error destroy_err = lntd_async_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;

	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "%s", lntd_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static lntd_error
monitor_init(struct monitor *monitor, lntd_ko admin_in,
             lntd_ko admin_out, lntd_ko kill_fifo, lntd_ko cwd,
             lntd_proc manager_pid, struct lntd_async_pool *pool,
             char const *process_name, char const *startup,
             char const *sandbox, char const *waiter)
{
	lntd_error err = 0;

	struct lntd_signal_task_wait *signal_wait_task;
	{
		struct lntd_signal_task_wait *xx;
		err = lntd_signal_task_wait_create(&xx, 0);
		if (err != 0)
			return err;
		signal_wait_task = xx;
	}

	struct lntd_admin_in_task_recv *admin_in_read_task;
	{
		struct lntd_admin_in_task_recv *xx;
		err = lntd_admin_in_task_recv_create(&xx, 0);
		if (err != 0)
			goto destroy_signal_wait_task;
		admin_in_read_task = xx;
	}

	struct lntd_admin_out_task_send *write_task;
	{
		struct lntd_admin_out_task_send *xx;
		err = lntd_admin_out_task_send_create(&xx, 0);
		if (err != 0)
			goto destroy_admin_in_read_task;
		write_task = xx;
	}

	struct lntd_io_task_read *kill_read_task;
	{
		struct lntd_io_task_read *xx;
		err = lntd_io_task_read_create(&xx, 0);
		if (err != 0)
			goto destroy_write_task;
		kill_read_task = xx;
	}

	struct lntd_unit_db *unit_db;
	{
		struct lntd_unit_db *xx;
		err = lntd_unit_db_create(&xx);
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
	monitor->sandbox = sandbox;
	monitor->waiter = waiter;
	monitor->time_to_exit = false;

	return 0;

destroy_kill_read_task:
	lntd_io_task_read_destroy(kill_read_task);

destroy_write_task:
	lntd_admin_out_task_send_destroy(write_task);

destroy_admin_in_read_task:
	lntd_admin_in_task_recv_destroy(admin_in_read_task);

destroy_signal_wait_task:
	lntd_signal_task_wait_destroy(signal_wait_task);

	return err;
}

static lntd_error monitor_destroy(struct monitor *monitor)
{
	return 0;
}

static lntd_error monitor_start(struct monitor *monitor)
{
	lntd_error err = 0;

	lntd_ko cwd = monitor->cwd;
	char const *startup = monitor->startup;

	struct lntd_async_pool *pool = monitor->pool;

	struct lntd_admin_in_task_recv *admin_in_read_task =
	    monitor->read_task;
	struct lntd_signal_task_wait *signal_wait_task =
	    monitor->signal_wait_task;
	struct lntd_io_task_read *kill_read_task =
	    monitor->kill_read_task;

	lntd_ko admin_in = monitor->admin_in;
	lntd_ko kill_fifo = monitor->kill_fifo;

	lntd_signal_listen_to_sigchld();

	lntd_signal_task_wait_submit(
	    pool, signal_wait_task,
	    (union lntd_async_ck){.u64 = SIGNAL_WAIT},
	    signal_wait_task);

	lntd_admin_in_task_recv_submit(
	    pool, admin_in_read_task,
	    (union lntd_async_ck){.u64 = ADMIN_IN_READ},
	    admin_in_read_task, admin_in);

	static char dummy;

	lntd_io_task_read_submit(
	    pool, kill_read_task,
	    (union lntd_async_ck){.u64 = KILL_READ}, kill_read_task,
	    kill_fifo, &dummy, sizeof dummy);

	lntd_signal_listen_to_sighup();
	lntd_signal_listen_to_sigint();
	lntd_signal_listen_to_sigquit();
	lntd_signal_listen_to_sigterm();

	struct lntd_spawn_attr *attr;
	{
		struct lntd_spawn_attr *xx;
		err = lntd_spawn_attr_init(&xx);
		if (err != 0)
			return err;
		attr = xx;
	}

	lntd_spawn_attr_set_die_on_parent_death(attr);

	{
		char const *const arguments[] = {startup, "admin-in",
		                                 "admin-out", 0};
		err =
		    lntd_spawn(0, cwd, startup, 0, attr, arguments, 0);
		if (err != 0)
			return err;
	}
	lntd_spawn_attr_destroy(attr);

	return 0;
}

static lntd_error monitor_stop(struct monitor *monitor)
{
	struct lntd_admin_in_task_recv *read_task = monitor->read_task;
	struct lntd_admin_out_task_send *write_task =
	    monitor->write_task;
	struct lntd_signal_task_wait *signal_wait_task =
	    monitor->signal_wait_task;
	struct lntd_io_task_read *kill_task_read =
	    monitor->kill_read_task;

	lntd_admin_in_task_recv_cancel(read_task);
	lntd_admin_out_task_send_cancel(write_task);
	lntd_signal_task_wait_cancel(signal_wait_task);
	lntd_io_task_read_cancel(kill_task_read);

	return 0;
}

static lntd_error dispatch(struct monitor *monitor,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_error err)
{
	switch (task_ck.u64) {
	case SIGNAL_WAIT:
		return monitor_on_signal(monitor, userstate, err);

	case ADMIN_IN_READ:
		return monitor_on_admin_in_read(monitor, userstate,
		                                err);

	case ADMIN_OUT_WRITE:
		return monitor_on_admin_out_write(monitor, userstate,
		                                  err);

	case KILL_READ:
		return monitor_on_kill_read(monitor, userstate, err);

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

static lntd_error
monitor_on_signal(struct monitor *monitor,
                  struct lntd_signal_task_wait *signal_wait_task,
                  lntd_error err)
{
	struct lntd_async_pool *pool = monitor->pool;

	if (LNTD_ERROR_CANCELLED == err)
		return 0;
	if (err != 0)
		return err;

	int signo = lntd_signal_task_wait_signo(signal_wait_task);

	lntd_signal_task_wait_submit(
	    pool, signal_wait_task,
	    (union lntd_async_ck){.u64 = SIGNAL_WAIT},
	    signal_wait_task);

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

	return 0;
}

static lntd_error monitor_on_admin_in_read(
    struct monitor *monitor,
    struct lntd_admin_in_task_recv *admin_in_read_task, lntd_error err)
{

	struct lntd_async_pool *pool = monitor->pool;
	struct lntd_admin_out_task_send *write_task =
	    monitor->write_task;
	lntd_proc manager_pid = monitor->manager_pid;
	lntd_ko admin_out = monitor->admin_out;

	/* Assume the other end did something bad and don't exit with
	 * an error. */
	if (err != 0)
		return 0;

	struct lntd_admin_request *request;
	{
		struct lntd_admin_request *xx;
		err = lntd_admin_in_task_recv_request(
		    &xx, admin_in_read_task);
		if (err != 0)
			return err;
		request = xx;
	}

	lntd_admin_type type = request->type;

	struct lntd_admin_reply reply;
	reply.type = type;
	switch (type) {
	case LNTD_ADMIN_ADD_UNIT: {
		struct lntd_admin_reply_add_unit yy = {0};
		err = on_add_unit(
		    monitor, &request->lntd_admin_request_u.add_unit,
		    &yy);
		reply.lntd_admin_reply_u.add_unit = yy;
		break;
	}

	case LNTD_ADMIN_ADD_SOCKET: {
		struct lntd_admin_reply_add_socket yy = {0};
		err = on_add_socket(
		    monitor, &request->lntd_admin_request_u.add_socket,
		    &yy);
		reply.lntd_admin_reply_u.add_socket = yy;
		break;
	}

	case LNTD_ADMIN_STATUS: {
		struct lntd_admin_reply_status yy = {0};
		err = on_status_request(
		    manager_pid, &request->lntd_admin_request_u.status,
		    &yy);
		reply.lntd_admin_reply_u.status = yy;
		break;
	}

	case LNTD_ADMIN_STOP: {
		struct lntd_admin_reply_stop yy = {0};
		err = on_stop_request(
		    manager_pid, &request->lntd_admin_request_u.stop,
		    &yy);
		reply.lntd_admin_reply_u.stop = yy;
		break;
	}

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
	lntd_admin_request_free(request);

	{
		struct lntd_admin_reply xx = reply;

		lntd_admin_out_task_send_submit(
		    pool, write_task,
		    (union lntd_async_ck){.u64 = ADMIN_OUT_WRITE},
		    write_task, admin_out, &xx);
	}

	return err;
}

static lntd_error
monitor_on_admin_out_write(struct monitor *monitor,
                           struct lntd_admin_out_task_send *write_task,
                           lntd_error err)
{
	struct lntd_async_pool *pool = monitor->pool;
	struct lntd_admin_in_task_recv *read_task = monitor->read_task;
	lntd_admin_in admin_in = monitor->admin_in;

	if (err != 0)
		return 0;

	lntd_admin_in_task_recv_submit(
	    pool, read_task,
	    (union lntd_async_ck){.u64 = ADMIN_IN_READ}, read_task,
	    admin_in);

	return 0;
}

static lntd_error
monitor_on_kill_read(struct monitor *monitor,
                     struct lntd_io_task_read *kill_read_task,
                     lntd_error err)
{
	struct lntd_async_pool *pool = monitor->pool;
	struct lntd_unit_db *unit_db = monitor->unit_db;
	lntd_proc manager_pid = monitor->manager_pid;
	lntd_ko kill_fifo = monitor->kill_fifo;

	if (LNTD_ERROR_CANCELLED == err)
		return 0;
	if (err != 0)
		return err;

	size_t db_size = lntd_unit_db_size(unit_db);
	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct lntd_unit *unit =
		    lntd_unit_db_get_unit(unit_db, ii);

		char const *name = unit->name;
		lntd_unit_type type = unit->type;

		if (type != LNTD_UNIT_TYPE_SERVICE)
			continue;

		lntd_proc pid;
		{
			lntd_proc xx;
			lntd_error pid_err =
			    lntd_unit_pid(&xx, manager_pid, name);
			if (ESRCH == pid_err)
				continue;
			if (pid_err != 0) {
				if (0 == err)
					err = pid_err;
				continue;
			}
			pid = xx;
		}

		lntd_error kill_err = lntd_proc_kill(pid, SIGTERM);
		if (kill_err != ESRCH) {
			LNTD_ASSERT(kill_err !=
			            LNTD_ERROR_INVALID_PARAMETER);
			LNTD_ASSERT(kill_err != LNTD_ERROR_PERMISSION);
			LNTD_ASSERT(0 == kill_err);
		}
	}

	monitor->time_to_exit = true;

	static char dummy;
	lntd_io_task_read_submit(
	    pool, kill_read_task,
	    (union lntd_async_ck){.u64 = KILL_READ}, kill_read_task,
	    kill_fifo, &dummy, sizeof dummy);
	return 0;
}

static lntd_error on_sigchld(struct monitor *monitor)
{
	char const *process_name = monitor->process_name;
	struct lntd_unit_db *unit_db = monitor->unit_db;

	lntd_error err = 0;

	for (;;) {
		lntd_proc pid;
		int exit_status;
		int exit_code;
		{
			siginfo_t info;
			info.si_pid = 0;
			int wait_status =
			    waitid(P_ALL, -1, &info,
			           WEXITED | WSTOPPED | WNOHANG);
			if (-1 == wait_status) {
				err = errno;
				LNTD_ASSUME(err != 0);
				if (ECHILD == err)
					return LNTD_ERROR_CANCELLED;
				return err;
			}

			pid = info.si_pid;
			if (0 == pid)
				break;

			exit_status = info.si_status;
			exit_code = info.si_code;
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
			err = on_child_trapped(monitor, pid,
			                       exit_status, unit_db);
			break;

		default:
			LNTD_ASSUME_UNREACHABLE();
		}
		if (err != 0)
			return err;
	}

	return 0;
}

static lntd_error on_death_sig(struct monitor *monitor, int signo)
{
	struct lntd_unit_db *unit_db = monitor->unit_db;
	lntd_proc manager_pid = monitor->manager_pid;

	lntd_error err = 0;

	size_t db_size = lntd_unit_db_size(unit_db);
	for (size_t ii = 0U; ii < db_size; ++ii) {
		struct lntd_unit *unit =
		    lntd_unit_db_get_unit(unit_db, ii);

		char const *name = unit->name;
		lntd_unit_type type = unit->type;

		if (type != LNTD_UNIT_TYPE_SERVICE)
			continue;

		lntd_proc pid;
		{
			lntd_proc xx;
			lntd_error pid_err =
			    lntd_unit_pid(&xx, manager_pid, name);
			if (ESRCH == pid_err)
				continue;
			if (pid_err != 0) {
				if (0 == err)
					err = pid_err;
				continue;
			}
			pid = xx;
		}
		lntd_error kill_err = lntd_proc_kill(pid, signo);
		if (kill_err != ESRCH) {
			LNTD_ASSERT(kill_err !=
			            LNTD_ERROR_INVALID_PARAMETER);
			LNTD_ASSERT(kill_err != LNTD_ERROR_PERMISSION);
			LNTD_ASSERT(0 == err);
		}
	}

	monitor->time_to_exit = true;

	return 0;
}

static lntd_error on_child_trapped(struct monitor *monitor,
                                   lntd_proc pid, int exit_status,
                                   struct lntd_unit_db *unit_db)
{
	bool time_to_exit = monitor->time_to_exit;
	char const *process_name = monitor->process_name;

	int event = exit_status >> 8U;
	exit_status = exit_status & 0xFF;
	switch (event) {
	case 0:
		return on_child_signaled(process_name, pid,
		                         exit_status);

	case PTRACE_EVENT_EXIT:
		return on_child_about_to_exit(monitor, time_to_exit,
		                              pid, unit_db);

	case PTRACE_EVENT_STOP:
		return on_child_ptrace_event_stopped(process_name, pid,
		                                     exit_status);

	case PTRACE_EVENT_VFORK:
	case PTRACE_EVENT_FORK:
	case PTRACE_EVENT_CLONE:
		return on_child_about_to_clone(pid);

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

static lntd_error on_child_stopped(char const *process_name,
                                   lntd_proc pid)
{
	lntd_error err = 0;

	lntd_error seize_err = lntd_ptrace_seize(
	    pid, PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK |
	             PTRACE_O_TRACEVFORK);
	if (0 == err)
		err = seize_err;

	lntd_error kill_err = lntd_proc_continue(pid);
	if (0 == err)
		err = kill_err;

	return err;
}

static lntd_error on_child_signaled(char const *process_name,
                                    lntd_proc pid, int signo)
{
	lntd_error err = 0;

	bool is_sigchld = SIGCHLD == signo;

	int child_code;
	lntd_proc child_pid;
	int child_signo;
	int child_status;
	int child_errno;
	uid_t child_uid;
	clock_t child_utime;
	clock_t child_stime;
	{
		siginfo_t info = {0};
		if (is_sigchld) {
			err = lntd_ptrace_getsiginfo(pid, &info);
		}

		lntd_error cont_err = lntd_ptrace_cont(pid, signo);
		if (0 == err)
			err = cont_err;

		if (err != 0)
			return err;

		if (!is_sigchld)
			return 0;

		child_code = info.si_code;
		child_pid = info.si_pid;
		child_signo = info.si_signo;
		child_status = info.si_status;
		child_errno = info.si_errno;
		child_uid = info.si_uid;
		child_utime = info.si_utime;
		child_stime = info.si_stime;
	}

	bool is_signal = true;
	char const *code_str;
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

	lntd_io_write_format(
	    LNTD_KO_STDERR, 0, "child exited!\n"
	                       "\tsignal: %s\n"
	                       "\terrno: %" PRIuMAX "\n"
	                       "\tcode: %s\n"
	                       "\tpid: %" PRIuMAX "\n"
	                       "\tuid: %" PRIuMAX "\n",
	    lntd_signal_string(child_signo), (uintmax_t)child_errno,
	    code_str, (uintmax_t)child_pid, (uintmax_t)child_uid);

	if (is_signal) {
		lntd_io_write_format(LNTD_KO_STDERR, 0,
		                     "\tstatus: %s\n",
		                     lntd_signal_string(child_status));
	} else {
		lntd_io_write_format(LNTD_KO_STDERR, 0,
		                     "\tstatus: %" PRIuMAX "\n",
		                     (uintmax_t)child_status);
	}

	lntd_io_write_format(
	    LNTD_KO_STDERR, 0, "\tutime: %" PRIuMAX "\n"
	                       "\tstime: %" PRIuMAX "\n",
	    (uintmax_t)child_utime, (uintmax_t)child_stime);

	return 0;
}

static lntd_error
on_child_ptrace_event_stopped(char const *process_name, lntd_proc pid,
                              int exit_status)
{
	lntd_error err = 0;

	lntd_proc self = lntd_proc_get_pid();

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
		char name[LNTD_UNIT_NAME_MAX + 1U];
		err = lntd_unit_name(pid, name);
		if (err != 0)
			return err;

		lntd_io_write_format(LNTD_KO_STDERR, 0,
		                     "%s: ptracing %" PRIi64 " %s\n",
		                     process_name, (intmax_t)pid, name);
	}

	err = lntd_ptrace_setoptions(pid, PTRACE_O_TRACEEXIT);

continue_process:
	;
	lntd_error cont_err = lntd_ptrace_cont(pid, 0);
	if (0 == err)
		err = cont_err;

	return err;
}

static lntd_error on_child_about_to_exit(struct monitor *monitor,
                                         bool time_to_exit,
                                         lntd_proc pid,
                                         struct lntd_unit_db *unit_db)
{

	lntd_error err = 0;
	struct lntd_unit *unit = 0;

	char const *process_name = monitor->process_name;

	err = service_children_terminate(pid);
	if (err != 0)
		goto detach_from_process;

	unsigned long status;
	{
		unsigned long xx;
		err = lntd_ptrace_geteventmsg(pid, &xx);
		if (err != 0)
			goto detach_from_process;
		status = xx;
	}

	{
		char name[LNTD_UNIT_NAME_MAX + 1U];
		err = lntd_unit_name(pid, name);
		if (err != 0)
			goto detach_from_process;

		if (WIFEXITED(status)) {
			int exit_status = WEXITSTATUS(status);

			lntd_io_write_format(LNTD_KO_STDERR, 0,
			                     "%s: %s: exited with %i\n",
			                     process_name, name,
			                     exit_status);

		} else if (WIFSIGNALED(status)) {
			int signo = WTERMSIG(status);

			char const *str = lntd_signal_string(signo);
			if (0 == str)
				str = "unknown signal";

			lntd_io_write_format(LNTD_KO_STDERR, 0,
			                     "%s: %s: killed by %s\n",
			                     process_name, name, str);
		} else {
			LNTD_ASSUME_UNREACHABLE();
		}

		unit = lntd_unit_db_get_unit_by_name(unit_db, name);
	}

detach_from_process:
	err = lntd_ptrace_detach(pid, 0);
	if (err != 0)
		return err;

	if (time_to_exit)
		return err;

	if (0 == unit)
		return err;
#if 0
	err = service_activate(monitor, unit, false);
#endif
	return err;
}

/* A sandbox creator is creating a sandbox */
static lntd_error on_child_about_to_clone(lntd_proc pid)
{
	return lntd_ptrace_detach(pid, 0);
}

static lntd_error
on_add_unit(struct monitor *monitor,
            struct lntd_admin_request_add_unit const *request,
            struct lntd_admin_reply_add_unit *reply)
{
	lntd_error err = 0;

	struct lntd_unit_db *unit_db = monitor->unit_db;

	char const *unit_name = request->name;
	char const *unit_fstab = request->fstab;
	char const *unit_chdir_path = request->chdir_path;

	size_t command_size = request->command.command_len;
	char **unit_command = request->command.command_val;

	size_t environment_size = request->environment.environment_len;
	char **unit_environment = request->environment.environment_val;

	int_least64_t *timer_slack_nsec = request->timer_slack_nsec;
	int_least64_t *priority = request->priority;
	int_least64_t *limit_no_file = request->limit_no_file;
	int_least64_t *limit_msgqueue = request->limit_msgqueue;
	int_least64_t *limit_locks = request->limit_locks;
	int_least64_t *limit_memlock = request->limit_memlock;

	bool has_timer_slack_nsec = timer_slack_nsec != 0;
	bool has_priority = priority != 0;
	bool has_limit_no_file = limit_no_file != 0;
	bool has_limit_msgqueue = limit_msgqueue != 0;
	bool has_limit_locks = limit_locks != 0;
	bool has_limit_memlock = limit_memlock != 0;

	bool clone_newuser = request->clone_newuser;
	bool clone_newcgroup = request->clone_newcgroup;
	bool clone_newpid = request->clone_newpid;
	bool clone_newipc = request->clone_newipc;
	bool clone_newnet = request->clone_newnet;
	bool clone_newns = request->clone_newns;
	bool clone_newuts = request->clone_newuts;

	bool no_new_privs = request->no_new_privs;

	char *name;
	{
		char *xx;
		err = lntd_str_dup(&xx, unit_name);
		if (err != 0)
			return err;
		name = xx;
	}

	char *fstab;
	if (0 == strcmp("", unit_fstab)) {
		fstab = 0;
	} else {
		char *xx;
		err = lntd_str_dup(&xx, unit_fstab);
		if (err != 0)
			goto free_name;
		fstab = xx;
	}

	char *chdir_path;
	if (0 == strcmp("", unit_chdir_path)) {
		chdir_path = 0;
	} else {
		char *xx;
		err = lntd_str_dup(&xx, unit_chdir_path);
		if (err != 0)
			goto free_fstab;
		chdir_path = xx;
	}

	char **command;
	{
		char **xx;
		err = dup_array((char const *const *)unit_command,
		                command_size, &xx);
		if (err != 0)
			goto free_chdir_path;
		command = xx;
	}

	char **environment;
	{
		char **xx;
		err = dup_array((char const *const *)unit_environment,
		                environment_size, &xx);
		if (err != 0)
			goto free_command;
		environment = xx;
	}

	struct lntd_unit *unit;
	{
		struct lntd_unit *xx;
		err = lntd_unit_db_add_unit(unit_db, &xx);
		if (err != 0)
			goto free_environment;
		unit = xx;
	}

	unit->type = LNTD_UNIT_TYPE_SERVICE;
	unit->name = name;

	struct lntd_unit_service *unit_service =
	    &unit->lntd_unit_u.service;

	unit_service->command = (char const *const *)command;
	unit_service->fstab = fstab;
	unit_service->chdir_path = chdir_path;
	unit_service->environment = (char const *const *)environment;

	if (has_timer_slack_nsec)
		unit_service->timer_slack_nsec = *timer_slack_nsec;

	if (has_priority)
		unit_service->priority = *priority;

	if (has_limit_no_file)
		unit_service->limit_no_file = *limit_no_file;

	if (has_limit_msgqueue)
		unit_service->limit_msgqueue = *limit_msgqueue;

	if (has_limit_locks)
		unit_service->limit_locks = *limit_locks;

	if (has_limit_memlock)
		unit_service->limit_memlock = *limit_memlock;

	/* These aren't fully implemented yet */
	unit_service->has_priority = has_priority;
	unit_service->has_timer_slack_nsec = has_timer_slack_nsec;
	unit_service->has_limit_no_file = has_limit_no_file;
	unit_service->has_limit_locks = has_limit_locks;
	unit_service->has_limit_msgqueue = has_limit_msgqueue;
	unit_service->has_limit_memlock = has_limit_memlock;

	unit_service->clone_newuser = clone_newuser;
	unit_service->clone_newcgroup = clone_newcgroup;
	unit_service->clone_newpid = clone_newpid;
	unit_service->clone_newipc = clone_newipc;
	unit_service->clone_newnet = clone_newnet;
	unit_service->clone_newns = clone_newns;
	unit_service->clone_newuts = clone_newuts;

	unit_service->no_new_privs = no_new_privs;

	err = service_activate(monitor, unit, false);

	return err;

free_environment:
	for (size_t ii = 0U; ii < environment_size; ++ii)
		lntd_mem_free(environment[ii]);
	lntd_mem_free(environment);

free_command:
	for (size_t ii = 0U; ii < command_size; ++ii)
		lntd_mem_free(command[ii]);
	lntd_mem_free(command);

free_chdir_path:
	lntd_mem_free(chdir_path);

free_fstab:
	lntd_mem_free(fstab);

free_name:
	lntd_mem_free(name);

	return err;
}

static lntd_error
on_add_socket(struct monitor *monitor,
              struct lntd_admin_request_add_socket const *request,
              struct lntd_admin_reply_add_socket *reply)
{
	lntd_error err = 0;

	struct lntd_unit_db *unit_db = monitor->unit_db;

	char const *unit_name = request->name;
	char const *unit_path = request->path;
	int32_t fifo_size = request->fifo_size;
	lntd_unit_socket_type type = request->sock_type;

	char *name;
	{
		char *xx;
		err = lntd_str_dup(&xx, unit_name);
		if (err != 0)
			return err;
		name = xx;
	}

	char *path;
	{
		char *xx;
		err = lntd_str_dup(&xx, unit_path);
		if (err != 0)
			goto free_name;
		path = xx;
	}

	struct lntd_unit *unit;
	{
		struct lntd_unit *xx;
		err = lntd_unit_db_add_unit(unit_db, &xx);
		if (err != 0) {
			lntd_mem_free(name);
			return err;
		}
		unit = xx;
	}

	unit->type = LNTD_UNIT_TYPE_SOCKET;
	unit->name = name;

	struct lntd_unit_socket *unit_socket =
	    &unit->lntd_unit_u.socket;

	unit_socket->path = path;
	unit_socket->fifo_size = fifo_size;
	unit_socket->type = type;

	err = socket_activate(unit_socket);

	return err;

free_name:
	lntd_mem_free(name);

	return err;
}

static lntd_error
on_status_request(lntd_proc manager_pid,
                  struct lntd_admin_request_status const *request,
                  struct lntd_admin_reply_status *reply)
{
	lntd_error err = 0;
	bool is_up;

	char const *unit_name = request->name;

	err = lntd_unit_pid(0, manager_pid, unit_name);
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

	reply->is_up = is_up;
	return 0;
}

static lntd_error
on_stop_request(lntd_proc manager_pid,
                struct lntd_admin_request_stop const *request,
                struct lntd_admin_reply_stop *reply)
{
	lntd_error err = 0;
	bool was_up;

	char const *unit_name = request->name;

	lntd_proc pid;
	{
		lntd_proc xx;
		err = lntd_unit_pid(&xx, manager_pid, unit_name);
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
	err = lntd_proc_terminate(pid);
	LNTD_ASSERT(err != LNTD_ERROR_INVALID_PARAMETER);
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

	reply->was_up = was_up;
	return 0;
}

static lntd_error service_children_terminate(lntd_proc pid)
{
	lntd_error err = 0;

	lntd_proc *children;
	size_t len;
	{
		lntd_proc *xx;
		size_t yy;
		err = lntd_proc_children(pid, &xx, &yy);
		if (err != 0)
			return err;
		children = xx;
		len = yy;
	}

	for (size_t ii = 0U; ii < len; ++ii) {
		err = lntd_proc_terminate(children[ii]);
		if (err != 0)
			goto free_children;
	}

free_children:
	lntd_mem_free(children);

	return err;
}

static size_t null_list_size(char const *const *list)
{
	for (size_t ii = 0U;; ++ii)
		if (0 == list[ii])
			return ii;
}

static lntd_error pid_is_child_of(lntd_proc parent, lntd_proc child,
                                  bool *isp)
{
	lntd_error err;

	lntd_proc ppid;
	{
		struct lntd_proc_stat xx;
		err = lntd_proc_stat(child, &xx);
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

static lntd_error service_activate(struct monitor *monitor,
                                   struct lntd_unit *unit, bool check)
{
	lntd_error err = 0;

	char const *process_name = monitor->process_name;
	char const *sandbox = monitor->sandbox;
	char const *waiter = monitor->waiter;
	lntd_proc manager_pid = monitor->manager_pid;
	lntd_ko cwd = monitor->cwd;

	char const *unit_name = unit->name;

	struct lntd_unit_service *unit_service =
	    &unit->lntd_unit_u.service;

	if (!check)
		goto spawn_service;

	lntd_proc child;
	{
		lntd_proc xx;
		err = lntd_unit_pid(&xx, manager_pid, unit_name);
		if (err != 0)
			goto service_not_found;
		child = xx;
	}

	lntd_io_write_format(LNTD_KO_STDERR, 0,
	                     "%s: ptracing %" PRIi64 " %s\n",
	                     process_name, (intmax_t)child, unit_name);

	return lntd_ptrace_seize(child, PTRACE_O_TRACEEXIT);

service_not_found:
	if (err != ESRCH)
		return err;
spawn_service:
	;
	char const *const *command = unit_service->command;
	char const *const *environment = unit_service->environment;
	bool no_new_privs = unit_service->no_new_privs;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;

	bool has_timer_slack_nsec = unit_service->has_timer_slack_nsec;
	bool has_priority = unit_service->has_priority;
	bool has_limit_no_file = unit_service->has_limit_no_file;
	bool has_limit_msgqueue = unit_service->has_limit_msgqueue;
	bool has_limit_locks = unit_service->has_limit_locks;
	bool has_limit_memlock = unit_service->has_limit_memlock;

	bool clone_newuser = unit_service->clone_newuser;
	bool clone_newcgroup = unit_service->clone_newcgroup;
	bool clone_newpid = unit_service->clone_newpid;
	bool clone_newipc = unit_service->clone_newipc;
	bool clone_newnet = unit_service->clone_newnet;
	bool clone_newns = unit_service->clone_newns;
	bool clone_newuts = unit_service->clone_newuts;

	int_least64_t *timer_slack_nsec = 0;
	if (has_timer_slack_nsec)
		timer_slack_nsec = &unit_service->timer_slack_nsec;

	lntd_sched_priority *priority = 0;
	if (has_priority)
		priority = &unit_service->priority;

	int_least64_t *limit_no_file = 0;
	if (has_limit_no_file)
		limit_no_file = &unit_service->limit_no_file;

	int_least64_t *limit_msgqueue = 0;
	if (has_limit_msgqueue)
		limit_msgqueue = &unit_service->limit_msgqueue;

	int_least64_t *limit_locks = 0;
	if (has_limit_locks)
		limit_locks = &unit_service->limit_locks;

	int_least64_t *limit_memlock = 0;
	if (has_limit_memlock)
		limit_memlock = &unit_service->limit_memlock;

	if (fstab != 0) {
		lntd_ko name_dir;
		{
			lntd_ko xx;
			err = lntd_dir_create(&xx, LNTD_KO_CWD,
			                      unit_name, 0U, S_IRWXU);
			if (err != 0)
				return err;
			name_dir = xx;
		}

		err =
		    lntd_dir_create(0, name_dir, "chroot", 0U, S_IRWXU);

		lntd_ko_close(name_dir);

		if (err != 0)
			return err;
	}

	if (fstab != 0 && !clone_newns)
		return LNTD_ERROR_INVALID_PARAMETER;

	bool drop_caps = true;

	char *limit_no_file_str = 0;
	char *limit_msgqueue_str = 0;
	char *limit_locks_str = 0;
	char *limit_memlock_str = 0;

	char *timer_slack_str = 0;
	char *prio_str = 0;
	char *chrootdir = 0;
	char *sandbox_base = 0;

	if (limit_no_file != 0) {
		char *xx;
		err = lntd_str_format(&xx, "%" PRIi64, *limit_no_file);
		if (err != 0)
			return err;
		limit_no_file_str = xx;
	}

	if (limit_msgqueue != 0) {
		char *xx;
		err = lntd_str_format(&xx, "%" PRIi64, *limit_msgqueue);
		if (err != 0)
			goto free_strs;
		limit_msgqueue_str = xx;
	}

	if (limit_locks != 0) {
		char *xx;
		err = lntd_str_format(&xx, "%" PRIi64, *limit_locks);
		if (err != 0)
			goto free_strs;
		limit_locks_str = xx;
	}

	if (limit_memlock != 0) {
		char *xx;
		err = lntd_str_format(&xx, "%" PRIi64, *limit_memlock);
		if (err != 0)
			goto free_strs;
		limit_memlock_str = xx;
	}

	if (timer_slack_nsec != 0) {
		char *xx;
		err =
		    lntd_str_format(&xx, "%" PRIi64, *timer_slack_nsec);
		if (err != 0)
			goto free_strs;
		timer_slack_str = xx;
	}

	if (priority != 0) {
		char *xx;
		err = lntd_str_format(&xx, "%i", *priority);
		if (err != 0)
			goto free_strs;
		prio_str = xx;
	}

	{
		char *xx;
		err = lntd_str_format(&xx, "%s/chroot", unit_name);
		if (err != 0)
			goto free_strs;
		chrootdir = xx;
	}

	{
		char *xx;
		err = lntd_path_base(&xx, sandbox);
		if (err != 0)
			goto free_strs;
		sandbox_base = xx;
	}

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
		    {"--limit-memlock", limit_memlock_str,
		     has_limit_memlock},
		    {"--dropcaps", 0, drop_caps},
		    {"--chdir", chdir_path, chdir_path != 0},
		    {"--timer-slack", timer_slack_str,
		     has_timer_slack_nsec},
		    {"--priority", prio_str, has_priority},
		    {"--seccomp-filter", allowed_syscalls,
		     no_new_privs},
		    {"--seccomp-arch-native", 0, true},
		    {"--clone-newuser", 0, clone_newuser},
		    {"--clone-newcgroup", 0, clone_newcgroup},
		    {"--clone-newpid", 0, clone_newpid},
		    {"--clone-newipc", 0, clone_newipc},
		    {"--clone-newnet", 0, clone_newnet},
		    {"--clone-newns", 0, clone_newns},
		    {"--clone-newuts", 0, clone_newuts}};

		num_options = 0U;
		for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(options);
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
			err = lntd_mem_alloc_array(&xx, args_size + 1U,
			                           sizeof command[0U]);
			if (err != 0)
				goto free_strs;
			args = xx;
		}
		args[0U] = sandbox_base;

		size_t ix = 1U;
		for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(options);
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

	struct lntd_spawn_attr *attr;
	{
		struct lntd_spawn_attr *xx;
		err = lntd_spawn_attr_init(&xx);
		if (err != 0)
			goto free_args;
		attr = xx;
	}

	lntd_spawn_attr_set_die_on_parent_death(attr);

	err = lntd_spawn(0, cwd, sandbox, 0, attr, args, environment);

	lntd_spawn_attr_destroy(attr);

free_args:
	lntd_mem_free(args);

free_strs:
	lntd_mem_free(sandbox_base);
	lntd_mem_free(chrootdir);
	lntd_mem_free(prio_str);
	lntd_mem_free(timer_slack_str);
	lntd_mem_free(limit_locks_str);
	lntd_mem_free(limit_msgqueue_str);
	lntd_mem_free(limit_no_file_str);

	return err;
}

lntd_error socket_activate(struct lntd_unit_socket *unit)
{
	lntd_error err = 0;

	lntd_unit_type type = unit->type;
	char const *path = unit->path;

	switch (type) {
	case LNTD_UNIT_SOCKET_TYPE_DIR:
		err =
		    lntd_dir_create(0, LNTD_KO_CWD, path, 0U, S_IRWXU);
		break;

	case LNTD_UNIT_SOCKET_TYPE_FILE:
		err = lntd_file_create(0, LNTD_KO_CWD, path, 0U,
		                       S_IRUSR | S_IWUSR);
		break;

	case LNTD_UNIT_SOCKET_TYPE_FIFO: {
		int_least32_t fifo_size = unit->fifo_size;

#if defined F_SETPIPE_SZ
		if (fifo_size >= 0) {
			lntd_ko fifo;
			{
				lntd_ko xx;
				err = lntd_fifo_create(
				    &xx, LNTD_KO_CWD, path,
				    LNTD_FIFO_RDWR, S_IRUSR | S_IWUSR);
				if (err != 0)
					return err;
				fifo = xx;
			}

			if (-1 ==
			    fcntl(fifo, F_SETPIPE_SZ, fifo_size)) {
				err = errno;
				LNTD_ASSUME(err != 0);
			}

			lntd_error close_err = lntd_ko_close(fifo);
			if (0 == err)
				err = close_err;
		} else
#endif
		{
			err = lntd_fifo_create(0, LNTD_KO_CWD, path, 0U,
			                       S_IRUSR | S_IWUSR);
		}

		break;
	}
	}

	return err;
}

static lntd_error dup_array(char const *const *strs, size_t strs_size,
                            char ***strsp)
{
	lntd_error err = 0;

	char **new_strs;
	{
		void *xx;
		err = lntd_mem_alloc_array(&xx, strs_size + 1U,
		                           sizeof new_strs[0U]);
		if (err != 0)
			return err;
		new_strs = xx;
	}

	size_t ii = 0U;
	for (; ii < strs_size; ++ii) {
		err = lntd_str_dup(&new_strs[ii], strs[ii]);
		if (err != 0)
			goto free_new;
	}
	new_strs[ii] = 0;

	*strsp = new_strs;

	return 0;

free_new:
	for (size_t jj = 0U; jj < ii; ++jj)
		lntd_mem_free(new_strs[jj]);
	lntd_mem_free(new_strs);

	return err;
}

static char const *const allowed_syscalls =
    /* Common */
    "execve,"
    "brk,"
    "access,"

    "mmap,"
    "mmap2,"

    "set_thread_area,"
    "time,"
    "gettimeofday,"
    "clock_gettime,"

    "open,"

    "stat,"
    "stat64,"
    "fstat,"
    "fstat64,"

    "close,"
    "read,"
    "mprotect,"
    "arch_prctl,"
    "munmap,"
    "set_tid_address,"
    "set_robust_list,"
    "futex,"
    "rt_sigaction,"
    "rt_sigprocmask,"

    "getrlimit,"
    "ugetrlimit,"

    "clone,"
    "openat,"
    "exit,"
    "exit_group,"
    "restart_syscall,"

    /* Not common */
    "pipe2,"
    "pause,"

    "lseek,"
    "_llseek,"

    "prctl,"
    "eventfd2,"

    "fcntl,"
    "fcntl64,"

    "epoll_create1,"
    "epoll_ctl,"
    "epoll_wait,"
    "readlink,"
    "clock_nanosleep,"
    "write,"
    "umask,"
    "uname,"
    "mkdir,"
    "poll,"

    "ftruncate,"
    "ftruncate64,"

    "writev,"

    "getdents,"
    "getdents64,"

    "statfs,"

    "pwrite64,"

    "getuid,"
    "getuid32,"

    "geteuid,"
    "geteuid32,"

    "kill,"
    "rt_sigtimedwait,"
    "unlink,"

    "getgid,"
    "getgid32,"

    "getegid,"
    "getegid32,"

    "fchown,"
    "fchown32,"

    "madvise,"
    "ioctl,"
    "pread64,"
    "sched_yield,"
    "fchmod,"

    "lstat,"
    "lstat64,"

    "socketcall,"
    "connect,"
    "socket,"
    "getpeername,"
    "setsockopt,"
    "getsockopt,"
    "getsockname,"
    "recvfrom,"
    "recvmsg,"
    "sendto,"
    "sendmsg,"
    "shutdown,"

    "tgkill,"

    "rt_sigreturn,"
    "sigreturn,"

    "ppoll,"

    "timerfd_create,"
    "timerfd_settime,"

    "sigaltstack,"
    "gettid,"

    "sched_setscheduler"

#if 0
    "clock_gettime,"
    "dup2,"
    "dup3,"
    "execveat,"
    "mincore,"
    "wait4,"

    /* Debugging related system calls */
    "gettid,"
    "getpid,"
    "nanosleep,"
    "sched_getaffinity,"
    "setrlimit,"
    "sigaltstack,"

    /* Apitrace related system calls */
    "dup,"

    /* Valgrind related system calls */
    "getcwd,"
    "getppid,"
    "gettimeofday,"
    "getxattr,"
    "mknod,"
    "pipe,"
    "pread64,"
    "time,"
    "tkill,"
#endif
    ;
