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

#include "linted/admin.h"
#include "linted/async.h"
#include "linted/dir.h"
#include "linted/env.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/fifo.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/path.h"
#include "linted/pid.h"
#include "linted/ptrace.h"
#include "linted/sched.h"
#include "linted/signal.h"
#include "linted/spawn.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

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
       LINTED_STARTUP,
       LINTED_SANDBOX,
       LINTED_WAITER,
};

static char const *const required_envs[] =
    {[MANAGERPID] = "MANAGERPID", [LINTED_STARTUP] = "LINTED_STARTUP",
     [LINTED_SANDBOX] = "LINTED_SANDBOX",
     [LINTED_WAITER] = "LINTED_WAITER"};

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
	struct linted_admin_in_task_recv *read_task;
	struct linted_admin_out_task_send *write_task;
	struct linted_signal_task_wait *signal_wait_task;
	struct linted_io_task_read *kill_read_task;
	struct linted_async_pool *pool;
	struct linted_unit_db *unit_db;
	linted_pid manager_pid;
	linted_ko kill_fifo;
	linted_admin_in admin_in;
	linted_admin_out admin_out;
	linted_ko cwd;
	bool time_to_exit : 1U;
};

static linted_error
monitor_init(struct monitor *monitor, linted_ko admin_in,
             linted_ko admin_out, linted_ko kill_fifo, linted_ko cwd,
             linted_pid manager_pid, struct linted_async_pool *pool,
             char const *process_name, char const *startup,
             char const *sandbox, char const *waiter);

static linted_error monitor_destroy(struct monitor *monitor);

static linted_error monitor_start(struct monitor *monitor);
static linted_error monitor_stop(struct monitor *monitor);

static linted_error dispatch(struct monitor *monitor,
                             union linted_async_ck task_ck,
                             void *userstate, linted_error err);

static linted_error
monitor_on_signal(struct monitor *monitor,
                  struct linted_signal_task_wait *signal_wait_task,
                  linted_error err);
static linted_error
monitor_on_admin_in_read(struct monitor *monitor,
                         struct linted_admin_in_task_recv *read_task,
                         linted_error err);
static linted_error monitor_on_admin_out_write(
    struct monitor *monitor,
    struct linted_admin_out_task_send *write_task, linted_error err);
static linted_error
monitor_on_kill_read(struct monitor *monitor,
                     struct linted_io_task_read *kill_read_task,
                     linted_error err);

static linted_error on_sigchld(struct monitor *monitor);
static linted_error on_death_sig(struct monitor *monitor, int signo);

static linted_error
on_add_unit(struct monitor *monitor,
            struct linted_admin_request_add_unit const *request,
            struct linted_admin_reply_add_unit *reply);
static linted_error
on_add_socket(struct monitor *monitor,
              struct linted_admin_request_add_socket const *request,
              struct linted_admin_reply_add_socket *reply);

static linted_error
on_status_request(linted_pid manager_pid,
                  struct linted_admin_request_status const *request,
                  struct linted_admin_reply_status *reply);
static linted_error
on_stop_request(linted_pid manager_pid,
                struct linted_admin_request_stop const *request,
                struct linted_admin_reply_stop *reply);

static linted_error on_child_stopped(char const *process_name,
                                     linted_pid pid);

static linted_error on_child_trapped(struct monitor *monitor,
                                     linted_pid pid, int exit_status,
                                     struct linted_unit_db *unit_db);
static linted_error on_child_signaled(char const *process_name,
                                      linted_pid pid, int exit_status);
static linted_error on_child_about_to_clone(linted_pid pid);
static linted_error
on_child_about_to_exit(struct monitor *monitor, bool time_to_exit,
                       linted_pid pid, struct linted_unit_db *unit_db);
static linted_error
on_child_ptrace_event_stopped(char const *process_name, linted_pid pid,
                              int exit_status);

static linted_error service_activate(struct monitor *monitor,
                                     struct linted_unit *unit,
                                     bool check);
linted_error socket_activate(struct linted_unit_socket *unit);

static size_t null_list_size(char const *const *list);

static linted_error service_children_terminate(linted_pid pid);

static linted_error pid_is_child_of(linted_pid parent, linted_pid child,
                                    bool *isp);
static linted_error dup_array(char const *const *strs, size_t strs_size,
                              char ***strsp);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor"};

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	char const *manager_pid_str;
	char const *startup;
	char const *sandbox;
	char const *waiter;
	{
		char *envs[LINTED_ARRAY_SIZE(required_envs)];
		for (size_t ii = 0U;
		     ii < LINTED_ARRAY_SIZE(required_envs); ++ii) {
			char const *req = required_envs[ii];
			char *value;
			{
				char *xx;
				err = linted_env_get(req, &xx);
				if (err != 0) {
					linted_log(
					    LINTED_LOG_ERROR,
					    "linted_env_get: %s",
					    linted_error_string(err));
					return EXIT_FAILURE;
				}
				value = xx;
			}
			if (0 == value) {
				linted_log(LINTED_LOG_ERROR,
				           "%s is a required "
				           "environment variable",
				           req);
				return EXIT_FAILURE;
			}
			envs[ii] = value;
		}
		manager_pid_str = envs[MANAGERPID];
		startup = envs[LINTED_STARTUP];
		sandbox = envs[LINTED_SANDBOX];
		waiter = envs[LINTED_WAITER];
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
	                   startup, sandbox, waiter);
	if (err != 0)
		goto destroy_pool;

	err = monitor_start(&monitor);
	if (err != 0)
		goto destroy_monitor;

	for (;;) {
		struct linted_async_result result;
		{
			struct linted_async_result xx;
			linted_async_pool_wait(pool, &xx);
			result = xx;
		}

		err = dispatch(&monitor, result.task_ck,
		               result.userstate, result.err);
		if (err != 0)
			goto cancel_tasks;
	}

cancel_tasks:
	if (LINTED_ERROR_CANCELLED == err)
		err = 0;

	monitor_stop(&monitor);

	for (;;) {
		struct linted_async_result result;
		{
			struct linted_async_result xx;
			if (LINTED_ERROR_AGAIN ==
			    linted_async_pool_poll(pool, &xx))
				break;
			result = xx;
		}

		linted_error dispatch_error =
		    dispatch(&monitor, result.task_ck, result.userstate,
		             result.err);
		if (0 == err)
			err = dispatch_error;
	}
	if (LINTED_ERROR_CANCELLED == err)
		err = 0;

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
             char const *process_name, char const *startup,
             char const *sandbox, char const *waiter)
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

	linted_async_pool_submit(
	    pool, linted_signal_task_wait_prepare(
	              signal_wait_task,
	              (union linted_async_ck){.u64 = SIGNAL_WAIT},
	              signal_wait_task));

	linted_async_pool_submit(
	    pool, linted_admin_in_task_recv_prepare(
	              admin_in_read_task,
	              (union linted_async_ck){.u64 = ADMIN_IN_READ},
	              admin_in_read_task, admin_in));

	static char dummy;

	linted_async_pool_submit(
	    pool, linted_io_task_read_prepare(
	              kill_read_task,
	              (union linted_async_ck){.u64 = KILL_READ},
	              kill_read_task, kill_fifo, &dummy, sizeof dummy));

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

	{
		char const *const arguments[] = {startup, "admin-in",
		                                 "admin-out", 0};
		err = linted_spawn(0, cwd, startup, 0, attr, arguments,
		                   0);
		if (err != 0)
			return err;
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
                             union linted_async_ck task_ck,
                             void *userstate, linted_error err)
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
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error
monitor_on_signal(struct monitor *monitor,
                  struct linted_signal_task_wait *signal_wait_task,
                  linted_error err)
{
	struct linted_async_pool *pool = monitor->pool;

	if (LINTED_ERROR_CANCELLED == err)
		return 0;
	if (err != 0)
		return err;

	int signo = linted_signal_task_wait_signo(signal_wait_task);

	linted_async_pool_submit(
	    pool, linted_signal_task_wait_prepare(
	              signal_wait_task,
	              (union linted_async_ck){.u64 = SIGNAL_WAIT},
	              signal_wait_task));

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

static linted_error monitor_on_admin_in_read(
    struct monitor *monitor,
    struct linted_admin_in_task_recv *admin_in_read_task,
    linted_error err)
{

	struct linted_async_pool *pool = monitor->pool;
	struct linted_admin_out_task_send *write_task =
	    monitor->write_task;
	linted_pid manager_pid = monitor->manager_pid;
	linted_ko admin_out = monitor->admin_out;

	/* Assume the other end did something bad and don't exit with
	 * an error. */
	if (err != 0)
		return 0;

	struct linted_admin_request *request;
	{
		struct linted_admin_request *xx;
		err = linted_admin_in_task_recv_request(
		    &xx, admin_in_read_task);
		if (err != 0)
			return err;
		request = xx;
	}

	linted_admin_type type = request->type;

	struct linted_admin_reply reply;
	reply.type = type;
	switch (type) {
	case LINTED_ADMIN_ADD_UNIT: {
		struct linted_admin_reply_add_unit yy = {0};
		err = on_add_unit(
		    monitor, &request->linted_admin_request_u.add_unit,
		    &yy);
		reply.linted_admin_reply_u.add_unit = yy;
		break;
	}

	case LINTED_ADMIN_ADD_SOCKET: {
		struct linted_admin_reply_add_socket yy = {0};
		err = on_add_socket(
		    monitor,
		    &request->linted_admin_request_u.add_socket, &yy);
		reply.linted_admin_reply_u.add_socket = yy;
		break;
	}

	case LINTED_ADMIN_STATUS: {
		struct linted_admin_reply_status yy = {0};
		err = on_status_request(
		    manager_pid,
		    &request->linted_admin_request_u.status, &yy);
		reply.linted_admin_reply_u.status = yy;
		break;
	}

	case LINTED_ADMIN_STOP: {
		struct linted_admin_reply_stop yy = {0};
		err = on_stop_request(
		    manager_pid, &request->linted_admin_request_u.stop,
		    &yy);
		reply.linted_admin_reply_u.stop = yy;
		break;
	}

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
	linted_admin_request_free(request);

	{
		struct linted_admin_reply xx = reply;

		linted_async_pool_submit(
		    pool,
		    linted_admin_out_task_send_prepare(
		        write_task,
		        (union linted_async_ck){.u64 = ADMIN_OUT_WRITE},
		        write_task, admin_out, &xx));
	}

	return err;
}

static linted_error monitor_on_admin_out_write(
    struct monitor *monitor,
    struct linted_admin_out_task_send *write_task, linted_error err)
{
	struct linted_async_pool *pool = monitor->pool;
	struct linted_admin_in_task_recv *read_task =
	    monitor->read_task;
	linted_admin_in admin_in = monitor->admin_in;

	if (err != 0)
		return 0;

	linted_async_pool_submit(
	    pool, linted_admin_in_task_recv_prepare(
	              read_task,
	              (union linted_async_ck){.u64 = ADMIN_IN_READ},
	              read_task, admin_in));

	return 0;
}

static linted_error
monitor_on_kill_read(struct monitor *monitor,
                     struct linted_io_task_read *kill_read_task,
                     linted_error err)
{
	struct linted_async_pool *pool = monitor->pool;
	struct linted_unit_db *unit_db = monitor->unit_db;
	linted_pid manager_pid = monitor->manager_pid;
	linted_ko kill_fifo = monitor->kill_fifo;

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

	static char dummy;
	linted_async_pool_submit(
	    pool, linted_io_task_read_prepare(
	              kill_read_task,
	              (union linted_async_ck){.u64 = KILL_READ},
	              kill_read_task, kill_fifo, &dummy, sizeof dummy));
	return 0;
}

static linted_error on_sigchld(struct monitor *monitor)
{
	char const *process_name = monitor->process_name;
	struct linted_unit_db *unit_db = monitor->unit_db;

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
			err = on_child_trapped(monitor, pid,
			                       exit_status, unit_db);
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

static linted_error on_child_trapped(struct monitor *monitor,
                                     linted_pid pid, int exit_status,
                                     struct linted_unit_db *unit_db)
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

	linted_error kill_err = linted_pid_continue(pid);
	if (0 == err)
		err = kill_err;

	return err;
}

static linted_error on_child_signaled(char const *process_name,
                                      linted_pid pid, int signo)
{
	linted_error err = 0;

	bool is_sigchld = SIGCHLD == signo;

	siginfo_t info = {0};
	if (is_sigchld) {
		err = linted_ptrace_getsiginfo(pid, &info);
	}

	linted_error cont_err = linted_ptrace_cont(pid, signo);
	if (0 == err)
		err = cont_err;

	if (err != 0)
		return err;

	if (!is_sigchld)
		return 0;

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

	return 0;
}

static linted_error
on_child_ptrace_event_stopped(char const *process_name, linted_pid pid,
                              int exit_status)
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
on_child_about_to_exit(struct monitor *monitor, bool time_to_exit,
                       linted_pid pid, struct linted_unit_db *unit_db)
{

	linted_error err = 0;
	struct linted_unit *unit = 0;

	char const *process_name = monitor->process_name;

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
            struct linted_admin_request_add_unit const *request,
            struct linted_admin_reply_add_unit *reply)
{
	linted_error err = 0;

	struct linted_unit_db *unit_db = monitor->unit_db;

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

	bool has_timer_slack_nsec = timer_slack_nsec != 0;
	bool has_priority = priority != 0;
	bool has_limit_no_file = limit_no_file != 0;
	bool has_limit_msgqueue = limit_msgqueue != 0;
	bool has_limit_locks = limit_locks != 0;

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
			goto free_name;
		fstab = xx;
	}

	char *chdir_path;
	if (0 == strcmp("", unit_chdir_path)) {
		chdir_path = 0;
	} else {
		char *xx;
		err = linted_str_dup(&xx, unit_chdir_path);
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

	struct linted_unit *unit;
	{
		struct linted_unit *xx;
		err = linted_unit_db_add_unit(unit_db, &xx);
		if (err != 0)
			goto free_environment;
		unit = xx;
	}

	unit->type = LINTED_UNIT_TYPE_SERVICE;
	unit->name = name;

	struct linted_unit_service *unit_service =
	    &unit->linted_unit_u.service;

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

	/* These aren't fully implemented yet */
	unit_service->has_priority = has_priority;
	unit_service->has_timer_slack_nsec = has_timer_slack_nsec;
	unit_service->has_limit_no_file = has_limit_no_file;
	unit_service->has_limit_locks = has_limit_locks;
	unit_service->has_limit_msgqueue = has_limit_msgqueue;

	unit_service->clone_newuser = clone_newuser;
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
		linted_mem_free(environment[ii]);
	linted_mem_free(environment);

free_command:
	for (size_t ii = 0U; ii < command_size; ++ii)
		linted_mem_free(command[ii]);
	linted_mem_free(command);

free_chdir_path:
	linted_mem_free(chdir_path);

free_fstab:
	linted_mem_free(fstab);

free_name:
	linted_mem_free(name);

	return err;
}

static linted_error
on_add_socket(struct monitor *monitor,
              struct linted_admin_request_add_socket const *request,
              struct linted_admin_reply_add_socket *reply)
{
	linted_error err = 0;

	struct linted_unit_db *unit_db = monitor->unit_db;

	char const *unit_name = request->name;
	char const *unit_path = request->path;
	int32_t fifo_size = request->fifo_size;
	linted_unit_socket_type type = request->sock_type;

	char *name;
	{
		char *xx;
		err = linted_str_dup(&xx, unit_name);
		if (err != 0)
			return err;
		name = xx;
	}

	char *path;
	{
		char *xx;
		err = linted_str_dup(&xx, unit_path);
		if (err != 0)
			goto free_name;
		path = xx;
	}

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

	unit->type = LINTED_UNIT_TYPE_SOCKET;
	unit->name = name;

	struct linted_unit_socket *unit_socket =
	    &unit->linted_unit_u.socket;

	unit_socket->path = path;
	unit_socket->fifo_size = fifo_size;
	unit_socket->type = type;

	err = socket_activate(unit_socket);

	return err;

free_name:
	linted_mem_free(name);

	return err;
}

static linted_error
on_status_request(linted_pid manager_pid,
                  struct linted_admin_request_status const *request,
                  struct linted_admin_reply_status *reply)
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

	reply->is_up = is_up;
	return 0;
}

static linted_error
on_stop_request(linted_pid manager_pid,
                struct linted_admin_request_stop const *request,
                struct linted_admin_reply_stop *reply)
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

	struct linted_unit_service *unit_service =
	    &unit->linted_unit_u.service;

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
	char const *const *environment = unit_service->environment;
	bool no_new_privs = unit_service->no_new_privs;
	char const *fstab = unit_service->fstab;
	char const *chdir_path = unit_service->chdir_path;

	bool has_timer_slack_nsec = unit_service->has_timer_slack_nsec;
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

	int_least64_t timer_slack_nsec;
	if (has_timer_slack_nsec)
		timer_slack_nsec = unit_service->timer_slack_nsec;

	linted_sched_priority priority;
	if (has_priority)
		priority = unit_service->priority;

	int_least64_t limit_no_file;
	if (has_limit_no_file)
		limit_no_file = unit_service->limit_no_file;

	int_least64_t limit_msgqueue;
	if (has_limit_msgqueue)
		limit_msgqueue = unit_service->limit_msgqueue;

	int_least64_t limit_locks;
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
	char *limit_msgqueue_str = 0;
	char *limit_locks_str = 0;

	char *timer_slack_str = 0;
	char *prio_str = 0;
	char *chrootdir = 0;
	char *sandbox_base = 0;

	if (has_limit_no_file) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)limit_no_file);
		if (err != 0)
			return err;
		limit_no_file_str = xx;
	}

	if (has_limit_msgqueue) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)limit_msgqueue);
		if (err != 0)
			goto free_strs;
		limit_msgqueue_str = xx;
	}

	if (has_limit_locks) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)limit_locks);
		if (err != 0)
			goto free_strs;
		limit_locks_str = xx;
	}

	if (has_timer_slack_nsec) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)timer_slack_nsec);
		if (err != 0)
			goto free_strs;
		timer_slack_str = xx;
	}

	if (has_priority) {
		char *xx;
		err = linted_str_format(&xx, "%" PRIiMAX,
		                        (intmax_t)priority);
		if (err != 0)
			goto free_strs;
		prio_str = xx;
	}

	{
		char *xx;
		err = linted_str_format(&xx, "%s/chroot", unit_name);
		if (err != 0)
			goto free_strs;
		chrootdir = xx;
	}

	{
		char *xx;
		err = linted_path_base(&xx, sandbox);
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
		    {"--dropcaps", 0, drop_caps},
		    {"--chdir", chdir_path, chdir_path != 0},
		    {"--timer-slack", timer_slack_str,
		     has_timer_slack_nsec},
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
				goto free_strs;
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

	err = linted_spawn(0, cwd, sandbox, 0, attr, args, environment);

	linted_spawn_attr_destroy(attr);

free_args:
	linted_mem_free(args);

free_strs:
	linted_mem_free(sandbox_base);
	linted_mem_free(chrootdir);
	linted_mem_free(prio_str);
	linted_mem_free(timer_slack_str);
	linted_mem_free(limit_locks_str);
	linted_mem_free(limit_msgqueue_str);
	linted_mem_free(limit_no_file_str);

	return err;
}

linted_error socket_activate(struct linted_unit_socket *unit)
{
	linted_error err = 0;

	linted_unit_type type = unit->type;
	char const *path = unit->path;

	switch (type) {
	case LINTED_UNIT_SOCKET_TYPE_DIR:
		err = linted_dir_create(0, LINTED_KO_CWD, path, 0U,
		                        S_IRWXU);
		break;

	case LINTED_UNIT_SOCKET_TYPE_FILE:
		err = linted_file_create(0, LINTED_KO_CWD, path, 0U,
		                         S_IRUSR | S_IWUSR);
		break;

	case LINTED_UNIT_SOCKET_TYPE_FIFO: {
		int_least32_t fifo_size = unit->fifo_size;

#if defined F_SETPIPE_SZ
		if (fifo_size >= 0) {
			linted_ko fifo;
			{
				linted_ko xx;
				err = linted_fifo_create(
				    &xx, LINTED_KO_CWD, path,
				    LINTED_FIFO_RDWR,
				    S_IRUSR | S_IWUSR);
				if (err != 0)
					return err;
				fifo = xx;
			}

			if (-1 ==
			    fcntl(fifo, F_SETPIPE_SZ, fifo_size)) {
				err = errno;
				LINTED_ASSUME(err != 0);
			}

			linted_error close_err = linted_ko_close(fifo);
			if (0 == err)
				err = close_err;
		} else
#endif
		{
			err = linted_fifo_create(0, LINTED_KO_CWD, path,
			                         0U, S_IRUSR | S_IWUSR);
		}

		break;
	}
	}

	return err;
}

static linted_error dup_array(char const *const *strs, size_t strs_size,
                              char ***strsp)
{
	linted_error err = 0;

	char **new_strs;
	{
		void *xx;
		err = linted_mem_alloc_array(&xx, strs_size + 1U,
		                             sizeof new_strs[0U]);
		if (err != 0)
			return err;
		new_strs = xx;
	}

	size_t ii = 0U;
	for (; ii < strs_size; ++ii) {
		err = linted_str_dup(&new_strs[ii], strs[ii]);
		if (err != 0)
			goto free_new;
	}
	new_strs[ii] = 0;

	*strsp = new_strs;

	return 0;

free_new:
	for (size_t jj = 0U; jj < ii; ++jj)
		linted_mem_free(new_strs[jj]);
	linted_mem_free(new_strs);

	return err;
}
