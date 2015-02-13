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
#define _POSIX_C_SOURCE 200112L

#include "linted/asynch.h"
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"
#include "linted/window.h"
#include "linted/xcb.h"

#include <errno.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <xcb/xcb.h>

enum { ON_POLL_CONN, ON_SENT_NOTICE, MAX_TASKS };

struct poll_conn_data
{
	xcb_connection_t *connection;
	struct linted_asynch_pool *pool;
	bool *time_to_quit;
};

struct notice_data
{
	struct linted_asynch_pool *pool;
};

static unsigned char window_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_sent_notice(struct linted_asynch_task *task);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-window", .start = window_start};

static uint32_t const window_opts[] = {0};

static unsigned char window_start(char const *process_name, size_t argc,
                                  char const *const argv[])
{
	linted_error errnum = 0;

	if (argc < 4U) {
		linted_log(LINTED_LOG_ERROR, "missing some of 3 file operands");
		return EXIT_FAILURE;
	}

	char const *kill_ko_path = argv[1U];
	char const *window_path = argv[2U];
	char const *gui_notifier_path = argv[3U];
	char const *drawer_notifier_path = argv[4U];

	linted_ko kill_ko;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, kill_ko_path,
		                        LINTED_KO_WRONLY);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_ko_open: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		kill_ko = xx;
	}

	linted_ko window_ko;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, window_path,
		                        LINTED_KO_WRONLY);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_ko_open: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		window_ko = xx;
	}

	linted_ko gui_notifier;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, gui_notifier_path,
		                        LINTED_KO_WRONLY);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_ko_open: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		gui_notifier = xx;
	}

	linted_ko drawer_notifier;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD,
		                        drawer_notifier_path, LINTED_KO_WRONLY);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR, "linted_ko_open: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		drawer_notifier = xx;
	}

	char const *root;
	{
		char *xx;
		errnum = linted_environment_get("MANAGERPID", &xx);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}
		root = xx;
	}
	if (0 == root) {
		fprintf(stderr, "%s: need MANAGERPID\n", process_name);
		return EXIT_FAILURE;
	}

	errno = 0;
	long int root_pid = strtol(root, 0, 10);
	errnum = errno;
	if (0 == errnum) {
		if (root_pid < 1) {
			errnum = ERANGE;
		}
		if (root_pid > INT_MAX) {
			errnum = ERANGE;
		}
	}
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "strtol: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	struct notice_data gui_notice_data;
	struct notice_data drawer_notice_data;
	struct poll_conn_data poll_conn_data;

	struct linted_io_task_poll *poll_conn_task;
	struct linted_window_task_notify *gui_notice_task;
	struct linted_window_task_notify *drawer_notice_task;

	{
		struct linted_window_task_notify *xx;
		errnum =
		    linted_window_task_notify_create(&xx, &gui_notice_data);
		if (errnum != 0)
			goto destroy_pool;
		gui_notice_task = xx;
	}

	{
		struct linted_window_task_notify *xx;
		errnum =
		    linted_window_task_notify_create(&xx, &drawer_notice_data);
		if (errnum != 0)
			goto destroy_pool;
		drawer_notice_task = xx;
	}

	{
		struct linted_io_task_poll *xx;
		errnum = linted_io_task_poll_create(&xx, &poll_conn_data);
		if (errnum != 0)
			goto destroy_pool;
		poll_conn_task = xx;
	}

	/* Open the connection to the X server */
	unsigned screen_number;
	xcb_connection_t *connection;
	{
		int xx;
		connection = xcb_connect(0, &xx);
		if (0 == connection) {
			errnum = LINTED_ERROR_UNIMPLEMENTED;
			goto destroy_pool;
		}
		screen_number = (unsigned)xx;
	}

	xcb_screen_t *screen = 0;
	{
		xcb_screen_iterator_t iter =
		    xcb_setup_roots_iterator(xcb_get_setup(connection));
		for (size_t ii = 0U; ii < screen_number; ++ii) {
			if (0 == iter.rem)
				break;

			xcb_screen_next(&iter);
		}

		if (0 == iter.rem) {
			errnum = EINVAL;
			goto close_display;
		}

		screen = iter.data;
	}

	xcb_window_t window = xcb_generate_id(connection);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto close_display;

	xcb_void_cookie_t create_win_ck = xcb_create_window_checked(
	    connection, XCB_COPY_FROM_PARENT, window, screen->root, 0, 0, 640,
	    480, 0, XCB_WINDOW_CLASS_INPUT_OUTPUT, screen->root_visual, 0,
	    window_opts);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto close_display;

	xcb_intern_atom_cookie_t protocols_ck = xcb_intern_atom(
	    connection, 1, strlen("WM_PROTOCOLS"), "WM_PROTOCOLS");
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t delete_ck = xcb_intern_atom(
	    connection, 0, strlen("WM_DELETE_WINDOW"), "WM_DELETE_WINDOW");
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t pid_ck = xcb_intern_atom(
	    connection, 0, strlen("_NET_WM_PID"), "_NET_WM_PID");
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_atom_t wm_protocols_atom;
	{
		xcb_intern_atom_reply_t *proto_reply;
		xcb_generic_error_t *proto_err;
		{
			xcb_generic_error_t *xx = 0;
			proto_reply = xcb_intern_atom_reply(connection,
			                                    protocols_ck, &xx);
			proto_err = xx;
		}
		errnum = linted_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (proto_err != 0) {
			errnum = linted_xcb_error(proto_err);
			linted_mem_free(proto_err);
			goto destroy_window;
		}

		wm_protocols_atom = proto_reply->atom;
		linted_mem_free(proto_reply);
	}

	xcb_atom_t wm_delete_window_atom;
	{
		xcb_intern_atom_reply_t *delete_ck_reply;
		xcb_generic_error_t *delete_ck_err;
		{
			xcb_generic_error_t *xx = 0;
			delete_ck_reply =
			    xcb_intern_atom_reply(connection, delete_ck, &xx);
			delete_ck_err = xx;
		}
		errnum = linted_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (delete_ck_err != 0) {
			errnum = linted_xcb_error(delete_ck_err);
			linted_mem_free(delete_ck_err);
			goto destroy_window;
		}

		wm_delete_window_atom = delete_ck_reply->atom;
		linted_mem_free(delete_ck_reply);
	}

	{
		xcb_atom_t xx = wm_delete_window_atom;
		xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
		                    wm_protocols_atom, XCB_ATOM_ATOM, 32, 1U,
		                    &xx);
	}
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_atom_t net_wm_pid_atom;
	{
		xcb_intern_atom_reply_t *pid_ck_reply;
		xcb_generic_error_t *pid_ck_err;
		{
			xcb_generic_error_t *xx = 0;
			pid_ck_reply =
			    xcb_intern_atom_reply(connection, pid_ck, &xx);
			pid_ck_err = xx;
		}
		errnum = linted_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (pid_ck_err != 0) {
			errnum = linted_xcb_error(pid_ck_err);
			linted_mem_free(pid_ck_err);
			goto destroy_window;
		}

		net_wm_pid_atom = pid_ck_reply->atom;
		linted_mem_free(pid_ck_reply);
	}

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_CLASS, XCB_ATOM_STRING, 8,
	                    sizeof PACKAGE_TARNAME + sizeof PACKAGE_NAME,
	                    PACKAGE_TARNAME "\0" PACKAGE_NAME);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8,
	                    strlen(PACKAGE_NAME), PACKAGE_NAME);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_ICON_NAME, XCB_ATOM_STRING, 8,
	                    strlen(PACKAGE_NAME), PACKAGE_NAME);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	{
		char buf[HOST_NAME_MAX + 1U];
		if (-1 == gethostname(buf, sizeof buf))
			goto get_hostname_failed;

		xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
		                    XCB_ATOM_WM_CLIENT_MACHINE, XCB_ATOM_STRING,
		                    8, strlen(buf), buf);
		goto get_hostname_succeeded;
	}

get_hostname_failed:
	errnum = errno;
	LINTED_ASSUME(errnum != 0);
	goto destroy_window;

get_hostname_succeeded:
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	{
		uint32_t xx = root_pid;
		xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
		                    net_wm_pid_atom, XCB_ATOM_CARDINAL, 32, 1,
		                    &xx);
	}
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_COMMAND, XCB_ATOM_STRING, 8,
	                    strlen(PACKAGE_TARNAME), PACKAGE_TARNAME);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_map_window(connection, window);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_generic_error_t *create_win_err =
	    xcb_request_check(connection, create_win_ck);
	if (create_win_err != 0) {
		errnum = linted_xcb_error(create_win_err);
		linted_mem_free(create_win_err);
		goto destroy_window;
	}

	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_flush(connection);

	bool time_to_quit;

	gui_notice_data.pool = pool;
	drawer_notice_data.pool = pool;

	poll_conn_data.time_to_quit = &time_to_quit;
	poll_conn_data.pool = pool;
	poll_conn_data.connection = connection;

	errnum = linted_window_write(window_ko, window);
	if (errnum != 0)
		goto destroy_window;

	linted_window_task_notify_prepare(gui_notice_task, ON_SENT_NOTICE,
	                                  gui_notifier);
	linted_asynch_pool_submit(
	    pool, linted_window_task_notify_to_asynch(gui_notice_task));

	linted_window_task_notify_prepare(drawer_notice_task, ON_SENT_NOTICE,
	                                  drawer_notifier);
	linted_asynch_pool_submit(
	    pool, linted_window_task_notify_to_asynch(drawer_notice_task));

	linted_io_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	linted_asynch_pool_submit(
	    pool, linted_io_task_poll_to_asynch(poll_conn_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		time_to_quit = false;

		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			errnum = linted_asynch_pool_wait(pool, &xx);
			if (errnum != 0)
				goto stop_pool;
			completed_task = xx;
		}
		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto stop_pool;

		if (time_to_quit)
			goto stop_pool;
	}

stop_pool:
	linted_asynch_task_cancel(
	    linted_window_task_notify_to_asynch(gui_notice_task));
	linted_asynch_task_cancel(
	    linted_window_task_notify_to_asynch(drawer_notice_task));
	linted_asynch_task_cancel(
	    linted_io_task_poll_to_asynch(poll_conn_task));

	for (;;) {
		struct linted_asynch_task *completed_task;
		linted_error poll_errnum;
		{
			struct linted_asynch_task *xx;
			poll_errnum = linted_asynch_pool_poll(pool, &xx);
			if (LINTED_ERROR_AGAIN == poll_errnum)
				break;
			completed_task = xx;
		}

		linted_error dispatch_errnum =
		    linted_asynch_task_errnum(completed_task);
		if (0 == errnum && dispatch_errnum != LINTED_ERROR_CANCELLED)
			errnum = dispatch_errnum;
	}

destroy_window : {
	xcb_void_cookie_t destroy_ck =
	    xcb_destroy_window_checked(connection, window);
	if (0 == errnum)
		errnum = linted_xcb_conn_error(connection);

	xcb_generic_error_t *destroy_err =
	    xcb_request_check(connection, destroy_ck);
	if (0 == errnum)
		errnum = linted_xcb_conn_error(connection);
	if (0 == errnum && destroy_err != 0)
		errnum = linted_xcb_error(destroy_err);
	linted_mem_free(destroy_err);
}

close_display:
	xcb_disconnect(connection);

destroy_pool : {
	linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
	if (0 == errnum)
		errnum = destroy_errnum;
}
	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)gui_notice_task;
	(void)drawer_notice_task;
	(void)poll_conn_task;

	/* Tell the manager to exit everything */
	if (0 == errnum) {
		char const dummy = 0U;
		errnum = linted_io_write_all(kill_ko, 0, &dummy, sizeof dummy);
		if (errnum != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_io_task_write_all: %s",
			           linted_error_string(errnum));
			return EXIT_FAILURE;
		}

		/* Wait for the manager to exit everything */
		for (;;)
			pause();
	}

	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "%s", linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	return errnum;
}

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (linted_asynch_task_action(task)) {
	case ON_POLL_CONN:
		return on_poll_conn(task);

	case ON_SENT_NOTICE:
		return on_sent_notice(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_poll_conn(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	struct linted_io_task_poll *poll_conn_task =
	    linted_io_task_poll_from_asynch(task);
	struct poll_conn_data *poll_conn_data =
	    linted_io_task_poll_data(poll_conn_task);

	xcb_connection_t *connection = poll_conn_data->connection;
	struct linted_asynch_pool *pool = poll_conn_data->pool;
	bool *time_to_quitp = poll_conn_data->time_to_quit;

	for (;;) {
		xcb_generic_event_t *event = xcb_poll_for_event(connection);
		if (0 == event)
			break;

		bool time_to_quit = false;
		switch (event->response_type & ~0x80) {
		case XCB_CLIENT_MESSAGE:
			goto quit_application;

		default:
			/* Unknown event type, ignore it */
			break;

		quit_application:
			time_to_quit = true;
			break;
		}
		linted_mem_free(event);

		*time_to_quitp = time_to_quit;
		if (time_to_quit)
			return 0;
	}

	poll_conn_data->time_to_quit = time_to_quitp;
	poll_conn_data->pool = pool;
	poll_conn_data->connection = connection;

	linted_io_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_sent_notice(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	return 0;
}
