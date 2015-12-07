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
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "lntd/async.h"
#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/start.h"
#include "lntd/util.h"
#include "lntd/window.h"
#include "lntd/xcb.h"

#include <errno.h>
#include <limits.h>
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <xcb/xcb.h>

enum { ON_POLL_CONN, ON_SENT_NOTICE };
#define MAX_TASKS 3U

struct window;

static lntd_error
window_init(struct window *window, struct lntd_async_pool *pool,
            char const *kill_ko_path, char const *window_path,
            char const *gui_notifier_path,
            char const *drawer_notifier_path, pid_t root_pid);
static lntd_error window_destroy(struct window *window);
static lntd_error window_stop(struct window *window);

static lntd_error dispatch(struct window *window,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_error err);
static lntd_error
window_on_conn_ready(struct window *window,
                     struct lntd_io_task_poll *poll_conn_task,
                     lntd_error err);
static lntd_error
window_on_notice_sent(struct window *window,
                      struct lntd_window_task_notify *notice_task,
                      lntd_error err);

struct window {
	xcb_connection_t *connection;
	struct lntd_async_pool *pool;
	struct lntd_window_task_notify *gui_notice_task;
	struct lntd_window_task_notify *drawer_notice_task;
	struct lntd_io_task_poll *poll_conn_task;
	lntd_ko kill_ko;
	lntd_ko window_ko;
	lntd_ko gui_notifier;
	lntd_ko drawer_notifier;
	xcb_window_t window_id;
	bool time_to_quit : 1U;
};

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-window", 0};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	if (argc < 4U) {
		lntd_log(LNTD_LOG_ERROR,
		         "missing some of 3 file operands");
		return EXIT_FAILURE;
	}

	char const *kill_ko_path = argv[1U];
	char const *window_path = argv[2U];
	char const *gui_notifier_path = argv[3U];
	char const *drawer_notifier_path = argv[4U];

	char const *root;
	{
		char *xx;
		err = lntd_env_get("MANAGERPID", &xx);
		if (err != 0)
			return err;
		root = xx;
	}
	if (0 == root) {
		lntd_log(LNTD_LOG_ERROR,
		         "%s is a required environment variable",
		         "MANAGERPID");
		return EXIT_FAILURE;
	}

	errno = 0;
	long int root_pid = strtol(root, 0, 10);
	err = errno;
	if (0 == err) {
		if (root_pid < 1) {
			err = ERANGE;
		}
		if (root_pid > INT_MAX) {
			err = ERANGE;
		}
	}
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "strtol: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
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

	static struct window window_obj = {0};

	err = window_init(&window_obj, pool, kill_ko_path, window_path,
	                  gui_notifier_path, drawer_notifier_path,
	                  root_pid);
	if (err != 0)
		goto destroy_pool;

	for (;;) {
		struct lntd_async_result result;
		{
			struct lntd_async_result xx;
			err = lntd_async_pool_wait(pool, &xx);
			if (err != 0)
				goto stop_pool;
			result = xx;
		}

		window_obj.time_to_quit = false;
		err = dispatch(&window_obj, result.task_ck,
		               result.userstate, result.err);
		if (err != 0)
			goto stop_pool;
		if (window_obj.time_to_quit)
			goto stop_pool;
	}

stop_pool:
	window_stop(&window_obj);

	for (;;) {
		struct lntd_async_result result;
		lntd_error poll_err;
		{
			struct lntd_async_result xx;
			poll_err = lntd_async_pool_poll(pool, &xx);
			if (LNTD_ERROR_AGAIN == poll_err)
				break;
			result = xx;
		}

		lntd_error dispatch_err = result.err;
		if (0 == err && dispatch_err != LNTD_ERROR_CANCELLED)
			err = dispatch_err;
	}

	/* Tell the manager to exit everything */
	if (0 == err) {
		static char const dummy = 0U;
		err = lntd_io_write_all(window_obj.kill_ko, 0, &dummy,
		                        sizeof dummy);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_io_task_write_all: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	window_destroy(&window_obj);

destroy_pool : {
	lntd_error destroy_err = lntd_async_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;
}
	if (0 == err) {
		/* Wait for the manager to exit everything */
		for (;;)
			pause();
	}

	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "%s", lntd_error_string(err));
		return EXIT_FAILURE;
	}

	return err;
}

static lntd_error
window_init(struct window *window, struct lntd_async_pool *pool,
            char const *kill_ko_path, char const *window_path,
            char const *gui_notifier_path,
            char const *drawer_notifier_path, pid_t root_pid)
{

	lntd_error err = 0;

	lntd_ko kill_ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, kill_ko_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			return err;
		kill_ko = xx;
	}

	lntd_ko window_ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, window_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			goto close_kill_ko;
		window_ko = xx;
	}

	lntd_ko gui_notifier;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, gui_notifier_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			goto close_window_ko;
		gui_notifier = xx;
	}

	lntd_ko drawer_notifier;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD,
		                   drawer_notifier_path, LNTD_KO_RDWR);
		if (err != 0)
			goto close_gui_notifier;
		drawer_notifier = xx;
	}

	struct lntd_window_task_notify *gui_notice_task;
	{
		struct lntd_window_task_notify *xx;
		err = lntd_window_task_notify_create(&xx, 0);
		if (err != 0)
			goto drawer_notifier;
		gui_notice_task = xx;
	}

	struct lntd_window_task_notify *drawer_notice_task;
	{
		struct lntd_window_task_notify *xx;
		err = lntd_window_task_notify_create(&xx, 0);
		if (err != 0)
			goto destroy_gui_notice_task;
		drawer_notice_task = xx;
	}

	struct lntd_io_task_poll *poll_conn_task;
	{
		struct lntd_io_task_poll *xx;
		err = lntd_io_task_poll_create(&xx, 0);
		if (err != 0)
			goto destroy_drawer_notice_task;
		poll_conn_task = xx;
	}

	/* Open the connection to the X server */
	unsigned screen_number;
	xcb_connection_t *connection;
	{
		int xx;
		connection = xcb_connect(0, &xx);
		if (0 == connection) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_poll_conn_task;
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto close_display;
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
			err = EINVAL;
			goto close_display;
		}

		screen = iter.data;
	}

	xcb_window_t window_id = xcb_generate_id(connection);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto close_display;

	xcb_void_cookie_t create_win_ck;
	{
		uint32_t value_mask =
		    XCB_CW_BACK_PIXMAP | XCB_CW_BACKING_STORE;
		uint32_t const window_opts[] = {
		    XCB_BACK_PIXMAP_NONE,
		    XCB_BACKING_STORE_WHEN_MAPPED};

		create_win_ck = xcb_create_window_checked(
		    connection, XCB_COPY_FROM_PARENT, window_id,
		    screen->root, 0, 0, 640, 480, 0,
		    XCB_WINDOW_CLASS_INPUT_OUTPUT, screen->root_visual,
		    value_mask, window_opts);
	}
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto close_display;

	xcb_intern_atom_cookie_t protocols_ck = xcb_intern_atom(
	    connection, 1, strlen("WM_PROTOCOLS"), "WM_PROTOCOLS");
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t delete_ck =
	    xcb_intern_atom(connection, 0, strlen("WM_DELETE_WINDOW"),
	                    "WM_DELETE_WINDOW");
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t pid_ck = xcb_intern_atom(
	    connection, 0, strlen("_NET_WM_PID"), "_NET_WM_PID");
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_atom_t wm_protocols_atom;
	{
		xcb_intern_atom_reply_t *proto_reply;
		xcb_generic_error_t *proto_err;
		{
			xcb_generic_error_t *xx = 0;
			proto_reply = xcb_intern_atom_reply(
			    connection, protocols_ck, &xx);
			proto_err = xx;
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		if (proto_err != 0) {
			err = lntd_xcb_error(proto_err);
			lntd_mem_free(proto_err);
			goto destroy_window;
		}

		wm_protocols_atom = proto_reply->atom;
		lntd_mem_free(proto_reply);
	}

	xcb_atom_t wm_delete_window_atom;
	{
		xcb_intern_atom_reply_t *delete_ck_reply;
		xcb_generic_error_t *delete_ck_err;
		{
			xcb_generic_error_t *xx = 0;
			delete_ck_reply = xcb_intern_atom_reply(
			    connection, delete_ck, &xx);
			delete_ck_err = xx;
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		if (delete_ck_err != 0) {
			err = lntd_xcb_error(delete_ck_err);
			lntd_mem_free(delete_ck_err);
			goto destroy_window;
		}

		wm_delete_window_atom = delete_ck_reply->atom;
		lntd_mem_free(delete_ck_reply);
	}

	xcb_void_cookie_t change_ck;
	{
		xcb_atom_t xx = wm_delete_window_atom;
		change_ck = xcb_change_property_checked(
		    connection, XCB_PROP_MODE_REPLACE, window_id,
		    wm_protocols_atom, XCB_ATOM_ATOM, 32, 1U, &xx);
	}
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_generic_error_t *change_err =
	    xcb_request_check(connection, change_ck);

	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;
	if (change_err != 0) {
		err = lntd_xcb_error(change_err);
		lntd_mem_free(change_err);
		goto destroy_window;
	}

	xcb_atom_t net_wm_pid_atom;
	{
		xcb_intern_atom_reply_t *pid_ck_reply;
		xcb_generic_error_t *pid_ck_err;
		{
			xcb_generic_error_t *xx = 0;
			pid_ck_reply = xcb_intern_atom_reply(
			    connection, pid_ck, &xx);
			pid_ck_err = xx;
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

		if (pid_ck_err != 0) {
			err = lntd_xcb_error(pid_ck_err);
			lntd_mem_free(pid_ck_err);
			goto destroy_window;
		}

		net_wm_pid_atom = pid_ck_reply->atom;
		lntd_mem_free(pid_ck_reply);
	}

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
	                    window_id, XCB_ATOM_WM_CLASS,
	                    XCB_ATOM_STRING, 8, sizeof PACKAGE_TARNAME +
	                                            sizeof PACKAGE_NAME,
	                    PACKAGE_TARNAME "\0" PACKAGE_NAME);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
	                    window_id, XCB_ATOM_WM_NAME,
	                    XCB_ATOM_STRING, 8, strlen(PACKAGE_NAME),
	                    PACKAGE_NAME);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
	                    window_id, XCB_ATOM_WM_ICON_NAME,
	                    XCB_ATOM_STRING, 8, strlen(PACKAGE_NAME),
	                    PACKAGE_NAME);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	{
		char buf[HOST_NAME_MAX + 1U];
		if (-1 == gethostname(buf, sizeof buf))
			goto get_hostname_failed;

		xcb_change_property(
		    connection, XCB_PROP_MODE_REPLACE, window_id,
		    XCB_ATOM_WM_CLIENT_MACHINE, XCB_ATOM_STRING, 8,
		    strlen(buf), buf);
		goto get_hostname_succeeded;
	}

get_hostname_failed:
	err = errno;
	LNTD_ASSUME(err != 0);
	goto destroy_window;

get_hostname_succeeded:
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	{
		uint32_t xx = root_pid;
		xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
		                    window_id, net_wm_pid_atom,
		                    XCB_ATOM_CARDINAL, 32, 1, &xx);
	}
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE,
	                    window_id, XCB_ATOM_WM_COMMAND,
	                    XCB_ATOM_STRING, 8, strlen(PACKAGE_TARNAME),
	                    PACKAGE_TARNAME);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_map_window(connection, window_id);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_generic_error_t *create_win_err =
	    xcb_request_check(connection, create_win_ck);
	if (create_win_err != 0) {
		err = lntd_xcb_error(create_win_err);
		lntd_mem_free(create_win_err);
		goto destroy_window;
	}

	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_window;

	xcb_flush(connection);

	err = lntd_window_write(window_ko, window_id);
	if (err != 0)
		goto destroy_window;

	lntd_async_pool_submit(
	    pool, lntd_window_task_notify_prepare(
	              drawer_notice_task,
	              (union lntd_async_ck){.u64 = ON_SENT_NOTICE},
	              drawer_notice_task, drawer_notifier));

	lntd_async_pool_submit(
	    pool, lntd_window_task_notify_prepare(
	              gui_notice_task,
	              (union lntd_async_ck){.u64 = ON_SENT_NOTICE},
	              gui_notice_task, gui_notifier));

	lntd_async_pool_submit(
	    pool, lntd_io_task_poll_prepare(
	              poll_conn_task,
	              (union lntd_async_ck){.u64 = ON_POLL_CONN},
	              poll_conn_task,
	              xcb_get_file_descriptor(connection), POLLIN));

	window->time_to_quit = false;
	window->pool = pool;
	window->connection = connection;
	window->poll_conn_task = poll_conn_task;
	window->drawer_notice_task = drawer_notice_task;
	window->gui_notice_task = gui_notice_task;

	window->kill_ko = kill_ko;
	window->window_ko = window_ko;
	window->gui_notifier = gui_notifier;
	window->drawer_notifier = drawer_notifier;

	return 0;

destroy_window : {
	xcb_void_cookie_t destroy_ck =
	    xcb_destroy_window_checked(connection, window_id);
	if (0 == err)
		err = lntd_xcb_conn_error(connection);

	xcb_generic_error_t *destroy_err =
	    xcb_request_check(connection, destroy_ck);
	if (0 == err)
		err = lntd_xcb_conn_error(connection);
	if (0 == err && destroy_err != 0)
		err = lntd_xcb_error(destroy_err);
	lntd_mem_free(destroy_err);
}

close_display:
	xcb_disconnect(connection);

destroy_poll_conn_task:
	lntd_io_task_poll_destroy(poll_conn_task);

destroy_drawer_notice_task:
	lntd_window_task_notify_destroy(drawer_notice_task);

destroy_gui_notice_task:
	lntd_window_task_notify_destroy(gui_notice_task);

drawer_notifier:
	lntd_ko_close(drawer_notifier);

close_gui_notifier:
	lntd_ko_close(gui_notifier);

close_window_ko:
	lntd_ko_close(window_ko);

close_kill_ko:
	lntd_ko_close(kill_ko);

	return err;
}

static lntd_error window_destroy(struct window *window)
{
	lntd_error err = 0;

	xcb_connection_t *connection = window->connection;
	struct lntd_window_task_notify *gui_notice_task =
	    window->gui_notice_task;
	struct lntd_window_task_notify *drawer_notice_task =
	    window->drawer_notice_task;
	struct lntd_io_task_poll *poll_conn_task =
	    window->poll_conn_task;
	lntd_ko kill_ko = window->kill_ko;
	lntd_ko window_ko = window->window_ko;
	lntd_ko gui_notifier = window->gui_notifier;
	lntd_ko drawer_notifier = window->drawer_notifier;

	xcb_window_t window_id = window->window_id;
	{
		xcb_void_cookie_t destroy_ck =
		    xcb_destroy_window_checked(connection, window_id);
		if (0 == err)
			err = lntd_xcb_conn_error(connection);

		xcb_generic_error_t *destroy_err =
		    xcb_request_check(connection, destroy_ck);
		if (0 == err)
			err = lntd_xcb_conn_error(connection);
		if (0 == err && destroy_err != 0)
			err = lntd_xcb_error(destroy_err);
		lntd_mem_free(destroy_err);
	}

	xcb_disconnect(connection);

	lntd_window_task_notify_destroy(drawer_notice_task);

	lntd_window_task_notify_destroy(gui_notice_task);

	lntd_io_task_poll_destroy(poll_conn_task);

	lntd_ko_close(drawer_notifier);

	lntd_ko_close(gui_notifier);

	lntd_ko_close(window_ko);

	lntd_ko_close(kill_ko);

	return err;
}

static lntd_error window_stop(struct window *window)
{
	struct lntd_window_task_notify *gui_notice_task =
	    window->gui_notice_task;
	struct lntd_window_task_notify *drawer_notice_task =
	    window->drawer_notice_task;
	struct lntd_io_task_poll *poll_conn_task =
	    window->poll_conn_task;

	lntd_async_task_cancel(
	    lntd_window_task_notify_to_async(gui_notice_task));
	lntd_async_task_cancel(
	    lntd_window_task_notify_to_async(drawer_notice_task));
	lntd_async_task_cancel(
	    lntd_io_task_poll_to_async(poll_conn_task));

	return 0;
}

static lntd_error dispatch(struct window *window,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_error err)
{
	switch (task_ck.u64) {
	case ON_POLL_CONN:
		return window_on_conn_ready(window, userstate, err);

	case ON_SENT_NOTICE:
		return window_on_notice_sent(window, userstate, err);

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

static lntd_error
window_on_conn_ready(struct window *window,
                     struct lntd_io_task_poll *poll_conn_task,
                     lntd_error err)
{
	if (err != 0)
		return err;

	xcb_connection_t *connection = window->connection;
	struct lntd_async_pool *pool = window->pool;

	for (;;) {
		xcb_generic_event_t *event =
		    xcb_poll_for_event(connection);
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
		lntd_mem_free(event);

		window->time_to_quit = time_to_quit;
		if (time_to_quit)
			return 0;
	}

	lntd_async_pool_submit(
	    pool, lntd_io_task_poll_prepare(
	              poll_conn_task,
	              (union lntd_async_ck){.u64 = ON_POLL_CONN},
	              poll_conn_task,
	              xcb_get_file_descriptor(connection), POLLIN));

	return 0;
}

static lntd_error
window_on_notice_sent(struct window *window,
                      struct lntd_window_task_notify *notice_task,
                      lntd_error err)
{

	if (err != 0)
		return err;

	return 0;
}
