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
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"
#include "linted/window-notifier.h"

#include <errno.h>
#include <limits.h>
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
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

static linted_ko kos[2U];

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-window",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos
};

static uint32_t const window_opts[] = { 0 };

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_sent_notice(struct linted_asynch_task *task);

static linted_error get_xcb_conn_error(xcb_connection_t *connection);
static linted_error get_xcb_error(xcb_generic_error_t *error);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	linted_window_notifier notifier = kos[1U];

	char const *root = getenv("MANAGERPID");
	if (NULL == root) {
		fprintf(stderr, "%s: need MANAGERPID\n", process_name);
		return EXIT_FAILURE;
	}

	errno = 0;
	long int root_pid = strtol(root, NULL, 10);
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
		errno = errnum;
		perror("strtol");
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

	struct notice_data notice_data;
	struct poll_conn_data poll_conn_data;

	struct linted_ko_task_poll *poll_conn_task;
	struct linted_window_notifier_task_send *notice_task;

	{
		struct linted_window_notifier_task_send *xx;
		errnum =
		    linted_window_notifier_task_send_create(&xx, &notice_data);
		if (errnum != 0)
			goto destroy_pool;
		notice_task = xx;
	}

	{
		struct linted_ko_task_poll *xx;
		errnum = linted_ko_task_poll_create(&xx, &poll_conn_data);
		if (errnum != 0)
			goto destroy_pool;
		poll_conn_task = xx;
	}

	/* Open the connection to the X server */
	unsigned screen_number;
	xcb_connection_t *connection;
	{
		int xx;
		connection = xcb_connect(NULL, &xx);
		if (NULL == connection) {
			errnum = ENOSYS;
			goto destroy_pool;
		}
		screen_number = (unsigned)xx;
	}

	xcb_screen_t *screen = NULL;
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
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto close_display;

	xcb_void_cookie_t create_win_ck = xcb_create_window_checked(
	    connection, XCB_COPY_FROM_PARENT, window, screen->root, 0, 0, 640,
	    480, 0, XCB_WINDOW_CLASS_INPUT_OUTPUT, screen->root_visual, 0,
	    window_opts);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto close_display;

	xcb_intern_atom_cookie_t protocols_ck = xcb_intern_atom(
	    connection, 1, strlen("WM_PROTOCOLS"), "WM_PROTOCOLS");
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t delete_ck = xcb_intern_atom(
	    connection, 0, strlen("WM_DELETE_WINDOW"), "WM_DELETE_WINDOW");
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t pid_ck = xcb_intern_atom(
	    connection, 0, strlen("_NET_WM_PID"), "_NET_WM_PID");
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_atom_t wm_protocols_atom;
	{
		xcb_intern_atom_reply_t *proto_reply;
		xcb_generic_error_t *proto_err;
		{
			xcb_generic_error_t *xx = NULL;
			proto_reply = xcb_intern_atom_reply(connection,
			                                    protocols_ck, &xx);
			proto_err = xx;
		}
		errnum = get_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (proto_err != NULL) {
			errnum = get_xcb_error(proto_err);
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
			xcb_generic_error_t *xx = NULL;
			delete_ck_reply =
			    xcb_intern_atom_reply(connection, delete_ck, &xx);
			delete_ck_err = xx;
		}
		errnum = get_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (delete_ck_err != NULL) {
			errnum = get_xcb_error(delete_ck_err);
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
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_atom_t net_wm_pid_atom;
	{
		xcb_intern_atom_reply_t *pid_ck_reply;
		xcb_generic_error_t *pid_ck_err;
		{
			xcb_generic_error_t *xx = NULL;
			pid_ck_reply =
			    xcb_intern_atom_reply(connection, pid_ck, &xx);
			pid_ck_err = xx;
		}
		errnum = get_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (pid_ck_err != NULL) {
			errnum = get_xcb_error(pid_ck_err);
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
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8,
	                    strlen(PACKAGE_NAME), PACKAGE_NAME);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_ICON_NAME, XCB_ATOM_STRING, 8,
	                    strlen(PACKAGE_NAME), PACKAGE_NAME);
	errnum = get_xcb_conn_error(connection);
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
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	{
		uint32_t xx = root_pid;
		xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
		                    net_wm_pid_atom, XCB_ATOM_CARDINAL, 32, 1,
		                    &xx);
	}
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
	                    XCB_ATOM_WM_COMMAND, XCB_ATOM_STRING, 8,
	                    strlen(PACKAGE_TARNAME), PACKAGE_TARNAME);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_map_window(connection, window);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_generic_error_t *create_win_err =
	    xcb_request_check(connection, create_win_ck);
	if (create_win_err != NULL) {
		errnum = get_xcb_error(create_win_err);
		linted_mem_free(create_win_err);
		goto destroy_window;
	}

	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_flush(connection);

	linted_window_notifier_task_send_prepare(notice_task, ON_SENT_NOTICE,
	                                         notifier, window);
	notice_data.pool = pool;
	linted_asynch_pool_submit(
	    pool, linted_window_notifier_task_send_to_asynch(notice_task));

	bool time_to_quit;

	linted_ko_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	poll_conn_data.time_to_quit = &time_to_quit;
	poll_conn_data.pool = pool;
	poll_conn_data.connection = connection;

	linted_asynch_pool_submit(
	    pool, linted_ko_task_poll_to_asynch(poll_conn_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		time_to_quit = false;

		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			errnum = linted_asynch_pool_wait(pool, &xx);
			if (errnum != 0)
				goto destroy_window;
			completed_task = xx;
		}
		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto destroy_window;

		if (time_to_quit)
			goto destroy_window;
	}

destroy_window : {
	xcb_void_cookie_t destroy_ck =
	    xcb_destroy_window_checked(connection, window);
	if (0 == errnum)
		errnum = get_xcb_conn_error(connection);

	xcb_generic_error_t *destroy_err =
	    xcb_request_check(connection, destroy_ck);
	if (0 == errnum)
		errnum = get_xcb_conn_error(connection);
	if (0 == errnum && destroy_err != NULL)
		errnum = get_xcb_error(destroy_err);
	linted_mem_free(destroy_err);
}

close_display:
	xcb_disconnect(connection);

destroy_pool:
	linted_asynch_pool_stop(pool);

	for (;;) {
		struct linted_asynch_task *completed_task;
		linted_error poll_errnum;
		{
			struct linted_asynch_task *xx;
			poll_errnum = linted_asynch_pool_poll(pool, &xx);
			if (EAGAIN == poll_errnum)
				break;
			completed_task = xx;
		}

		linted_error dispatch_errnum =
		    linted_asynch_task_errnum(completed_task);
		if (0 == errnum)
			errnum = dispatch_errnum;
	}

	{
		linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
		if (0 == errnum)
			errnum = destroy_errnum;
	}
	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)notice_task;
	(void)poll_conn_task;

	/* Tell the manager to exit everything */
	if (0 == errnum) {
		if (-1 == kill(root_pid, SIGTERM)) {
			perror("kill");
			return EXIT_FAILURE;
		}

		/* Wait for the manager to exit everything */
		for (;;)
			pause();
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

	struct linted_ko_task_poll *poll_conn_task =
	    linted_ko_task_poll_from_asynch(task);
	struct poll_conn_data *poll_conn_data =
	    linted_ko_task_poll_data(poll_conn_task);

	xcb_connection_t *connection = poll_conn_data->connection;
	struct linted_asynch_pool *pool = poll_conn_data->pool;
	bool *time_to_quitp = poll_conn_data->time_to_quit;

	for (;;) {
		xcb_generic_event_t *event = xcb_wait_for_event(connection);
		if (NULL == event)
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

	linted_ko_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	poll_conn_data->time_to_quit = time_to_quitp;
	poll_conn_data->pool = pool;
	poll_conn_data->connection = connection;

	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_sent_notice(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	struct linted_window_notifier_task_send *notice_task =
	    linted_window_notifier_task_send_from_asynch(task);
	struct notice_data *notice_data =
	    linted_window_notifier_task_send_data(notice_task);

	linted_asynch_pool_submit(notice_data->pool, task);

	return 0;
}

static linted_error get_xcb_conn_error(xcb_connection_t *connection)
{
	switch (xcb_connection_has_error(connection)) {
	case 0:
		return 0;

	case XCB_CONN_ERROR:
		return EPROTO;

	case XCB_CONN_CLOSED_EXT_NOTSUPPORTED:
		return ENOSYS;

	case XCB_CONN_CLOSED_MEM_INSUFFICIENT:
		return ENOMEM;

	case XCB_CONN_CLOSED_REQ_LEN_EXCEED:
		return EINVAL;

	case XCB_CONN_CLOSED_PARSE_ERR:
		return EINVAL;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error get_xcb_error(xcb_generic_error_t *error)
{
	/* For now just be crappy. */
	return ENOSYS;
}
