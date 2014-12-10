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
#include "linted/gpu.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"
#include "linted/xcb.h"

#include <errno.h>
#include <poll.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <xcb/xcb.h>
#include <X11/Xlib.h>

/**
 * @file
 *
 * @todo Handle sudden window death better.
 */

enum { ON_POLL_CONN, MAX_TASKS };

struct window_model
{
	bool viewable : 1U;
};

struct poll_conn_data
{
	struct linted_gpu_context *gpu_context;
	xcb_connection_t *connection;
	struct linted_asynch_pool *pool;

	struct window_model *window_model;
	struct linted_window_notifier_task_receive *notice_task;
};

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-linted",
	.kos_size = 0U,
	.kos = NULL
};

static uint32_t const window_opts[] = { XCB_EVENT_MASK_STRUCTURE_NOTIFY, 0 };

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	struct window_model window_model = { .viewable = false };

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	struct poll_conn_data poll_conn_data;

	struct linted_ko_task_poll *poll_conn_task;

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

	/* Use a separate connection just for GPU functions so that we
	 * don't get false notifications of notices from GPU
	 * functionality getting a notification.
	 */
	Display *gpu_display = XOpenDisplay(NULL);
	if (NULL == gpu_display) {
		errnum = ENOSYS;
		goto close_display;
	}

	struct linted_gpu_context *gpu_context;
	{
		struct linted_gpu_context *xx;
		errnum = linted_gpu_context_create(gpu_display, &xx);
		if (errnum != 0)
			return errnum;
		gpu_context = xx;
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
		errnum = linted_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (proto_err != NULL) {
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
			xcb_generic_error_t *xx = NULL;
			delete_ck_reply =
			    xcb_intern_atom_reply(connection, delete_ck, &xx);
			delete_ck_err = xx;
		}
		errnum = linted_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (delete_ck_err != NULL) {
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

	xcb_map_window(connection, window);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_generic_error_t *create_win_err =
	    xcb_request_check(connection, create_win_ck);
	if (create_win_err != NULL) {
		errnum = linted_xcb_error(create_win_err);
		linted_mem_free(create_win_err);
		goto destroy_window;
	}

	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_flush(connection);


	xcb_change_window_attributes(connection, window, XCB_CW_EVENT_MASK,
	                             window_opts);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		return errnum;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, window);
	errnum = linted_xcb_conn_error(connection);
	if (errnum != 0)
		return errnum;

	unsigned width, height;
	{
		xcb_generic_error_t *error;
		xcb_get_geometry_reply_t *reply;
		{
			xcb_generic_error_t *xx;
			reply =
			    xcb_get_geometry_reply(connection, geom_ck, &xx);

			errnum = linted_xcb_conn_error(connection);
			if (errnum != 0)
				return errnum;

			error = xx;
		}

		if (error != NULL) {
			errnum = linted_xcb_error(error);
			linted_mem_free(error);
			return errnum;
		}

		width = reply->width;
		height = reply->height;

		linted_mem_free(reply);
	}

	window_model.viewable = true;

	linted_gpu_setwindow(gpu_context, window);
	linted_gpu_resize(gpu_context, width, height);

	{
		struct linted_gpu_update gpu_update;

		gpu_update.x_rotation = 3.141593;
		gpu_update.y_rotation = 0.000000;

		gpu_update.x_position = 0.000000;
		gpu_update.y_position = 0.000000;
		gpu_update.z_position = 1.500000;

		linted_gpu_update_state(gpu_context, &gpu_update);
	}

	linted_ko_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	poll_conn_data.window_model = &window_model;
	poll_conn_data.gpu_context = gpu_context;
	poll_conn_data.pool = pool;
	poll_conn_data.connection = connection;

	linted_asynch_pool_submit(
	    pool, linted_ko_task_poll_to_asynch(poll_conn_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;

			if (window_model.viewable) {
				errnum = linted_asynch_pool_poll(pool, &xx);
			} else {
				errnum = linted_asynch_pool_wait(pool, &xx);
			}
			if (EAGAIN == errnum)
				goto draw_frame;
			if (errnum != 0)
				goto cleanup_gpu;
			completed_task = xx;
		}

		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto cleanup_gpu;

		continue;

	draw_frame:
		/* Draw or resize if we have time to waste */
		linted_gpu_draw(gpu_context);
	}

cleanup_gpu:
	linted_gpu_context_destroy(gpu_context);
	XCloseDisplay(gpu_display);

destroy_window : {
	xcb_void_cookie_t destroy_ck =
	    xcb_destroy_window_checked(connection, window);
	if (0 == errnum)
		errnum = linted_xcb_conn_error(connection);

	xcb_generic_error_t *destroy_err =
	    xcb_request_check(connection, destroy_ck);
	if (0 == errnum)
		errnum = linted_xcb_conn_error(connection);
	if (0 == errnum && destroy_err != NULL)
		errnum = linted_xcb_error(destroy_err);
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
	(void)poll_conn_task;

	return errnum;
}

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (linted_asynch_task_action(task)) {
	case ON_POLL_CONN:
		return on_poll_conn(task);

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
	struct window_model *window_model = poll_conn_data->window_model;
	struct linted_gpu_context *gpu_context = poll_conn_data->gpu_context;

	for (;;) {
		xcb_generic_event_t *event = xcb_poll_for_event(connection);
		if (NULL == event)
			break;

		bool time_to_quit = false;
		switch (event->response_type & ~0x80) {
		case XCB_CONFIGURE_NOTIFY: {
			xcb_configure_notify_event_t const *configure_event =
			    (void *)event;
			linted_gpu_resize(gpu_context, configure_event->width,
			                  configure_event->height);
			break;
		}

		case XCB_UNMAP_NOTIFY:
			window_model->viewable = false;
			break;

		case XCB_MAP_NOTIFY:
			window_model->viewable = true;
			break;

		case XCB_CLIENT_MESSAGE:
			time_to_quit = true;
			break;

		default:
			/* Unknown event type, ignore it */
			break;
		}
		linted_mem_free(event);

		if (time_to_quit) {
			linted_gpu_unsetwindow(gpu_context);
			window_model->viewable = false;
			return ECANCELED;
		}
	}

	linted_ko_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	poll_conn_data->window_model = window_model;
	poll_conn_data->pool = pool;
	poll_conn_data->connection = connection;

	linted_asynch_pool_submit(pool, task);

	return 0;
}
