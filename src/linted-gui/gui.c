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

#include "linted/asynch.h"
#include "linted/controller.h"
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
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <xcb/xcb.h>
#include <xcb/xkb.h>
#include <xkbcommon/xkbcommon.h>
#include <xkbcommon/xkbcommon-keysyms.h>
#include <xkbcommon/xkbcommon-x11.h>

/**
 * @file
 *
 * @todo Replace as much xkbcommon code with xcb-xkb code as possible
 * to get more accurate error handling.
 */

enum { ON_RECEIVE_NOTICE, ON_POLL_CONN, ON_SENT_CONTROL, MAX_TASKS };

static uint32_t const window_opts[] = {
    XCB_EVENT_MASK_FOCUS_CHANGE | XCB_EVENT_MASK_POINTER_MOTION |
        XCB_EVENT_MASK_STRUCTURE_NOTIFY | XCB_EVENT_MASK_KEY_PRESS |
        XCB_EVENT_MASK_KEY_RELEASE,
    0};

struct controller_data;

struct window_model {
	unsigned width;
	unsigned height;
};

struct poll_conn_data {
	xcb_connection_t *connection;
	struct window_model *window_model;
	struct linted_asynch_pool *pool;
	struct controller_data *controller_data;
	struct linted_controller_task_send *controller_task;
	xcb_window_t *window;
	linted_ko controller;
	int32_t device_id;
	struct xkb_state **keyboard_state;
	struct xkb_keymap *keymap;
};

struct controller_data {
	struct linted_controller_message update;
	bool update_pending : 1U;
	bool update_in_progress : 1U;
};

struct controller_task_data {
	struct controller_data *controller_data;
	struct linted_asynch_pool *pool;
	linted_ko controller;
};

struct notice_data {
	struct linted_asynch_pool *pool;
	xcb_connection_t *connection;
	struct window_model *window_model;
	struct controller_data *controller_data;
	struct linted_controller_task_send *controller_task;
	xcb_window_t *window;
	linted_ko controller;
	linted_window window_ko;
};

static unsigned char gui_start(char const *process_name, size_t argc,
                               char const *const argv[]);

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_receive_notice(struct linted_asynch_task *task);
static linted_error on_sent_control(struct linted_asynch_task *task);

static void maybe_update_controller(
    struct linted_asynch_pool *pool,
    struct controller_data *controller_data,
    struct linted_controller_task_send *controller_task,
    linted_controller controller);

static linted_error get_mouse_position(xcb_connection_t *connection,
                                       xcb_window_t window, int *x,
                                       int *y);

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_model const *window_model,
                    struct controller_data *controller_data);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-gui", .start = gui_start};

static unsigned char gui_start(char const *process_name, size_t argc,
                               char const *const argv[])
{
	linted_error err = 0;

	if (argc < 4U) {
		linted_log(LINTED_LOG_ERROR,
		           "missing some of 3 file operands");
		return EXIT_FAILURE;
	}

	char const *window_path = argv[1U];
	char const *window_notifier_path = argv[2U];
	char const *controller_path = argv[3U];

	linted_window window_ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, window_path,
		                     LINTED_KO_RDWR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		window_ko = xx;
	}

	linted_window_notifier notifier;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     window_notifier_path,
		                     LINTED_KO_RDWR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		notifier = xx;
	}

	linted_controller controller;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     controller_path, LINTED_KO_RDWR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		controller = xx;
	}

	struct controller_data controller_data = {0};
	struct window_model window_model = {.width = 1, .height = 1};

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		err = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (err != 0)
			return err;
		pool = xx;
	}

	xcb_window_t window = 0;

	struct notice_data notice_data;
	struct controller_task_data controller_task_data;
	struct poll_conn_data poll_conn_data;

	struct linted_window_task_watch *notice_task;
	struct linted_controller_task_send *controller_task;
	struct linted_io_task_poll *poll_conn_task;

	{
		struct linted_window_task_watch *xx;
		err =
		    linted_window_task_watch_create(&xx, &notice_data);
		if (err != 0)
			goto destroy_pool;
		notice_task = xx;
	}

	{
		struct linted_controller_task_send *xx;
		err = linted_controller_task_send_create(
		    &xx, &controller_task_data);
		if (err != 0)
			goto destroy_pool;
		controller_task = xx;
	}

	{
		struct linted_io_task_poll *xx;
		err = linted_io_task_poll_create(&xx, &poll_conn_data);
		if (err != 0)
			goto destroy_pool;
		poll_conn_task = xx;
	}

	xcb_connection_t *connection = xcb_connect(0, 0);
	if (0 == connection) {
		err = LINTED_ERROR_UNIMPLEMENTED;
		goto destroy_pool;
	}
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto close_connection;

	if (!xkb_x11_setup_xkb_extension(
	        connection, XKB_X11_MIN_MAJOR_XKB_VERSION,
	        XKB_X11_MIN_MINOR_XKB_VERSION, 0, 0, 0, 0, 0)) {
		err = LINTED_ERROR_UNIMPLEMENTED;
		goto close_connection;
	}

	struct xkb_context *keyboard_ctx =
	    xkb_context_new(XKB_CONTEXT_NO_FLAGS);
	if (0 == keyboard_ctx) {
		err = LINTED_ERROR_UNIMPLEMENTED;
		goto close_connection;
	}

	int32_t device_id;
	xcb_xkb_get_device_info_cookie_t device_info_ck =
	    xcb_xkb_get_device_info(connection, XCB_XKB_ID_USE_CORE_KBD,
	                            0, 0, 0, 0, 0, 0);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto destroy_keyboard_ctx;

	{
		xcb_generic_error_t *error;
		xcb_xkb_get_device_info_reply_t *reply;
		{
			xcb_generic_error_t *xx = 0;
			reply = xcb_xkb_get_device_info_reply(
			    connection, device_info_ck, &xx);
			error = xx;
		}

		err = linted_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_keyboard_ctx;

		if (error != 0) {
			err = linted_xcb_error(error);
			linted_mem_free(error);
			goto destroy_keyboard_ctx;
		}

		device_id = reply->deviceID;

		linted_mem_free(reply);
	}

	struct xkb_keymap *keymap = xkb_x11_keymap_new_from_device(
	    keyboard_ctx, connection, device_id,
	    XKB_KEYMAP_COMPILE_NO_FLAGS);
	if (0 == keymap) {
		err = LINTED_ERROR_UNIMPLEMENTED;
		goto destroy_keyboard_ctx;
	}

	struct xkb_state *keyboard_state =
	    xkb_x11_state_new_from_device(keymap, connection,
	                                  device_id);
	if (0 == keyboard_state) {
		err = LINTED_ERROR_UNIMPLEMENTED;
		goto destroy_keymap;
	}

	notice_data.window = &window;
	notice_data.pool = pool;
	notice_data.window_model = &window_model;
	notice_data.connection = connection;
	notice_data.controller = controller;
	notice_data.window_ko = window_ko;
	notice_data.controller_data = &controller_data;
	notice_data.controller_task = controller_task;

	poll_conn_data.window = &window;
	poll_conn_data.pool = pool;
	poll_conn_data.window_model = &window_model;
	poll_conn_data.connection = connection;
	poll_conn_data.controller = controller;
	poll_conn_data.controller_data = &controller_data;
	poll_conn_data.controller_task = controller_task;
	poll_conn_data.device_id = device_id;
	poll_conn_data.keyboard_state = &keyboard_state;
	poll_conn_data.keymap = keymap;

	{
		uint_fast32_t xx;
		err = linted_window_read(window_ko, &xx);
		if (err != 0)
			goto on_window_read_err;
		window = xx;
	}
on_window_read_err:
	if (EPROTO == err) {
		/* Do nothing */
	} else if (err != 0) {
		return err;
	} else {
		xcb_change_window_attributes(
		    connection, window, XCB_CW_EVENT_MASK, window_opts);
		err = linted_xcb_conn_error(connection);
		if (err != 0)
			return err;

		xcb_get_geometry_cookie_t geom_ck =
		    xcb_get_geometry(connection, window);
		err = linted_xcb_conn_error(connection);
		if (err != 0)
			return err;

		xcb_query_pointer_cookie_t point_ck =
		    xcb_query_pointer(connection, window);
		err = linted_xcb_conn_error(connection);
		if (err != 0)
			return err;

		unsigned width, height;
		{
			xcb_generic_error_t *error;
			xcb_get_geometry_reply_t *reply;
			{
				xcb_generic_error_t *xx = 0;
				reply = xcb_get_geometry_reply(
				    connection, geom_ck, &xx);
				error = xx;
			}

			err = linted_xcb_conn_error(connection);
			if (err != 0)
				return err;

			if (error != 0) {
				err = linted_xcb_error(error);
				linted_mem_free(error);
				return err;
			}

			width = reply->width;
			height = reply->height;

			linted_mem_free(reply);
		}

		int x, y;
		{
			xcb_generic_error_t *error;
			xcb_query_pointer_reply_t *reply;
			{
				xcb_generic_error_t *xx = 0;
				reply = xcb_query_pointer_reply(
				    connection, point_ck, &xx);
				error = xx;
			}

			err = linted_xcb_conn_error(connection);
			if (err != 0)
				return err;

			if (error != 0) {
				err = linted_xcb_error(error);
				linted_mem_free(error);
				return err;
			}

			x = reply->win_x;
			y = reply->win_y;

			linted_mem_free(reply);
		}

		window_model.width = width;
		window_model.height = height;

		on_tilt(x, y, &window_model, &controller_data);

		maybe_update_controller(pool, &controller_data,
		                        controller_task, controller);
	}

	linted_window_task_watch_prepare(notice_task, ON_RECEIVE_NOTICE,
	                                 notifier);
	linted_asynch_pool_submit(
	    pool, linted_window_task_watch_to_asynch(notice_task));

	linted_io_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	linted_asynch_pool_submit(
	    pool, linted_io_task_poll_to_asynch(poll_conn_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			err = linted_asynch_pool_wait(pool, &xx);
			if (err != 0)
				goto stop_pool;
			completed_task = xx;
		}

		err = dispatch(completed_task);
		if (err != 0)
			goto stop_pool;
	}

stop_pool:
	linted_asynch_task_cancel(
	    linted_window_task_watch_to_asynch(notice_task));
	linted_asynch_task_cancel(
	    linted_controller_task_send_to_asynch(controller_task));
	linted_asynch_task_cancel(
	    linted_io_task_poll_to_asynch(poll_conn_task));

	for (;;) {
		struct linted_asynch_task *task;
		linted_error poll_err;
		{
			struct linted_asynch_task *xx;
			poll_err = linted_asynch_pool_poll(pool, &xx);
			if (LINTED_ERROR_AGAIN == poll_err)
				break;
			task = xx;
		}

		linted_error dispatch_err =
		    linted_asynch_task_err(task);
		if (0 == err && dispatch_err != LINTED_ERROR_CANCELLED)
			err = dispatch_err;
	}

	xkb_state_unref(keyboard_state);

destroy_keymap:
	xkb_keymap_unref(keymap);

destroy_keyboard_ctx:
	xkb_context_unref(keyboard_ctx);

close_connection:
	xcb_disconnect(connection);

destroy_pool : {
	linted_error destroy_err = linted_asynch_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;
}

	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)notice_task;
	(void)controller_task;
	(void)poll_conn_task;

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (linted_asynch_task_action(task)) {
	case ON_POLL_CONN:
		return on_poll_conn(task);

	case ON_RECEIVE_NOTICE:
		return on_receive_notice(task);

	case ON_SENT_CONTROL:
		return on_sent_control(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_poll_conn(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (err != 0)
		return err;

	struct linted_io_task_poll *poll_conn_task =
	    linted_io_task_poll_from_asynch(task);
	struct poll_conn_data *poll_conn_data =
	    linted_io_task_poll_data(poll_conn_task);

	xcb_connection_t *connection = poll_conn_data->connection;
	xcb_window_t *window = poll_conn_data->window;
	struct window_model *window_model =
	    poll_conn_data->window_model;

	struct linted_asynch_pool *pool = poll_conn_data->pool;
	linted_ko controller = poll_conn_data->controller;
	struct controller_data *controller_data =
	    poll_conn_data->controller_data;
	struct linted_controller_task_send *controller_task =
	    poll_conn_data->controller_task;
	int32_t device_id = poll_conn_data->device_id;
	struct xkb_state **keyboard_state =
	    poll_conn_data->keyboard_state;
	struct xkb_keymap *keymap = poll_conn_data->keymap;

	bool had_focus_change = false;
	bool focused;

	bool had_motion = false;
	int motion_x;
	int motion_y;

	bool had_resize = false;
	unsigned resize_width;
	unsigned resize_height;

	bool mapping_notify = false;
	bool window_destroyed = false;
	for (;;) {
		xcb_generic_event_t *event =
		    xcb_poll_for_event(connection);
		if (0 == event)
			break;

		bool is_key_down;
		switch (event->response_type & ~0x80) {
		case XCB_CONFIGURE_NOTIFY: {
			xcb_configure_notify_event_t const *
			    configure_event = (void *)event;

			resize_width = configure_event->width;
			resize_height = configure_event->height;
			had_resize = true;
			break;
		}

		case XCB_DESTROY_NOTIFY:
			window_destroyed = true;
			break;

		case XCB_MOTION_NOTIFY: {
			xcb_motion_notify_event_t const *motion_event =
			    (void *)event;

			motion_x = motion_event->event_x;
			motion_y = motion_event->event_y;
			had_motion = true;
			break;
		}

		case XCB_FOCUS_IN:
			focused = true;
			had_focus_change = true;
			break;

		case XCB_FOCUS_OUT:
			focused = false;
			had_focus_change = true;
			break;

		case XCB_KEY_PRESS:
			is_key_down = true;
			goto on_key_event;

		case XCB_KEY_RELEASE:
			is_key_down = false;
			goto on_key_event;

		case XCB_MAPPING_NOTIFY:
			mapping_notify = true;
			break;

		default:
			/* Unknown event type, ignore it */
			break;

		on_key_event : {
			xcb_key_press_event_t *key_event =
			    (void *)event;
			xcb_keycode_t keycode = key_event->detail;
			xkb_keysym_t keysym = xkb_state_key_get_one_sym(
			    *keyboard_state, keycode);
			switch (keysym) {
			case XKB_KEY_space:
				goto jump;

			case XKB_KEY_Control_L:
				goto move_left;

			case XKB_KEY_Alt_L:
				goto move_right;

			case XKB_KEY_z:
				goto move_forward;

			case XKB_KEY_Shift_L:
				goto move_backward;

			default:
				break;
			}
			break;
		}

		jump:
			controller_data->update.jumping = is_key_down;
			controller_data->update_pending = true;
			break;

		move_left:
			controller_data->update.left = is_key_down;
			controller_data->update_pending = true;
			break;

		move_right:
			controller_data->update.right = is_key_down;
			controller_data->update_pending = true;
			break;

		move_forward:
			controller_data->update.forward = is_key_down;
			controller_data->update_pending = true;
			break;

		move_backward:
			controller_data->update.back = is_key_down;
			controller_data->update_pending = true;
			break;
		}
		linted_mem_free(event);
	}

	bool clear_controls = false;

	if (had_resize) {
		window_model->width = resize_width;
		window_model->height = resize_height;
	}

	if (had_motion)
		on_tilt(motion_x, motion_y, window_model,
		        controller_data);

	if (window_destroyed) {
		clear_controls = true;
		goto clear;
	}

	if (mapping_notify) {
		struct xkb_state *new_state =
		    xkb_x11_state_new_from_device(keymap, connection,
		                                  device_id);
		xkb_state_unref(*keyboard_state);
		*keyboard_state = new_state;
	}

	if (had_focus_change) {
		if (focused) {
			int x, y;
			err = get_mouse_position(connection, *window,
			                         &x, &y);
			if (err != 0)
				return err;

			on_tilt(x, y, window_model, controller_data);
		} else {
			clear_controls = true;
		}
	}

clear:
	if (clear_controls) {
		controller_data->update.z_tilt = 0;
		controller_data->update.x_tilt = 0;

		controller_data->update.left = 0;
		controller_data->update.right = 0;
		controller_data->update.forward = 0;
		controller_data->update.back = 0;

		controller_data->update.jumping = 0;

		controller_data->update_pending = true;
	}

	maybe_update_controller(pool, controller_data, controller_task,
	                        controller);

	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_receive_notice(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (err != 0)
		return err;

	struct linted_window_task_watch *notice_task =
	    linted_window_task_watch_from_asynch(task);
	struct notice_data *notice_data =
	    linted_window_task_watch_data(notice_task);
	struct linted_asynch_pool *pool = notice_data->pool;
	xcb_connection_t *connection = notice_data->connection;
	struct window_model *window_model = notice_data->window_model;
	linted_ko controller = notice_data->controller;
	struct controller_data *controller_data =
	    notice_data->controller_data;
	struct linted_controller_task_send *controller_task =
	    notice_data->controller_task;
	xcb_window_t *windowp = notice_data->window;
	linted_window window_ko = notice_data->window_ko;

	uint_fast32_t window;
	{
		uint_fast32_t xx;
		err = linted_window_read(window_ko, &xx);
		if (EPROTO == err) {
			err = 0;
			goto reset_notice;
		}
		if (err != 0)
			goto reset_notice;
		window = xx;
	}

	xcb_change_window_attributes(connection, window,
	                             XCB_CW_EVENT_MASK, window_opts);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto reset_notice;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, window);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto reset_notice;

	xcb_query_pointer_cookie_t point_ck =
	    xcb_query_pointer(connection, window);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto reset_notice;

	unsigned width, height;
	{
		xcb_generic_error_t *error;
		xcb_get_geometry_reply_t *reply;
		{
			xcb_generic_error_t *xx = 0;
			reply = xcb_get_geometry_reply(connection,
			                               geom_ck, &xx);
			error = xx;
		}

		err = linted_xcb_conn_error(connection);
		if (err != 0)
			goto reset_notice;

		if (error != 0) {
			err = linted_xcb_error(error);
			linted_mem_free(error);
			goto reset_notice;
		}

		width = reply->width;
		height = reply->height;

		linted_mem_free(reply);
	}

	int x, y;
	{
		xcb_generic_error_t *error;
		xcb_query_pointer_reply_t *reply;
		{
			xcb_generic_error_t *xx = 0;
			reply = xcb_query_pointer_reply(connection,
			                                point_ck, &xx);
			error = xx;
		}

		err = linted_xcb_conn_error(connection);
		if (err != 0)
			goto reset_notice;

		if (error != 0) {
			err = linted_xcb_error(error);
			linted_mem_free(error);
			goto reset_notice;
		}

		x = reply->win_x;
		y = reply->win_y;

		linted_mem_free(reply);
	}

	window_model->width = width;
	window_model->height = height;

	on_tilt(x, y, window_model, controller_data);

	maybe_update_controller(pool, controller_data, controller_task,
	                        controller);

	*windowp = window;

reset_notice:
	linted_asynch_pool_submit(pool, task);

	return err;
}

static linted_error on_sent_control(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (ENOENT == err)
		err = 0;
	if (ECONNREFUSED == err)
		err = 0;
	if (err != 0)
		return err;

	struct linted_controller_task_send *controller_task =
	    linted_controller_task_send_from_asynch(task);
	struct controller_task_data *controller_task_data =
	    linted_controller_task_send_data(controller_task);
	struct controller_data *controller_data =
	    controller_task_data->controller_data;
	struct linted_asynch_pool *pool = controller_task_data->pool;
	linted_ko controller = controller_task_data->controller;

	controller_data->update_in_progress = false;

	maybe_update_controller(pool, controller_data, controller_task,
	                        controller);

	return 0;
}

static void maybe_update_controller(
    struct linted_asynch_pool *pool,
    struct controller_data *controller_data,
    struct linted_controller_task_send *controller_task,
    linted_controller controller)
{
	if (!controller_data->update_pending)
		return;

	if (controller_data->update_in_progress)
		return;

	struct controller_task_data *controller_task_data =
	    linted_controller_task_send_data(controller_task);
	linted_controller_task_send_prepare(controller_task,
	                                    ON_SENT_CONTROL, controller,
	                                    &controller_data->update);
	controller_task_data->controller_data = controller_data;
	controller_task_data->pool = pool;
	controller_task_data->controller = controller;

	linted_asynch_pool_submit(
	    pool,
	    linted_controller_task_send_to_asynch(controller_task));

	controller_data->update_pending = false;
	controller_data->update_in_progress = true;
}

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_model const *window_model,
                    struct controller_data *controller_data)
{
	unsigned width = window_model->width;
	unsigned height = window_model->height;

	int32_t x = (2 * mouse_x - (int)width) / 2;
	int32_t y = (2 * mouse_y - (int)height) / 2;

	/* Normalize and scale up to UINT32_MAX sized screen */
	x *= INT32_MAX / (intmax_t)width;
	y *= INT32_MAX / (intmax_t)height;

	controller_data->update.z_tilt = x;
	controller_data->update.x_tilt = y;

	controller_data->update_pending = true;
}

static linted_error get_mouse_position(xcb_connection_t *connection,
                                       xcb_window_t window, int *xp,
                                       int *yp)
{
	linted_error err;

	xcb_query_pointer_cookie_t ck =
	    xcb_query_pointer(connection, window);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_generic_error_t *error;
	xcb_query_pointer_reply_t *reply;
	{
		xcb_generic_error_t *xx = 0;
		reply = xcb_query_pointer_reply(connection, ck, &xx);
		error = xx;
	}
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		return err;

	if (error != 0) {
		err = linted_xcb_error(error);
		linted_mem_free(error);
		LINTED_ASSUME(err != 0);
		return err;
	}

	int x = reply->win_x;
	int y = reply->win_x;

	linted_mem_free(reply);

	*xp = x;
	*yp = y;

	return 0;
}
