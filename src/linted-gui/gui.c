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
#include "lntd/controller.h"
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
 * @todo Replace as much xkb x11 code with xcb xkb code as possible
 * to get more accurate error handling.
 */

enum { ON_RECEIVE_NOTICE, ON_POLL_CONN, ON_SENT_CONTROL, MAX_TASKS };

static uint32_t const window_opts[] = {
    XCB_EVENT_MASK_FOCUS_CHANGE | XCB_EVENT_MASK_POINTER_MOTION |
        XCB_EVENT_MASK_STRUCTURE_NOTIFY | XCB_EVENT_MASK_KEY_PRESS |
        XCB_EVENT_MASK_KEY_RELEASE,
    0};

struct gui {
	struct lntd_controller_message update;

	struct lntd_async_pool *pool;
	struct lntd_controller_task_send *controller_task;
	struct lntd_io_task_poll *poll_conn_task;
	struct lntd_window_task_watch *notice_task;
	struct xkb_context *keyboard_ctx;
	struct xkb_keymap *keymap;
	struct xkb_state *keyboard_state;

	xcb_connection_t *connection;
	int32_t device_id;
	lntd_ko controller;
	lntd_ko notifier;
	lntd_window window_ko;
	xcb_window_t window;
	unsigned width;
	unsigned height;

	bool focused : 1U;

	bool update_pending : 1U;
	bool update_in_progress : 1U;
};

static lntd_error gui_init(struct gui *gui,
                           struct lntd_async_pool *pool,
                           char const *window_path,
                           char const *window_notifier,
                           char const *controller_path);
static lntd_error gui_stop(struct gui *gui);
static lntd_error gui_destroy(struct gui *gui);

static lntd_error dispatch(struct gui *gui, union lntd_async_ck task_ck,
                           void *userstate, lntd_error err);
static lntd_error
gui_on_conn_ready(struct gui *gui,
                  struct lntd_io_task_poll *poll_conn_task,
                  lntd_error err);
static lntd_error
gui_on_window_change(struct gui *gui,
                     struct lntd_window_task_watch *notice_task,
                     lntd_error err);
static lntd_error gui_on_sent_controller_state(
    struct gui *gui, struct lntd_controller_task_send *controller_task,
    lntd_error err);

static lntd_error gui_refresh_window(struct gui *gui);
static lntd_error gui_remove_window(struct gui *gui);

static void maybe_update_controller(struct gui *gui);

static lntd_error get_mouse_position(xcb_connection_t *connection,
                                     xcb_window_t window, int *x,
                                     int *y);

static void on_tilt(struct gui *gui, int_fast32_t mouse_x,
                    int_fast32_t mouse_y);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-gui", 0};

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

	char const *window_path = argv[1U];
	char const *window_notifier_path = argv[2U];
	char const *controller_path = argv[3U];

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

	static struct gui gui = {0};
	err = gui_init(&gui, pool, window_path, window_notifier_path,
	               controller_path);
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

		err = dispatch(&gui, result.task_ck, result.userstate,
		               result.err);
		if (err != 0)
			goto stop_pool;
	}

stop_pool:
	gui_stop(&gui);

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

	gui_destroy(&gui);

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

static lntd_error gui_init(struct gui *gui,
                           struct lntd_async_pool *pool,
                           char const *window_path,
                           char const *window_notifier_path,
                           char const *controller_path)
{
	lntd_error err = 0;

	lntd_window window_ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, window_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			return err;
		window_ko = xx;
	}

	lntd_window_notifier notifier;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD,
		                   window_notifier_path, LNTD_KO_RDWR);
		if (err != 0)
			goto close_window_ko;
		notifier = xx;
	}

	lntd_controller controller;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, controller_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			goto close_notifier;
		controller = xx;
	}

	struct lntd_window_task_watch *notice_task;
	{
		struct lntd_window_task_watch *xx;
		err = lntd_window_task_watch_create(&xx, 0);
		if (err != 0)
			goto close_controller;
		notice_task = xx;
	}

	struct lntd_controller_task_send *controller_task;
	{
		struct lntd_controller_task_send *xx;
		err = lntd_controller_task_send_create(&xx, 0);
		if (err != 0)
			goto destroy_notice_task;
		controller_task = xx;
	}

	struct lntd_io_task_poll *poll_conn_task;
	{
		struct lntd_io_task_poll *xx;
		err = lntd_io_task_poll_create(&xx, 0);
		if (err != 0)
			goto destroy_controller_task;
		poll_conn_task = xx;
	}

	xcb_connection_t *connection = xcb_connect(0, 0);
	if (0 == connection) {
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto destroy_poll_conn_task;
	}
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto close_connection;

	if (!xkb_x11_setup_xkb_extension(
	        connection, XKB_X11_MIN_MAJOR_XKB_VERSION,
	        XKB_X11_MIN_MINOR_XKB_VERSION, 0, 0, 0, 0, 0)) {
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto close_connection;
	}

	struct xkb_context *keyboard_ctx =
	    xkb_context_new(XKB_CONTEXT_NO_FLAGS);
	if (0 == keyboard_ctx) {
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto close_connection;
	}

	int32_t device_id;
	xcb_xkb_get_device_info_cookie_t device_info_ck =
	    xcb_xkb_get_device_info(connection, XCB_XKB_ID_USE_CORE_KBD,
	                            0, 0, 0, 0, 0, 0);
	err = lntd_xcb_conn_error(connection);
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

		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_keyboard_ctx;

		if (error != 0) {
			err = lntd_xcb_error(error);
			lntd_mem_free(error);
			goto destroy_keyboard_ctx;
		}

		device_id = reply->deviceID;

		lntd_mem_free(reply);
	}

	struct xkb_keymap *keymap = xkb_x11_keymap_new_from_device(
	    keyboard_ctx, connection, device_id,
	    XKB_KEYMAP_COMPILE_NO_FLAGS);
	if (0 == keymap) {
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto destroy_keyboard_ctx;
	}

	struct xkb_state *keyboard_state =
	    xkb_x11_state_new_from_device(keymap, connection,
	                                  device_id);
	if (0 == keyboard_state) {
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto destroy_keymap;
	}

	gui->connection = connection;
	gui->controller = controller;
	gui->notifier = notifier;
	gui->controller_task = controller_task;
	gui->device_id = device_id;
	gui->height = 1;
	gui->keyboard_ctx = keyboard_ctx;
	gui->keyboard_state = keyboard_state;
	gui->keymap = keymap;
	gui->notice_task = notice_task;
	gui->poll_conn_task = poll_conn_task;
	gui->pool = pool;
	gui->width = 1;
	gui->window = 0;
	gui->window_ko = window_ko;
	gui->focused = true;

	lntd_async_pool_submit(
	    pool, lntd_window_task_watch_prepare(
	              notice_task,
	              (union lntd_async_ck){.u64 = ON_RECEIVE_NOTICE},
	              notice_task, notifier));

	lntd_async_pool_submit(
	    pool, lntd_io_task_poll_prepare(
	              poll_conn_task,
	              (union lntd_async_ck){.u64 = ON_POLL_CONN},
	              poll_conn_task,
	              xcb_get_file_descriptor(connection), POLLIN));

	err = gui_refresh_window(gui);
	if (err != 0)
		goto destroy_keyboard_state;

	return 0;

destroy_keyboard_state:
	xkb_state_unref(keyboard_state);

destroy_keymap:
	xkb_keymap_unref(keymap);

destroy_keyboard_ctx:
	xkb_context_unref(keyboard_ctx);

close_connection:
	xcb_disconnect(connection);

destroy_poll_conn_task:
	lntd_io_task_poll_destroy(poll_conn_task);

destroy_controller_task:
	lntd_controller_task_send_destroy(controller_task);

destroy_notice_task:
	lntd_window_task_watch_destroy(notice_task);

close_controller:
	lntd_ko_close(controller);

close_notifier:
	lntd_ko_close(notifier);

close_window_ko:
	lntd_ko_close(window_ko);

	return err;
}

static lntd_error gui_stop(struct gui *gui)
{
	struct lntd_window_task_watch *notice_task = gui->notice_task;
	struct lntd_controller_task_send *controller_task =
	    gui->controller_task;
	struct lntd_io_task_poll *poll_conn_task = gui->poll_conn_task;

	lntd_async_task_cancel(
	    lntd_window_task_watch_to_async(notice_task));
	lntd_async_task_cancel(
	    lntd_controller_task_send_to_async(controller_task));
	lntd_async_task_cancel(
	    lntd_io_task_poll_to_async(poll_conn_task));

	return 0;
}

static lntd_error gui_destroy(struct gui *gui)
{
	lntd_ko window_ko = gui->window_ko;
	lntd_ko notifier = gui->notifier;
	lntd_ko controller = gui->controller;

	struct lntd_window_task_watch *notice_task = gui->notice_task;
	struct lntd_controller_task_send *controller_task =
	    gui->controller_task;
	struct lntd_io_task_poll *poll_conn_task = gui->poll_conn_task;

	xcb_connection_t *connection = gui->connection;
	struct xkb_context *keyboard_ctx = gui->keyboard_ctx;
	struct xkb_keymap *keymap = gui->keymap;
	struct xkb_state *keyboard_state = gui->keyboard_state;

	xkb_state_unref(keyboard_state);

	xkb_keymap_unref(keymap);

	xkb_context_unref(keyboard_ctx);

	xcb_disconnect(connection);

	lntd_io_task_poll_destroy(poll_conn_task);

	lntd_controller_task_send_destroy(controller_task);

	lntd_window_task_watch_destroy(notice_task);

	lntd_ko_close(controller);

	lntd_ko_close(notifier);

	lntd_ko_close(window_ko);

	return 0;
}

static lntd_error dispatch(struct gui *gui, union lntd_async_ck task_ck,
                           void *userstate, lntd_error err)
{
	switch (task_ck.u64) {
	case ON_POLL_CONN:
		return gui_on_conn_ready(gui, userstate, err);

	case ON_RECEIVE_NOTICE:
		return gui_on_window_change(gui, userstate, err);

	case ON_SENT_CONTROL:
		return gui_on_sent_controller_state(gui, userstate,
		                                    err);

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

static lntd_error
gui_on_conn_ready(struct gui *gui,
                  struct lntd_io_task_poll *poll_conn_task,
                  lntd_error err)
{

	if (err != 0)
		return err;

	xcb_connection_t *connection = gui->connection;
	xcb_window_t window = gui->window;

	struct lntd_async_pool *pool = gui->pool;
	int32_t device_id = gui->device_id;
	struct xkb_state *keyboard_state = gui->keyboard_state;
	struct xkb_keymap *keymap = gui->keymap;
	bool focused = gui->focused;

	bool had_focus_change = false;

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
			if (!focused)
				break;

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
			if (!focused)
				break;

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
			    keyboard_state, keycode);
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
			gui->update.jumping = is_key_down;
			gui->update_pending = true;
			break;

		move_left:
			gui->update.left = is_key_down;
			gui->update_pending = true;
			break;

		move_right:
			gui->update.right = is_key_down;
			gui->update_pending = true;
			break;

		move_forward:
			gui->update.forward = is_key_down;
			gui->update_pending = true;
			break;

		move_backward:
			gui->update.back = is_key_down;
			gui->update_pending = true;
			break;
		}
		lntd_mem_free(event);
	}

	bool clear_controls = false;

	if (had_resize) {
		gui->width = resize_width;
		gui->height = resize_height;
	}

	if (had_motion)
		on_tilt(gui, motion_x, motion_y);

	if (window_destroyed) {
		clear_controls = true;
		goto clear;
	}

	if (mapping_notify) {
		struct xkb_state *new_state =
		    xkb_x11_state_new_from_device(keymap, connection,
		                                  device_id);
		xkb_state_unref(keyboard_state);
		gui->keyboard_state = new_state;
	}

	if (had_focus_change) {
		if (focused) {
			int x, y;
			err = get_mouse_position(connection, window, &x,
			                         &y);
			if (err != 0)
				return err;

			on_tilt(gui, x, y);
		} else {
			clear_controls = true;
		}
		gui->focused = focused;
	}

	/* All X11 processing should be done by this point */
	lntd_async_pool_submit(
	    pool, lntd_io_task_poll_prepare(
	              poll_conn_task,
	              (union lntd_async_ck){.u64 = ON_POLL_CONN},
	              poll_conn_task,
	              xcb_get_file_descriptor(connection), POLLIN));

clear:
	if (clear_controls) {
		gui->update.z_tilt = 0;
		gui->update.x_tilt = 0;

		gui->update.left = 0;
		gui->update.right = 0;
		gui->update.forward = 0;
		gui->update.back = 0;

		gui->update.jumping = 0;

		gui->update_pending = true;
	}

	maybe_update_controller(gui);

	return 0;
}

static lntd_error
gui_on_window_change(struct gui *gui,
                     struct lntd_window_task_watch *notice_task,
                     lntd_error err)
{

	if (err != 0)
		return err;

	lntd_ko notifier = gui->notifier;
	struct lntd_async_pool *pool = gui->pool;

	lntd_async_pool_submit(
	    pool, lntd_window_task_watch_prepare(
	              notice_task,
	              (union lntd_async_ck){.u64 = ON_RECEIVE_NOTICE},
	              notice_task, notifier));

	return gui_refresh_window(gui);
}

static lntd_error gui_on_sent_controller_state(
    struct gui *gui, struct lntd_controller_task_send *controller_task,
    lntd_error err)
{
	if (err != 0)
		return err;

	gui->update_in_progress = false;

	maybe_update_controller(gui);

	return 0;
}

static void maybe_update_controller(struct gui *gui)
{
	struct lntd_async_pool *pool = gui->pool;
	struct lntd_controller_task_send *controller_task =
	    gui->controller_task;
	lntd_ko controller = gui->controller;

	if (!gui->update_pending)
		return;

	if (gui->update_in_progress)
		return;

	lntd_async_pool_submit(
	    pool, lntd_controller_task_send_prepare(
	              controller_task,
	              (union lntd_async_ck){.u64 = ON_SENT_CONTROL},
	              controller_task, controller, &gui->update));

	gui->update_pending = false;
	gui->update_in_progress = true;
}

static lntd_error gui_remove_window(struct gui *gui)
{
	lntd_error err = 0;

	xcb_connection_t *connection = gui->connection;
	xcb_window_t window = gui->window;

	static uint32_t const no_opts[] = {0, 0};

	xcb_change_window_attributes(connection, window,
	                             XCB_CW_EVENT_MASK, no_opts);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_flush(connection);

	return 0;
}

static lntd_error gui_refresh_window(struct gui *gui)
{
	lntd_error err = 0;

	err = gui_remove_window(gui);
	if (err != 0)
		return err;

	xcb_connection_t *connection = gui->connection;
	lntd_window window_ko = gui->window_ko;

	uint_fast32_t window;
	{
		uint_fast32_t xx;
		err = lntd_window_read(window_ko, &xx);
		if (EPROTO == err)
			return 0;
		if (err != 0)
			return err;
		window = xx;
	}
	gui->window = window;

	xcb_change_window_attributes(connection, window,
	                             XCB_CW_EVENT_MASK, window_opts);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, window);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_query_pointer_cookie_t point_ck =
	    xcb_query_pointer(connection, window);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

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

		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		if (error != 0) {
			err = lntd_xcb_error(error);
			lntd_mem_free(error);
			return err;
		}

		width = reply->width;
		height = reply->height;

		lntd_mem_free(reply);
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

		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		if (error != 0) {
			err = lntd_xcb_error(error);
			lntd_mem_free(error);
			return err;
		}

		x = reply->win_x;
		y = reply->win_y;

		lntd_mem_free(reply);
	}

	xcb_flush(connection);

	gui->width = width;
	gui->height = height;

	on_tilt(gui, x, y);

	maybe_update_controller(gui);

	return 0;
}

static void on_tilt(struct gui *gui, int_fast32_t mouse_x,
                    int_fast32_t mouse_y)
{
	unsigned width = gui->width;
	unsigned height = gui->height;

	int32_t x = (2 * mouse_x - (int)width) / 2;
	int32_t y = (2 * mouse_y - (int)height) / 2;

	/* Normalize and scale up to UINT32_MAX sized screen */
	x *= INT32_MAX / (intmax_t)width;
	y *= INT32_MAX / (intmax_t)height;

	gui->update.z_tilt = x;
	gui->update.x_tilt = y;

	gui->update_pending = true;
}

static lntd_error get_mouse_position(xcb_connection_t *connection,
                                     xcb_window_t window, int *xp,
                                     int *yp)
{
	lntd_error err;

	xcb_query_pointer_cookie_t ck =
	    xcb_query_pointer(connection, window);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_generic_error_t *error;
	xcb_query_pointer_reply_t *reply;
	{
		xcb_generic_error_t *xx = 0;
		reply = xcb_query_pointer_reply(connection, ck, &xx);
		error = xx;
	}
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	if (error != 0) {
		err = lntd_xcb_error(error);
		lntd_mem_free(error);
		LNTD_ASSUME(err != 0);
		return err;
	}

	int x = reply->win_x;
	int y = reply->win_x;

	lntd_mem_free(reply);

	*xp = x;
	*yp = y;

	return 0;
}
