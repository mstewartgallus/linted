/*
 * Copyright 2015 Steven Stewart-Gallus
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
#include "config.h"

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/util.h"
#include "lntd/window.h"
#include "lntd/xcb.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>

#include <xcb/xcb.h>
#include <xcb/xkb.h>
#include <xkbcommon/xkbcommon.h>
#include <xkbcommon/xkbcommon-keysyms.h>
#include <xkbcommon/xkbcommon-x11.h>

module LntdGuiP
{
	uses interface LntdMainLoop;
	uses interface LntdLogger;
	uses interface LntdStdio;

	uses interface LntdPoller as Poller;
	uses interface LntdReader as WindowNotifier;
	uses interface LntdControllerWriter as ControllerWriter;
}
implementation
{
	lntd_ko window_ko;
	lntd_ko window_notifier;
	lntd_ko controller;

	struct lntd_controller_writer_input update;

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

	bool focused;
	bool update_pending;

	char dummy;

	uint32_t const window_opts[] = {
	    XCB_EVENT_MASK_FOCUS_CHANGE |
	        XCB_EVENT_MASK_POINTER_MOTION |
	        XCB_EVENT_MASK_STRUCTURE_NOTIFY |
	        XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_KEY_RELEASE,
	    0};

	void finish(lntd_error err);
	lntd_error gui_remove_window(void);
	lntd_error gui_refresh_window(void);
	void maybe_update_controller(void);
	void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y);
	lntd_error get_mouse_position(xcb_connection_t * connection,
	                              xcb_window_t window, int *xp,
	                              int *yp);

	event void LntdMainLoop.boot(size_t argc,
	                             char const *const *argv)
	{
		lntd_error err = 0;

		char const *window_path;
		char const *window_notifier_path;
		char const *controller_path;

		int32_t device_id;
		xcb_xkb_get_device_info_cookie_t device_info_ck;

		if (argc < 4U) {
			call LntdLogger.log(
			    LNTD_LOGGER_ERROR,
			    "missing some of 3 file operands!");
			call LntdMainLoop.exit(EXIT_FAILURE);
			return;
		}

		window_path = argv[1U];
		window_notifier_path = argv[2U];
		controller_path = argv[3U];

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   window_path, LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			window_ko = xx;
		}

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   window_notifier_path,
			                   LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			window_notifier = xx;
		}

		{
			lntd_ko xx;
			err =
			    lntd_ko_open(&xx, LNTD_KO_CWD,
			                 controller_path, LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			controller = xx;
		}

		connection = xcb_connect(0, 0);
		if (0 == connection) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}
		err = lntd_xcb_conn_error(connection);
		if (err != 0) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}

		if (!xkb_x11_setup_xkb_extension(
		        connection, XKB_X11_MIN_MAJOR_XKB_VERSION,
		        XKB_X11_MIN_MINOR_XKB_VERSION, 0, 0, 0, 0, 0)) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}

		keyboard_ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
		if (0 == keyboard_ctx) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}

		device_info_ck = xcb_xkb_get_device_info(
		    connection, XCB_XKB_ID_USE_CORE_KBD, 0, 0, 0, 0, 0,
		    0);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			goto destroy_window;

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
				goto destroy_window;

			if (error != 0) {
				err = lntd_xcb_error(error);
				lntd_mem_free(error);
				goto destroy_window;
			}

			device_id = reply->deviceID;

			lntd_mem_free(reply);
		}

		keymap = xkb_x11_keymap_new_from_device(
		    keyboard_ctx, connection, device_id,
		    XKB_KEYMAP_COMPILE_NO_FLAGS);
		if (0 == keymap) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}

		keyboard_state = xkb_x11_state_new_from_device(
		    keymap, connection, device_id);
		if (0 == keyboard_state) {
			err = LNTD_ERROR_UNIMPLEMENTED;
			goto destroy_window;
		}

		call WindowNotifier.execute(window_notifier, &dummy,
		                            sizeof dummy);
		call Poller.execute(xcb_get_file_descriptor(connection),
		                    LNTD_ASYNC_POLLER_IN);

		return;

	destroy_window:
		finish(err);
	}

	event int LntdMainLoop.shutdown(lntd_error err)
	{
		lntd_ko_close(window_ko);
		lntd_ko_close(window_notifier);
		lntd_ko_close(controller);

		if (err != 0)
			return EXIT_FAILURE;

		return EXIT_SUCCESS;
	}

	event void LntdMainLoop.recv_cancel(void)
	{
		finish(0);
	}

	event void Poller.poll_done(lntd_error err,
	                            uint_fast64_t revents)
	{
		bool had_focus_change = false;

		bool had_motion = false;
		int motion_x;
		int motion_y;

		bool had_resize = false;
		unsigned resize_width;
		unsigned resize_height;

		bool mapping_notify = false;
		bool window_destroyed = false;

		bool clear_controls;

		if (err != 0) {
			finish(err);
			return;
		}

		for (;;) {
			xcb_generic_event_t *myevent;
			bool is_key_down;

			myevent = xcb_poll_for_event(connection);
			if (0 == myevent)
				break;

			switch (myevent->response_type & ~0x80) {
			case XCB_CONFIGURE_NOTIFY: {
				xcb_configure_notify_event_t const *
				    configure_event = (void *)myevent;

				resize_width = configure_event->width;
				resize_height = configure_event->height;
				had_resize = true;
				break;
			}

			case XCB_DESTROY_NOTIFY:
				window_destroyed = true;
				break;

			case XCB_MOTION_NOTIFY: {
				xcb_motion_notify_event_t const *
				    motion_event;

				if (!focused)
					break;

				motion_event = (void *)myevent;

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
				    (void *)myevent;
				xcb_keycode_t keycode =
				    key_event->detail;
				xkb_keysym_t keysym =
				    xkb_state_key_get_one_sym(
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
				update.jumping = is_key_down;
				update_pending = true;
				break;

			move_left:
				update.left = is_key_down;
				update_pending = true;
				break;

			move_right:
				update.right = is_key_down;
				update_pending = true;
				break;

			move_forward:
				update.forward = is_key_down;
				update_pending = true;
				break;

			move_backward:
				update.back = is_key_down;
				update_pending = true;
				break;
			}
			lntd_mem_free(myevent);
		}

		clear_controls = false;

		if (had_resize) {
			width = resize_width;
			height = resize_height;
		}

		if (had_motion)
			on_tilt(motion_x, motion_y);

		if (window_destroyed) {
			clear_controls = true;
			goto clear;
		}

		if (mapping_notify) {
			struct xkb_state *new_state;

			new_state = xkb_x11_state_new_from_device(
			    keymap, connection, device_id);
			xkb_state_unref(keyboard_state);
			keyboard_state = new_state;
		}

		if (had_focus_change) {
			if (focused) {
				int x, y;
				err = get_mouse_position(
				    connection, window, &x, &y);
				if (err != 0) {
					finish(err);
					return;
				}

				on_tilt(x, y);
			} else {
				clear_controls = true;
			}
			focused = focused;
		}

		/* All X11 processing should be done by this point */
		call Poller.execute(xcb_get_file_descriptor(connection),
		                    LNTD_ASYNC_POLLER_IN);

	clear:
		if (clear_controls) {
			update.z_tilt = 0;
			update.x_tilt = 0;

			update.left = 0;
			update.right = 0;
			update.forward = 0;
			update.back = 0;

			update.jumping = 0;

			update_pending = true;
		}

		maybe_update_controller();
	}

	event void ControllerWriter.write_done(lntd_error err)
	{
		if (err != 0) {
			finish(err);
			return;
		}

		maybe_update_controller();
	}

	event void WindowNotifier.read_done(lntd_error err,
	                                    size_t bytes)
	{
		if (err != 0) {
			finish(err);
			return;
		}

		if (bytes != sizeof dummy) {
			finish(EPROTO);
			return;
		}

		call WindowNotifier.execute(window_notifier, &dummy,
		                            sizeof dummy);

		err = gui_refresh_window();
		if (err != 0) {
			finish(err);
			return;
		}
	}

	void finish(lntd_error err)
	{
		if (err != 0) {
			char *errmsg;

			errno = err;
			if (-1 == asprintf(&errmsg, "%m")) {
				return;
			}

			call LntdLogger.log(LNTD_LOGGER_ERROR, errmsg);

			free(errmsg);
		}

		call LntdMainLoop.exit(err);
	}

	lntd_error gui_remove_window(void)
	{
		lntd_error err = 0;

		static uint32_t const no_opts[] = {0, 0};

		xcb_change_window_attributes(
		    connection, window, XCB_CW_EVENT_MASK, no_opts);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		xcb_flush(connection);

		return 0;
	}

	lntd_error gui_refresh_window(void)
	{
		lntd_error err = 0;

		uint_fast32_t new_window;
		xcb_get_geometry_cookie_t geom_ck;
		xcb_query_pointer_cookie_t point_ck;
		int x, y;

		err = gui_remove_window();
		if (err != 0)
			return err;

		{
			uint_fast32_t xx;
			err = lntd_window_read(window_ko, &xx);
			if (EPROTO == err)
				return 0;
			if (err != 0)
				return err;
			new_window = xx;
		}
		window = new_window;

		xcb_change_window_attributes(connection, new_window,
		                             XCB_CW_EVENT_MASK,
		                             window_opts);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		geom_ck = xcb_get_geometry(connection, new_window);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		point_ck = xcb_query_pointer(connection, window);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		{
			xcb_generic_error_t *error;
			xcb_get_geometry_reply_t *reply;
			{
				xcb_generic_error_t *xx = 0;
				reply = xcb_get_geometry_reply(
				    connection, geom_ck, &xx);
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

		{
			xcb_generic_error_t *error;
			xcb_query_pointer_reply_t *reply;
			{
				xcb_generic_error_t *xx = 0;
				reply = xcb_query_pointer_reply(
				    connection, point_ck, &xx);
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

		width = width;
		height = height;

		on_tilt(x, y);

		maybe_update_controller();

		return 0;
	}

	void maybe_update_controller(void)
	{
		if (!update_pending)
			return;

		update_pending = true;
		call ControllerWriter.write(controller, &update);
	}

	void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y)
	{
		int32_t x = (2 * mouse_x - (int)width) / 2;
		int32_t y = (2 * mouse_y - (int)height) / 2;

		/* Normalize and scale up to UINT32_MAX sized screen */
		x *= INT32_MAX / (intmax_t)width;
		y *= INT32_MAX / (intmax_t)height;

		update.z_tilt = x;
		update.x_tilt = y;

		update_pending = true;
	}

	lntd_error get_mouse_position(xcb_connection_t * connection,
	                              xcb_window_t window, int *xp,
	                              int *yp)
	{
		lntd_error err;

		xcb_query_pointer_cookie_t ck;
		xcb_generic_error_t *error;
		xcb_query_pointer_reply_t *reply;
		int x;
		int y;

		ck = xcb_query_pointer(connection, window);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		{
			xcb_generic_error_t *xx = 0;
			reply = xcb_query_pointer_reply(connection, ck,
			                                &xx);
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

		x = reply->win_x;
		y = reply->win_x;

		lntd_mem_free(reply);

		*xp = x;
		*yp = y;

		return 0;
	}
}
