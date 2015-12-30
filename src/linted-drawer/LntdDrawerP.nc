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

#include "bool.h"

#include "lntd/error.h"
#include "lntd/gpu.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/util.h"
#include "lntd/window.h"
#include "lntd/xcb.h"

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <xcb/xcb.h>

/**
 * @file
 *
 * @bug Sudden window death freezes the drawer process on Mesa.
 *      Unfortunately, this is a deadlock deep inside Mesa's `glClear`
 *      and not really fixable.
 *
 * @todo Fall back to an error display in the window if the drawer fails
 *       to initialize.
 *
 * @bug If the window is moved partially offscreen and resized it fails
 *      to draw to a part of the window opposite from the part
 *      offscreen. This is not our bug but and happens glxgears too.
 *
 * @bug The GPU process can't be destroyed if the window is destroyed
 *      before and stuff hangs.
 */

module LntdDrawerP
{
	uses interface LntdMainLoop;
	uses interface LntdLogger;
	uses interface LntdStdio;

	uses interface LntdPoller as Poller;
	uses interface LntdReader as WindowNotifier;
	uses interface LntdUpdateReader as UpdateReader;
}
implementation
{
	lntd_ko updater;
	lntd_ko window_ko;
	lntd_ko notifier;

	struct lntd_gpu_context *gpu_context;
	xcb_connection_t *connection;
	xcb_window_t window;

	bool time_to_quit;

	char dummy;

	uint32_t const window_opts[] = {XCB_EVENT_MASK_STRUCTURE_NOTIFY,
	                                0};

	void finish(lntd_error err);
	lntd_error drawer_remove_window(void);
	lntd_error drawer_refresh_window(void);
	void maybe_update_controller(void);

	event void LntdMainLoop.boot(size_t argc,
	                             char const *const *argv)
	{
		lntd_error err = 0;

		char const *window_path;
		char const *window_notifier_path;
		char const *updater_path;

		if (argc < 4U) {
			call LntdLogger.log(
			    LNTD_LOGGER_ERROR,
			    "missing some of 3 file operands!");
			call LntdMainLoop.exit(EXIT_FAILURE);
			return;
		}

		window_path = argv[1U];
		window_notifier_path = argv[2U];
		updater_path = argv[3U];

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
			notifier = xx;
		}

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   updater_path, LNTD_KO_RDWR);
			if (err != 0)
				goto destroy_window;
			updater = xx;
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

		{
			struct lntd_gpu_context *xx;
			err = lntd_gpu_context_create(&xx);
			if (err != 0)
				goto destroy_window;
			gpu_context = xx;
		}

		call WindowNotifier.execute(notifier, &dummy,
		                            sizeof dummy);
		call Poller.execute(xcb_get_file_descriptor(connection),
		                    LNTD_ASYNC_POLLER_IN);
		call UpdateReader.start(updater);

		drawer_refresh_window();

		return;

	destroy_window:
		finish(err);
	}

	event int LntdMainLoop.shutdown(lntd_error err)
	{
		lntd_ko_close(window_ko);
		lntd_ko_close(notifier);
		lntd_ko_close(updater);

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
		bool window_destroyed = false;
		if (err != 0) {
			finish(err);
			return;
		}

		for (;;) {
			xcb_generic_event_t *myevent;

			myevent = xcb_poll_for_event(connection);
			if (0 == myevent)
				break;

			switch (myevent->response_type & ~0x80) {
			case XCB_CONFIGURE_NOTIFY: {
				xcb_configure_notify_event_t const *
				    configure_event = (void *)myevent;

				unsigned width = configure_event->width;
				unsigned height =
				    configure_event->height;

				lntd_gpu_resize(gpu_context, width,
				                height);
				break;
			}

			case XCB_UNMAP_NOTIFY:
				lntd_gpu_hide(gpu_context);
				break;

			case XCB_MAP_NOTIFY:
				lntd_gpu_show(gpu_context);
				break;

			case XCB_DESTROY_NOTIFY:
				window_destroyed = true;
				break;

			default:
				/* Unknown event type, ignore it */
				break;
			}
			lntd_mem_free(myevent);
		}

		call Poller.execute(xcb_get_file_descriptor(connection),
		                    LNTD_ASYNC_POLLER_IN);

		if (window_destroyed) {
			lntd_gpu_remove_window(gpu_context);
		}
	}

	event void UpdateReader.read_input(
	    lntd_error err,
	    struct lntd_update_reader_input const *update)
	{
		int_fast32_t x_position;
		int_fast32_t y_position;
		int_fast32_t z_position;

		uint_fast32_t z_rotation;
		uint_fast32_t x_rotation;

		float gpu_z_rotation;
		float gpu_x_rotation;

		float gpu_x_position;
		float gpu_y_position;
		float gpu_z_position;

		if (err != 0) {
			finish(err);
			return;
		}

		z_rotation = update->z_rotation;
		x_rotation = update->x_rotation;

		x_position = update->x_position;
		y_position = update->y_position;
		z_position = update->z_position;

		gpu_z_rotation =
		    z_rotation * (6.2831853071795864769252867665590 /
		                  (((uintmax_t)UINT32_MAX) + 1U));
		gpu_x_rotation =
		    x_rotation * (6.2831853071795864769252867665590 /
		                  (((uintmax_t)UINT32_MAX) + 1U));

		gpu_x_position = x_position * (1 / 4096.0);
		gpu_y_position = y_position * (1 / 4096.0);
		gpu_z_position = z_position * (1 / 4096.0);

		{
			struct lntd_gpu_update gpu_update;

			gpu_update.z_rotation = gpu_z_rotation;
			gpu_update.x_rotation = gpu_x_rotation;

			gpu_update.x_position = gpu_x_position;
			gpu_update.y_position = gpu_y_position;
			gpu_update.z_position = gpu_z_position;

			lntd_gpu_update_state(gpu_context, &gpu_update);
		}
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

		call WindowNotifier.execute(notifier, &dummy,
		                            sizeof dummy);

		err = drawer_refresh_window();
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

	lntd_error drawer_remove_window(void)
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

	lntd_error drawer_refresh_window(void)
	{
		lntd_error err = 0;

		uint_fast32_t new_window;
		xcb_get_geometry_cookie_t geom_ck;
		unsigned width, height;

		err = drawer_remove_window();
		if (err != 0)
			return err;

		{
			uint_fast32_t xx;
			err = lntd_window_read(window_ko, &xx);
			if (EPROTO == err) {
				return 0;
			}
			if (err != 0)
				return err;
			new_window = xx;
		}

		xcb_change_window_attributes_checked(
		    connection, new_window, XCB_CW_EVENT_MASK,
		    window_opts);
		err = lntd_xcb_conn_error(connection);
		if (err != 0)
			return err;

		geom_ck = xcb_get_geometry(connection, new_window);
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

		xcb_flush(connection);

		lntd_gpu_remove_window(gpu_context);
		lntd_gpu_set_x11_window(gpu_context, new_window);
		lntd_gpu_resize(gpu_context, width, height);

		window = new_window;

		return 0;
	}
}
