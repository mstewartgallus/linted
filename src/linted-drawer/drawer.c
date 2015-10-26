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
#include "config.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/gpu.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/updater.h"
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
 *offscreen.
 *      This is not our bug but and happens glxgears too.
 */

struct drawer;

static linted_error
drawer_init(struct drawer *drawer, struct linted_async_pool *pool,
            char const *process_name, char const *window_path,
            char const *notifier_path, char const *updater_path);

static linted_error drawer_start(struct drawer *drawer);
static linted_error drawer_stop(struct drawer *drawer);
static linted_error drawer_destroy(struct drawer *drawer);

static linted_error dispatch(struct drawer *drawer,
                             struct linted_async_task *task);
static linted_error
drawer_on_conn_ready(struct drawer *drawer,
                     struct linted_async_task *task);
static linted_error
drawer_on_update_recved(struct drawer *drawer,
                        struct linted_async_task *task);
static linted_error
drawer_on_notice_recved(struct drawer *drawer,
                        struct linted_async_task *task);
static linted_error drawer_update_window(struct drawer *drawer);

struct drawer {
	struct linted_async_pool *pool;
	struct linted_gpu_context *gpu_context;
	struct linted_io_task_poll *poll_conn_task;
	struct linted_update_task_recv *updater_task;
	struct linted_window_task_watch *notice_task;
	xcb_connection_t *connection;
	xcb_window_t window;
	linted_window_notifier notifier;
	linted_updater updater;
	linted_window window_ko;

	bool window_viewable : 1U;
};

enum { ON_RECEIVE_UPDATE, ON_POLL_CONN, ON_RECEIVE_NOTICE, MAX_TASKS };

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-drawer", 0};

static uint32_t const window_opts[] = {XCB_EVENT_MASK_STRUCTURE_NOTIFY,
                                       0};

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	if (argc < 4U)
		return LINTED_ERROR_INVALID_PARAMETER;

	char const *window_path = argv[1U];
	char const *notifier_path = argv[2U];
	char const *updater_path = argv[3U];

	struct linted_async_pool *pool;
	{
		struct linted_async_pool *xx;
		err = linted_async_pool_create(&xx, MAX_TASKS);
		if (err != 0)
			return err;
		pool = xx;
	}

	static struct drawer drawer = {0};

	err = drawer_init(&drawer, pool, process_name, window_path,
	                  notifier_path, updater_path);
	if (err != 0)
		goto destroy_pool;

	err = drawer_start(&drawer);
	if (err != 0)
		goto destroy_drawer;

	for (;;) {
		struct linted_async_task *completed_task;
		{
			struct linted_async_task *xx;
			linted_async_pool_wait(pool, &xx);
			completed_task = xx;
		}

		err = dispatch(&drawer, completed_task);
		if (err != 0)
			goto stop_drawer;
	}
stop_drawer:
	drawer_stop(&drawer);

	for (;;) {
		struct linted_async_task *completed_task;
		linted_error poll_err;
		{
			struct linted_async_task *xx;
			poll_err = linted_async_pool_poll(pool, &xx);
			if (LINTED_ERROR_AGAIN == poll_err)
				break;
			completed_task = xx;
		}

		linted_error dispatch_err =
		    linted_async_task_err(completed_task);
		if (0 == err && dispatch_err != LINTED_ERROR_CANCELLED)
			err = dispatch_err;
	}

destroy_drawer:
	drawer_destroy(&drawer);

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
drawer_init(struct drawer *drawer, struct linted_async_pool *pool,
            char const *process_name, char const *window_path,
            char const *notifier_path, char const *updater_path)
{
	linted_error err = 0;

	linted_window window_ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, window_path,
		                     LINTED_KO_RDWR);
		if (err != 0)
			return err;
		window_ko = xx;
	}

	linted_window_notifier notifier;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, notifier_path,
		                     LINTED_KO_RDWR);
		if (err != 0)
			goto close_window_ko;
		notifier = xx;
	}

	linted_ko updater;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, updater_path,
		                     LINTED_KO_RDWR);
		if (err != 0)
			goto close_notifier;
		updater = xx;
	}

	struct linted_window_task_watch *notice_task;
	{
		struct linted_window_task_watch *xx;
		err = linted_window_task_watch_create(&xx, 0);
		if (err != 0)
			goto close_updater;
		notice_task = xx;
	}

	struct linted_update_task_recv *updater_task;
	{
		struct linted_update_task_recv *xx;
		err = linted_update_task_recv_create(&xx, 0);
		if (err != 0)
			goto free_notice_task;
		updater_task = xx;
	}

	struct linted_io_task_poll *poll_conn_task;
	{
		struct linted_io_task_poll *xx;
		err = linted_io_task_poll_create(&xx, 0);
		if (err != 0)
			goto free_updater_task;
		poll_conn_task = xx;
	}

	xcb_connection_t *connection = xcb_connect(0, 0);
	if (0 == connection) {
		err = LINTED_ERROR_UNIMPLEMENTED;
		goto free_poll_conn_task;
	}
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto close_conn;

	struct linted_gpu_context *gpu_context;
	{
		struct linted_gpu_context *xx;
		err = linted_gpu_context_create(&xx);
		if (err != 0)
			goto close_conn;
		gpu_context = xx;
	}

	drawer->connection = connection;
	drawer->gpu_context = gpu_context;
	drawer->notice_task = notice_task;
	drawer->poll_conn_task = poll_conn_task;
	drawer->updater_task = updater_task;
	drawer->pool = pool;
	drawer->window = 0;
	drawer->notifier = notifier;
	drawer->window_ko = window_ko;
	drawer->updater = updater;
	drawer->window_viewable = false;

	return 0;

close_conn:
	xcb_disconnect(connection);

free_poll_conn_task:
	linted_io_task_poll_destroy(poll_conn_task);

free_updater_task:
	linted_update_task_recv_destroy(updater_task);

free_notice_task:
	linted_window_task_watch_destroy(notice_task);

close_updater:
	linted_ko_close(updater);

close_notifier:
	linted_ko_close(notifier);

close_window_ko:
	linted_ko_close(window_ko);

	return err;
}

static linted_error drawer_destroy(struct drawer *drawer)
{
	linted_ko window_ko = drawer->window_ko;
	linted_ko updater = drawer->updater;
	linted_ko notifier = drawer->notifier;

	struct linted_gpu_context *gpu_context = drawer->gpu_context;
	xcb_connection_t *connection = drawer->connection;

	struct linted_update_task_recv *updater_task =
	    drawer->updater_task;
	struct linted_window_task_watch *notice_task =
	    drawer->notice_task;
	struct linted_io_task_poll *poll_conn_task =
	    drawer->poll_conn_task;

	linted_gpu_context_destroy(gpu_context);

	xcb_disconnect(connection);

	linted_io_task_poll_destroy(poll_conn_task);

	linted_update_task_recv_destroy(updater_task);

	linted_window_task_watch_destroy(notice_task);

	linted_ko_close(updater);

	linted_ko_close(notifier);

	linted_ko_close(window_ko);

	return 0;
}

static linted_error drawer_start(struct drawer *drawer)
{
	struct linted_async_pool *pool = drawer->pool;
	linted_ko updater = drawer->updater;
	linted_ko notifier = drawer->notifier;
	xcb_connection_t *connection = drawer->connection;

	struct linted_io_task_poll *poll_conn_task =
	    drawer->poll_conn_task;
	struct linted_update_task_recv *updater_task =
	    drawer->updater_task;
	struct linted_window_task_watch *notice_task =
	    drawer->notice_task;

	linted_window_task_watch_prepare(
	    notice_task,
	    (union linted_async_ck){.u64 = ON_RECEIVE_NOTICE},
	    notifier);
	linted_async_pool_submit(
	    pool, linted_window_task_watch_to_async(notice_task));

	linted_io_task_poll_prepare(
	    poll_conn_task,
	    (union linted_async_ck){.u64 = ON_POLL_CONN},
	    xcb_get_file_descriptor(connection), POLLIN);
	linted_async_pool_submit(
	    pool, linted_io_task_poll_to_async(poll_conn_task));

	linted_update_task_recv_prepare(
	    updater_task,
	    (union linted_async_ck){.u64 = ON_RECEIVE_UPDATE}, updater);
	linted_async_pool_submit(
	    pool, linted_update_task_recv_to_async(updater_task));

	return drawer_update_window(drawer);
}

static linted_error drawer_stop(struct drawer *drawer)
{
	struct linted_update_task_recv *updater_task =
	    drawer->updater_task;
	struct linted_window_task_watch *notice_task =
	    drawer->notice_task;
	struct linted_io_task_poll *poll_conn_task =
	    drawer->poll_conn_task;

	linted_async_task_cancel(
	    linted_update_task_recv_to_async(updater_task));
	linted_async_task_cancel(
	    linted_window_task_watch_to_async(notice_task));
	linted_async_task_cancel(
	    linted_io_task_poll_to_async(poll_conn_task));

	return 0;
}

static linted_error dispatch(struct drawer *drawer,
                             struct linted_async_task *task)
{
	switch (linted_async_task_ck(task).u64) {
	case ON_RECEIVE_UPDATE:
		return drawer_on_update_recved(drawer, task);

	case ON_POLL_CONN:
		return drawer_on_conn_ready(drawer, task);

	case ON_RECEIVE_NOTICE:
		return drawer_on_notice_recved(drawer, task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error drawer_on_conn_ready(struct drawer *drawer,
                                         struct linted_async_task *task)
{
	xcb_connection_t *connection = drawer->connection;
	struct linted_async_pool *pool = drawer->pool;
	struct linted_gpu_context *gpu_context = drawer->gpu_context;

	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	bool window_destroyed = false;
	for (;;) {
		xcb_generic_event_t *event =
		    xcb_poll_for_event(connection);
		if (0 == event)
			break;

		switch (event->response_type & ~0x80) {
		case XCB_CONFIGURE_NOTIFY: {
			xcb_configure_notify_event_t const *
			    configure_event = (void *)event;

			unsigned width = configure_event->width;
			unsigned height = configure_event->height;

			linted_gpu_resize(gpu_context, width, height);
			break;
		}

		case XCB_UNMAP_NOTIFY:
			drawer->window_viewable = false;
			break;

		case XCB_MAP_NOTIFY:
			drawer->window_viewable = true;
			break;

		case XCB_DESTROY_NOTIFY:
			window_destroyed = true;
			break;

		default:
			/* Unknown event type, ignore it */
			break;
		}
		linted_mem_free(event);
	}

	{
		struct linted_io_task_poll *poll_conn_task =
		    drawer->poll_conn_task;

		linted_io_task_poll_prepare(
		    poll_conn_task,
		    (union linted_async_ck){.u64 = ON_POLL_CONN},
		    xcb_get_file_descriptor(connection), POLLIN);

		linted_async_pool_submit(pool, task);
	}

	if (window_destroyed) {
		linted_gpu_remove_window(gpu_context);
		drawer->window_viewable = false;
	}

	return 0;
}

static linted_error
drawer_on_update_recved(struct drawer *drawer,
                        struct linted_async_task *task)
{
	struct linted_gpu_context *gpu_context = drawer->gpu_context;
	struct linted_async_pool *pool = drawer->pool;

	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_update_task_recv *updater_task =
	    linted_update_task_recv_from_async(task);

	linted_updater_int x_position;
	linted_updater_int y_position;
	linted_updater_int z_position;

	linted_updater_angle z_rotation;
	linted_updater_angle x_rotation;
	{
		struct linted_updater_update update;

		linted_updater_decode(updater_task, &update);

		z_rotation = update.z_rotation;
		x_rotation = update.x_rotation;

		x_position = update.x_position;
		y_position = update.y_position;
		z_position = update.z_position;
	}

	linted_async_pool_submit(pool, task);

	float gpu_z_rotation =
	    linted_updater_angle_to_double(z_rotation);
	float gpu_x_rotation =
	    linted_updater_angle_to_double(x_rotation);

	float gpu_x_position = x_position * (1 / 2048.0);
	float gpu_y_position = y_position * (1 / 2048.0);
	float gpu_z_position = z_position * (1 / 2048.0);

	struct linted_gpu_update gpu_update;

	gpu_update.z_rotation = gpu_z_rotation;
	gpu_update.x_rotation = gpu_x_rotation;

	gpu_update.x_position = gpu_x_position;
	gpu_update.y_position = gpu_y_position;
	gpu_update.z_position = gpu_z_position;

	linted_gpu_update_state(gpu_context, &gpu_update);

	return 0;
}

static linted_error
drawer_on_notice_recved(struct drawer *drawer,
                        struct linted_async_task *task)
{
	struct linted_async_pool *pool = drawer->pool;

	linted_error err = 0;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	err = drawer_update_window(drawer);

	linted_async_pool_submit(pool, task);

	return err;
}

static linted_error drawer_update_window(struct drawer *drawer)
{
	linted_error err = 0;

	xcb_connection_t *connection = drawer->connection;
	linted_window window_ko = drawer->window_ko;
	struct linted_gpu_context *gpu_context = drawer->gpu_context;

	uint_fast32_t new_window;
	{
		uint_fast32_t xx;
		err = linted_window_read(window_ko, &xx);
		if (EPROTO == err) {
			return 0;
		}
		if (err != 0)
			return err;
		new_window = xx;
	}

	xcb_change_window_attributes_checked(
	    connection, new_window, XCB_CW_EVENT_MASK, window_opts);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, new_window);
	err = linted_xcb_conn_error(connection);
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

	linted_gpu_remove_window(gpu_context);
	linted_gpu_set_x11_window(gpu_context, new_window);
	linted_gpu_resize(gpu_context, width, height);

	drawer->window_viewable = true;
	drawer->window = new_window;

	return 0;
}
