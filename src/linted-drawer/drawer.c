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
#include "linted/sched.h"
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
 * @todo Handle sudden window death better.
 */

enum { ON_IDLE,
       ON_RECEIVE_UPDATE,
       ON_POLL_CONN,
       ON_RECEIVE_NOTICE,
       MAX_TASKS };

struct drawer {
	struct linted_async_pool *pool;
	struct linted_gpu_context *gpu_context;
	struct linted_sched_task_idle *idle_task;
	xcb_connection_t *connection;
	xcb_window_t window;
	linted_window window_ko;
	bool window_viewable : 1U;
	bool idle_in_progress : 1U;
};

static unsigned char drawer_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

static linted_error dispatch(struct drawer *drawer,
                             struct linted_async_task *task);
static linted_error on_idle(struct drawer *drawer,
                            struct linted_async_task *task);
static linted_error on_poll_conn(struct drawer *drawer,
                                 struct linted_async_task *task);
static linted_error on_receive_update(struct drawer *drawer,
                                      struct linted_async_task *task);
static linted_error on_receive_notice(struct drawer *drawer,
                                      struct linted_async_task *task);

static void maybe_idle(struct drawer *drawer);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-drawer",
    .start = drawer_start};

static uint32_t const window_opts[] = {XCB_EVENT_MASK_STRUCTURE_NOTIFY,
                                       0};

static unsigned char drawer_start(char const *process_name, size_t argc,
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
	char const *updater_path = argv[3U];

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

	linted_ko updater;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, updater_path,
		                     LINTED_KO_RDWR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		updater = xx;
	}

	struct linted_async_pool *pool;
	{
		struct linted_async_pool *xx;
		err = linted_async_pool_create(&xx, MAX_TASKS);
		if (err != 0)
			return err;
		pool = xx;
	}

	struct linted_sched_task_idle *idle_task;
	struct linted_window_task_watch *notice_task;
	struct linted_updater_task_receive *updater_task;
	struct linted_io_task_poll *poll_conn_task;

	{
		struct linted_sched_task_idle *xx;
		err = linted_sched_task_idle_create(&xx, 0);
		if (err != 0)
			goto destroy_pool;
		idle_task = xx;
	}

	{
		struct linted_window_task_watch *xx;
		err = linted_window_task_watch_create(&xx, 0);
		if (err != 0)
			goto destroy_pool;
		notice_task = xx;
	}

	{
		struct linted_updater_task_receive *xx;
		err = linted_updater_task_receive_create(&xx, 0);
		if (err != 0)
			goto destroy_pool;
		updater_task = xx;
	}

	{
		struct linted_io_task_poll *xx;
		err = linted_io_task_poll_create(&xx, 0);
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
		goto close_display;

	struct linted_gpu_context *gpu_context;
	{
		struct linted_gpu_context *xx;
		err = linted_gpu_context_create(&xx);
		if (err != 0)
			goto close_display;
		gpu_context = xx;
	}

	struct drawer drawer = {0};

	drawer.connection = connection;
	drawer.gpu_context = gpu_context;
	drawer.idle_task = idle_task;
	drawer.pool = pool;
	drawer.window = 0;
	drawer.window_ko = window_ko;
	drawer.window_viewable = false;
	drawer.idle_in_progress = false;

	{
		uint_fast32_t xx;
		err = linted_window_read(window_ko, &xx);
		if (err != 0)
			goto on_window_read_err;
		drawer.window = xx;
	}
on_window_read_err:
	if (EPROTO == err) {
		/* Do nothing */
	} else if (err != 0) {
		return err;
	} else {
		xcb_change_window_attributes(connection, drawer.window,
		                             XCB_CW_EVENT_MASK,
		                             window_opts);
		err = linted_xcb_conn_error(connection);
		if (err != 0)
			return err;

		xcb_get_geometry_cookie_t geom_ck =
		    xcb_get_geometry(connection, drawer.window);
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

		drawer.window_viewable = true;

		linted_gpu_set_x11_window(gpu_context, drawer.window);
		linted_gpu_resize(gpu_context, width, height);

		maybe_idle(&drawer);
	}

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

	linted_updater_task_receive_prepare(
	    updater_task,
	    (union linted_async_ck){.u64 = ON_RECEIVE_UPDATE}, updater);
	linted_async_pool_submit(
	    pool, linted_updater_task_receive_to_async(updater_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		struct linted_async_task *completed_task;
		{
			struct linted_async_task *xx;
			err = linted_async_pool_wait(pool, &xx);
			if (err != 0)
				goto stop_pool;
			completed_task = xx;
		}

		err = dispatch(&drawer, completed_task);
		if (err != 0)
			goto stop_pool;
	}

stop_pool:
	linted_async_task_cancel(
	    linted_sched_task_idle_to_async(idle_task));
	linted_async_task_cancel(
	    linted_updater_task_receive_to_async(updater_task));
	linted_async_task_cancel(
	    linted_window_task_watch_to_async(notice_task));
	linted_async_task_cancel(
	    linted_io_task_poll_to_async(poll_conn_task));

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

	linted_gpu_context_destroy(gpu_context);

close_display:
	xcb_disconnect(connection);

destroy_pool : {
	linted_error destroy_err = linted_async_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;
}
	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)idle_task;
	(void)updater_task;
	(void)notice_task;
	(void)poll_conn_task;

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error dispatch(struct drawer *drawer,
                             struct linted_async_task *task)
{
	switch (linted_async_task_ck(task).u64) {
	case ON_IDLE:
		return on_idle(drawer, task);

	case ON_RECEIVE_UPDATE:
		return on_receive_update(drawer, task);

	case ON_POLL_CONN:
		return on_poll_conn(drawer, task);

	case ON_RECEIVE_NOTICE:
		return on_receive_notice(drawer, task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_idle(struct drawer *drawer,
                            struct linted_async_task *task)
{
	struct linted_gpu_context *gpu_context = drawer->gpu_context;

	drawer->idle_in_progress = false;

	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	/* Draw or resize if we have time to waste */
	err = linted_gpu_draw(gpu_context);
	if (err != 0)
		return err;

	maybe_idle(drawer);

	return 0;
}

static linted_error on_poll_conn(struct drawer *drawer,
                                 struct linted_async_task *task)
{
	xcb_connection_t *connection = drawer->connection;
	struct linted_async_pool *pool = drawer->pool;
	struct linted_gpu_context *gpu_context = drawer->gpu_context;

	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_io_task_poll *poll_conn_task =
	    linted_io_task_poll_from_async(task);

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

	if (window_destroyed) {
		linted_gpu_remove_window(gpu_context);
		drawer->window_viewable = false;
	}

	if (drawer->window_viewable)
		maybe_idle(drawer);

	linted_io_task_poll_prepare(
	    poll_conn_task,
	    (union linted_async_ck){.u64 = ON_POLL_CONN},
	    xcb_get_file_descriptor(connection), POLLIN);

	linted_async_pool_submit(pool, task);

	return 0;
}

static linted_error on_receive_update(struct drawer *drawer,
                                      struct linted_async_task *task)
{
	struct linted_gpu_context *gpu_context = drawer->gpu_context;
	struct linted_async_pool *pool = drawer->pool;

	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_updater_task_receive *updater_task =
	    linted_updater_task_receive_from_async(task);

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

static linted_error on_receive_notice(struct drawer *drawer,
                                      struct linted_async_task *task)
{
	xcb_connection_t *connection = drawer->connection;

	struct linted_async_pool *pool = drawer->pool;
	linted_window window_ko = drawer->window_ko;
	struct linted_gpu_context *gpu_context = drawer->gpu_context;

	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	uint_fast32_t new_window;
	{
		uint_fast32_t xx;
		err = linted_window_read(window_ko, &xx);
		if (EPROTO == err) {
			err = 0;
			goto reset_notice;
		}
		if (err != 0)
			goto reset_notice;
		new_window = xx;
	}

	xcb_change_window_attributes_checked(
	    connection, new_window, XCB_CW_EVENT_MASK, window_opts);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto reset_notice;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, new_window);
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

	drawer->window_viewable = true;
	drawer->window = new_window;

	linted_gpu_set_x11_window(gpu_context, new_window);
	linted_gpu_resize(gpu_context, width, height);

	maybe_idle(drawer);

reset_notice:
	linted_async_pool_submit(pool, task);

	return err;
}

static void maybe_idle(struct drawer *drawer)
{
	struct linted_sched_task_idle *idle_task = drawer->idle_task;
	struct linted_async_pool *pool = drawer->pool;
	bool idle_in_progress = drawer->idle_in_progress;
	bool window_viewable = drawer->window_viewable;

	if (idle_in_progress)
		return;

	if (!window_viewable)
		return;

	drawer->idle_in_progress = true;

	linted_sched_task_idle_prepare(
	    idle_task, (union linted_async_ck){.u64 = ON_IDLE});
	linted_async_pool_submit(
	    pool, linted_sched_task_idle_to_async(idle_task));
}
