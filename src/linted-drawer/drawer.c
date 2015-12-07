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

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/gpu.h"
#include "lntd/io.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/signal.h"
#include "lntd/start.h"
#include "lntd/updater.h"
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

struct drawer;

static lntd_error
drawer_init(struct drawer *drawer, struct lntd_async_pool *pool,
            char const *process_name, char const *window_path,
            char const *notifier_path, char const *updater_path);

static lntd_error drawer_start(struct drawer *drawer);
static lntd_error drawer_stop(struct drawer *drawer);
static lntd_error drawer_destroy(struct drawer *drawer);

static lntd_error dispatch(struct drawer *drawer,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_error err);
static lntd_error
drawer_on_signal(struct drawer *drawer,
                 struct lntd_signal_task_wait *signal_wait_task,
                 lntd_error err);
static lntd_error
drawer_on_conn_ready(struct drawer *drawer,
                     struct lntd_io_task_poll *poll_conn_task,
                     lntd_error err);
static lntd_error
drawer_on_update_recved(struct drawer *drawer,
                        struct lntd_updater_task_recv *updater_task,
                        lntd_error err);
static lntd_error
drawer_on_notice_recved(struct drawer *drawer,
                        struct lntd_window_task_watch *notice_task,
                        lntd_error err);
static lntd_error drawer_remove_window(struct drawer *drawer);
static lntd_error drawer_update_window(struct drawer *drawer);

struct drawer {
	struct lntd_async_pool *pool;
	struct lntd_gpu_context *gpu_context;
	struct lntd_signal_task_wait *signal_wait_task;
	struct lntd_io_task_poll *poll_conn_task;
	struct lntd_updater_task_recv *updater_task;
	struct lntd_window_task_watch *notice_task;
	xcb_connection_t *connection;
	xcb_window_t window;
	lntd_window_notifier notifier;
	lntd_updater updater;
	lntd_window window_ko;

	bool time_to_quit : 1U;
};

enum { ON_SIGNAL,
       ON_RECEIVE_UPDATE,
       ON_POLL_CONN,
       ON_RECEIVE_NOTICE,
       MAX_TASKS };

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-drawer", 0};

static uint32_t const window_opts[] = {XCB_EVENT_MASK_STRUCTURE_NOTIFY,
                                       0};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	if (argc < 4U)
		return LNTD_ERROR_INVALID_PARAMETER;

	char const *window_path = argv[1U];
	char const *notifier_path = argv[2U];
	char const *updater_path = argv[3U];

	struct lntd_async_pool *pool;
	{
		struct lntd_async_pool *xx;
		err = lntd_async_pool_create(&xx, MAX_TASKS);
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
		struct lntd_async_result result;
		{
			struct lntd_async_result xx;
			lntd_async_pool_wait(pool, &xx);
			result = xx;
		}

		err = dispatch(&drawer, result.task_ck,
		               result.userstate, result.err);
		if (err != 0)
			goto stop_drawer;
		if (drawer.time_to_quit)
			break;
	}
stop_drawer:
	drawer_stop(&drawer);

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

destroy_drawer:
	drawer_destroy(&drawer);

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

static lntd_error
drawer_init(struct drawer *drawer, struct lntd_async_pool *pool,
            char const *process_name, char const *window_path,
            char const *notifier_path, char const *updater_path)
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
		err = lntd_ko_open(&xx, LNTD_KO_CWD, notifier_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			goto close_window_ko;
		notifier = xx;
	}

	lntd_ko updater;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, updater_path,
		                   LNTD_KO_RDWR);
		if (err != 0)
			goto close_notifier;
		updater = xx;
	}

	struct lntd_signal_task_wait *signal_wait_task;
	{
		struct lntd_signal_task_wait *xx;
		err = lntd_signal_task_wait_create(&xx, 0);
		if (err != 0)
			goto close_updater;
		signal_wait_task = xx;
	}

	struct lntd_window_task_watch *notice_task;
	{
		struct lntd_window_task_watch *xx;
		err = lntd_window_task_watch_create(&xx, 0);
		if (err != 0)
			goto free_signal_wait_task;
		notice_task = xx;
	}

	struct lntd_updater_task_recv *updater_task;
	{
		struct lntd_updater_task_recv *xx;
		err = lntd_updater_task_recv_create(&xx, 0);
		if (err != 0)
			goto free_notice_task;
		updater_task = xx;
	}

	struct lntd_io_task_poll *poll_conn_task;
	{
		struct lntd_io_task_poll *xx;
		err = lntd_io_task_poll_create(&xx, 0);
		if (err != 0)
			goto free_updater_task;
		poll_conn_task = xx;
	}

	xcb_connection_t *connection = xcb_connect(0, 0);
	if (0 == connection) {
		err = LNTD_ERROR_UNIMPLEMENTED;
		goto free_poll_conn_task;
	}
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		goto close_conn;

	struct lntd_gpu_context *gpu_context;
	{
		struct lntd_gpu_context *xx;
		err = lntd_gpu_context_create(&xx);
		if (err != 0)
			goto close_conn;
		gpu_context = xx;
	}

	drawer->connection = connection;
	drawer->gpu_context = gpu_context;
	drawer->signal_wait_task = signal_wait_task;
	drawer->notice_task = notice_task;
	drawer->poll_conn_task = poll_conn_task;
	drawer->updater_task = updater_task;
	drawer->pool = pool;
	drawer->window = 0;
	drawer->notifier = notifier;
	drawer->window_ko = window_ko;
	drawer->updater = updater;
	drawer->time_to_quit = false;

	return 0;

close_conn:
	xcb_disconnect(connection);

free_poll_conn_task:
	lntd_io_task_poll_destroy(poll_conn_task);

free_updater_task:
	lntd_updater_task_recv_destroy(updater_task);

free_notice_task:
	lntd_window_task_watch_destroy(notice_task);

free_signal_wait_task:
	lntd_signal_task_wait_destroy(signal_wait_task);

close_updater:
	lntd_ko_close(updater);

close_notifier:
	lntd_ko_close(notifier);

close_window_ko:
	lntd_ko_close(window_ko);

	return err;
}

static lntd_error drawer_destroy(struct drawer *drawer)
{
	lntd_ko window_ko = drawer->window_ko;
	lntd_ko updater = drawer->updater;
	lntd_ko notifier = drawer->notifier;

	xcb_connection_t *connection = drawer->connection;

	struct lntd_signal_task_wait *signal_wait_task =
	    drawer->signal_wait_task;
	struct lntd_updater_task_recv *updater_task =
	    drawer->updater_task;
	struct lntd_window_task_watch *notice_task =
	    drawer->notice_task;
	struct lntd_io_task_poll *poll_conn_task =
	    drawer->poll_conn_task;

	xcb_disconnect(connection);

	lntd_io_task_poll_destroy(poll_conn_task);

	lntd_updater_task_recv_destroy(updater_task);

	lntd_window_task_watch_destroy(notice_task);

	lntd_signal_task_wait_destroy(signal_wait_task);

	lntd_ko_close(updater);

	lntd_ko_close(notifier);

	lntd_ko_close(window_ko);

	return 0;
}

static lntd_error drawer_start(struct drawer *drawer)
{
	struct lntd_async_pool *pool = drawer->pool;
	lntd_ko updater = drawer->updater;
	lntd_ko notifier = drawer->notifier;
	xcb_connection_t *connection = drawer->connection;

	struct lntd_signal_task_wait *signal_wait_task =
	    drawer->signal_wait_task;
	struct lntd_io_task_poll *poll_conn_task =
	    drawer->poll_conn_task;
	struct lntd_updater_task_recv *updater_task =
	    drawer->updater_task;
	struct lntd_window_task_watch *notice_task =
	    drawer->notice_task;

	lntd_signal_listen_to_sighup();
	lntd_signal_listen_to_sigint();
	lntd_signal_listen_to_sigquit();
	lntd_signal_listen_to_sigterm();

	lntd_async_pool_submit(
	    pool, lntd_signal_task_wait_prepare(
	              signal_wait_task,
	              (union lntd_async_ck){.u64 = ON_SIGNAL},
	              signal_wait_task));

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

	lntd_async_pool_submit(
	    pool, lntd_updater_task_recv_prepare(
	              updater_task,
	              (union lntd_async_ck){.u64 = ON_RECEIVE_UPDATE},
	              updater_task, updater));

	return drawer_update_window(drawer);
}

static lntd_error drawer_stop(struct drawer *drawer)
{
	struct lntd_signal_task_wait *signal_wait_task =
	    drawer->signal_wait_task;
	struct lntd_updater_task_recv *updater_task =
	    drawer->updater_task;
	struct lntd_window_task_watch *notice_task =
	    drawer->notice_task;
	struct lntd_io_task_poll *poll_conn_task =
	    drawer->poll_conn_task;

	lntd_async_task_cancel(
	    lntd_signal_task_wait_to_async(signal_wait_task));
	lntd_async_task_cancel(
	    lntd_updater_task_recv_to_async(updater_task));
	lntd_async_task_cancel(
	    lntd_window_task_watch_to_async(notice_task));
	lntd_async_task_cancel(
	    lntd_io_task_poll_to_async(poll_conn_task));

	return 0;
}

static lntd_error dispatch(struct drawer *drawer,
                           union lntd_async_ck task_ck, void *userstate,
                           lntd_error err)
{
	switch (task_ck.u64) {
	case ON_SIGNAL:
		return drawer_on_signal(drawer, userstate, err);

	case ON_RECEIVE_UPDATE:
		return drawer_on_update_recved(drawer, userstate, err);

	case ON_POLL_CONN:
		return drawer_on_conn_ready(drawer, userstate, err);

	case ON_RECEIVE_NOTICE:
		return drawer_on_notice_recved(drawer, userstate, err);

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

static lntd_error
drawer_on_signal(struct drawer *drawer,
                 struct lntd_signal_task_wait *signal_wait_task,
                 lntd_error err)
{
	if (err != 0)
		return err;

	drawer->time_to_quit = true;

	return 0;
}

static lntd_error
drawer_on_conn_ready(struct drawer *drawer,
                     struct lntd_io_task_poll *poll_conn_task,
                     lntd_error err)
{
	xcb_connection_t *connection = drawer->connection;
	struct lntd_async_pool *pool = drawer->pool;
	struct lntd_gpu_context *gpu_context = drawer->gpu_context;

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

			lntd_gpu_resize(gpu_context, width, height);
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
		lntd_mem_free(event);
	}

	lntd_async_pool_submit(
	    pool, lntd_io_task_poll_prepare(
	              poll_conn_task,
	              (union lntd_async_ck){.u64 = ON_POLL_CONN},
	              poll_conn_task,
	              xcb_get_file_descriptor(connection), POLLIN));

	if (window_destroyed) {
		lntd_gpu_remove_window(gpu_context);
	}

	return 0;
}

static lntd_error
drawer_on_update_recved(struct drawer *drawer,
                        struct lntd_updater_task_recv *updater_task,
                        lntd_error err)
{
	struct lntd_gpu_context *gpu_context = drawer->gpu_context;
	struct lntd_async_pool *pool = drawer->pool;
	lntd_ko updater = drawer->updater;

	if (err != 0)
		return err;

	lntd_updater_int x_position;
	lntd_updater_int y_position;
	lntd_updater_int z_position;

	lntd_updater_angle z_rotation;
	lntd_updater_angle x_rotation;
	{
		struct lntd_updater_update update;

		lntd_updater_decode(updater_task, &update);

		z_rotation = update.z_rotation;
		x_rotation = update.x_rotation;

		x_position = update.x_position;
		y_position = update.y_position;
		z_position = update.z_position;
	}

	lntd_async_pool_submit(
	    pool, lntd_updater_task_recv_prepare(
	              updater_task,
	              (union lntd_async_ck){.u64 = ON_RECEIVE_UPDATE},
	              updater_task, updater));

	float gpu_z_rotation = lntd_updater_angle_to_double(z_rotation);
	float gpu_x_rotation = lntd_updater_angle_to_double(x_rotation);

	float gpu_x_position = x_position * (1 / 4096.0);
	float gpu_y_position = y_position * (1 / 4096.0);
	float gpu_z_position = z_position * (1 / 4096.0);

	{
		struct lntd_gpu_update gpu_update;

		gpu_update.z_rotation = gpu_z_rotation;
		gpu_update.x_rotation = gpu_x_rotation;

		gpu_update.x_position = gpu_x_position;
		gpu_update.y_position = gpu_y_position;
		gpu_update.z_position = gpu_z_position;

		lntd_gpu_update_state(gpu_context, &gpu_update);
	}

	return 0;
}

static lntd_error
drawer_on_notice_recved(struct drawer *drawer,
                        struct lntd_window_task_watch *notice_task,
                        lntd_error err)
{
	struct lntd_async_pool *pool = drawer->pool;
	lntd_ko notifier = drawer->notifier;

	if (err != 0)
		return err;

	lntd_async_pool_submit(
	    pool, lntd_window_task_watch_prepare(
	              notice_task,
	              (union lntd_async_ck){.u64 = ON_RECEIVE_NOTICE},
	              notice_task, notifier));

	return drawer_update_window(drawer);
}

static lntd_error drawer_remove_window(struct drawer *drawer)
{
	lntd_error err = 0;

	xcb_connection_t *connection = drawer->connection;
	xcb_window_t window = drawer->window;

	static uint32_t const no_opts[] = {0, 0};

	xcb_change_window_attributes(connection, window,
	                             XCB_CW_EVENT_MASK, no_opts);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_flush(connection);

	return 0;
}

static lntd_error drawer_update_window(struct drawer *drawer)
{
	lntd_error err = 0;

	err = drawer_remove_window(drawer);
	if (err != 0)
		return err;

	xcb_connection_t *connection = drawer->connection;
	lntd_window window_ko = drawer->window_ko;
	struct lntd_gpu_context *gpu_context = drawer->gpu_context;

	uint_fast32_t new_window;
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
	    connection, new_window, XCB_CW_EVENT_MASK, window_opts);
	err = lntd_xcb_conn_error(connection);
	if (err != 0)
		return err;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, new_window);
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

	xcb_flush(connection);

	lntd_gpu_remove_window(gpu_context);
	lntd_gpu_set_x11_window(gpu_context, new_window);
	lntd_gpu_resize(gpu_context, width, height);

	drawer->window = new_window;

	return 0;
}
