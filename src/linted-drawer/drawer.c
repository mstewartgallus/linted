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

struct window_model {
	bool viewable : 1U;
};

struct idle_data {
	struct linted_gpu_context *gpu_context;
	struct linted_async_pool *pool;
	struct window_model *window_model;
	bool idle_in_progress : 1U;
};

struct poll_conn_data {
	struct linted_gpu_context *gpu_context;
	xcb_connection_t *connection;
	struct linted_async_pool *pool;

	struct window_model *window_model;
	struct linted_sched_task_idle *idle_task;
};

struct updater_data {
	struct linted_gpu_context *gpu_context;
	struct linted_async_pool *pool;
};

struct notice_data {
	xcb_connection_t *connection;

	struct window_model *window_model;
	struct linted_gpu_context *gpu_context;
	xcb_window_t *window;
	struct linted_sched_task_idle *idle_task;
	struct linted_async_pool *pool;
	linted_window window_ko;
};

static unsigned char drawer_start(char const *process_name, size_t argc,
                                  char const *const argv[]);

static linted_error dispatch(struct linted_async_task *task);
static linted_error on_idle(struct linted_async_task *task);
static linted_error on_poll_conn(struct linted_async_task *task);
static linted_error on_receive_update(struct linted_async_task *task);
static linted_error on_receive_notice(struct linted_async_task *task);

static void maybe_idle(struct linted_sched_task_idle *task);

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

	struct window_model window_model = {.viewable = false};

	struct linted_async_pool *pool;
	{
		struct linted_async_pool *xx;
		err = linted_async_pool_create(&xx, MAX_TASKS);
		if (err != 0)
			return err;
		pool = xx;
	}

	xcb_window_t window;
	struct idle_data idle_data;
	struct notice_data notice_data;
	struct updater_data updater_data;
	struct poll_conn_data poll_conn_data;

	struct linted_sched_task_idle *idle_task;
	struct linted_window_task_watch *notice_task;
	struct linted_updater_task_receive *updater_task;
	struct linted_io_task_poll *poll_conn_task;

	{
		struct linted_sched_task_idle *xx;
		err = linted_sched_task_idle_create(&xx, &idle_data);
		if (err != 0)
			goto destroy_pool;
		idle_task = xx;
	}

	{
		struct linted_window_task_watch *xx;
		err =
		    linted_window_task_watch_create(&xx, &notice_data);
		if (err != 0)
			goto destroy_pool;
		notice_task = xx;
	}

	{
		struct linted_updater_task_receive *xx;
		err = linted_updater_task_receive_create(&xx,
		                                         &updater_data);
		if (err != 0)
			goto destroy_pool;
		updater_task = xx;
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
		goto close_display;

	struct linted_gpu_context *gpu_context;
	{
		struct linted_gpu_context *xx;
		err = linted_gpu_context_create(&xx);
		if (err != 0)
			goto close_display;
		gpu_context = xx;
	}

	idle_data.pool = pool;
	idle_data.gpu_context = gpu_context;
	idle_data.window_model = &window_model;
	idle_data.idle_in_progress = false;

	notice_data.connection = connection;
	notice_data.window_model = &window_model;
	notice_data.gpu_context = gpu_context;
	notice_data.window = &window;
	notice_data.window_ko = window_ko;
	notice_data.idle_task = idle_task;
	notice_data.pool = pool;

	poll_conn_data.window_model = &window_model;
	poll_conn_data.gpu_context = gpu_context;
	poll_conn_data.pool = pool;
	poll_conn_data.connection = connection;
	poll_conn_data.idle_task = idle_task;

	updater_data.gpu_context = gpu_context;
	updater_data.pool = pool;

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

		window_model.viewable = true;

		linted_gpu_set_x11_window(gpu_context, window);
		linted_gpu_resize(gpu_context, width, height);

		maybe_idle(idle_task);
	}

	linted_window_task_watch_prepare(
	    notice_task,
	    (union linted_async_action){.u64 = ON_RECEIVE_NOTICE},
	    notifier);
	linted_async_pool_submit(
	    pool, linted_window_task_watch_to_async(notice_task));

	linted_io_task_poll_prepare(
	    poll_conn_task,
	    (union linted_async_action){.u64 = ON_POLL_CONN},
	    xcb_get_file_descriptor(connection), POLLIN);
	linted_async_pool_submit(
	    pool, linted_io_task_poll_to_async(poll_conn_task));

	linted_updater_task_receive_prepare(
	    updater_task,
	    (union linted_async_action){.u64 = ON_RECEIVE_UPDATE},
	    updater);
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

		err = dispatch(completed_task);
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

static linted_error dispatch(struct linted_async_task *task)
{
	switch (linted_async_task_action(task).u64) {
	case ON_IDLE:
		return on_idle(task);

	case ON_RECEIVE_UPDATE:
		return on_receive_update(task);

	case ON_POLL_CONN:
		return on_poll_conn(task);

	case ON_RECEIVE_NOTICE:
		return on_receive_notice(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_idle(struct linted_async_task *task)
{
	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_sched_task_idle *idle_task =
	    linted_sched_task_idle_from_async(task);
	struct idle_data *idle_data =
	    linted_sched_task_idle_data(idle_task);

	struct linted_gpu_context *gpu_context = idle_data->gpu_context;

	idle_data->idle_in_progress = false;

	/* Draw or resize if we have time to waste */
	err = linted_gpu_draw(gpu_context);
	if (err != 0)
		return err;

	maybe_idle(idle_task);

	return 0;
}

static linted_error on_poll_conn(struct linted_async_task *task)
{
	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_io_task_poll *poll_conn_task =
	    linted_io_task_poll_from_async(task);
	struct poll_conn_data *poll_conn_data =
	    linted_io_task_poll_data(poll_conn_task);

	xcb_connection_t *connection = poll_conn_data->connection;
	struct linted_async_pool *pool = poll_conn_data->pool;
	struct window_model *window_model =
	    poll_conn_data->window_model;
	struct linted_gpu_context *gpu_context =
	    poll_conn_data->gpu_context;
	struct linted_sched_task_idle *idle_task =
	    poll_conn_data->idle_task;

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
			window_model->viewable = false;
			break;

		case XCB_MAP_NOTIFY:
			window_model->viewable = true;
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
		window_model->viewable = false;
	}

	if (window_model->viewable)
		maybe_idle(idle_task);

	linted_io_task_poll_prepare(
	    poll_conn_task,
	    (union linted_async_action){.u64 = ON_POLL_CONN},
	    xcb_get_file_descriptor(connection), POLLIN);
	poll_conn_data->window_model = window_model;
	poll_conn_data->pool = pool;
	poll_conn_data->connection = connection;

	linted_async_pool_submit(pool, task);

	return 0;
}

static linted_error on_receive_update(struct linted_async_task *task)
{
	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_updater_task_receive *updater_task =
	    linted_updater_task_receive_from_async(task);
	struct updater_data *updater_data =
	    linted_updater_task_receive_data(updater_task);
	struct linted_gpu_context *gpu_context =
	    updater_data->gpu_context;
	struct linted_async_pool *pool = updater_data->pool;

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

static linted_error on_receive_notice(struct linted_async_task *task)
{
	linted_error err;

	err = linted_async_task_err(task);
	if (err != 0)
		return err;

	struct linted_window_task_watch *notice_task =
	    linted_window_task_watch_from_async(task);
	struct notice_data *notice_data =
	    linted_window_task_watch_data(notice_task);

	xcb_connection_t *connection = notice_data->connection;
	struct window_model *window_model = notice_data->window_model;

	struct linted_async_pool *pool = notice_data->pool;
	xcb_window_t *windowp = notice_data->window;
	linted_window window_ko = notice_data->window_ko;
	struct linted_gpu_context *gpu_context =
	    notice_data->gpu_context;

	struct linted_sched_task_idle *idle_task =
	    notice_data->idle_task;

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

	xcb_change_window_attributes_checked(
	    connection, window, XCB_CW_EVENT_MASK, window_opts);
	err = linted_xcb_conn_error(connection);
	if (err != 0)
		goto reset_notice;

	xcb_get_geometry_cookie_t geom_ck =
	    xcb_get_geometry(connection, window);
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

	window_model->viewable = true;
	*windowp = window;

	linted_gpu_set_x11_window(gpu_context, window);
	linted_gpu_resize(gpu_context, width, height);

	maybe_idle(idle_task);

reset_notice:
	linted_async_pool_submit(pool, task);

	return err;
}

static void maybe_idle(struct linted_sched_task_idle *task)
{
	struct idle_data *task_data = linted_sched_task_idle_data(task);

	if (task_data->idle_in_progress)
		return;

	if (!task_data->window_model->viewable)
		return;

	task_data->idle_in_progress = true;

	linted_sched_task_idle_prepare(
	    task, (union linted_async_action){.u64 = ON_IDLE});
	linted_async_pool_submit(task_data->pool,
	                         linted_sched_task_idle_to_async(task));
}
