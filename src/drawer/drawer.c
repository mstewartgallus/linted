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
#define _GNU_SOURCE

#include "config.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/gpu.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"
#include "linted/xcb.h"
#include "linted/window-notifier.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <xcb/xcb.h>

/**
 * @file
 *
 * @todo Handle sudden window death better.
 */

enum { ON_RECEIVE_UPDATE, ON_POLL_CONN, ON_RECEIVE_NOTICE, MAX_TASKS };

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

struct updater_data
{
	struct linted_gpu_context *gpu_context;
	struct linted_asynch_pool *pool;
};

struct notice_data
{
	xcb_connection_t *connection;

	struct window_model *window_model;
	struct linted_gpu_context *gpu_context;
	xcb_window_t *window;
};

static linted_ko kos[1U];

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-drawer",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos
};

static uint32_t const window_opts[] = { XCB_EVENT_MASK_STRUCTURE_NOTIFY, 0 };

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_receive_update(struct linted_asynch_task *task);
static linted_error on_receive_notice(struct linted_asynch_task *task);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	linted_window_notifier notifier = kos[0U];

	struct window_model window_model = { .viewable = false };

	linted_ko updater =
	    socket(AF_UNIX, SOCK_DGRAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
	if (-1 == updater) {
		perror("socket");
		return EXIT_FAILURE;
	}

	linted_log log = open("/run/log", O_WRONLY | O_APPEND | O_CLOEXEC);
	if (-1 == log) {
		perror("open");
		return EXIT_FAILURE;
	}

	{
		struct sockaddr_un addr = { 0 };
		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/run/updater/updater");

		for (;;) {
			if (-1 == bind(updater, (void *)&addr,
			               offsetof(struct sockaddr_un, sun_path) +
			                   strlen(addr.sun_path))) {
				if (errno == EADDRINUSE) {
					unlink(addr.sun_path);
					continue;
				}
				perror("bind");
				return EXIT_FAILURE;
			}
			break;
		}
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	xcb_window_t window;

	struct notice_data notice_data;
	struct updater_data updater_data;
	struct poll_conn_data poll_conn_data;

	struct linted_window_notifier_task_receive *notice_task;
	struct linted_updater_task_receive *updater_task;
	struct linted_ko_task_poll *poll_conn_task;

	{
		struct linted_window_notifier_task_receive *xx;
		errnum = linted_window_notifier_task_receive_create(
		    &xx, &notice_data);
		if (errnum != 0)
			goto destroy_pool;
		notice_task = xx;
	}

	{
		struct linted_updater_task_receive *xx;
		errnum = linted_updater_task_receive_create(&xx, &updater_data);
		if (errnum != 0)
			goto destroy_pool;
		updater_task = xx;
	}

	{
		struct linted_ko_task_poll *xx;
		errnum = linted_ko_task_poll_create(&xx, &poll_conn_data);
		if (errnum != 0)
			goto destroy_pool;
		poll_conn_task = xx;
	}

	xcb_connection_t *connection = xcb_connect(NULL, NULL);
	if (NULL == connection) {
		errnum = ENOSYS;
		goto destroy_pool;
	}

	struct linted_gpu_context *gpu_context;
	{
		struct linted_gpu_context *xx;
		errnum = linted_gpu_context_create(&xx);
		if (errnum != 0)
			goto close_display;
		gpu_context = xx;
	}

	notice_data.connection = connection;

	notice_data.window_model = &window_model;
	notice_data.gpu_context = gpu_context;
	notice_data.window = &window;

	linted_window_notifier_task_receive_prepare(
	    notice_task, ON_RECEIVE_NOTICE, notifier);
	linted_asynch_pool_submit(
	    pool, linted_window_notifier_task_receive_to_asynch(notice_task));

	linted_ko_task_poll_prepare(poll_conn_task, ON_POLL_CONN,
	                            xcb_get_file_descriptor(connection),
	                            POLLIN);
	poll_conn_data.window_model = &window_model;
	poll_conn_data.gpu_context = gpu_context;
	poll_conn_data.pool = pool;
	poll_conn_data.connection = connection;
	poll_conn_data.notice_task = notice_task;

	linted_asynch_pool_submit(
	    pool, linted_ko_task_poll_to_asynch(poll_conn_task));

	linted_updater_task_receive_prepare(updater_task, ON_RECEIVE_UPDATE,
	                                    updater);
	updater_data.gpu_context = gpu_context;
	updater_data.pool = pool;
	linted_asynch_pool_submit(
	    pool, linted_updater_task_receive_to_asynch(updater_task));

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
		linted_gpu_draw(gpu_context, log);
	}

cleanup_gpu:
	linted_gpu_context_destroy(gpu_context);

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
	(void)updater_task;
	(void)notice_task;
	(void)poll_conn_task;

	return errnum;
}

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (linted_asynch_task_action(task)) {
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

	struct linted_window_notifier_task_receive *notice_task =
	    poll_conn_data->notice_task;
	xcb_connection_t *connection = poll_conn_data->connection;
	struct linted_asynch_pool *pool = poll_conn_data->pool;
	struct window_model *window_model = poll_conn_data->window_model;
	struct linted_gpu_context *gpu_context = poll_conn_data->gpu_context;

	bool window_destroyed = false;
	for (;;) {
		xcb_generic_event_t *event = xcb_poll_for_event(connection);
		if (NULL == event)
			break;

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
		linted_gpu_unsetwindow(gpu_context);
		window_model->viewable = false;

		linted_asynch_pool_submit(
		    pool,
		    linted_window_notifier_task_receive_to_asynch(notice_task));
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

static linted_error on_receive_update(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	struct linted_updater_task_receive *updater_task =
	    linted_updater_task_receive_from_asynch(task);
	struct updater_data *updater_data =
	    linted_updater_task_receive_data(updater_task);
	struct linted_gpu_context *gpu_context = updater_data->gpu_context;
	struct linted_asynch_pool *pool = updater_data->pool;

	struct linted_updater_update update;

	linted_updater_decode(updater_task, &update);

	linted_asynch_pool_submit(pool, task);

	struct linted_gpu_update gpu_update;

	gpu_update.x_rotation = linted_sim_angle_to_double(update.x_rotation);
	gpu_update.y_rotation = linted_sim_angle_to_double(update.y_rotation);

	gpu_update.x_position = update.x_position * (1 / 2048.0);
	gpu_update.y_position = update.y_position * (1 / 2048.0);
	gpu_update.z_position = update.z_position * (1 / 2048.0);

	linted_gpu_update_state(gpu_context, &gpu_update);

	return 0;
}

static linted_error on_receive_notice(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = linted_asynch_task_errnum(task);
	if (errnum != 0)
		return errnum;

	struct linted_window_notifier_task_receive *notice_task =
	    linted_window_notifier_task_receive_from_asynch(task);
	struct notice_data *notice_data =
	    linted_window_notifier_task_receive_data(notice_task);

	xcb_connection_t *connection = notice_data->connection;
	struct window_model *window_model = notice_data->window_model;

	xcb_window_t *windowp = notice_data->window;
	struct linted_gpu_context *gpu_context = notice_data->gpu_context;

	xcb_window_t window = linted_window_notifier_decode(notice_task);

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

	window_model->viewable = true;
	*windowp = window;

	linted_gpu_setwindow(gpu_context, window);
	linted_gpu_resize(gpu_context, width, height);

	return 0;
}
