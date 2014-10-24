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
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"
#include "linted/window-notifier.h"

#include <errno.h>
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include <xcb/xcb.h>
#include <X11/Xlib.h>
#include <X11/Xlib-xcb.h>

enum {
	ON_RECEIVE_UPDATE,
	ON_POLL_CONN,
	ON_RECEIVE_NOTICE,
	MAX_TASKS
};

struct window_model
{
	bool viewable : 1U;
};

struct poll_conn_task
{
	struct linted_ko_task_poll parent;

	struct linted_gpu_context *gpu_context;
	Display *display;
	xcb_connection_t *connection;
	struct linted_asynch_pool *pool;

	struct window_model *window_model;
	bool *time_to_quit;
};

struct updater_task
{
	struct linted_updater_task_receive parent;
	struct linted_gpu_context *gpu_context;
	struct linted_asynch_pool *pool;
};
#define UPDATER_UPCAST(X) LINTED_UPDATER_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define UPDATER_DOWNCAST(X)                                                    \
	LINTED_DOWNCAST(struct updater_task, LINTED_UPDATER_RECEIVE_DOWNCAST(X))

struct notice_task
{
	struct linted_window_notifier_task_receive parent;

	struct poll_conn_task *poll_conn_task;
	struct updater_task *updater_task;

	struct linted_asynch_pool *pool;
	xcb_connection_t *connection;
	Display *display;
	linted_ko updater;
	linted_log log;

	struct window_model *window_model;
	bool *time_to_quit;
	struct linted_gpu_context **gpu_context;
	xcb_window_t *window;
};
#define NOTICE_UPCAST(X) LINTED_WINDOW_NOTIFIER_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define NOTICE_DOWNCAST(X)                                                     \
	LINTED_DOWNCAST(struct notice_task,                                    \
	                LINTED_WINDOW_NOTIFIER_RECEIVE_DOWNCAST(X))

static linted_ko kos[3U];

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-drawer",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos,
	.seccomp_bpf = NULL
};

static uint32_t const window_opts[] = { XCB_EVENT_MASK_STRUCTURE_NOTIFY, 0 };

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_receive_update(struct linted_asynch_task *task);
static linted_error on_receive_notice(struct linted_asynch_task *task);

static linted_error get_xcb_conn_error(xcb_connection_t *connection);
static linted_error get_xcb_error(xcb_generic_error_t *error);

static linted_error get_window_size(xcb_connection_t *connection,
                                    xcb_window_t window, unsigned *width,
                                    unsigned *height);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	linted_log log = kos[0U];
	linted_window_notifier notifier = kos[1U];
	linted_updater updater = kos[2U];

	struct window_model window_model = { .viewable = false };

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	bool time_to_quit;
	xcb_window_t window;
	struct linted_gpu_context *gpu_context = NULL;
	struct notice_task notice_task;
	struct updater_task updater_task;
	struct poll_conn_task poll_conn_task;

	Display *display = XOpenDisplay(NULL);
	if (NULL == display) {
		errnum = ENOSYS;
		goto destroy_pool;
	}

	xcb_connection_t *connection = XGetXCBConnection(display);

	linted_window_notifier_receive(LINTED_UPCAST(&notice_task),
	                               ON_RECEIVE_NOTICE, notifier);
	notice_task.poll_conn_task = &poll_conn_task;
	notice_task.updater_task = &updater_task;

	notice_task.pool = pool;
	notice_task.connection = connection;
	notice_task.display = display;
	notice_task.updater = updater;
	notice_task.log = log;

	notice_task.window_model = &window_model;
	notice_task.time_to_quit = &time_to_quit;
	notice_task.gpu_context = &gpu_context;
	notice_task.window = &window;

	linted_asynch_pool_submit(pool, NOTICE_UPCAST(&notice_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		for (;;) {
			time_to_quit = false;

			struct linted_asynch_task *completed_task;
			{
				struct linted_asynch_task *xx;
				errnum = linted_asynch_pool_poll(pool, &xx);
				if (EAGAIN == errnum)
					break;

				if (errnum != 0)
					goto cleanup_gpu;

				completed_task = xx;
			}

			errnum = dispatch(completed_task);
			if (errnum != 0)
				goto cleanup_gpu;

			if (time_to_quit)
				goto cleanup_gpu;
		}

		/* Only draw or resize if we have time to waste */

		if (window_model.viewable) {
			linted_gpu_draw(gpu_context, log);
		} else {
			time_to_quit = false;

			struct linted_asynch_task *completed_task;
			{
				struct linted_asynch_task *xx;
				errnum = linted_asynch_pool_wait(pool, &xx);
				if (errnum != 0)
					goto cleanup_gpu;
				completed_task = xx;
			}
			errnum = dispatch(completed_task);
			if (errnum != 0)
				goto cleanup_gpu;

			if (time_to_quit)
				goto cleanup_gpu;
		}
	}

cleanup_gpu:
	if (gpu_context != NULL)
		linted_gpu_context_destroy(gpu_context);

	XCloseDisplay(display);

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

		linted_error dispatch_errnum = completed_task->errnum;
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
	switch (task->task_action) {
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

	errnum = task->errnum;
	if (errnum != 0)
		return errnum;

	struct poll_conn_task *poll_conn_task =
	    LINTED_DOWNCAST(struct poll_conn_task,
	                    LINTED_DOWNCAST(struct linted_ko_task_poll, task));

	Display *display = poll_conn_task->display;
	xcb_connection_t *connection = poll_conn_task->connection;
	struct linted_asynch_pool *pool = poll_conn_task->pool;
	struct window_model *window_model = poll_conn_task->window_model;
	struct linted_gpu_context *gpu_context = poll_conn_task->gpu_context;
	bool *time_to_quitp = poll_conn_task->time_to_quit;

	/* We have to use the Xlib event queue for the drawer events
	 * because of broken Mesa libraries which abuse it.
	 */
	while (XPending(display) > 0) {
		XEvent event;
		XNextEvent(display, &event);

		bool time_to_quit = false;
		switch (event.type) {
		case ConfigureNotify: {
			XConfigureEvent const *configure_event =
			    &event.xconfigure;

			linted_gpu_resize(gpu_context, configure_event->width,
			                  configure_event->height);
			break;
		}

		case UnmapNotify:
			window_model->viewable = false;
			break;

		case MapNotify:
			window_model->viewable = true;
			break;

		case DestroyNotify:
			goto quit_application;

		default:
			/* Unknown event type, ignore it */
			break;

		quit_application:
			time_to_quit = true;
			break;
		}

		*time_to_quitp = time_to_quit;
		if (time_to_quit)
			return 0;
	}

	linted_ko_task_poll(LINTED_UPCAST(poll_conn_task), ON_POLL_CONN,
	                    xcb_get_file_descriptor(connection), POLLIN);
	poll_conn_task->time_to_quit = time_to_quitp;
	poll_conn_task->window_model = window_model;
	poll_conn_task->pool = pool;
	poll_conn_task->display = display;
	poll_conn_task->connection = connection;

	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_receive_update(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct updater_task *updater_task = UPDATER_DOWNCAST(task);
	struct linted_gpu_context *gpu_context = updater_task->gpu_context;
	struct linted_asynch_pool *pool = updater_task->pool;

	struct linted_updater_update update;

	linted_updater_decode(LINTED_UPCAST(updater_task), &update);

	linted_asynch_pool_submit(pool, task);

	struct linted_gpu_update gpu_update;

	gpu_update.x_rotation =
	    linted_updater_angle_to_float(update.x_rotation);
	gpu_update.y_rotation =
	    linted_updater_angle_to_float(update.y_rotation);

	gpu_update.x_position = update.x_position * (1 / 2048.0);
	gpu_update.y_position = update.y_position * (1 / 2048.0);
	gpu_update.z_position = update.z_position * (1 / 2048.0);

	linted_gpu_update_state(gpu_context, &gpu_update);

	return 0;
}

static linted_error on_receive_notice(struct linted_asynch_task *task)
{
	linted_error errnum;

	errnum = task->errnum;
	if (errnum != 0)
		return errnum;

	struct notice_task *notice_task = NOTICE_DOWNCAST(task);
	struct poll_conn_task *poll_conn_task = notice_task->poll_conn_task;
	Display *display = notice_task->display;
	struct linted_asynch_pool *pool = notice_task->pool;

	xcb_connection_t *connection = notice_task->connection;
	struct window_model *window_model = notice_task->window_model;

	linted_ko log = notice_task->log;
	linted_ko updater = notice_task->updater;
	struct updater_task *updater_task = notice_task->updater_task;

	bool *time_to_quitp = notice_task->time_to_quit;
	xcb_window_t *windowp = notice_task->window;
	struct linted_gpu_context **gpu_contextp = notice_task->gpu_context;

	uint_fast32_t window =
	    linted_window_notifier_decode(LINTED_UPCAST(notice_task));

	xcb_void_cookie_t chattr_ck = xcb_change_window_attributes_checked(
	    connection, window, XCB_CW_EVENT_MASK, window_opts);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		return errnum;

	xcb_generic_error_t *chattr_err =
	    xcb_request_check(connection, chattr_ck);
	if (chattr_err != NULL) {
		errnum = get_xcb_error(chattr_err);
		linted_mem_free(chattr_err);
		return errnum;
	}

	unsigned width, height;
	{
		unsigned xx, yy;
		errnum = get_window_size(connection, window, &xx, &yy);
		if (errnum != 0)
			return errnum;
		width = xx;
		height = yy;
	}

	window_model->viewable = true;
	*windowp = window;

	struct linted_gpu_context *gpu_context;
	{
		struct linted_gpu_context *xx;
		errnum = linted_gpu_context_create(display, window, &xx, log);
		if (errnum != 0)
			return errnum;
		gpu_context = xx;
	}
	*gpu_contextp = gpu_context;

	linted_gpu_resize(gpu_context, width, height);

	linted_updater_receive(LINTED_UPCAST(updater_task), ON_RECEIVE_UPDATE,
	                       updater);
	updater_task->gpu_context = gpu_context;
	updater_task->pool = pool;
	linted_asynch_pool_submit(pool, UPDATER_UPCAST(updater_task));

	linted_ko_task_poll(LINTED_UPCAST(poll_conn_task), ON_POLL_CONN,
	                    xcb_get_file_descriptor(connection), POLLIN);
	poll_conn_task->time_to_quit = time_to_quitp;
	poll_conn_task->window_model = window_model;
	poll_conn_task->gpu_context = gpu_context;
	poll_conn_task->pool = pool;
	poll_conn_task->connection = connection;
	poll_conn_task->display = display;
	linted_asynch_pool_submit(pool,
	                          LINTED_UPCAST(LINTED_UPCAST(poll_conn_task)));

	return 0;
}

static linted_error get_window_size(xcb_connection_t *connection,
                                    xcb_window_t window, unsigned *width,
                                    unsigned *height)
{
	linted_error errnum;

	xcb_get_geometry_cookie_t ck = xcb_get_geometry(connection, window);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		return errnum;

	xcb_generic_error_t *error;
	xcb_get_geometry_reply_t *reply;
	{
		xcb_generic_error_t *xx;
		reply = xcb_get_geometry_reply(connection, ck, &xx);

		errnum = get_xcb_conn_error(connection);
		if (errnum != 0)
			return errnum;

		error = xx;
	}

	if (error != NULL) {
		errnum = get_xcb_error(error);
		linted_mem_free(error);
		return errnum;
	}

	*width = reply->width;
	*height = reply->height;

	linted_mem_free(reply);

	return 0;
}

static linted_error get_xcb_conn_error(xcb_connection_t *connection)
{
	switch (xcb_connection_has_error(connection)) {
	case 0:
		return 0;

	case XCB_CONN_ERROR:
		return EPROTO;

	case XCB_CONN_CLOSED_EXT_NOTSUPPORTED:
		return ENOSYS;

	case XCB_CONN_CLOSED_MEM_INSUFFICIENT:
		return ENOMEM;

	case XCB_CONN_CLOSED_REQ_LEN_EXCEED:
		return EINVAL;

	case XCB_CONN_CLOSED_PARSE_ERR:
		return EINVAL;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error get_xcb_error(xcb_generic_error_t *error)
{
	/* For now just be crappy. */
	return ENOSYS;
}
