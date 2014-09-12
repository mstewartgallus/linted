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

#define XK_LATIN1
#define XK_MISCELLANY

#include "config.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/controller.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"
#include "linted/window-notifier.h"

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <xcb/xcb.h>
#include <X11/keysymdef.h>
#include <X11/Xlib.h>
#include <X11/Xlib-xcb.h>

#include <linux/filter.h>
#include <linux/seccomp.h>

enum { ON_RECEIVE_NOTICE, ON_POLL_CONN, ON_SENT_CONTROL, MAX_TASKS };

#define INPUT_EVENT_MASK                                                       \
	(XCB_EVENT_MASK_ENTER_WINDOW | XCB_EVENT_MASK_LEAVE_WINDOW |           \
	 XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_KEY_RELEASE |               \
	 XCB_EVENT_MASK_POINTER_MOTION | XCB_EVENT_MASK_STRUCTURE_NOTIFY)

struct controller_data;
struct controller_task;

struct window_model
{
	unsigned width;
	unsigned height;
};

struct poll_conn_task
{
	struct linted_ko_task_poll parent;
	bool *time_to_quit;
	Display *display;
	xcb_connection_t *connection;
	struct window_model *window_model;
	struct linted_asynch_pool *pool;
	struct controller_data *controller_data;
	struct controller_task *controller_task;
	xcb_window_t *window;
	linted_ko controller;
};

struct controller_data
{
	struct linted_controller_message update;
	bool update_pending : 1U;
	bool update_in_progress : 1U;
};

struct controller_task
{
	struct linted_controller_task_send parent;
	struct controller_data *controller_data;
	struct linted_asynch_pool *pool;
	linted_ko controller;
};

#define CONTROLLER_UPCAST(X) LINTED_CONTROLLER_SEND_UPCAST(LINTED_UPCAST(X))
#define CONTROLLER_DOWNCAST(X)                                                 \
	LINTED_DOWNCAST(struct controller_task,                                \
	                LINTED_CONTROLLER_SEND_DOWNCAST(X))

struct notice_task
{
	struct linted_window_notifier_task_receive parent;
	struct poll_conn_task *poll_conn_task;
	bool *time_to_quit;
	Display *display;
	struct linted_asynch_pool *pool;
	xcb_connection_t *connection;
	struct window_model *window_model;
	linted_ko controller;
	struct controller_data *controller_data;
	struct controller_task *controller_task;
	xcb_window_t *window;
};
#define NOTICE_UPCAST(X) LINTED_WINDOW_NOTIFIER_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define NOTICE_DOWNCAST(X)                                                     \
	LINTED_DOWNCAST(struct notice_task,                                    \
	                LINTED_WINDOW_NOTIFIER_RECEIVE_DOWNCAST(X))

static linted_ko kos[3U];
static struct sock_fprog const seccomp_filter;

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-gui",
    .kos_size = LINTED_ARRAY_SIZE(kos),
    .kos = kos,
    .seccomp_bpf = &seccomp_filter};

static struct timespec const sleep_time = {.tv_sec = 0, .tv_nsec = 10000000};

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_receive_notice(struct linted_asynch_task *task);
static linted_error on_sent_control(struct linted_asynch_task *task);

static void maybe_update_controller(struct linted_asynch_pool *pool,
                                    struct controller_data *controller_data,
                                    struct controller_task *controller_task,
                                    linted_controller controller);

static linted_error get_xcb_conn_error(xcb_connection_t *connection);
static linted_error get_xcb_error(xcb_generic_error_t *error);

static linted_error get_window_size(xcb_connection_t *connection,
                                    xcb_window_t window, unsigned *width,
                                    unsigned *height);
static linted_error get_mouse_position(xcb_connection_t *connection,
                                       xcb_window_t window, int *x, int *y);

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_model const *window_model,
                    struct controller_data *controller_data);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	linted_controller controller = kos[1U];
	linted_window_notifier notifier = kos[2U];

	struct controller_data controller_data = {0};
	struct window_model window_model = {.width = 1, .height = 1};

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	bool time_to_quit;
	xcb_window_t window = 0;
	struct notice_task notice_task;
	struct controller_task controller_task;
	struct poll_conn_task poll_conn_task;

	Display *display = XOpenDisplay(NULL);
	if (NULL == display) {
		errnum = ENOSYS;
		goto destroy_pool;
	}

	xcb_connection_t *connection = XGetXCBConnection(display);

	linted_window_notifier_receive(LINTED_UPCAST(&notice_task),
	                               ON_RECEIVE_NOTICE, notifier);
	notice_task.time_to_quit = &time_to_quit;
	notice_task.poll_conn_task = &poll_conn_task;
	notice_task.window = &window;
	notice_task.display = display;
	notice_task.pool = pool;
	notice_task.window_model = &window_model;
	notice_task.connection = connection;
	notice_task.controller = controller;
	notice_task.controller_data = &controller_data;
	notice_task.controller_task = &controller_task;

	linted_asynch_pool_submit(pool, NOTICE_UPCAST(&notice_task));

	/* TODO: Detect SIGTERM and exit normally */
	do {
		time_to_quit = false;

		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;

			errnum = linted_asynch_pool_wait(pool, &xx);
			if (errnum != 0)
				goto close_display;

			completed_task = xx;
		}

		errnum = dispatch(completed_task);
		if (errnum != 0)
			goto close_display;

	} while (!time_to_quit);

close_display:
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
	(void)notice_task;
	(void)controller_task;
	(void)poll_conn_task;

	return errnum;
}

#define ALLOW(XX)                                                              \
	BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_##XX, 0U, 1U),                \
	    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW)

static struct sock_filter const real_filter[] = {
    /*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
                    offsetof(struct seccomp_data, nr)),
    /*  */ ALLOW(access),
    /*  */ ALLOW(arch_prctl),
    /*  */ ALLOW(brk),
    /*  */ ALLOW(chdir),
    /*  */ ALLOW(clock_nanosleep),
    /*  */ ALLOW(clone),
    /*  */ ALLOW(close),
    /*  */ ALLOW(connect),
    /*  */ ALLOW(dup2),
    /*  */ ALLOW(execve),
    /*  */ ALLOW(exit),
    /*  */ ALLOW(exit_group),
    /*  */ ALLOW(fcntl),
    /*  */ ALLOW(fstat),
    /*  */ ALLOW(futex),
    /*  */ ALLOW(getdents),
    /*  */ ALLOW(getegid),
    /*  */ ALLOW(geteuid),
    /*  */ ALLOW(getgid),
    /*  */ ALLOW(getpeername),
    /*  */ ALLOW(getpid),
    /*  */ ALLOW(getrlimit),
    /*  */ ALLOW(gettid),
    /*  */ ALLOW(getuid),
    /*  */ ALLOW(ioctl),
    /*  */ ALLOW(lseek),
    /*  */ ALLOW(madvise),
    /*  */ ALLOW(mincore),
    /*  */ ALLOW(mmap),
    /*  */ ALLOW(mprotect),
    /*  */ ALLOW(mq_timedreceive),
    /*  */ ALLOW(mq_timedsend),
    /*  */ ALLOW(munmap),
    /*  */ ALLOW(open),
    /*  */ ALLOW(openat),
    /*  */ ALLOW(poll),
    /*  */ ALLOW(prctl),
    /*  */ ALLOW(read),
    /*  */ ALLOW(recvfrom),
    /*  */ ALLOW(restart_syscall),
    /*  */ ALLOW(rt_sigaction),
    /*  */ ALLOW(rt_sigprocmask),
    /*  */ ALLOW(sched_getaffinity),
    /*  */ ALLOW(setrlimit),
    /*  */ ALLOW(set_robust_list),
    /*  */ ALLOW(set_tid_address),
    /*  */ ALLOW(shutdown),
    /*  */ ALLOW(socket),
    /*  */ ALLOW(stat),
    /*  */ ALLOW(tgkill),
    /*  */ ALLOW(uname),
    /*  */ ALLOW(write),
    /*  */ ALLOW(writev),
    /*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)};

static struct sock_fprog const seccomp_filter = {
    .len = LINTED_ARRAY_SIZE(real_filter),
    .filter = (struct sock_filter *)real_filter};

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (task->task_action) {
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
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct poll_conn_task *poll_conn_task =
	    LINTED_DOWNCAST(struct poll_conn_task,
	                    LINTED_DOWNCAST(struct linted_ko_task_poll, task));
	Display *display = poll_conn_task->display;
	xcb_connection_t *connection = poll_conn_task->connection;
	xcb_window_t *window = poll_conn_task->window;
	struct window_model *window_model = poll_conn_task->window_model;
	bool *time_to_quitp = poll_conn_task->time_to_quit;

	struct linted_asynch_pool *pool = poll_conn_task->pool;
	linted_ko controller = poll_conn_task->controller;
	struct controller_data *controller_data =
	    poll_conn_task->controller_data;
	struct controller_task *controller_task =
	    poll_conn_task->controller_task;

	/* We have to use the Xlib event queue for input events as XCB
	 * isn't quite ready in this respect yet.
	 */
	while (XPending(display) > 0) {
		XEvent event;
		XNextEvent(display, &event);

		bool time_to_quit = false;
		bool is_key_down;
		switch (event.type) {
		case ConfigureNotify: {
			XConfigureEvent const *configure_event =
			    &event.xconfigure;
			window_model->width = configure_event->width;
			window_model->height = configure_event->height;
			break;
		}

		case MotionNotify: {
			XMotionEvent const *motion_event = &event.xmotion;
			on_tilt(motion_event->x, motion_event->y, window_model,
			        controller_data);
			break;
		}

		case EnterNotify: {
			int x, y;
			errnum =
			    get_mouse_position(connection, *window, &x, &y);
			if (errnum != 0)
				return errnum;

			on_tilt(x, y, window_model, controller_data);
			break;
		}

		case LeaveNotify:
			controller_data->update.x_tilt = 0;
			controller_data->update.y_tilt = 0;

			controller_data->update.left = 0;
			controller_data->update.right = 0;
			controller_data->update.forward = 0;
			controller_data->update.back = 0;

			controller_data->update.jumping = 0;

			controller_data->update_pending = true;
			break;

		case KeyPress:
			is_key_down = true;
			goto on_key_event;

		case KeyRelease:
			is_key_down = false;
			goto on_key_event;

		case MappingNotify: {
			XMappingEvent *mapping_event = &event.xmapping;
			XRefreshKeyboardMapping(mapping_event);
			break;
		}

		default:
			/* Unknown event type, ignore it */
			break;

		on_key_event : {
			XKeyEvent *key_event = &event.xkey;
			switch (XLookupKeysym(key_event, 0)) {
			case XK_q:
				goto quit_application;

			case XK_space:
				goto jump;

			case XK_Control_L:
				goto move_left;

			case XK_Alt_L:
				goto move_right;

			case XK_z:
				goto move_forward;

			case XK_Shift_L:
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

		quit_application:
			time_to_quit = true;
			break;
		}

		maybe_update_controller(pool, controller_data, controller_task,
		                        controller);

		*time_to_quitp = time_to_quit;
		if (time_to_quit)
			return 0;
	}

	linted_ko_task_poll(LINTED_UPCAST(poll_conn_task), ON_POLL_CONN,
	                    xcb_get_file_descriptor(connection), POLLIN);
	poll_conn_task->time_to_quit = time_to_quitp;
	poll_conn_task->window = window;
	poll_conn_task->pool = pool;
	poll_conn_task->window_model = window_model;
	poll_conn_task->display = display;
	poll_conn_task->connection = connection;
	poll_conn_task->controller = controller;
	poll_conn_task->controller_data = controller_data;
	poll_conn_task->controller_task = controller_task;

	linted_asynch_pool_submit(pool, task);

	return 0;
}

static linted_error on_receive_notice(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct notice_task *notice_task = NOTICE_DOWNCAST(task);
	struct poll_conn_task *poll_conn_task = notice_task->poll_conn_task;
	Display *display = notice_task->display;
	struct linted_asynch_pool *pool = notice_task->pool;
	xcb_connection_t *connection = notice_task->connection;
	struct window_model *window_model = notice_task->window_model;
	linted_ko controller = notice_task->controller;
	struct controller_data *controller_data = notice_task->controller_data;
	struct controller_task *controller_task = notice_task->controller_task;
	xcb_window_t *windowp = notice_task->window;

	uint_fast32_t window =
	    linted_window_notifier_decode(LINTED_UPCAST(notice_task));

	linted_asynch_pool_submit(pool, task);

	if (-1 == XSelectInput(display, window, INPUT_EVENT_MASK))
		return ENOSYS;

	unsigned width, height;
	int x, y;
	{
		unsigned xx, yy;
		errnum = get_window_size(connection, window, &xx, &yy);
		if (errnum != 0)
			return errnum;
		width = xx;
		height = yy;
	}
	{
		int xx, yy;
		errnum = get_mouse_position(connection, window, &xx, &yy);
		if (errnum != 0)
			return errnum;
		x = xx;
		y = yy;
	}

	window_model->width = width;
	window_model->height = height;

	on_tilt(x, y, window_model, controller_data);

	maybe_update_controller(pool, controller_data, controller_task,
	                        controller);

	*windowp = window;

	/* TODO: Check if this is in progress */
	linted_ko_task_poll(LINTED_UPCAST(poll_conn_task), ON_POLL_CONN,
	                    xcb_get_file_descriptor(connection), POLLIN);
	poll_conn_task->time_to_quit = notice_task->time_to_quit;
	poll_conn_task->window = windowp;
	poll_conn_task->pool = pool;
	poll_conn_task->window_model = window_model;
	poll_conn_task->display = display;
	poll_conn_task->connection = connection;
	poll_conn_task->controller = controller;
	poll_conn_task->controller_data = controller_data;
	poll_conn_task->controller_task = controller_task;

	linted_asynch_pool_submit(pool,
	                          LINTED_UPCAST(LINTED_UPCAST(poll_conn_task)));

	return 0;
}

static linted_error on_sent_control(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct controller_task *controller_task = CONTROLLER_DOWNCAST(task);

	struct controller_data *controller_data =
	    controller_task->controller_data;
	struct linted_asynch_pool *pool = controller_task->pool;
	linted_ko controller = controller_task->controller;

	controller_data->update_in_progress = false;

	maybe_update_controller(pool, controller_data, controller_task,
	                        controller);

	return 0;
}

static void maybe_update_controller(struct linted_asynch_pool *pool,
                                    struct controller_data *controller_data,
                                    struct controller_task *controller_task,
                                    linted_controller controller)
{
	if (!controller_data->update_pending)
		return;

	if (controller_data->update_in_progress)
		return;

	linted_controller_send(LINTED_UPCAST(controller_task), ON_SENT_CONTROL,
	                       controller, &controller_data->update);
	controller_task->controller_data = controller_data;
	controller_task->pool = pool;
	controller_task->controller = controller;

	linted_asynch_pool_submit(pool, CONTROLLER_UPCAST(controller_task));

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
	x *= INT32_MAX / width;
	y *= INT32_MAX / height;

	controller_data->update.x_tilt = x;
	controller_data->update.y_tilt = y;

	controller_data->update_pending = true;
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

static linted_error get_mouse_position(xcb_connection_t *connection,
                                       xcb_window_t window, int *x, int *y)
{
	linted_error errnum;

	xcb_query_pointer_cookie_t cookie =
	    xcb_query_pointer(connection, window);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		return errnum;

	xcb_generic_error_t *error;
	xcb_query_pointer_reply_t *reply;
	{
		xcb_generic_error_t *xx;
		reply = xcb_query_pointer_reply(connection, cookie, &xx);

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

	*x = reply->win_x;
	*y = reply->win_y;

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
