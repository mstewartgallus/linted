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

#include "linted/assets.h"
#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"
#include "linted/window-notifier.h"

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <poll.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <EGL/egl.h>
#include <GLES2/gl2.h>
#include <xcb/xcb.h>
#include <X11/Xlib.h>
#include <X11/Xlib-xcb.h>

#include <linux/filter.h>
#include <linux/seccomp.h>

enum { ON_RECEIVE_UPDATE, ON_POLL_CONN, ON_SENT_NOTICE, MAX_TASKS };

struct window_model;

struct poll_conn_task
{
	struct linted_ko_task_poll parent;
	struct window_model *window_model;
	Display *display;
	xcb_connection_t *connection;
	struct linted_asynch_pool *pool;
	bool *time_to_quit;
};

struct window_model
{
	unsigned width;
	unsigned height;

	bool resize_pending : 1U;
	bool viewable : 1U;
};

struct graphics_state
{
	GLuint program;
	GLint model_view_projection_matrix;
};

struct sim_model
{
	float x_rotation;
	float y_rotation;

	float x_position;
	float y_position;
	float z_position;
};

struct updater_task
{
	struct linted_updater_task_receive parent;
	struct sim_model *sim_model;
	struct linted_asynch_pool *pool;
};
#define UPDATER_UPCAST(X) LINTED_UPDATER_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define UPDATER_DOWNCAST(X)                                                    \
	LINTED_DOWNCAST(struct updater_task, LINTED_UPDATER_RECEIVE_DOWNCAST(X))

static linted_ko kos[3U];
static struct sock_fprog const seccomp_filter;

static EGLint const attr_list[] = {EGL_RED_SIZE,     5,             /*  */
                                   EGL_GREEN_SIZE,   6,             /*  */
                                   EGL_BLUE_SIZE,    5,             /*  */
                                   EGL_ALPHA_SIZE,   EGL_DONT_CARE, /*  */
                                   EGL_DEPTH_SIZE,   16,            /*  */
                                   EGL_STENCIL_SIZE, EGL_DONT_CARE, /*  */
                                   EGL_NONE,         EGL_NONE};

static EGLint const context_attr[] = {EGL_CONTEXT_CLIENT_VERSION, 2, /*  */
                                      EGL_NONE, EGL_NONE};

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-drawer",
    .kos_size = LINTED_ARRAY_SIZE(kos),
    .kos = kos,
    .seccomp_bpf = &seccomp_filter};

static uint32_t const window_opts[] = {XCB_EVENT_MASK_STRUCTURE_NOTIFY, 0};

static linted_error dispatch(struct linted_asynch_task *task);
static linted_error on_poll_conn(struct linted_asynch_task *task);
static linted_error on_receive_update(struct linted_asynch_task *task);
static linted_error on_sent_notice(struct linted_asynch_task *task);

static linted_error egl_error(void);

static linted_error get_xcb_conn_error(xcb_connection_t *connection);
static linted_error get_xcb_error(xcb_generic_error_t *error);

static linted_error init_graphics(linted_log log,
                                  struct graphics_state *graphics_state);
static void destroy_graphics(struct graphics_state *graphics_state);
static void draw_graphics(struct graphics_state const *graphics_state,
                          struct sim_model const *sim_model,
                          struct window_model const *window_model);
static void resize_graphics(struct graphics_state *graphics_state,
                            unsigned width, unsigned height);

static void flush_gl_errors(void);
static void matrix_multiply(GLfloat const a[restrict 4U][4U],
                            GLfloat const b[restrict 4U][4U],
                            GLfloat result[restrict 4U][4U]);

/**
 * @todo get_gl_error's use of glGetError is incorrect. Multiple error
 *       flags may be set and returned by a single function.
 */
static linted_error get_gl_error(void);

static double square(double x);

static linted_error log_str(linted_log log, struct linted_str start,
                            char const *str);

unsigned char linted_start(char const *process_name, size_t argc,
                           char const *const argv[])
{
	linted_error errnum = 0;

	linted_log log = kos[0U];
	linted_window_notifier notifier = kos[1U];
	linted_updater updater = kos[2U];

	struct window_model window_model = {.width = 640,
	                                    .height = 480,
	                                    .viewable = true,

	                                    /* Do the initial resize */
	                                    .resize_pending = true};

	struct sim_model sim_model = {0};

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0)
			return errnum;
		pool = xx;
	}

	struct linted_window_notifier_task_send notice_task;
	struct updater_task updater_task;
	struct poll_conn_task poll_conn_task;

	Display *display = XOpenDisplay(NULL);
	if (NULL == display) {
		errnum = ENOSYS;
		goto destroy_pool;
	}

	xcb_connection_t *connection = XGetXCBConnection(display);
	unsigned screen_number = XDefaultScreen(display);

	xcb_screen_t *screen = NULL;
	{
		xcb_screen_iterator_t iter =
		    xcb_setup_roots_iterator(xcb_get_setup(connection));
		for (size_t ii = 0U; ii < screen_number; ++ii) {
			if (0 == iter.rem)
				break;

			xcb_screen_next(&iter);
		}

		if (0 == iter.rem) {
			errnum = EINVAL;
			goto close_display;
		}

		screen = iter.data;
	}

	xcb_window_t window = xcb_generate_id(connection);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto close_display;

	linted_window_notifier_send(&notice_task, ON_SENT_NOTICE, notifier,
	                            window);
	linted_asynch_pool_submit(
	    pool, LINTED_WINDOW_NOTIFIER_SEND_UPCAST(&notice_task));

	xcb_void_cookie_t create_win_ck = xcb_create_window_checked(
	    connection, XCB_COPY_FROM_PARENT, window, screen->root, 0, 0,
	    window_model.width, window_model.height, 0,
	    XCB_WINDOW_CLASS_INPUT_OUTPUT, screen->root_visual,
	    XCB_CW_EVENT_MASK, window_opts);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto close_display;

	xcb_intern_atom_cookie_t protocols_ck =
	    xcb_intern_atom(connection, 1, 12, "WM_PROTOCOLS");
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_intern_atom_cookie_t delete_ck =
	    xcb_intern_atom(connection, 0, 16, "WM_DELETE_WINDOW");
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_atom_t wm_delete_window_atom;
	xcb_atom_t wm_protocols_atom;
	{
		xcb_intern_atom_reply_t *proto_reply;
		xcb_generic_error_t *proto_err;
		{
			xcb_generic_error_t *xx = NULL;
			proto_reply = xcb_intern_atom_reply(connection,
			                                    protocols_ck, &xx);
			proto_err = xx;
		}
		errnum = get_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (proto_err != NULL) {
			errnum = get_xcb_error(proto_err);
			linted_mem_free(proto_err);
			goto destroy_window;
		}

		wm_protocols_atom = proto_reply->atom;
		linted_mem_free(proto_reply);
	}

	{
		xcb_intern_atom_reply_t *delete_ck_reply;
		xcb_generic_error_t *delete_ck_err;
		{
			xcb_generic_error_t *xx = NULL;
			delete_ck_reply =
			    xcb_intern_atom_reply(connection, delete_ck, &xx);
			delete_ck_err = xx;
		}
		errnum = get_xcb_conn_error(connection);
		if (errnum != 0)
			goto destroy_window;

		if (delete_ck_err != NULL) {
			errnum = get_xcb_error(delete_ck_err);
			linted_mem_free(delete_ck_err);
			goto destroy_window;
		}

		wm_delete_window_atom = delete_ck_reply->atom;
		linted_mem_free(delete_ck_reply);
	}

	xcb_void_cookie_t ch_prop_ck = xcb_change_property_checked(
	    connection, XCB_PROP_MODE_REPLACE, window, wm_protocols_atom, 4, 32,
	    1, &wm_delete_window_atom);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_void_cookie_t map_ck = xcb_map_window(connection, window);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_generic_error_t *create_win_err =
	    xcb_request_check(connection, create_win_ck);
	if (create_win_err != NULL) {
		errnum = get_xcb_error(create_win_err);
		linted_mem_free(create_win_err);
		goto destroy_window;
	}

	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;

	xcb_generic_error_t *ch_prop_err =
	    xcb_request_check(connection, ch_prop_ck);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;
	if (ch_prop_err != NULL) {
		errnum = get_xcb_error(ch_prop_err);
		linted_mem_free(ch_prop_err);
		goto destroy_window;
	}

	xcb_generic_error_t *map_err = xcb_request_check(connection, map_ck);
	errnum = get_xcb_conn_error(connection);
	if (errnum != 0)
		goto destroy_window;
	if (map_err != NULL) {
		errnum = get_xcb_error(map_err);
		linted_mem_free(map_err);
		goto destroy_window;
	}

	EGLDisplay egl_display = eglGetDisplay(display);
	if (EGL_NO_DISPLAY == egl_display) {
		errnum = egl_error();
		goto destroy_window;
	}

	if (!eglInitialize(egl_display, NULL, NULL)) {
		errnum = egl_error();
		goto destroy_window;
	}

	EGLint num_configs;
	{
		EGLint xx;
		if (!eglGetConfigs(egl_display, NULL, 0, &xx)) {
			errnum = egl_error();
			goto destroy_egl_display;
		}
		num_configs = xx;
	}

	EGLConfig egl_config;
	{
		EGLConfig xx;
		EGLint yy = num_configs;
		if (!eglChooseConfig(egl_display, attr_list, &xx, 1U, &yy)) {
			errnum = egl_error();
			goto destroy_egl_display;
		}
		egl_config = xx;
	}

	EGLSurface egl_surface =
	    eglCreateWindowSurface(egl_display, egl_config, window, NULL);
	if (EGL_NO_SURFACE == egl_surface) {
		errnum = egl_error();
		goto destroy_egl_display;
	}

	bool time_to_quit;

	linted_updater_receive(LINTED_UPCAST(&updater_task), ON_RECEIVE_UPDATE,
	                       updater);
	updater_task.sim_model = &sim_model;
	updater_task.pool = pool;

	linted_asynch_pool_submit(pool, UPDATER_UPCAST(&updater_task));

	linted_ko_task_poll(LINTED_UPCAST(&poll_conn_task), ON_POLL_CONN,
	                    xcb_get_file_descriptor(connection), POLLIN);
	poll_conn_task.time_to_quit = &time_to_quit;
	poll_conn_task.window_model = &window_model;
	poll_conn_task.pool = pool;
	poll_conn_task.connection = connection;
	poll_conn_task.display = display;

	linted_asynch_pool_submit(
	    pool, LINTED_UPCAST(LINTED_UPCAST(&poll_conn_task)));

reopen_graphics_context:
	;
	EGLContext egl_context = eglCreateContext(egl_display, egl_config,
	                                          EGL_NO_CONTEXT, context_attr);
	if (EGL_NO_CONTEXT == egl_context) {
		errnum = egl_error();
		goto destroy_pool;
	}

	if (!eglMakeCurrent(egl_display, egl_surface, egl_surface,
	                    egl_context)) {
		errnum = egl_error();
		goto destroy_egl_context;
	}

	struct graphics_state graphics_state;

	errnum = init_graphics(log, &graphics_state);
	if (errnum != 0) {
		errnum = egl_error();
		goto use_no_egl_context;
	}

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
					goto cleanup_gl;

				completed_task = xx;
			}
			errnum = dispatch(completed_task);
			if (errnum != 0)
				goto cleanup_gl;

			if (time_to_quit)
				goto cleanup_gl;
		}

		/* Only draw or resize if we have time to waste */

		if (window_model.resize_pending) {
			resize_graphics(&graphics_state, window_model.width,
			                window_model.height);
			window_model.resize_pending = false;
		} else if (window_model.viewable) {
			draw_graphics(&graphics_state, &sim_model,
			              &window_model);
			eglSwapBuffers(egl_display, egl_surface);
		} else {
			time_to_quit = false;

			struct linted_asynch_task *completed_task;
			{
				struct linted_asynch_task *xx;
				errnum = linted_asynch_pool_wait(pool, &xx);
				if (errnum != 0)
					goto cleanup_gl;
				completed_task = xx;
			}
			errnum = dispatch(completed_task);
			if (errnum != 0)
				goto cleanup_gl;

			if (time_to_quit)
				goto cleanup_gl;
		}
	}

cleanup_gl:
	destroy_graphics(&graphics_state);

use_no_egl_context:
	if (!eglMakeCurrent(egl_display, EGL_NO_SURFACE, EGL_NO_SURFACE,
	                    EGL_NO_CONTEXT)) {
		if (0 == errnum)
			errnum = egl_error();
	}

destroy_egl_context:
	if (!eglDestroyContext(egl_display, egl_context)) {
		if (0 == errnum)
			errnum = egl_error();
	}

	if (ENOSYS == errnum)
		goto reopen_graphics_context;

	if (!eglDestroySurface(egl_display, egl_surface)) {
		if (0 == errnum)
			errnum = egl_error();
	}

destroy_egl_display:
	if (!eglTerminate(egl_display)) {
		if (0 == errnum)
			errnum = egl_error();
	}

	if (!eglReleaseThread()) {
		if (0 == errnum)
			errnum = egl_error();
	}

destroy_window : {
	xcb_void_cookie_t destroy_ck =
	    xcb_destroy_window_checked(connection, window);
	if (0 == errnum)
		errnum = get_xcb_conn_error(connection);

	xcb_generic_error_t *destroy_err =
	    xcb_request_check(connection, destroy_ck);
	if (0 == errnum)
		errnum = get_xcb_conn_error(connection);
	if (0 == errnum && destroy_err != NULL)
		errnum = get_xcb_error(destroy_err);
	linted_mem_free(destroy_err);
}

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
	(void)updater_task;
	(void)notice_task;
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
	case ON_RECEIVE_UPDATE:
		return on_receive_update(task);

	case ON_POLL_CONN:
		return on_poll_conn(task);

	case ON_SENT_NOTICE:
		return on_sent_notice(task);

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
	struct linted_asynch_pool *pool = poll_conn_task->pool;
	struct window_model *window_model = poll_conn_task->window_model;
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
			window_model->width = configure_event->width;
			window_model->height = configure_event->height;
			window_model->resize_pending = true;
			break;
		}

		case UnmapNotify:
			window_model->viewable = false;
			break;

		case MapNotify:
			window_model->viewable = true;
			break;

		case ClientMessage:
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
	struct sim_model *sim_model = updater_task->sim_model;
	struct linted_asynch_pool *pool = updater_task->pool;

	struct linted_updater_update update;

	linted_updater_decode(LINTED_UPCAST(updater_task), &update);

	linted_asynch_pool_submit(pool, task);

	sim_model->x_rotation =
	    linted_updater_angle_to_float(update.x_rotation);
	sim_model->y_rotation =
	    linted_updater_angle_to_float(update.y_rotation);

	sim_model->x_position = update.x_position * (1 / 2048.0);
	sim_model->y_position = update.y_position * (1 / 2048.0);
	sim_model->z_position = update.z_position * (1 / 2048.0);

	return 0;
}

static linted_error on_sent_notice(struct linted_asynch_task *task)
{
	return task->errnum;
}

static linted_error init_graphics(linted_log log,
                                  struct graphics_state *graphics_state)
{
	linted_error errnum = 0;

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	/* The brightest colour is (1, 1, 1) */
	/* The darkest colour is (0, 0, 0) */
	/* We want a neutral, middle brightness */

	/* It might be possible to use a physically based model based
	 * off the energy contained in a ray of light of a certain hue
	 * but we have chosen to model brightnes as:
	 */
	/* brightness = r red + g green + b blue */

	/* Note that this is a crappy approximation that only works
	 * for some monitors and eyes.
	 */

	/* brightest = r + g + b */
	/* darkest = 0 */

	/* We calculate a middling brightness taking into account
	 * gamma and nonlinearity.
	 */

	/* Note that how bright we want our background colour to be is
	 * really a matter of taste and not math. The halfway point is
	 * simply a good starting point.
	 */
	double r = 0.2126;
	double g = 0.7152;
	double b = 0.0722;

	double brightness = (r + g + b) * square(0.5);

	/* We can then carve off some red and green to make room for more
	 * blue but still keep the same amount of brightness.
	 */
	/* brightness = r red + g green + b blue */
	/* red = green = x */
	/* brightness = (r + g) x + b blue */
	/* brightness - b blue = (r + g) x */
	/* (brightness - b blue) / (r + g) = x */

	/* adjust blue to taste */
	double blue = square(0.75);
	double x = (brightness - b * blue) / (r + g);
	double red = x;
	double green = x;

	glClearColor(red, green, blue, 1);

	flush_gl_errors();
	GLuint program = glCreateProgram();
	if (0 == program)
		return get_gl_error();

	{
		flush_gl_errors();
		GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
		if (0 == fragment_shader) {
			errnum = get_gl_error();
			goto cleanup_program;
		}
		glAttachShader(program, fragment_shader);
		glDeleteShader(fragment_shader);

		glShaderSource(fragment_shader, 1U,
		               (GLchar const **)&linted_assets_fragment_shader,
		               NULL);
		glCompileShader(fragment_shader);

		GLint is_valid;
		glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &is_valid);
		if (!is_valid) {
			errnum = EINVAL;

			GLint info_log_length;
			glGetShaderiv(fragment_shader, GL_INFO_LOG_LENGTH,
			              &info_log_length);

			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (0 == mem_errnum) {
				GLchar *info_log = xx;
				glGetShaderInfoLog(fragment_shader,
				                   info_log_length, NULL,
				                   info_log);
				log_str(log, LINTED_STR("Invalid shader: "),
				        info_log);
				linted_mem_free(info_log);
			}
			goto cleanup_program;
		}
	}

	{
		flush_gl_errors();
		GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
		if (0 == vertex_shader) {
			errnum = get_gl_error();
			goto cleanup_program;
		}
		glAttachShader(program, vertex_shader);
		glDeleteShader(vertex_shader);

		glShaderSource(vertex_shader, 1U,
		               (GLchar const **)&linted_assets_vertex_shader,
		               NULL);
		glCompileShader(vertex_shader);

		GLint is_valid;
		glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &is_valid);
		if (!is_valid) {
			errnum = EINVAL;

			GLint info_log_length;
			glGetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH,
			              &info_log_length);

			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (0 == mem_errnum) {
				GLchar *info_log = xx;
				glGetShaderInfoLog(vertex_shader,
				                   info_log_length, NULL,
				                   info_log);
				log_str(log, LINTED_STR("Invalid shader: "),
				        info_log);
				linted_mem_free(info_log);
			}
			goto cleanup_program;
		}
	}
	glLinkProgram(program);

	glValidateProgram(program);

	GLint is_valid;
	glGetProgramiv(program, GL_VALIDATE_STATUS, &is_valid);
	if (!is_valid) {
		errnum = EINVAL;

		GLint info_log_length;
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_log_length);

		void *xx;
		linted_error mem_errnum =
		    linted_mem_alloc(&xx, info_log_length);
		if (0 == mem_errnum) {
			GLchar *info_log = xx;
			glGetProgramInfoLog(program, info_log_length, NULL,
			                    info_log);
			log_str(log, LINTED_STR("Invalid program: "), info_log);
			linted_mem_free(info_log);
		}
		goto cleanup_program;
	}

	GLint mvp_matrix =
	    glGetUniformLocation(program, "model_view_projection_matrix");
	if (-1 == mvp_matrix) {
		errnum = EINVAL;
		goto cleanup_program;
	}

	GLint vertex = glGetAttribLocation(program, "vertex");
	if (-1 == vertex) {
		errnum = EINVAL;
		goto cleanup_program;
	}

	GLint normal = glGetAttribLocation(program, "normal");
	if (-1 == normal) {
		errnum = EINVAL;
		goto cleanup_program;
	}

	glEnableVertexAttribArray(vertex);
	glEnableVertexAttribArray(normal);

	glVertexAttribPointer(vertex,
	                      LINTED_ARRAY_SIZE(linted_assets_vertices[0U]),
	                      GL_FLOAT, false, 0, linted_assets_vertices);
	glVertexAttribPointer(normal,
	                      LINTED_ARRAY_SIZE(linted_assets_normals[0U]),
	                      GL_FLOAT, false, 0, linted_assets_normals);

	graphics_state->program = program;
	graphics_state->model_view_projection_matrix = mvp_matrix;

	glUseProgram(program);

	return 0;

cleanup_program:
	glDeleteProgram(program);

	return errnum;
}

static void destroy_graphics(struct graphics_state *graphics_state)
{
	glUseProgram(0);
	glDeleteProgram(graphics_state->program);

	graphics_state->program = 0;
	graphics_state->model_view_projection_matrix = 0;
}

static void resize_graphics(struct graphics_state *graphics_state,
                            unsigned width, unsigned height)
{
	glViewport(0, 0, width, height);
}

static void draw_graphics(struct graphics_state const *graphics_state,
                          struct sim_model const *sim_model,
                          struct window_model const *window_model)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	/* X, Y, Z, W coords of the resultant vector are the
	 * sums of the columns (row major order).
	 */

	{
		GLfloat x_rotation = sim_model->x_rotation;
		GLfloat y_rotation = sim_model->y_rotation;

		GLfloat x_position = sim_model->x_position;
		GLfloat y_position = sim_model->y_position;
		GLfloat z_position = sim_model->z_position;

		/* Rotate the camera */
		GLfloat cos_y = cosf(y_rotation);
		GLfloat sin_y = sinf(y_rotation);
		GLfloat const y_rotation_matrix[][4U] = {{1, 0, 0, 0},
		                                         {0, cos_y, -sin_y, 0},
		                                         {0, sin_y, cos_y, 0},
		                                         {0, 0, 0, 1}};

		GLfloat cos_x = cosf(x_rotation);
		GLfloat sin_x = sinf(x_rotation);
		GLfloat const x_rotation_matrix[][4U] = {{cos_x, 0, sin_x, 0},
		                                         {0, 1, 0, 0},
		                                         {-sin_x, 0, cos_x, 0},
		                                         {0, 0, 0, 1}};

		/* Translate the camera */
		GLfloat const camera[][4U] = {
		    {1, 0, 0, 0},
		    {0, 1, 0, 0},
		    {0, 0, 1, 0},
		    {x_position, y_position, z_position, 1}};

		GLfloat aspect =
		    window_model->width / (GLfloat)window_model->height;
		double fov = acos(-1.0) / 4;

		double d = 1 / tan(fov / 2);
		double far = 1000;
		double near = 1;

		GLfloat const projection[][4U] = {
		    {d / aspect, 0, 0, 0},
		    {0, d, 0, 0},
		    {0, 0, (far + near) / (near - far),
		     2 * far * near / (near - far)},
		    {0, 0, -1, 0}};

		GLfloat rotations[4U][4U];
		GLfloat model_view[4U][4U];
		GLfloat model_view_projection[4U][4U];

		matrix_multiply(x_rotation_matrix, y_rotation_matrix,
		                rotations);
		matrix_multiply((const GLfloat(*)[4U])camera,
		                (const GLfloat(*)[4U])rotations, model_view);
		matrix_multiply((const GLfloat(*)[4U])model_view, projection,
		                model_view_projection);

		glUniformMatrix4fv(graphics_state->model_view_projection_matrix,
		                   1U, false, model_view_projection[0U]);
	}

	glDrawElements(GL_TRIANGLES, 3U * linted_assets_indices_size,
	               GL_UNSIGNED_BYTE, linted_assets_indices);
}

static void flush_gl_errors(void)
{
	GLenum error;
	do {
		error = glGetError();
	} while (error != GL_NO_ERROR);
}

static linted_error get_gl_error(void)
{
	/* Get the first error. Note that a single OpenGL call may return
       * multiple errors so this only gives the first of many possible
       * errors.
       */
	switch (glGetError()) {
	case GL_NO_ERROR:
		return 0;

	case GL_INVALID_ENUM:
	case GL_INVALID_VALUE:
	case GL_INVALID_OPERATION:
	case GL_INVALID_FRAMEBUFFER_OPERATION:
		return EINVAL;

	case GL_OUT_OF_MEMORY:
		return ENOMEM;

	default:
		return ENOSYS;
	}
}

static void matrix_multiply(GLfloat const a[restrict 4U][4U],
                            GLfloat const b[restrict 4U][4U],
                            GLfloat result[restrict 4U][4U])
{
	result[0U][0U] = a[0U][0U] * b[0U][0U] + a[0U][1U] * b[1U][0U] +
	                 a[0U][2U] * b[2U][0U] + a[0U][3U] * b[3U][0U];
	result[1U][0U] = a[1U][0U] * b[0U][0U] + a[1U][1U] * b[1U][0U] +
	                 a[1U][2U] * b[2U][0U] + a[1U][3U] * b[3U][0U];
	result[2U][0U] = a[2U][0U] * b[0U][0U] + a[2U][1U] * b[1U][0U] +
	                 a[2U][2U] * b[2U][0U] + a[2U][3U] * b[3U][0U];
	result[3U][0U] = a[3U][0U] * b[0U][0U] + a[3U][1U] * b[1U][0U] +
	                 a[3U][2U] * b[2U][0U] + a[3U][3U] * b[3U][0U];

	result[0U][1U] = a[0U][0U] * b[0U][1U] + a[0U][1U] * b[1U][1U] +
	                 a[0U][2U] * b[2U][1U] + a[0U][3U] * b[3U][1U];
	result[1U][1U] = a[1U][0U] * b[0U][1U] + a[1U][1U] * b[1U][1U] +
	                 a[1U][2U] * b[2U][1U] + a[1U][3U] * b[3U][1U];
	result[2U][1U] = a[2U][0U] * b[0U][1U] + a[2U][1U] * b[1U][1U] +
	                 a[2U][2U] * b[2U][1U] + a[2U][3U] * b[3U][1U];
	result[3U][1U] = a[3U][0U] * b[0U][1U] + a[3U][1U] * b[1U][1U] +
	                 a[3U][2U] * b[2U][1U] + a[3U][3U] * b[3U][1U];

	result[0U][2U] = a[0U][0U] * b[0U][2U] + a[0U][1U] * b[1U][2U] +
	                 a[0U][2U] * b[2U][2U] + a[0U][3U] * b[3U][2U];
	result[1U][2U] = a[1U][0U] * b[0U][2U] + a[1U][1U] * b[1U][2U] +
	                 a[1U][2U] * b[2U][2U] + a[1U][3U] * b[3U][2U];
	result[2U][2U] = a[2U][0U] * b[0U][2U] + a[2U][1U] * b[1U][2U] +
	                 a[2U][2U] * b[2U][2U] + a[2U][3U] * b[3U][2U];
	result[3U][2U] = a[3U][0U] * b[0U][2U] + a[3U][1U] * b[1U][2U] +
	                 a[3U][2U] * b[2U][2U] + a[3U][3U] * b[3U][2U];

	result[0U][3U] = a[0U][0U] * b[0U][3U] + a[0U][1U] * b[1U][3U] +
	                 a[0U][2U] * b[2U][3U] + a[0U][3U] * b[3U][3U];
	result[1U][3U] = a[1U][0U] * b[0U][3U] + a[1U][1U] * b[1U][3U] +
	                 a[1U][2U] * b[2U][3U] + a[1U][3U] * b[3U][3U];
	result[2U][3U] = a[2U][0U] * b[0U][3U] + a[2U][1U] * b[1U][3U] +
	                 a[2U][2U] * b[2U][3U] + a[2U][3U] * b[3U][3U];
	result[3U][3U] = a[3U][0U] * b[0U][3U] + a[3U][1U] * b[1U][3U] +
	                 a[3U][2U] * b[2U][3U] + a[3U][3U] * b[3U][3U];
}

static linted_error egl_error(void)
{
	switch (eglGetError()) {
	case EGL_SUCCESS:
		return 0;

	case EGL_NOT_INITIALIZED:
		return ENOSYS;

	case EGL_BAD_ACCESS:
		return EAGAIN;

	case EGL_BAD_ALLOC:
		return ENOMEM;

	case EGL_BAD_ATTRIBUTE:
	case EGL_BAD_CONTEXT:
	case EGL_BAD_CONFIG:
	case EGL_BAD_CURRENT_SURFACE:
	case EGL_BAD_DISPLAY:
	case EGL_BAD_SURFACE:
	case EGL_BAD_MATCH:
	case EGL_BAD_PARAMETER:
	case EGL_BAD_NATIVE_PIXMAP:
	case EGL_BAD_NATIVE_WINDOW:
		return EINVAL;

	case EGL_CONTEXT_LOST:
		return ENOSYS;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
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

static double square(double x)
{
	return x * x;
}

static linted_error log_str(linted_log log, struct linted_str start,
                            char const *error)
{
	linted_error errnum;
	size_t error_size = strlen(error);

	char *full_string;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, error_size + start.size);
		if (errnum != 0)
			/* Silently drop log */
			return errnum;
		full_string = xx;
	}

	memcpy(full_string, start.bytes, start.size);
	memcpy(full_string + start.size, error, error_size);

	errnum = linted_log_write(log, full_string, start.size + error_size);

	linted_mem_free(full_string);

	return errnum;
}
