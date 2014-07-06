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

#include "assets.h"
#include "gl_core.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/controller.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/logger.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <GL/glx.h>
#include <xcb/xcb.h>
#include <X11/Xlib.h>
#include <X11/Xlib-xcb.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

enum {
    ON_RECEIVE_UPDATE,
    ON_SENT_CONTROL,
    MAX_TASKS
};

struct controller_data
{
    struct linted_controller_message update;
    bool update_pending : 1;
    bool update_in_progress : 1;
};

struct graphics_state
{
    GLuint program;
};

struct window_model
{
    unsigned width;
    unsigned height;

    bool resize_pending : 1;
    bool viewable : 1;
    bool focused : 1;
};

struct sim_model
{
    float x_rotation;
    float y_rotation;

    float x_position;
    float y_position;
    float z_position;
};

static int const attribute_list[][2u]
    = { { GLX_RGBA, True },
        { GLX_RED_SIZE, 5 },
        { GLX_GREEN_SIZE, 5 },
        { GLX_BLUE_SIZE, 3 },
        { GLX_DOUBLEBUFFER, True },
        { GLX_DEPTH_SIZE, 16 },
        { None, 0 /* A waste of an int. Oh well. */ } };

struct on_gui_event_args
{
    xcb_connection_t *connection;
    struct window_model *window_model;
    struct controller_data *controller_data;
    bool *time_to_quit;
    xcb_window_t window;
};

struct gui_controller_task;

struct gui_updater_task
{
    struct linted_updater_task_receive parent;
    struct sim_model *sim_model;
    struct gui_controller_task *controller_task;
    struct controller_data *controller_data;
    struct linted_asynch_pool *pool;
    linted_ko controller;
};

struct gui_controller_task
{
    struct linted_controller_task_send parent;
    struct controller_data *controller_data;
    struct linted_asynch_pool *pool;
    linted_ko controller;
};

static linted_error on_gui_event(XEvent *event, struct on_gui_event_args args);

static linted_error dispatch(struct linted_asynch_task *completed_task);
static linted_error on_receive_update(struct linted_asynch_task *task);
static linted_error on_sent_control(struct linted_asynch_task *task);

static linted_error errnum_from_connection(xcb_connection_t *connection);

static linted_error get_mouse_position(xcb_connection_t *connection,
                                       xcb_window_t window, int *x, int *y);

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_model const *window_model,
                    struct controller_data *controller_data);

static linted_error init_graphics(linted_logger logger,
                                  struct graphics_state *graphics_state,
                                  struct window_model const *window_model);
static void render_graphics(struct graphics_state const *graphics_state,
                            struct sim_model const *sim_model,
                            struct window_model const *window_model);
static void destroy_graphics(struct graphics_state *graphics_state);
static void resize_graphics(unsigned width, unsigned height);

static void flush_gl_errors(void);

/**
 * @todo get_gl_error's use of glGetError is incorrect. Multiple error
 *       flags may be set and returned by a single function.
 */
static linted_error get_gl_error(void);

static double square(double x);

static linted_error gui_help(linted_ko ko, char const *program_name,
                             struct linted_str package_name,
                             struct linted_str package_url,
                             struct linted_str package_bugreport);
static linted_error failure(linted_ko ko, char const *program_name,
                            struct linted_str message, linted_error errnum);
static linted_error log_str(linted_logger logger, struct linted_str start,
                            char const *str);

static linted_ko kos[3u];

struct linted_start_config const linted_start_config
    = { .canonical_process_name = PACKAGE_NAME "-gui",
        .open_current_working_directory = false,
        .kos_size = LINTED_ARRAY_SIZE(kos),
        .kos = kos };

uint_fast8_t linted_start(int cwd, char const *const program_name, size_t argc,
                          char const *const argv[const])
{
    linted_error errnum = 0;

    linted_logger logger = kos[0u];
    linted_controller controller = kos[1u];
    linted_updater updater = kos[2u];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    for (size_t ii = 1u; ii < argc; ++ii) {
        char const *argument = argv[ii];

        if (0 == strcmp(HELP_OPTION, argument)) {
            need_help = true;
        } else if (0 == strcmp(VERSION_OPTION, argument)) {
            need_version = true;
        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        gui_help(STDOUT_FILENO, program_name, LINTED_STR(PACKAGE_NAME),
                 LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(STDERR_FILENO, program_name, bad_option);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_string(STDERR_FILENO, NULL, program_name);
        linted_io_write_str(STDERR_FILENO, NULL,
                            LINTED_STR(": no DISPLAY environment variable\n"));
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    size_t display_string_length = strlen(original_display) + 1;
    char *display_env_var;
    {
        linted_error xx;
        display_env_var = linted_mem_alloc(&xx, display_string_length);
        errnum = xx;
    }
    if (errnum != 0) {
        failure(STDERR_FILENO, program_name,
                LINTED_STR("no DISPLAY environment variable"), errnum);
        return EXIT_FAILURE;
    }
    memcpy(display_env_var, original_display, display_string_length);

    if ((errnum = linted_util_sanitize_environment()) != 0) {
        failure(STDERR_FILENO, program_name,
                LINTED_STR("cannot sanitize the program environment"), errnum);
        return EXIT_FAILURE;
    }

    struct linted_asynch_pool *pool;
    {
        struct linted_asynch_pool *xx;
        if ((errnum = linted_asynch_pool_create(&xx, MAX_TASKS)) != 0) {
            goto shutdown;
        }
        pool = xx;
    }

    struct window_model window_model = { .width = 640,
                                         .height = 480,
                                         .viewable = true,

                                         /* Do the initial resize */
                                         .resize_pending = true };

    struct controller_data controller_data = {
        .update
        = { .forward = false, .back = false, .right = false, .left = false },
        .update_pending = false,
        .update_in_progress = false
    };

    struct sim_model sim_model = { .x_rotation = 0,
                                   .y_rotation = 0,
                                   .x_position = 0,
                                   .y_position = 0,
                                   .z_position = 0 };

    Display *display = XOpenDisplay(display_env_var);
    if (NULL == display) {
        errnum = ENOSYS;
        goto destroy_pool;
    }

    xcb_connection_t *connection = XGetXCBConnection(display);
    unsigned screen_number = XDefaultScreen(display);

    xcb_screen_t *screen = NULL;
    {
        xcb_screen_iterator_t iter
            = xcb_setup_roots_iterator(xcb_get_setup(connection));
        for (size_t ii = 0u; ii < screen_number; ++ii) {
            if (0 == iter.rem) {
                break;
            }

            xcb_screen_next(&iter);
        }

        if (0 == iter.rem) {
            errnum = EINVAL;
            goto disconnect;
        }

        screen = iter.data;
    }

    /* Query framebuffer configurations */
    XVisualInfo visual_info;
    {
        XVisualInfo *ptr = glXChooseVisual(display, screen_number,
                                           (int *)&attribute_list[0u][0u]);
        if (NULL == ptr) {
            errnum = ENOSYS;
            goto disconnect;
        }
        visual_info = *ptr;
        XFree(ptr);
    }

    xcb_colormap_t colormap = xcb_generate_id(connection);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        goto disconnect;
    }

    xcb_create_colormap(connection, XCB_COLORMAP_ALLOC_NONE, colormap,
                        screen->root, visual_info.visualid);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        goto destroy_colormap;
    }

    xcb_window_t window = xcb_generate_id(connection);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        goto destroy_colormap;
    }

    {
        uint32_t event_max = 0u;
        event_max |= XCB_EVENT_MASK_STRUCTURE_NOTIFY;
        event_max |= XCB_EVENT_MASK_ENTER_WINDOW | XCB_EVENT_MASK_LEAVE_WINDOW;
        event_max |= XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_KEY_RELEASE;
        event_max |= XCB_EVENT_MASK_POINTER_MOTION;

        uint32_t values[] = { event_max, colormap, 0u };
        xcb_create_window(connection, visual_info.depth, window, screen->root,
                          0, 0, window_model.width, window_model.height, 0,
                          XCB_WINDOW_CLASS_INPUT_OUTPUT, visual_info.visualid,
                          XCB_CW_EVENT_MASK | XCB_CW_COLORMAP, values);
    }
    if ((errnum = errnum_from_connection(connection)) != 0) {
        goto destroy_colormap;
    }

    xcb_atom_t wm_delete_window;
    {
        xcb_intern_atom_cookie_t cookie
            = xcb_intern_atom(connection, 1, 12, "WM_PROTOCOLS");
        if ((errnum = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }

        xcb_intern_atom_reply_t *reply
            = xcb_intern_atom_reply(connection, cookie, 0);
        if ((errnum = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }
        xcb_atom_t wm_protocols = reply->atom;
        linted_mem_free(reply);

        xcb_intern_atom_cookie_t cookie2
            = xcb_intern_atom(connection, 0, 16, "WM_DELETE_WINDOW");
        if ((errnum = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }

        xcb_intern_atom_reply_t *reply2
            = xcb_intern_atom_reply(connection, cookie2, 0);
        if ((errnum = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }
        wm_delete_window = reply2->atom;
        linted_mem_free(reply2);

        xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
                            wm_protocols, 4, 32, 1, &wm_delete_window);
        if ((errnum = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }
    }

    xcb_map_window(connection, window);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        goto destroy_window;
    }

    xcb_flush(connection);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        goto destroy_window;
    }

    GLXContext glx_context
        = glXCreateContext(display, &visual_info, NULL, GL_TRUE);
    if (NULL == glx_context) {
        errnum = ENOSYS;
        goto destroy_window;
    }

    if (!glXMakeContextCurrent(display, window, window, glx_context)) {
        errnum = ENOSYS;
        goto destroy_glx_context;
    }

    struct graphics_state graphics_state;

    if ((errnum = init_graphics(logger, &graphics_state, &window_model)) != 0) {
        goto destroy_glx_context;
    }

    struct gui_updater_task updater_task;
    struct gui_controller_task controller_task;

    linted_updater_receive(LINTED_UPCAST(&updater_task), ON_RECEIVE_UPDATE,
                           updater);
    updater_task.sim_model = &sim_model;
    updater_task.controller_task = &controller_task;
    updater_task.controller_data = &controller_data;
    updater_task.pool = pool;
    updater_task.controller = controller;

    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&updater_task))));

    /* TODO: Detect SIGTERM and exit normally */
    for (;;) {
        /* Handle GUI events first before rendering */
        /* We have to use the Xlib event queue because of broken Mesa
         * libraries which abuse it.
         */
        bool had_gui_event = XPending(display) > 0;
        if (had_gui_event) {
            XEvent event;
            XNextEvent(display, &event);

            bool time_to_quit;
            struct on_gui_event_args args
                = { .connection = connection,
                    .window = window,
                    .window_model = &window_model,
                    .controller_data = &controller_data,
                    .time_to_quit = &time_to_quit };
            if ((errnum = on_gui_event(&event, args)) != 0) {
                goto cleanup_gl;
            }
            if (time_to_quit) {
                goto cleanup_gl;
            }
        }

        struct linted_asynch_task *completed_tasks[20u];
        size_t task_count;
        linted_error poll_errnum;
        {
            size_t xx;
            poll_errnum = linted_asynch_pool_poll(
                pool, completed_tasks, LINTED_ARRAY_SIZE(completed_tasks), &xx);
            task_count = xx;
        }

        bool had_asynch_event = poll_errnum != EAGAIN;
        if (had_asynch_event) {
            for (size_t ii = 0u; ii < task_count; ++ii) {
                if ((errnum = dispatch(completed_tasks[ii])) != 0) {
                    goto cleanup_gl;
                }
            }
        }

        /* Only render or resize if we have time to waste */
        if (!had_gui_event && !had_asynch_event) {
            if (window_model.resize_pending) {
                resize_graphics(window_model.width, window_model.height);
                window_model.resize_pending = false;
            } else if (window_model.viewable) {
                render_graphics(&graphics_state, &sim_model, &window_model);
                glXSwapBuffers(display, window);
            } else {
                /*
                 * This is an ugly hack and waiting on the X11 file
                 * descriptor should be implemented eventually.
                 */
                struct linted_asynch_task_sleep_until sleeper_task;
                {
                    struct timespec request = {.tv_sec = 0, .tv_nsec = 10};
                    linted_asynch_sleep_until(&sleeper_task, 0, 0, &request);
                }
                linted_asynch_pool_submit(NULL, LINTED_UPCAST(&sleeper_task));
                errnum = LINTED_UPCAST(&sleeper_task)->errnum;
                if (errnum != 0) {
                    assert(errnum != EINVAL);
                    assert(errnum != EFAULT);
                    goto cleanup_gl;
                }
            }
        }
    }

cleanup_gl:
    destroy_graphics(&graphics_state);

destroy_glx_context:
    glXMakeContextCurrent(display, None, None, NULL);
    glXDestroyContext(display, glx_context);

destroy_window:
    xcb_destroy_window(connection, window);
    {
        linted_error conn_errnum = errnum_from_connection(connection);
        if (0 == errnum) {
            errnum = conn_errnum;
        }
    }

destroy_colormap:
    xcb_free_colormap(connection, colormap);
    {
        linted_error conn_errnum = errnum_from_connection(connection);
        if (0 == errnum) {
            errnum = conn_errnum;
        }
    }

disconnect:
    xcb_flush(connection);
    {
        linted_error conn_errnum = errnum_from_connection(connection);
        if (0 == errnum) {
            errnum = conn_errnum;
        }
    }

    /**
     * @todo Check for errors that have accumulated on the connection.
     */

    XCloseDisplay(display);

destroy_pool : {
    linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
    if (0 == errnum) {
        errnum = destroy_errnum;
    }
}

shutdown:
    return errnum;
}

static linted_error dispatch(struct linted_asynch_task *completed_task)
{
    switch (completed_task->task_action) {
    case ON_RECEIVE_UPDATE:
        return on_receive_update(completed_task);

    case ON_SENT_CONTROL:
        return on_sent_control(completed_task);

    default:
        assert(false);
    }
}

static linted_error on_gui_event(XEvent *event, struct on_gui_event_args args)
{
    xcb_connection_t *connection = args.connection;
    xcb_window_t window = args.window;
    struct window_model *window_model = args.window_model;
    struct controller_data *controller_data = args.controller_data;
    bool *time_to_quitp = args.time_to_quit;

    linted_error errnum;
    bool time_to_quit = false;
    bool is_key_down;
    switch (event->type) {
    case ConfigureNotify: {
        XConfigureEvent const *configure_event = &event->xconfigure;
        window_model->width = configure_event->width;
        window_model->height = configure_event->height;
        window_model->resize_pending = true;
        break;
    }

    case MotionNotify: {
        XMotionEvent const *motion_event = &event->xmotion;
        on_tilt(motion_event->x, motion_event->y, window_model,
                controller_data);
        break;
    }

    case UnmapNotify:
        window_model->viewable = false;
        break;

    case MapNotify:
        window_model->viewable = true;
        break;

    case EnterNotify: {
        window_model->focused = true;

        int x, y;
        if ((errnum = get_mouse_position(connection, window, &x, &y)) != 0) {
            return errnum;
        }

        on_tilt(x, y, window_model, controller_data);
        break;
    }

    case LeaveNotify:
        window_model->focused = false;

        memset(&controller_data->update, 0, sizeof controller_data->update);

        controller_data->update_pending = true;
        break;

    case MappingNotify: {
        XMappingEvent *mapping_event = &event->xmapping;
        XRefreshKeyboardMapping(mapping_event);
    }

    case KeyPress:
        is_key_down = true;
        goto on_key_event;

    case KeyRelease:
        is_key_down = false;
        goto on_key_event;

    case ClientMessage:
        goto quit_application;

    default:
        /* Unknown event type, ignore it */
        break;

    on_key_event : {
        XKeyEvent *key_event = &event->xkey;
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

    *time_to_quitp = time_to_quit;
    return 0;
}

static linted_error on_receive_update(struct linted_asynch_task *task)
{
    linted_error errnum;

    if ((errnum = task->errnum) != 0) {
        return errnum;
    }

    struct gui_updater_task *updater_task
        = LINTED_DOWNCAST(struct gui_updater_task, task);

    struct sim_model *sim_model = updater_task->sim_model;
    linted_ko controller = updater_task->controller;
    struct gui_controller_task *controller_task = updater_task->controller_task;
    struct controller_data *controller_data = updater_task->controller_data;
    struct linted_asynch_pool *pool = updater_task->pool;

    {
        struct linted_updater_update update;
        linted_updater_decode(LINTED_UPCAST(updater_task), &update);

        linted_asynch_pool_submit(pool, task);

        sim_model->x_rotation
            = linted_updater_angle_to_float(update.x_rotation);
        sim_model->y_rotation
            = linted_updater_angle_to_float(update.y_rotation);

        sim_model->x_position = update.x_position * (1 / (double)2048);
        sim_model->y_position = update.y_position * (1 / (double)2048);
        sim_model->z_position = update.z_position * (1 / (double)2048);
    }

    if (!controller_data->update_pending
        || controller_data->update_in_progress) {
        return 0;
    }

    linted_controller_send(LINTED_UPCAST(controller_task), ON_SENT_CONTROL,
                           controller, &controller_data->update);
    controller_task->controller_data = controller_data;
    controller_task->pool = pool;
    controller_task->controller = controller;

    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(controller_task))));

    controller_data->update_pending = false;
    controller_data->update_in_progress = true;

    return 0;
}

static linted_error on_sent_control(struct linted_asynch_task *task)
{
    linted_error errnum;

    if ((errnum = task->errnum) != 0) {
        return errnum;
    }

    struct gui_controller_task *controller_task
        = LINTED_DOWNCAST(struct gui_controller_task, task);

    struct controller_data *controller_data = controller_task->controller_data;
    struct linted_asynch_pool *pool = controller_task->pool;
    linted_ko controller = controller_task->controller;

    controller_data->update_in_progress = false;

    if (!controller_data->update_pending) {
        return 0;
    }

    linted_controller_send(LINTED_UPCAST(controller_task), ON_SENT_CONTROL,
                           controller, &controller_data->update);
    linted_asynch_pool_submit(pool, task);

    controller_data->update_pending = false;
    controller_data->update_in_progress = true;

    return 0;
}

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_model const *window_model,
                    struct controller_data *controller_data)
{
    int32_t x = (2 * mouse_x - (int)window_model->width) / 2;
    int32_t y = (2 * mouse_y - (int)window_model->height) / 2;

    /* Normalize and scale up to UINT32_MAX sized screen */
    x *= INT32_MAX / window_model->width;
    y *= INT32_MAX / window_model->height;

    controller_data->update.x_tilt = x;
    controller_data->update.y_tilt = y;

    controller_data->update_pending = true;
}

static linted_error init_graphics(linted_logger logger,
                                  struct graphics_state *graphics_state,
                                  struct window_model const *window_model)
{
    linted_error errnum = 0;

    if (linted_gl_ogl_LOAD_FAILED == linted_gl_ogl_LoadFunctions()) {
        return ENOSYS;
    }

    glEnable(GL_VERTEX_ARRAY);
    glEnable(GL_NORMAL_ARRAY);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    /* The brightest colour is (1, 1, 1) */
    /* The darkest colour is (0, 0, 0) */
    /* We want a neutral, middle brightness */

    /* It might be possible to use a physically based model based off
   * the energy contained in a ray of light of a certain hue but we
   * have chosen to model brightnes as:
   */
    /* brightness = 0.2126 red + 0.7152 green + 0.0722 blue */

    /* Note that this is a crappy approximation that only works for
   * some monitors and eyes
   */

    /* brightest = 0.2126 + 0.7152 + 0.0722 */
    /* darkest = 0 */

    /* We calculate a middling brightness taking into account gamma
   * and nonlinearity
   */

    /* Note that how bright we want our background colour to be is
   * really a matter of taste and not math. The halfway point is
   * simply a good starting point.
   */
    double brightness = (0.2126 + 0.7152 + 0.0722) * square(0.5);

    /* We can then carve off some red and green to make room for more
   * blue but still keep the same amount of brightness.
   */
    /* brightness = 0.2126 red + 0.7152 green + 0.0722 blue */
    /* red = green = x */
    /* brightness = (0.2126 + 0.7152) x + 0.0722 blue */
    /* brightness - 0.0722 blue = (0.2126 + 0.7152) x */
    /* (brightness - 0.0722 blue) / (0.2126 + 0.7152) = x */

    /* adjust blue to taste */
    double blue = square(0.75);
    double x = (brightness - 0.0722 * blue) / (0.2126 + 0.7152);
    double red = x;
    double green = x;

    glClearColor(red, green, blue, 1);

    glVertexPointer(LINTED_ARRAY_SIZE(linted_assets_triangle_vertices[0u]),
                    GL_FLOAT, 0, linted_assets_triangle_vertices);

    glNormalPointer(GL_FLOAT, 0, linted_assets_triangle_normals);

    flush_gl_errors();
    GLuint program = glCreateProgram();
    if (0 == program) {
        return get_gl_error();
    }

    {
        flush_gl_errors();
        GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
        if (0 == fragment_shader) {
            errnum = get_gl_error();
            goto cleanup_program;
        }
        glAttachShader(program, fragment_shader);
        glDeleteShader(fragment_shader);

        glShaderSource(fragment_shader, 1, &linted_assets_fragment_shader,
                       NULL);
        glCompileShader(fragment_shader);

        GLint is_valid;
        glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &is_valid);
        if (!is_valid) {
            errnum = EINVAL;

            GLint info_log_length;
            glGetShaderiv(fragment_shader, GL_INFO_LOG_LENGTH,
                          &info_log_length);

            linted_error mem_errnum;
            GLchar *info_log = linted_mem_alloc(&mem_errnum, info_log_length);
            if (0 == mem_errnum) {
                glGetShaderInfoLog(fragment_shader, info_log_length, NULL,
                                   info_log);
                log_str(logger, LINTED_STR("Invalid shader: "), info_log);
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

        glShaderSource(vertex_shader, 1, &linted_assets_vertex_shader, NULL);
        glCompileShader(vertex_shader);

        GLint is_valid;
        glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &is_valid);
        if (!is_valid) {
            errnum = EINVAL;

            GLint info_log_length;
            glGetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH, &info_log_length);

            linted_error mem_errnum;
            GLchar *info_log = linted_mem_alloc(&mem_errnum, info_log_length);
            if (0 == mem_errnum) {
                glGetShaderInfoLog(vertex_shader, info_log_length, NULL,
                                   info_log);
                log_str(logger, LINTED_STR("Invalid shader: "), info_log);
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

        linted_error mem_errnum;
        GLchar *info_log = linted_mem_alloc(&mem_errnum, info_log_length);
        if (0 == mem_errnum) {
            glGetProgramInfoLog(program, info_log_length, NULL, info_log);
            log_str(logger, LINTED_STR("Invalid program: "), info_log);
            linted_mem_free(info_log);
        }
        goto cleanup_program;
    }
    glUseProgram(program);

    graphics_state->program = program;

    return 0;

cleanup_program:
    glDeleteProgram(program);

    return errnum;
}

static void destroy_graphics(struct graphics_state *graphics_state)
{
    glUseProgram(0);
    glDeleteProgram(graphics_state->program);
    memset(graphics_state, 0, sizeof *graphics_state);
}

static void resize_graphics(unsigned width, unsigned height)
{
    glViewport(0, 0, width, height);

    glMatrixMode(GL_PROJECTION);

    {
        GLfloat aspect = width / (GLfloat)height;
        double fov = acosf(-1.0f) / 4;

        double d = 1 / tan(fov / 2);
        double far = 1000;
        double near = 1;

        GLfloat projection[][4u]
            = { { d / aspect, 0, 0, 0 }, { 0, d, 0, 0 },
                { 0,                           0,
                  (far + near) / (near - far), 2 * far * near / (near - far) },
                { 0, 0, -1, 0 } };
        glLoadMatrixf(projection[0u]);
    }

    glMatrixMode(GL_MODELVIEW);
}

static void render_graphics(struct graphics_state const *graphics_state,
                            struct sim_model const *sim_model,
                            struct window_model const *window_model)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    /*  X, Y, Z, W coords of the resultant vector are the
   *  sums of the columns (row major order).
   */
    glLoadIdentity();

    /* Rotate the camera */
    {
        GLfloat cos_y = cosf(sim_model->y_rotation);
        GLfloat sin_y = sinf(sim_model->y_rotation);
        GLfloat const rotation[][4u] = { { 1, 0, 0, 0 },
                                         { 0, cos_y, -sin_y, 0 },
                                         { 0, sin_y, cos_y, 0 },
                                         { 0, 0, 0, 1 } };
        glMultMatrixf(rotation[0u]);
    }

    {
        GLfloat cos_x = cosf(sim_model->x_rotation);
        GLfloat sin_x = sinf(sim_model->x_rotation);
        GLfloat const rotation[][4u] = { { cos_x, 0, sin_x, 0 },
                                         { 0, 1, 0, 0 },
                                         { -sin_x, 0, cos_x, 0 },
                                         { 0, 0, 0, 1 } };
        glMultMatrixf(rotation[0u]);
    }

    /* Move the camera */
    {
        GLfloat const camera[][4u]
            = { { 1, 0, 0, 0 }, { 0, 1, 0, 0 }, { 0, 0, 1, 0 },
                { sim_model->x_position, sim_model->y_position,
                  sim_model->z_position, 1 } };
        glMultMatrixf(camera[0u]);
    }

    glDrawElements(GL_TRIANGLES, 3 * linted_assets_triangle_indices_size,
                   GL_UNSIGNED_BYTE, linted_assets_triangle_indices);
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
        return EINVAL;

    case GL_STACK_OVERFLOW:
    case GL_STACK_UNDERFLOW:

    case GL_OUT_OF_MEMORY:
        return ENOMEM;

    default:
        return ENOSYS;
    }
}

static linted_error errnum_from_connection(xcb_connection_t *connection)
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
        assert(false);
    }
}

static linted_error get_mouse_position(xcb_connection_t *connection,
                                       xcb_window_t window, int *x, int *y)
{
    linted_error errnum;

    xcb_query_pointer_cookie_t cookie = xcb_query_pointer(connection, window);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        return errnum;
    }

    xcb_generic_error_t *error;
    xcb_query_pointer_reply_t *reply
        = xcb_query_pointer_reply(connection, cookie, &error);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        linted_mem_free(reply);
        return errnum;
    }

    *x = reply->win_x;
    *y = reply->win_y;

    linted_mem_free(reply);

    return 0;
}

static double square(double x)
{
    return x * x;
}

static linted_error gui_help(linted_ko ko, char const *program_name,
                             struct linted_str package_name,
                             struct linted_str package_url,
                             struct linted_str package_bugreport)
{
    linted_error errnum;

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(ko, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(" [OPTIONS]\n")))
        != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Run the gui program.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --logger            the logger file descriptor\n\
  --controller        the controller file descriptor\n\
  --updater           the updater file descriptor\n\
  --shutdowner        the shutdowner file descriptor\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static linted_error failure(linted_ko ko, char const *program_name,
                            struct linted_str message, linted_error error)
{
    linted_error errnum;

    if ((errnum = linted_io_write_string(ko, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, message)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    char const *error_string = linted_error_string_alloc(error);
    errnum = linted_io_write_string(ko, NULL, error_string);
    linted_error_string_free(error_string);

    if (errnum != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static linted_error log_str(linted_logger logger, struct linted_str start,
                            char const *error)
{
    linted_error errnum;
    size_t error_size = strlen(error);

    char *full_string = linted_mem_alloc(&errnum, error_size + start.size);
    if (errnum != 0) {
        /* Silently drop log */
        return errnum;
    }

    memcpy(full_string, start.bytes, start.size);
    memcpy(full_string + start.size, error, error_size);

    errnum = linted_logger_log(logger, full_string, start.size + error_size);

    linted_mem_free(full_string);

    return errnum;
}
