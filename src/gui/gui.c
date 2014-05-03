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
#include "config.h"

#include "assets.h"
#include "gl_core.h"

#include "linted/controller.h"
#include "linted/io.h"
#include "linted/locale.h"
#include "linted/logger.h"
#include "linted/mq.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <poll.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <GL/glx.h>
#include <xcb/xcb.h>
#include <X11/Xlib.h>
#include <X11/Xlib-xcb.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define LOGGER_OPTION "--logger"
#define CONTROLLER_OPTION "--controller"
#define SHUTDOWNER_OPTION "--shutdowner"
#define UPDATER_OPTION "--updater"

enum transition {
    SHOULD_EXIT,
    SHOULD_RESIZE,
    DO_NOTHING
};

struct controller_data {
    struct linted_controller_message update;
    bool update_pending;
};

struct graphics_state {
    GLuint program;
};

struct window_model {
    unsigned width;
    unsigned height;

    bool viewable:1;
    bool focused:1;
};

struct sim_model {
    float x_rotation;
    float y_rotation;

    float x_position;
    float y_position;
    float z_position;
};

static errno_t errnum_from_connection(xcb_connection_t * connection)
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

static errno_t get_mouse_position(xcb_connection_t * connection,
                                  xcb_window_t window, int *x, int *y)
{
    errno_t errnum;

    xcb_query_pointer_cookie_t cookie = xcb_query_pointer(connection, window);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        return errnum;
    }

    xcb_generic_error_t *error;
    xcb_query_pointer_reply_t *reply = xcb_query_pointer_reply(connection,
                                                               cookie,
                                                               &error);
    if ((errnum = errnum_from_connection(connection)) != 0) {
        free(reply);
        return errnum;
    }

    *x = reply->win_x;
    *y = reply->win_y;

    free(reply);

    return 0;
}

static void flush_gl_errors(void);

/* TODO: This usage of glGetError is incorrect. Multiple error flags
 * be set and returned by a single function.
 */
static errno_t get_gl_error(void);

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_model const *window_model,
                    struct controller_data *controller_data);

static errno_t on_updater_readable(linted_updater updater,
                                   struct sim_model *sim_model);
static errno_t on_controller_writeable(linted_controller controller,
                                       struct controller_data *controller_data);

static int const attrib_list[];
static errno_t init_graphics(linted_logger logger,
                             struct graphics_state *graphics_state,
                             struct window_model const *window_model);
static void render_graphics(struct graphics_state const *graphics_state,
                            struct sim_model const *sim_model,
                            struct window_model const *window_model);
static void destroy_graphics(struct graphics_state *graphics_state);
static void resize_graphics(unsigned width, unsigned height);

static double square(double x);

static errno_t gui_help(int fildes, char const *program_name,
                        struct linted_str package_name,
                        struct linted_str package_url,
                        struct linted_str package_bugreport);
static errno_t missing_option(int fildes, char const *program_name,
                              struct linted_str help_option);
static errno_t invalid_fildes(int fildes, char const *program_name,
                              struct linted_str option, errno_t errnum);
static errno_t failure(int fildes, char const *program_name,
                       struct linted_str message, errno_t errnum);
static errno_t log_str(linted_logger logger, struct linted_str start,
                       char const *str);

int main(int argc, char *argv[])
{
    if (argc < 1) {
        linted_locale_missing_process_name(STDERR_FILENO,
                                           LINTED_STR(PACKAGE_TARNAME "-gui"));
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    char const *logger_name = NULL;
    char const *controller_name = NULL;
    char const *shutdowner_name = NULL;
    char const *updater_name = NULL;
    for (unsigned ii = 1; ii < (unsigned)argc; ++ii) {
        char *argument = argv[ii];

        if (0 == strcmp(HELP_OPTION, argument)) {
            need_help = true;
        } else if (0 == strcmp(VERSION_OPTION, argument)) {
            need_version = true;
        } else if (0 == strncmp(argument, LOGGER_OPTION "=",
                                strlen(LOGGER_OPTION "="))) {
            logger_name = argument + strlen(LOGGER_OPTION "=");
        } else if (0 == strncmp(argument, CONTROLLER_OPTION "=",
                                strlen(CONTROLLER_OPTION "="))) {
            controller_name = argument + strlen(CONTROLLER_OPTION "=");
        } else if (0 == strncmp(argument, SHUTDOWNER_OPTION "=",
                                strlen(SHUTDOWNER_OPTION "="))) {
            shutdowner_name = argument + strlen(SHUTDOWNER_OPTION "=");
        } else if (0 == strncmp(argument, UPDATER_OPTION "=",
                                strlen(UPDATER_OPTION "="))) {
            updater_name = argument + strlen(UPDATER_OPTION "=");
        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        gui_help(STDOUT_FILENO,
                 program_name,
                 LINTED_STR(PACKAGE_NAME),
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

    if (NULL == logger_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(LOGGER_OPTION));
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == controller_name) {
        missing_option(STDERR_FILENO,
                       program_name, LINTED_STR(CONTROLLER_OPTION));
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == shutdowner_name) {
        missing_option(STDERR_FILENO,
                       program_name, LINTED_STR(SHUTDOWNER_OPTION));
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == updater_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(UPDATER_OPTION));
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    linted_logger logger;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(logger_name, &fd);
        if (errnum != 0) {
            invalid_fildes(STDERR_FILENO,
                           program_name, LINTED_STR(LOGGER_OPTION), errnum);
            linted_locale_try_for_more_help(STDERR_FILENO,
                                            program_name,
                                            LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        logger = fd;
    }

    linted_controller controller;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(controller_name, &fd);
        if (errnum != 0) {
            invalid_fildes(STDERR_FILENO,
                           program_name, LINTED_STR(CONTROLLER_OPTION), errnum);
            linted_locale_try_for_more_help(STDERR_FILENO,
                                            program_name,
                                            LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        controller = fd;
    }

    linted_shutdowner shutdowner;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(shutdowner_name, &fd);
        if (errnum != 0) {
            invalid_fildes(STDERR_FILENO,
                           program_name, LINTED_STR(SHUTDOWNER_OPTION), errnum);
            linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                            LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        shutdowner = fd;
    }

    linted_updater updater;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(updater_name, &fd);
        if (errnum != 0) {
            invalid_fildes(STDERR_FILENO,
                           program_name, LINTED_STR(UPDATER_OPTION), errnum);
            linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                            LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        updater = fd;
    }

    fcntl(logger, F_SETFD, fcntl(logger, F_GETFD) | FD_CLOEXEC);
    fcntl(updater, F_SETFD, fcntl(updater, F_GETFD) | FD_CLOEXEC);
    fcntl(shutdowner, F_SETFD, fcntl(shutdowner, F_GETFD) | FD_CLOEXEC);
    fcntl(controller, F_SETFD, fcntl(controller, F_GETFD) | FD_CLOEXEC);

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
    char *display_env_var = malloc(display_string_length);
    if (NULL == display_env_var) {
        failure(STDERR_FILENO,
                program_name, LINTED_STR("no DISPLAY environment variable"),
                errno);
        return EXIT_FAILURE;
    }
    memcpy(display_env_var, original_display, display_string_length);

    {
        int kept_fds[] = {
            STDERR_FILENO,
            STDIN_FILENO,
            STDOUT_FILENO,

            logger,
            controller,
            updater,
            shutdowner
        };
        errno_t errnum = linted_util_sanitize_environment(kept_fds,
                                                          LINTED_ARRAY_SIZE
                                                          (kept_fds));
        if (errnum != 0) {
            failure(STDERR_FILENO,
                    program_name,
                    LINTED_STR("cannot sanitize the program environment"),
                    errnum);
            return EXIT_FAILURE;
        }
    }

    errno_t error_status = 0;

    struct window_model window_model = {.width = 640,.height = 800,
        .viewable = true
    };

    struct controller_data controller_data = {
        .update = {.forward = false,.back = false,.right = false,.left = false},
        .update_pending = false
    };

    struct sim_model sim_model = {
        .x_rotation = 0,
        .y_rotation = 0,

        .x_position = 0,
        .y_position = 0,
        .z_position = 0
    };

    Display *display = XOpenDisplay(display_env_var);
    if (NULL == display) {
        errno = ENOSYS;
        goto shutdown;
    }

    xcb_connection_t *connection = XGetXCBConnection(display);
    unsigned screen_number = XDefaultScreen(display);

    xcb_screen_t *screen = NULL;
    {
        xcb_screen_iterator_t iter =
            xcb_setup_roots_iterator(xcb_get_setup(connection));
        for (size_t ii = 0; ii < screen_number; ++ii) {
            if (0 == iter.rem) {
                break;
            }

            xcb_screen_next(&iter);
        }

        if (0 == iter.rem) {
            error_status = EINVAL;
            goto disconnect;
        }

        screen = iter.data;
    }

    /* Query framebuffer configurations */
    XVisualInfo visual_info;
    {
        XVisualInfo *ptr = glXChooseVisual(display, screen_number,
                                           (int*)attrib_list);
        if (NULL == ptr) {
            error_status = ENOSYS;
            goto disconnect;
        }
        visual_info = *ptr;
        XFree(ptr);
    }

    xcb_colormap_t colormap = xcb_generate_id(connection);
    if ((error_status = errnum_from_connection(connection)) != 0) {
        goto disconnect;
    }

    xcb_create_colormap(connection, XCB_COLORMAP_ALLOC_NONE,
                        colormap, screen->root, visual_info.visualid);
    if ((error_status = errnum_from_connection(connection)) != 0) {
        goto disconnect;
    }

    xcb_window_t window = xcb_generate_id(connection);
    if ((error_status = errnum_from_connection(connection)) != 0) {
        goto disconnect;
    }

    {
        uint32_t values[] = {
            XCB_EVENT_MASK_STRUCTURE_NOTIFY
                | XCB_EVENT_MASK_ENTER_WINDOW | XCB_EVENT_MASK_LEAVE_WINDOW
                | XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_KEY_RELEASE
                | XCB_EVENT_MASK_POINTER_MOTION,
            colormap,
            0
        };
        xcb_create_window(connection,
                          visual_info.depth,
                          window,
                          screen->root,
                          0, 0,
                          window_model.width, window_model.height,
                          0,
                          XCB_WINDOW_CLASS_INPUT_OUTPUT,
                          visual_info.visualid,
                          XCB_CW_EVENT_MASK | XCB_CW_COLORMAP, values);
    }
    if ((error_status = errnum_from_connection(connection)) != 0) {
        goto disconnect;
    }

    xcb_atom_t wm_delete_window;
    {
        xcb_intern_atom_cookie_t cookie = xcb_intern_atom(connection, 1, 12,
                                                          "WM_PROTOCOLS");
        if ((error_status = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }

        xcb_intern_atom_reply_t *reply =
            xcb_intern_atom_reply(connection, cookie, 0);
        if ((error_status = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }
        xcb_atom_t wm_protocols = reply->atom;
        free(reply);

        xcb_intern_atom_cookie_t cookie2 = xcb_intern_atom(connection, 0, 16,
                                                           "WM_DELETE_WINDOW");
        if ((error_status = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }

        xcb_intern_atom_reply_t *reply2 =
            xcb_intern_atom_reply(connection, cookie2, 0);
        if ((error_status = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }
        wm_delete_window = reply2->atom;
        free(reply2);

        xcb_change_property(connection, XCB_PROP_MODE_REPLACE, window,
                            wm_protocols, 4, 32, 1, &wm_delete_window);
        if ((error_status = errnum_from_connection(connection)) != 0) {
            goto destroy_window;
        }
    }

    xcb_map_window(connection, window);
    if ((error_status = errnum_from_connection(connection)) != 0) {
        goto destroy_window;
    }

    xcb_flush(connection);
    if ((error_status = errnum_from_connection(connection)) != 0) {
        goto destroy_window;
    }

    GLXContext glx_context = glXCreateContext(display, &visual_info,
                                              NULL, GL_TRUE);
    if (NULL == glx_context) {
        error_status = ENOSYS;
        goto destroy_window;
    }

    if (!glXMakeContextCurrent(display, window, window, glx_context)) {
        error_status = ENOSYS;
        goto destroy_glx_context;
    }

    struct graphics_state graphics_state;

    if ((error_status = init_graphics(logger, &graphics_state,
                                      &window_model)) != 0) {
        goto destroy_glx_context;
    }

    /* Do the initial resize */
    resize_graphics(window_model.width, window_model.height);
    for (;;) {
        /* Handle GUI events first before rendering */
        /* We have to use the Xlib event queue because of broken Mesa
         * libraries which abuse it.
         */
        bool had_gui_event = XPending(display) > 0;
        if (had_gui_event) {
            XEvent event;
            XNextEvent(display, &event);
            switch (event.type) {
                {
            case ConfigureNotify:;
                    XConfigureEvent *configure_event = &event.xconfigure;
                    window_model.width = configure_event->width;
                    window_model.height = configure_event->height;
                    resize_graphics(window_model.width, window_model.height);
                    break;
                }

                {
            case MotionNotify:;
                    XMotionEvent *motion_event = &event.xmotion;
                    on_tilt(motion_event->x, motion_event->y,
                            &window_model, &controller_data);
                    break;
                }

            case UnmapNotify:
                window_model.viewable = false;
                break;

            case MapNotify:
                window_model.viewable = true;
                break;

                {
            case EnterNotify:
                    window_model.focused = true;

                    int x, y;
                    if ((error_status = get_mouse_position(connection,
                                                           window,
                                                           &x, &y)) != 0) {
                        goto cleanup_gl;
                    }

                    on_tilt(x, y, &window_model, &controller_data);
                    break;
                }

            case LeaveNotify:
                window_model.focused = false;

                controller_data.update.x_tilt = 0;
                controller_data.update.y_tilt = 0;

                controller_data.update_pending = true;
                break;

                {
            case MappingNotify:;
                    XMappingEvent *mapping_event = &event.xmapping;
                    XRefreshKeyboardMapping(mapping_event);
                }

                {
                    bool is_key_down;

            case KeyPress:
                    is_key_down = true;
                    goto on_key_event;

            case KeyRelease:
                    is_key_down = false;
                    goto on_key_event;

 on_key_event:     ;
                    XKeyEvent *key_event = &event.xkey;
                    switch (XLookupKeysym(key_event, 0)) {
                    default:
                        goto no_key_event;

                    case XK_space:
                        controller_data.update.jumping = is_key_down;
                        break;

                    case XK_Control_L:
                        controller_data.update.left = is_key_down;
                        break;

                    case XK_Alt_L:
                        controller_data.update.right = is_key_down;
                        break;

                    case XK_z:
                        controller_data.update.forward = is_key_down;
                        break;

                    case XK_Shift_L:
                        controller_data.update.back = is_key_down;
                        break;
                    }

                    controller_data.update_pending = true;

 no_key_event:
                    break;
                }

            case ClientMessage:;
                goto cleanup_gl;

            default:
                /* Unknown event type, ignore it */
                break;
            }
        }

        enum {
            UPDATER,
            CONTROLLER
        };

        struct pollfd fds[] = {
            [UPDATER] = {.fd = updater,.events = POLLIN},
            [CONTROLLER] = {
                            .fd =
                            controller_data.update_pending ? controller : -1,
                            .events = POLLOUT},
        };

        errno_t poll_status;
        int fds_active;

        do {
            fds_active = poll(fds, LINTED_ARRAY_SIZE(fds), 0);
            poll_status = -1 == fds_active ? errno : 0;
        } while (EINTR == poll_status);
        if (poll_status != 0) {
            error_status = poll_status;
            goto cleanup_gl;
        }

        bool had_selected_event = fds_active > 0;

        if ((fds[UPDATER].revents & POLLIN) != 0) {
            errno_t errnum = on_updater_readable(updater, &sim_model);
            if (errnum != 0) {
                error_status = errnum;
                goto cleanup_gl;
            }
        }

        if (controller_data.update_pending
            && (fds[CONTROLLER].revents & POLLOUT) != 0) {
            errno_t errnum = on_controller_writeable(controller,
                                                     &controller_data);
            if (errnum != 0) {
                error_status = errnum;
                goto cleanup_gl;
            }
        }

        /* Only render if we have time to waste */
        if (!had_gui_event && !had_selected_event) {
            if (window_model.viewable) {
                render_graphics(&graphics_state, &sim_model, &window_model);
                glXSwapBuffers(display, window);
            } else {
                /*
                 * This is an ugly hack and waiting the X11 file
                 * descriptor should be implemented eventually.
                 */
                struct timespec request = {
                    .tv_sec = 0,
                    .tv_nsec = 10
                };
                errno_t errnum;
                do {
                    int status = nanosleep(&request, &request);
                    errnum = -1 == status ? errno : 0;
                } while (EINTR == errnum);
                if (errnum != 0) {
                    error_status = errnum;
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
        errno_t errnum = errnum_from_connection(connection);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

 disconnect:
    xcb_flush(connection);
    {
        errno_t errnum = errnum_from_connection(connection);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

    XCloseDisplay(display);

 shutdown:
    {
        errno_t shutdown_status;
        do {
            shutdown_status = linted_shutdowner_send_shutdown(shutdowner);
        } while (EINTR == shutdown_status);
        if (0 == error_status) {
            error_status = shutdown_status;
        }
    }

    return error_status;
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

static errno_t on_updater_readable(linted_updater updater,
                                   struct sim_model *sim_model)
{
    struct linted_updater_update update;

    errno_t read_status;
    do {
        read_status = linted_updater_receive_update(updater, &update);
    } while (EINTR == read_status);

    if (EAGAIN == read_status) {
        return 0;
    }

    if (read_status != 0) {
        return read_status;
    }

    float pi = acosf(-1.0f);

    sim_model->x_rotation = update.x_rotation * (2 * pi / UINT32_MAX);
    sim_model->y_rotation = update.y_rotation * (2 * pi / UINT32_MAX);

    sim_model->x_position = update.x_position * (1 / (double)2048);
    sim_model->y_position = update.y_position * (1 / (double)2048);
    sim_model->z_position = update.z_position * (1 / (double)2048);

    return 0;
}

static int on_controller_writeable(linted_controller controller,
                                   struct controller_data *controller_data)
{
    errno_t send_status;
    do {
        send_status = linted_controller_send(controller,
                                             &controller_data->update);
    } while (EINTR == send_status);

    if (EAGAIN == send_status) {
        return 0;
    }

    if (send_status != 0) {
        return send_status;
    }

    controller_data->update_pending = false;

    return 0;
}

static int const attrib_list[] = {
    GLX_RGBA, True,

    GLX_RED_SIZE, 5,
    GLX_GREEN_SIZE, 5,
    GLX_BLUE_SIZE, 3,

    GLX_DOUBLEBUFFER, True,
    GLX_DEPTH_SIZE, 16,
    None
};

static errno_t init_graphics(linted_logger logger,
                             struct graphics_state *graphics_state,
                             struct window_model const *window_model)
{
    errno_t error_status = 0;

    if (linted_gl_ogl_LOAD_FAILED == linted_gl_ogl_LoadFunctions()) {
        return ENOSYS;
    }

    glDisable(GL_DITHER);

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

    glVertexPointer(LINTED_ARRAY_SIZE(linted_assets_triangle_vertices[0]),
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
            error_status = get_gl_error();
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
            error_status = EINVAL;

            GLint info_log_length;
            glGetShaderiv(fragment_shader,
                          GL_INFO_LOG_LENGTH, &info_log_length);

            GLchar *info_log = malloc(info_log_length);
            if (info_log != NULL) {
                glGetShaderInfoLog(fragment_shader, info_log_length, NULL,
                                   info_log);
                log_str(logger, LINTED_STR("Invalid shader: "), info_log);
                free(info_log);
            }
            goto cleanup_program;
        }
    }

    {
        flush_gl_errors();
        GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
        if (0 == vertex_shader) {
            error_status = get_gl_error();
            goto cleanup_program;
        }
        glAttachShader(program, vertex_shader);
        glDeleteShader(vertex_shader);

        glShaderSource(vertex_shader, 1, &linted_assets_vertex_shader, NULL);
        glCompileShader(vertex_shader);

        GLint is_valid;
        glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &is_valid);
        if (!is_valid) {
            error_status = EINVAL;

            GLint info_log_length;
            glGetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH, &info_log_length);

            GLchar *info_log = malloc(info_log_length);
            if (info_log != NULL) {
                glGetShaderInfoLog(vertex_shader, info_log_length, NULL,
                                   info_log);
                log_str(logger, LINTED_STR("Invalid shader: "), info_log);
                free(info_log);
            }
            goto cleanup_program;
        }
    }
    glLinkProgram(program);

    glValidateProgram(program);

    GLint is_valid;
    glGetProgramiv(program, GL_VALIDATE_STATUS, &is_valid);
    if (!is_valid) {
        error_status = EINVAL;

        GLint info_log_length;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_log_length);

        GLchar *info_log = malloc(info_log_length);
        if (info_log != NULL) {
            glGetProgramInfoLog(program, info_log_length, NULL, info_log);
            log_str(logger, LINTED_STR("Invalid program: "), info_log);
            free(info_log);
        }
        goto cleanup_program;
    }
    glUseProgram(program);

    graphics_state->program = program;

    return 0;

 cleanup_program:;
    glDeleteProgram(program);

    return error_status;
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
        GLfloat aspect = width / (GLfloat) height;
        double fov = acosf(-1.0f) / 4;

        double d = 1 / tan(fov / 2);
        double far = 1000;
        double near = 1;

        GLfloat projection[][4] = {
            {d / aspect, 0, 0, 0},
            {0, d, 0, 0},
            {0, 0, (far + near) / (near - far), 2 * far * near / (near - far)},
            {0, 0, -1, 0}
        };
        glLoadMatrixf(projection[0]);
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
        GLfloat const rotation[][4] = {
            {1, 0, 0, 0},
            {0, cos_y, -sin_y, 0},
            {0, sin_y, cos_y, 0},
            {0, 0, 0, 1}
        };
        glMultMatrixf(rotation[0]);
    }

    {
        GLfloat cos_x = cosf(sim_model->x_rotation);
        GLfloat sin_x = sinf(sim_model->x_rotation);
        GLfloat const rotation[][4] = {
            {cos_x, 0, sin_x, 0},
            {0, 1, 0, 0},
            {-sin_x, 0, cos_x, 0},
            {0, 0, 0, 1}
        };
        glMultMatrixf(rotation[0]);
    }

    /* Move the camera */
    {
        GLfloat const camera[][4] = {
            {1, 0, 0, 0},
            {0, 1, 0, 0},
            {0, 0, 1, 0},
            {sim_model->x_position, sim_model->y_position,
             sim_model->z_position, 1}
        };
        glMultMatrixf(camera[0]);
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

static errno_t get_gl_error(void)
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

static double square(double x)
{
    return x * x;
}

static errno_t gui_help(int fildes, char const *program_name,
                        struct linted_str package_name,
                        struct linted_str package_url,
                        struct linted_str package_bugreport)
{
    errno_t errnum;

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" [OPTIONS]\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Run the gui program.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --logger            the logger file descriptor\n\
  --controller        the controller file descriptor\n\
  --updater           the updater file descriptor\n\
  --shutdowner        the shutdowner file descriptor\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t invalid_fildes(int fildes, char const *program_name,
                              struct linted_str option, errno_t error_display)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, option)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" argument: "))) != 0) {
        return errnum;
    }

    char const *error_string = linted_error_string_alloc(error_display);
    errnum = linted_io_write_string(fildes, NULL, error_string);
    linted_error_string_free(error_string);

    if (errnum != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t missing_option(int fildes, char const *program_name,
                              struct linted_str option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(": missing "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, option)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" option\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t failure(int fildes, char const *program_name,
                       struct linted_str message, errno_t error)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, message)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(": "))) != 0) {
        return errnum;
    }

    char const *error_string = linted_error_string_alloc(error);
    errnum = linted_io_write_string(fildes, NULL, error_string);
    linted_error_string_free(error_string);

    if (errnum != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t log_str(linted_logger logger, struct linted_str start,
                       char const *error)
{
    size_t error_size = strlen(error);

    char *full_string = malloc(error_size + start.size);
    if (NULL == full_string) {
        /* Silently drop log */
        return errno;
    }

    memcpy(full_string, start.bytes, start.size);
    memcpy(full_string + start.size, error, error_size);

    errno_t errnum = linted_logger_log(logger,
                                       full_string, start.size + error_size);

    free(full_string);

    return errnum;
}
