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
#include "linted/logger.h"
#include "linted/io.h"
#include "linted/mq.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include "SDL.h"

#include <errno.h>
#include <poll.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define LOGGER_OPTION "--logger"
#define CONTROLLER_OPTION "--controller"
#define SHUTDOWNER_OPTION "--shutdowner"
#define UPDATER_OPTION "--updater"

struct attribute_value_pair {
    SDL_GLattr attribute;
    int value;
};

static struct attribute_value_pair const attribute_values[] = {
    {SDL_GL_RED_SIZE, 5},
    {SDL_GL_GREEN_SIZE, 5},
    {SDL_GL_BLUE_SIZE, 5},

    {SDL_GL_DOUBLEBUFFER, 1},

    {SDL_GL_BUFFER_SIZE, 16},
    {SDL_GL_DEPTH_SIZE, 16},

    /* The following are unused */
    {SDL_GL_ALPHA_SIZE, 0},
    {SDL_GL_STENCIL_SIZE, 0},

    {SDL_GL_ACCUM_RED_SIZE, 0},
    {SDL_GL_ACCUM_GREEN_SIZE, 0},
    {SDL_GL_ACCUM_BLUE_SIZE, 0},
    {SDL_GL_ACCUM_ALPHA_SIZE, 0},

    {SDL_GL_STEREO, 0},

    {SDL_GL_MULTISAMPLEBUFFERS, 0},
    {SDL_GL_MULTISAMPLESAMPLES, 0}
};

static SDL_EventType const default_events[] = {
    SDL_KEYDOWN, SDL_KEYUP,
    SDL_MOUSEMOTION,
    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP,
    SDL_JOYAXISMOTION, SDL_JOYBALLMOTION,
    SDL_JOYHATMOTION,
    SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP,
    SDL_QUIT,
    SDL_SYSWMEVENT,
    SDL_WINDOWEVENT,
    SDL_USEREVENT
};

static SDL_EventType const enabled_events[] = {
    SDL_WINDOWEVENT,

    SDL_MOUSEMOTION,

    SDL_KEYDOWN,
    SDL_KEYUP,

    SDL_QUIT
};

enum transition {
    SHOULD_EXIT,
    SHOULD_RESIZE,
    DO_NOTHING
};

struct window_state {
    unsigned width;
    unsigned height;

    bool viewable:1;
    bool focused:1;
};

struct controller_state {
    struct linted_controller_message update;
    bool update_pending;
};

struct gui_state {
    float x_rotation;
    float y_rotation;

    float x_position;
    float y_position;
    float z_position;
};

struct gl_state {
    GLuint program;
};

static void flush_gl_errors(void);

/* TODO: This usage of glGetError is incorrect. Multiple error flags
 * be set and returned by a single function.
 */
static errno_t get_gl_error(void);

static errno_t on_sdl_event(SDL_Event const *sdl_event,
                            struct window_state *window_state,
                            struct controller_state *controller_state,
                            enum transition *transition);
static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_state const *window_state,
                    struct controller_state *controller_state);

static errno_t on_updater_readable(linted_updater updater,
                                   struct gui_state *gui_state);
static errno_t on_controller_writeable(linted_controller controller, struct controller_state
                                       *controller_state);

static errno_t init_graphics(linted_logger logger,
                             struct gl_state *gl_state,
                             struct window_state const *window_state);
static void render_graphics(struct gl_state const *gl_state,
                            struct gui_state const *gui_state,
                            struct window_state const *window_state);
static void destroy_gl(struct gl_state *gl_state);

static double square(double x);

static errno_t missing_process_name(int fildes, struct linted_str package_name);
static errno_t gui_help(int fildes, char const *program_name,
                        struct linted_str package_name,
                        struct linted_str package_url,
                        struct linted_str package_bugreport);
static errno_t on_bad_option(int fildes, char const *program_name,
                             char const *bad_option);
static errno_t try_for_more_help(int fildes, char const *program_name,
                                 struct linted_str help_option);
static errno_t version_text(int fildes, struct linted_str package_string,
                            struct linted_str copyright_year);
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
        missing_process_name(STDERR_FILENO, LINTED_STR(PACKAGE_TARNAME "-gui"));
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
        on_bad_option(STDERR_FILENO, program_name, bad_option);
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        version_text(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                     LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    if (NULL == logger_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(LOGGER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == controller_name) {
        missing_option(STDERR_FILENO,
                       program_name, LINTED_STR(CONTROLLER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == shutdowner_name) {
        missing_option(STDERR_FILENO,
                       program_name, LINTED_STR(SHUTDOWNER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (NULL == updater_name) {
        missing_option(STDERR_FILENO, program_name, LINTED_STR(UPDATER_OPTION));
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    linted_logger logger;
    {
        int fd;
        errno_t errnum = linted_io_strtofd(logger_name, &fd);
        if (errnum != 0) {
            invalid_fildes(STDERR_FILENO,
                           program_name, LINTED_STR(LOGGER_OPTION), errnum);
            try_for_more_help(STDERR_FILENO,
                              program_name, LINTED_STR(HELP_OPTION));
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
            try_for_more_help(STDERR_FILENO,
                              program_name, LINTED_STR(HELP_OPTION));
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
            try_for_more_help(STDERR_FILENO, program_name,
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
            try_for_more_help(STDERR_FILENO, program_name,
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
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    size_t display_string_length = strlen(original_display) + 1;
    char *display = malloc(display_string_length);
    if (NULL == display) {
        failure(STDERR_FILENO,
                program_name, LINTED_STR("no DISPLAY environment variable"),
                errno);
        return EXIT_FAILURE;
    }
    memcpy(display, original_display, display_string_length);

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
                                                          LINTED_ARRAY_SIZE(kept_fds));
        if (errnum != 0) {
            failure(STDERR_FILENO,
                    program_name,
                    LINTED_STR("cannot sanitize the program environment"),
                    errnum);
            return EXIT_FAILURE;
        }
    }

    if (-1 == setenv("DISPLAY", display, true)) {
        failure(STDERR_FILENO,
                program_name,
                LINTED_STR("cannot set the environment variable `DISPLAY'"),
                errno);
        return EXIT_FAILURE;
    }

    free(display);

    errno_t error_status = 0;
    struct window_state window_state = {.width = 640,.height = 800,
        .viewable = true
    };

    struct controller_state controller_state = {
        .update = {.up = false,.down = false,.right = false,.left = false},
        .update_pending = false
    };

    struct gui_state gui_state = {
        .x_rotation = 0,
        .y_rotation = 0,

        .x_position = 0,
        .y_position = 0,
        .z_position = 0
    };
    SDL_Window *window;
    SDL_GLContext gl_context;
    struct gl_state gl_state;

    if (-1 == SDL_Init(SDL_INIT_EVENTS | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE)) {
        error_status = ENOSYS;
        log_str(logger, LINTED_STR("cannot initialize the GUI: "),
                SDL_GetError());
        goto shutdown;
    }

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(default_events); ++ii) {
        SDL_EventState(default_events[ii], SDL_IGNORE);
    }

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(enabled_events); ++ii) {
        SDL_EventState(enabled_events[ii], SDL_ENABLE);
    }

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(attribute_values); ++ii) {
        struct attribute_value_pair const pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            error_status = ENOSYS;
            log_str(logger, LINTED_STR("cannot set an SDL OpenGL attribute: "),
                    SDL_GetError());
            goto cleanup_SDL;
        }
    }

    window = SDL_CreateWindow(PACKAGE_NAME,
                              SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                              window_state.width, window_state.height,
                              SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
    if (NULL == window) {
        error_status = ENOSYS;
        log_str(logger, LINTED_STR("cannot create a window: "), SDL_GetError());
        goto cleanup_SDL;
    }

    gl_context = SDL_GL_CreateContext(window);
    if (NULL == gl_context) {
        error_status = ENOSYS;
        log_str(logger, LINTED_STR("cannot create an OpenGL context: "),
                SDL_GetError());
        goto destroy_window;
    }

    /* Get actual window size, and not the requested window size */
    {
        int width;
        int height;
        SDL_GetWindowSize(window, &width, &height);
        window_state.width = width;
        window_state.height = height;
    }

    {
        errno_t errnum = init_graphics(logger, &gl_state, &window_state);
        if (errnum != 0) {
            error_status = errnum;
            goto delete_context;
        }
    }

    for (;;) {
        bool had_sdl_event;

        /* Handle SDL events first before rendering */
        {
            SDL_Event sdl_event;
            had_sdl_event = SDL_PollEvent(&sdl_event);
            if (had_sdl_event) {
                enum transition transition;
                errno_t errnum = on_sdl_event(&sdl_event, &window_state,
                                              &controller_state,
                                              &transition);
                if (errnum != 0) {
                    error_status = errnum;
                    goto cleanup_gl;
                }

                switch (transition) {
                case DO_NOTHING:
                    break;

                case SHOULD_EXIT:
                    goto cleanup_gl;

                case SHOULD_RESIZE:
                    glViewport(0, 0, window_state.width, window_state.height);
                    break;
                }
            }
        }

        enum {
            UPDATER,

            CONTROLLER
        };

        size_t fds_size;
        struct pollfd *fds;

        struct pollfd fds_with_controller[] = {
            [UPDATER] = {.fd = updater,.events = POLLIN},

            [CONTROLLER] = {.fd = controller,.events = POLLOUT},
        };

        struct pollfd fds_without_controller[] = {
            [UPDATER] = {.fd = updater,.events = POLLIN},
        };

        if (controller_state.update_pending) {
            fds = fds_with_controller;
            fds_size = LINTED_ARRAY_SIZE(fds_with_controller);
        } else {
            fds = fds_without_controller;
            fds_size = LINTED_ARRAY_SIZE(fds_without_controller);
        }

        errno_t poll_status;
        int fds_active;

        do {
            fds_active = poll(fds, fds_size, 0);
            poll_status = -1 == fds_active ? errno : 0;
        } while (EINTR == poll_status);
        if (poll_status != 0) {
            error_status = poll_status;
            goto cleanup_gl;
        }

        bool had_selected_event = fds_active > 0;

        if ((fds[UPDATER].revents & POLLIN) != 0) {
            errno_t errnum = on_updater_readable(updater, &gui_state);
            if (errnum != 0) {
                error_status = errnum;
                goto cleanup_gl;
            }
        }

        if (controller_state.update_pending
            && (fds[CONTROLLER].revents & POLLOUT) != 0) {
            errno_t errnum = on_controller_writeable(controller,
                                                     &controller_state);
            if (errnum != 0) {
                error_status = errnum;
                goto cleanup_gl;
            }
        }

        /* Only render if we have time to waste */
        if (!had_sdl_event && !had_selected_event) {
            if (window_state.viewable) {
                render_graphics(&gl_state, &gui_state, &window_state);
                SDL_GL_SwapWindow(window);
            } else {
                /*
                 * This is an ugly hack but SDL cannot give us a
                 * better solution. Even SDL event filters and a pipe
                 * still require us to pump events with
                 * SDL_PumpEvents.
                 */
                SDL_Delay(10);
            }
        }
    }

 cleanup_gl:
    destroy_gl(&gl_state);

 delete_context:
    SDL_GL_DeleteContext(gl_context);

 destroy_window:
    SDL_DestroyWindow(window);

 cleanup_SDL:
    SDL_Quit();

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

static errno_t on_sdl_event(SDL_Event const *sdl_event,
                            struct window_state *window_state,
                            struct controller_state *controller_state,
                            enum transition *transition)
{
    switch (sdl_event->type) {
    default:
        *transition = DO_NOTHING;
        return 0;

    case SDL_QUIT:
        *transition = SHOULD_EXIT;
        return 0;

        {
        case SDL_WINDOWEVENT:;
            SDL_WindowEvent const *const window_event = &sdl_event->window;
            switch (window_event->event) {
            case SDL_WINDOWEVENT_RESIZED:
                /*
                 * Fuse multiple resize attempts into just one to prevent the
                 * worse case scenario of a whole bunch of resize events from
                 * killing the application's speed.
                 */
                window_state->width = window_event->data1;
                window_state->height = window_event->data2;
                *transition = SHOULD_RESIZE;
                return 0;

                {
                case SDL_WINDOWEVENT_ENTER:
                    window_state->focused = true;

                    int x, y;
                    SDL_GetMouseState(&x, &y);

                    on_tilt(x, y,
                            window_state, controller_state);

                    *transition = DO_NOTHING;
                    return 0;
                }

            case SDL_WINDOWEVENT_LEAVE:
                window_state->focused = false;

                controller_state->update.x_tilt = 0;
                controller_state->update.y_tilt = 0;

                controller_state->update_pending = true;

                *transition = DO_NOTHING;
                return 0;

            case SDL_WINDOWEVENT_HIDDEN:
                window_state->viewable = false;
                *transition = DO_NOTHING;
                return 0;

            case SDL_WINDOWEVENT_SHOWN:
            case SDL_WINDOWEVENT_EXPOSED:
                window_state->viewable = true;
                *transition = DO_NOTHING;
                return 0;

            default:
                *transition = DO_NOTHING;
                return 0;
            }
        }

        {
        case SDL_MOUSEMOTION:;
            SDL_MouseMotionEvent const *const motion_event = &sdl_event->motion;

            on_tilt(motion_event->x, motion_event->y,
                    window_state, controller_state);

            *transition = DO_NOTHING;
            return 0;
        }

    case SDL_KEYDOWN:
    case SDL_KEYUP:{
            bool is_key_down = SDL_KEYDOWN == sdl_event->type;

            switch (sdl_event->key.keysym.sym) {
            default:
                *transition = DO_NOTHING;
                return 0;

            case SDLK_q:
            case SDLK_ESCAPE:
                *transition = is_key_down ? DO_NOTHING : SHOULD_EXIT;
                return 0;

            case SDLK_SPACE:
                controller_state->update.jumping = is_key_down;
                break;

            case SDLK_LEFT:
                controller_state->update.left = is_key_down;
                break;

            case SDLK_RIGHT:
                controller_state->update.right = is_key_down;
                break;

            case SDLK_UP:
                controller_state->update.up = is_key_down;
                break;

            case SDLK_DOWN:
                controller_state->update.down = is_key_down;
                break;
            }

            controller_state->update_pending = true;

            *transition = DO_NOTHING;
            return 0;
        }
    }
}

static void on_tilt(int_fast32_t mouse_x, int_fast32_t mouse_y,
                    struct window_state const *window_state,
                    struct controller_state *controller_state)
{
    int32_t x = (2 * mouse_x - (int)window_state->width)/2;
    int32_t y = (2 * mouse_y - (int)window_state->height)/2;

    /* Normalize and scale up to UINT32_MAX sized screen */
    x *= INT32_MAX / window_state->width;
    y *= INT32_MAX / window_state->height;

    controller_state->update.x_tilt = x;
    controller_state->update.y_tilt = y;

    controller_state->update_pending = true;
}

static errno_t on_updater_readable(linted_updater updater,
                                   struct gui_state *gui_state)
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

    gui_state->x_rotation = update.x_rotation * (2 * M_PI / UINT32_MAX);
    gui_state->y_rotation = update.y_rotation * (2 * M_PI / UINT32_MAX);

    gui_state->x_position = update.x_position * (1 / (double)255);
    gui_state->y_position = update.y_position * (1 / (double)255);
    gui_state->z_position = update.z_position * (1 / (double)255);

    return 0;
}

static int on_controller_writeable(linted_controller controller,
                                   struct controller_state *controller_state)
{
    errno_t send_status;
    do {
        send_status = linted_controller_send(controller,
                                             &controller_state->update);
    } while (EINTR == send_status);

    if (EAGAIN == send_status) {
        return 0;
    }

    if (send_status != 0) {
        return send_status;
    }

    controller_state->update_pending = false;

    return 0;
}

static errno_t init_graphics(linted_logger logger,
                             struct gl_state *gl_state,
                             struct window_state const *window_state)
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

    glCullFace(GL_FRONT);

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
    glViewport(0, 0, window_state->width, window_state->height);

    glVertexPointer(LINTED_ARRAY_SIZE(linted_assets_triangle_vertices[0]),
                    GL_FLOAT, 0, linted_assets_triangle_vertices);

    glNormalPointer(GL_FLOAT, 0, linted_assets_triangle_normals);

    glMatrixMode(GL_PROJECTION);

    {
        GLfloat projection[][4] = {
            {1, 0, 0, 0},
            {0, 1, 0, 0},
            {0, 0, 1, 1},
            {0, 0, 0, 1}
        };
        glLoadMatrixf(projection[0]);
    }

    glMatrixMode(GL_MODELVIEW);

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

    gl_state->program = program;

    return 0;

 cleanup_program:;
    glDeleteProgram(program);

    return error_status;
}

static void destroy_gl(struct gl_state *gl_state)
{
    glUseProgram(0);
    glDeleteProgram(gl_state->program);
    memset(gl_state, 0, sizeof *gl_state);
}

static void render_graphics(struct gl_state const *gl_state,
                            struct gui_state const *gui_state,
                            struct window_state const *window_state)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    /*  X, Y, Z, W coords of the resultant vector are the
     *  sums of the columns (row major order).
     */
    glLoadIdentity();

    /* Correct the aspect ratio */
    /* TODO: Move this into the projection matrix */
    GLfloat aspect = ((GLfloat) window_state->width) / window_state->height;
    {
        GLfloat const matrix[][4] = {
            {1, 0, 0, 0},
            {0, aspect, 0, 0},
            {0, 0, 1, 0},
            {0, 0, 0, 1}
        };
        glMultMatrixf(matrix[0]);
    }

    /* Rotate the camera */
    {
        GLfloat cos_y = cosf(gui_state->y_rotation);
        GLfloat sin_y = sinf(gui_state->y_rotation);
        GLfloat const rotation[][4] = {
            {1, 0,     0,      0},
            {0, cos_y, -sin_y, 0},
            {0, sin_y, cos_y,  0},
            {0, 0,     0,      1}
        };
        glMultMatrixf(rotation[0]);
    }

    {
        GLfloat cos_x = cosf(gui_state->x_rotation);
        GLfloat sin_x = sinf(gui_state->x_rotation);
        GLfloat const rotation[][4] = {
            {cos_x,  0, sin_x, 0},
            {0,      1, 0,     0},
            {-sin_x, 0, cos_x, 0},
            {0,      0, 0,     1}
        };
        glMultMatrixf(rotation[0]);
    }

    /* Move the camera */
    {
        GLfloat const camera[][4] = {
            {1, 0, 0, 0},
            {0, 1, 0, 0},
            {0, 0, 1, 0},
            {gui_state->x_position, gui_state->y_position, gui_state->z_position, 1}
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

static errno_t missing_process_name(int fildes, struct linted_str package_name)
{
    errno_t errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
: missing process name\n"))) != 0) {
        return errnum;
    }

    return 0;
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

static errno_t try_for_more_help(int fildes, char const *program_name,
                                 struct linted_str help_option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("Try `"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(" "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, help_option)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
' for more information.\n"))) != 0) {
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

static errno_t on_bad_option(int fildes, char const *program_name,
                             char const *bad_option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
: unrecognised option '"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_string(fildes, NULL, bad_option)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("'\n"))) != 0) {
        return errnum;
    }

    return 0;
}

static errno_t version_text(int fildes, struct linted_str package_string,
                            struct linted_str copyright_year)
{
    errno_t errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, package_string)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Copyright (C) "))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, copyright_year)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n"))) != 0) {
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
