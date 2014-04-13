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
#include "SDL.h"
#undef HAVE_MALLOC
#undef HAVE_REALLOC

#include "config.h"

#include "assets.h"
#include "gl_core.h"

#include "linted/controller.h"
#include "linted/io.h"
#include "linted/mq.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

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
    {SDL_GL_ACCUM_ALPHA_SIZE, 0}
};

static Uint8 const default_events[] = {
    SDL_ACTIVEEVENT,
    SDL_KEYDOWN, SDL_KEYUP,
    SDL_MOUSEMOTION,
    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP,
    SDL_JOYAXISMOTION, SDL_JOYBALLMOTION,
    SDL_JOYHATMOTION,
    SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP,
    SDL_QUIT,
    SDL_SYSWMEVENT,
    SDL_VIDEORESIZE,
    SDL_VIDEOEXPOSE,
    SDL_USEREVENT
};

static Uint8 const enabled_events[] = {
    SDL_VIDEORESIZE,

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
};

struct controller_state {
    struct linted_controller_message update;
    bool update_pending;
};

struct gui_state {
    float x;
    float y;
};

struct gl_state {
    GLuint program;
};

static errno_t on_sdl_event(SDL_Event const *sdl_event,
                            struct window_state *window_state,
                            struct controller_state *controller_state,
                            enum transition *transition);
static errno_t on_updater_readable(linted_updater updater,
                               struct gui_state *gui_state);
static errno_t on_controller_writeable(linted_controller controller,
                                      struct controller_state *controller_state);

static errno_t init_graphics(struct gl_state *gl_state,
                             struct window_state const *window_state);
static void render_graphics(struct gl_state const *gl_state,
                            struct gui_state const *gui_state,
                            struct window_state const *window_state);
static void destroy_gl(struct gl_state *gl_state);

static double square(double x);
static int max_fd(int x, int y);

static errno_t missing_option(int fildes, char const * program_name,
                              struct linted_str help_option);
static errno_t try_for_more_help(int fildes, char const * program_name,
                                 struct linted_str help_option);
static errno_t invalid_fildes(int fildes, char const * program_name,
                              struct linted_str option, errno_t errnum);

int main(int argc, char *argv[])
{
    if (argc < 1) {
        linted_io_write_str(STDERR_FILENO, NULL,
                            LINTED_STR(PACKAGE_TARNAME "-gui"));
        linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\
: missing process name\n"));
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    char const *controller_name = NULL;
    char const *shutdowner_name = NULL;
    char const *updater_name = NULL;
    for (unsigned ii = 1; ii < (unsigned)argc; ++ii) {
        char *argument = argv[ii];

        if (0 == strcmp(HELP_OPTION, argument)) {
            need_help = true;
        } else if (0 == strcmp(VERSION_OPTION, argument)) {
            need_version = true;
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
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("Usage: "));
        linted_io_write_string(STDOUT_FILENO, NULL, program_name);
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(" [OPTIONS]\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
Run the " PACKAGE_NAME " program gui.\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --controller        the controller message queue file descriptor\n\
  --updater           the updater message queue file descriptor\n\
  --shutdowner        the shutdowner message queue file descriptor\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
Report bugs to <"));
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_BUGREPORT));
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(">\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_NAME));
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
 home page: <"));
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_URL));
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(">\n"));

        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_io_write_string(STDERR_FILENO, NULL, program_name);
        linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\
: unrecognised option '"));
        linted_io_write_string(STDERR_FILENO, NULL, bad_option);
        linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("'\n"));

        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_STRING));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n\n"));

        linted_io_write_format(STDOUT_FILENO, NULL, "\
Copyright (C) %d Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n", COPYRIGHT_YEAR);

        return EXIT_SUCCESS;
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
            try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
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
            try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
            return EXIT_FAILURE;
        }
        updater = fd;
    }

    fcntl(updater, F_SETFD, fcntl(updater, F_GETFD) | FD_CLOEXEC);
    fcntl(shutdowner, F_SETFD, fcntl(shutdowner, F_GETFD) | FD_CLOEXEC);
    fcntl(controller, F_SETFD, fcntl(controller, F_GETFD) | FD_CLOEXEC);

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: no DISPLAY environment variable\n",
                               program_name);
        try_for_more_help(STDERR_FILENO, program_name, LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    size_t display_string_length = strlen(original_display) + 1;
    char *display = malloc(display_string_length);
    if (NULL == display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: can't allocate DISPLAY string\n",
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }
    memcpy(display, original_display, display_string_length);

    {
        fd_set essential_fds;
        FD_ZERO(&essential_fds);

        FD_SET(STDERR_FILENO, &essential_fds);
        FD_SET(fileno(stdin), &essential_fds);
        FD_SET(fileno(stdout), &essential_fds);

        FD_SET(controller, &essential_fds);
        FD_SET(updater, &essential_fds);
        FD_SET(shutdowner, &essential_fds);

        int errnum = linted_util_sanitize_environment(&essential_fds);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s", program_name,
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
    }

    if (-1 == setenv("DISPLAY", display, true)) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not set the environment: %s", program_name, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    free(display);

    errno_t error_status = 0;
    Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    struct window_state window_state = {.width = 640,.height = 800};

    struct controller_state controller_state = {
        .update = {.up = false,.down = false,.right = false,.left = false},
        .update_pending = false
    };

    struct gui_state gui_state = {.x = 0,.y = 0};

    if (-1 == SDL_Init(SDL_INIT_EVENTTHREAD
                       | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE)) {
        error_status = errno;
        syslog(LOG_ERR, "could not initialize the GUI: %s", SDL_GetError());
        goto shutdown;
    }

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(default_events); ++ii) {
        SDL_EventState(default_events[ii], SDL_IGNORE);
    }

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(enabled_events); ++ii) {
        SDL_EventState(enabled_events[ii], SDL_ENABLE);
    }

    SDL_WM_SetCaption(PACKAGE_NAME, NULL);

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(attribute_values); ++ii) {
        struct attribute_value_pair const pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            error_status = errno;
            syslog(LOG_ERR, "could not set a double buffer attribute: %s",
                   SDL_GetError());
            goto cleanup_SDL;
        }
    }

    /* Initialize SDL */
    if (NULL == SDL_SetVideoMode(window_state.width, window_state.height, 0,
                                 sdl_flags)) {
        error_status = errno;
        syslog(LOG_ERR, "could not set the video mode: %s", SDL_GetError());
        goto cleanup_SDL;
    }

    /* Get actual window size, and not requested window size */
    {
        SDL_VideoInfo const *video_info = SDL_GetVideoInfo();
        window_state.width = video_info->current_w;
        window_state.height = video_info->current_h;
    }

    for (;;) {
        struct gl_state gl_state;

        bool time_to_quit = false;
        bool should_resize = false;

        if (NULL == SDL_SetVideoMode(window_state.width, window_state.height,
                                     0, sdl_flags)) {
            error_status = errno;
            syslog(LOG_ERR, "could not set the video mode: %s", SDL_GetError());
            goto cleanup_SDL;
        }

        {
            errno_t errnum = init_graphics(&gl_state, &window_state);
            if (errnum != 0) {
                error_status = errnum;
                goto cleanup_SDL;
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
                        time_to_quit = true;
                        goto cleanup_gl;

                    case SHOULD_RESIZE:
                        should_resize = true;
                        break;
                    }
                }
            }

            fd_set watched_read_fds;
            fd_set watched_write_fds;
            int greatest = -1;

            FD_ZERO(&watched_read_fds);
            FD_SET(updater, &watched_read_fds);

            greatest = max_fd(greatest, updater);

            FD_ZERO(&watched_write_fds);

            if (controller_state.update_pending) {
                FD_SET(controller, &watched_write_fds);
                greatest = max_fd(greatest, controller);
            }

            fd_set active_read_fds;
            fd_set active_write_fds;
            errno_t select_status;
            int fds_active;

            do {
                active_read_fds = watched_read_fds;
                active_write_fds = watched_write_fds;

                struct timeval timeval = {.tv_sec = 0,.tv_usec = 0 };
                fds_active = select(greatest + 1, &active_read_fds,
                                    &active_write_fds, NULL, &timeval);
                select_status = -1 == fds_active ? errno : 0;
            } while (EINTR == select_status);
            if (select_status != 0) {
                error_status = select_status;
                goto cleanup_gl;
            }

            bool had_selected_event = fds_active > 0;

            if (FD_ISSET(updater, &active_read_fds)) {
                errno_t errnum = on_updater_readable(updater, &gui_state);
                if (errnum != 0) {
                    error_status = errnum;
                    goto cleanup_gl;
                }
            }

            if (controller_state.update_pending
                && FD_ISSET(controller, &active_write_fds)) {

                errno_t errnum = on_controller_writeable(controller,
                                                         &controller_state);
                if (errnum != 0) {
                    error_status = errnum;
                    goto cleanup_gl;
                }
            }

            /* Only render if we have time to waste */
            if (!had_sdl_event && !had_selected_event) {
                /* Only resize if we have time to waste */
                if (should_resize) {
                    break;
                }

                render_graphics(&gl_state, &gui_state, &window_state);
            }
        }

     cleanup_gl:
        destroy_gl(&gl_state);

        if (error_status != 0 || time_to_quit) {
            break;
        }
    }

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

    case SDL_VIDEORESIZE:
        /*
         * Fuse multiple resize attempts into just one to prevent the
         * worse case scenario of a whole bunch of resize events from
         * killing the application's speed.
         */
        window_state->width = sdl_event->resize.w;
        window_state->height = sdl_event->resize.h;
        *transition = SHOULD_RESIZE;
        return 0;

    case SDL_KEYDOWN:
    case SDL_KEYUP:
        goto on_keypress;
    }

 on_keypress:;
    bool is_key_down = SDL_KEYDOWN == sdl_event->type;

    switch (sdl_event->key.keysym.sym) {
    default:
        *transition = DO_NOTHING;
        return 0;

    case SDLK_q:
    case SDLK_ESCAPE:
        *transition = is_key_down ? DO_NOTHING : SHOULD_EXIT;
        return 0;

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

    gui_state->x = ((float)update.x_position) / 255;
    gui_state->y = ((float)update.y_position) / 255;

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

static errno_t init_graphics(struct gl_state *gl_state,
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

    GLuint program = glCreateProgram();
    if (0 == program) {
        return errno;
    }

    {
        GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
        if (0 == fragment_shader) {
            error_status = errno;
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
            error_status = errno;

            GLint info_log_length;
            glGetShaderiv(fragment_shader,
                          GL_INFO_LOG_LENGTH, &info_log_length);

            GLchar *info_log = malloc(info_log_length);
            if (NULL == info_log) {
                syslog(LOG_ERR, "Invalid shader: no memory to log info log");
            } else {
                glGetShaderInfoLog(fragment_shader, info_log_length, NULL,
                                   info_log);
                syslog(LOG_ERR, "Invalid shader: %s", info_log);
                free(info_log);
            }
            goto cleanup_program;
        }
    }

    {
        GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
        if (0 == vertex_shader) {
            error_status = errno;
            goto cleanup_program;
        }
        glAttachShader(program, vertex_shader);
        glDeleteShader(vertex_shader);

        glShaderSource(vertex_shader, 1, &linted_assets_vertex_shader, NULL);
        glCompileShader(vertex_shader);

        GLint is_valid;
        glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &is_valid);
        if (!is_valid) {
            error_status = errno;

            GLint info_log_length;
            glGetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH, &info_log_length);

            GLchar *info_log = malloc(info_log_length);
            if (NULL == info_log) {
                syslog(LOG_ERR, "Invalid shader: no memory to log info log");
            } else {
                glGetShaderInfoLog(vertex_shader, info_log_length, NULL,
                                   info_log);
                syslog(LOG_ERR, "Invalid shader: %s", info_log);
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
        error_status = errno;

        GLint info_log_length;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_log_length);

        GLchar *info_log = malloc(info_log_length);
        if (NULL == info_log) {
            syslog(LOG_ERR, "Invalid program: no memory to log info log");
        } else {
            glGetProgramInfoLog(program, info_log_length, NULL, info_log);
            syslog(LOG_ERR, "Invalid program: %s", info_log);
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

    /* Move the camera */
    {
        GLfloat const camera[][4] = {
            {1, 0, 0, 0},
            {0, 1, 0, 0},
            {0, 0, 1, 0},
            {gui_state->x, gui_state->y, 3, 1}
        };
        glMultMatrixf(camera[0]);
    }

    glDrawElements(GL_TRIANGLES, 3 * linted_assets_triangle_indices_size,
                   GL_UNSIGNED_BYTE, linted_assets_triangle_indices);

    for (;;) {
        GLenum error = glGetError();
        if (GL_NO_ERROR == error) {
            break;
        }
        syslog(LOG_ERR, "GL Error: %s", glGetString(error));
    }

    SDL_GL_SwapBuffers();
}

static double square(double x)
{
    return x * x;
}

static int max_fd(int x, int y)
{
    return x > y ? x : y;
}

static errno_t try_for_more_help(int fildes, char const * program_name,
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

static errno_t invalid_fildes(int fildes, char const * program_name,
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

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(" argument: "))) != 0) {
        return errnum;
    }

    char const * error_string = linted_error_string_alloc(error_display);
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

static errno_t missing_option(int fildes, char const * program_name,
                              struct linted_str option)
{
    errno_t errnum;

    if ((errnum = linted_io_write_string(STDERR_FILENO, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR(": missing "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(STDERR_FILENO, NULL, option)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR(" option\n"))) != 0) {
        return errnum;
    }

    return 0;
}
