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

#include "linted/assets.h"
#include "linted/gui.h"
#include "linted/mq.h"
#include "linted/util.h"

#include "SDL.h"
#include "SDL_opengl.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

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

enum transition {
    SHOULD_EXIT,
    SHOULD_RESIZE,
    DO_NOTHING
};

struct window_state {
    unsigned width;
    unsigned height;
};

struct gui_state {
    float x;
    float y;
};

static int on_sdl_event(SDL_Event const *sdl_event, struct window_state *window_state,
                        linted_controller controller,
                        enum transition *transition);

static void init_graphics(struct window_state const * window_state);
static void render_graphics(struct gui_state const * gui_state);

int linted_gui_run(linted_updater updater, linted_shutdowner shutdowner,
                   linted_controller controller)
{
    int exit_status = -1;
    Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    struct window_state window_state = {
        .width = 640,
        .height = 800
    };

    if (-1 == SDL_Init(SDL_INIT_EVENTTHREAD
                       | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE)) {
        int errnum = errno;
        syslog(LOG_ERR, "could not initialize the GUI: %s", SDL_GetError());
        errno = errnum;
        goto shutdown;
    }

    SDL_WM_SetCaption(PACKAGE_NAME, NULL);

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(attribute_values); ++ii) {
        struct attribute_value_pair const pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            int errnum = errno;
            syslog(LOG_ERR, "could not set a double buffer attribute: %s",
                   SDL_GetError());
            errno = errnum;
            goto cleanup_SDL;
        }
    }

    /* Initialize SDL */
    if (NULL == SDL_SetVideoMode(window_state.width, window_state.height,
                                 0, sdl_flags)) {
        int errnum = errno;
        syslog(LOG_ERR, "could not set the video mode: %s", SDL_GetError());
        errno = errnum;
        goto cleanup_SDL;
    }

    /* Get actual window size, and not requested window size */
    SDL_VideoInfo const *video_info = SDL_GetVideoInfo();
    window_state.width = video_info->current_w;
    window_state.height = video_info->current_h;

    struct gui_state gui_state = {
        .x = 0,
        .y = 0
    };

 setup_window:;
    SDL_Surface *const video_surface = SDL_SetVideoMode(window_state.width,
                                                        window_state.height,
                                                        0, sdl_flags);
    if (NULL == video_surface) {
        int errnum = errno;
        syslog(LOG_ERR, "could not set the video mode: %s", SDL_GetError());
        errno = errnum;
        goto cleanup_SDL;
    }

    init_graphics(&window_state);

    bool should_resize = false;
    for (;;) {
        /* Handle SDL events first before rendering */
        SDL_Event sdl_event;
        bool const had_sdl_event = SDL_PollEvent(&sdl_event);
        if (had_sdl_event) {
            enum transition transition;
            if (-1 == on_sdl_event(&sdl_event, &window_state, controller,
                                   &transition)) {
                goto cleanup_SDL;
            }

            switch (transition) {
            case DO_NOTHING:
                break;

            case SHOULD_EXIT:
                goto exit_main_loop;

            case SHOULD_RESIZE:
                should_resize = true;
                break;
            }
        }

        bool had_gui_command = false;

        {
            struct linted_updater_update update;

            int read_status;
            do {
                read_status = linted_updater_receive_update(updater, &update);
            } while (-1 == read_status && EINTR == errno);
            if (-1 == read_status) {
                if (EAGAIN == errno) {
                    goto skip_update;
                }

                goto cleanup_SDL;
            }

            gui_state.x = ((float)update.x_position) / 255;
            gui_state.y = ((float)update.y_position) / 255;

            had_gui_command = true;

         skip_update:;
        }

        /* Only render if we have time to waste */
        if (!had_sdl_event && !had_gui_command) {
            if (should_resize) {
                goto setup_window;
            }

            render_graphics(&gui_state);
        }
    }

 exit_main_loop:
    exit_status = 0;

 cleanup_SDL:
    {
        int errnum = errno;
        SDL_Quit();
        errno = errnum;
    }

 shutdown:
    {
        int errnum = errno;
        int shutdown_status;
        do {
            shutdown_status = linted_shutdowner_send_shutdown(shutdowner);
        } while (-1 == shutdown_status && EINTR == errno);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == shutdown_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}

static int on_sdl_event(SDL_Event const *sdl_event, struct window_state *window_state,
                        linted_controller controller,
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
        goto on_key_left;
    case SDLK_RIGHT:
        goto on_key_right;
    case SDLK_UP:
        goto on_key_up;
    case SDLK_DOWN:
        goto on_key_down;
    }

    enum linted_controller_direction direction;

 on_key_left:
    direction = LINTED_CONTROLLER_LEFT;
    goto update_controller;

 on_key_right:
    direction = LINTED_CONTROLLER_RIGHT;
    goto update_controller;

 on_key_up:
    direction = LINTED_CONTROLLER_UP;
    goto update_controller;

 on_key_down:
    direction = LINTED_CONTROLLER_DOWN;
    goto update_controller;

 update_controller:;
    int send_status;
    do {
        send_status = linted_controller_send_movement(controller, direction,
                                                      is_key_down);
    } while (-1 == send_status && EINTR == errno);
    if (-1 == send_status) {
        return -1;
    }

    *transition = DO_NOTHING;
    return 0;
}

static void init_graphics(struct window_state const * window_state)
{
    glDisable(GL_DITHER);

    glEnable(GL_VERTEX_ARRAY);

    glClearColor(1.0f, 0.2f, 0.3f, 0.0f);
    glViewport(0, 0, window_state->width, window_state->height);

    glVertexPointer(2, GL_FLOAT, 0, triangle_data);

    glMatrixMode(GL_MODELVIEW);
}

static void render_graphics(struct gui_state const * gui_state)
{
    glClear(GL_COLOR_BUFFER_BIT);

    GLfloat modelview_matrix[] = {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        gui_state->x, gui_state->y, 0, 1
    };
    /*  X, Y, Z, W coords of the resultant vector are the
     *  sums of the columns (row major order).
     */
    glLoadMatrixf(modelview_matrix);

    glDrawArrays(GL_TRIANGLES, 0, triangle_data_size);

    for (;;) {
        GLenum error = glGetError();
        if (GL_NO_ERROR == error) {
            break;
        }
        syslog(LOG_ERR, "GL Error: %s", glGetString(error));
    }

    SDL_GL_SwapBuffers();
}
