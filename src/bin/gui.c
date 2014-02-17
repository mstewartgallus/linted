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

static void on_key_movement(linted_controller controller,
                            enum linted_controller_direction direction,
                            bool moving);

int linted_gui_run(linted_updater updater, linted_shutdowner shutdowner,
                   linted_controller controller)
{
    int exit_status = -1;
    Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    unsigned width = 640, height = 800;
    SDL_VideoInfo const * video_info;

    if (-1 == SDL_Init(SDL_INIT_EVENTTHREAD
                       | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE))
    {
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
    if (NULL == SDL_SetVideoMode(width, height, 0, sdl_flags)) {
        int errnum = errno;
        syslog(LOG_ERR, "could not set the video mode: %s",
               SDL_GetError());
        errno = errnum;
        goto cleanup_SDL;
    }

    /* Get actual window size, and not requested window size */
    video_info = SDL_GetVideoInfo();
    width = video_info->current_w;
    height = video_info->current_h;

    {
        float x = 0;
        float y = 0;

    setup_window:;
        SDL_Surface *const video_surface = SDL_SetVideoMode(width, height,
                                                            0, sdl_flags);
        if (NULL == video_surface) {
            int errnum = errno;
            syslog(LOG_ERR, "could not set the video mode: %s",
                   SDL_GetError());
            errno = errnum;
            goto cleanup_SDL;
        }

        glDisable(GL_DITHER);

        glClearColor(1.0f, 0.2f, 0.3f, 0.0f);
        glViewport(0, 0, width, height);

        bool should_resize = false;
        for (;;) {
            /* Handle SDL events first before rendering */
            SDL_Event sdl_event;
            bool const had_sdl_event = SDL_PollEvent(&sdl_event);
            switch (sdl_event.type) {
            case SDL_VIDEORESIZE:
                /*
                  Fuse multiple resize attempts into just one to prevent
                  the worse case scenario of a whole bunch of resize
                  events from killing the application's speed.
                */
                width = sdl_event.resize.w;
                height = sdl_event.resize.h;
                should_resize = true;
                break;

            case SDL_KEYDOWN:
                switch (sdl_event.key.keysym.sym) {
                case SDLK_q:
                case SDLK_ESCAPE:
                    goto exit_main_loop;

                case SDLK_LEFT:
                    on_key_movement(controller, LINTED_CONTROLLER_LEFT, true);
                    break;

                case SDLK_RIGHT:
                    on_key_movement(controller, LINTED_CONTROLLER_RIGHT, true);
                    break;

                case SDLK_UP:
                    on_key_movement(controller, LINTED_CONTROLLER_UP, true);
                    break;

                case SDLK_DOWN:
                    on_key_movement(controller, LINTED_CONTROLLER_DOWN, true);
                    break;

                default:
                    break;
                }
                break;

            case SDL_KEYUP:
                switch (sdl_event.key.keysym.sym) {
                case SDLK_LEFT:
                    on_key_movement(controller, LINTED_CONTROLLER_LEFT, false);
                    break;

                case SDLK_RIGHT:
                    on_key_movement(controller, LINTED_CONTROLLER_RIGHT, false);
                    break;

                case SDLK_UP:
                    on_key_movement(controller, LINTED_CONTROLLER_UP, false);
                    break;

                case SDLK_DOWN:
                    on_key_movement(controller, LINTED_CONTROLLER_DOWN, false);
                    break;

                default:
                    break;
                }
                break;

            case SDL_QUIT:
                goto exit_main_loop;
            }

            bool had_gui_command = false;
            {
                struct linted_updater_update update;

                int read_status;
                do {
                    read_status = linted_updater_receive_update(updater, &update);
                } while (-1 == read_status && EINTR == errno);
                if (-1 == read_status) {
                    if (errno != EAGAIN) {
                        goto cleanup_SDL;
                    }
                } else {
                    x = ((float)update.x_position) / 255;
                    y = ((float)update.y_position) / 255;

                    had_gui_command = true;
                }
            }

            /* Only render if we have time to waste */
            if (!had_sdl_event && !had_gui_command) {
                if (should_resize) {
                    goto setup_window;
                }

                glClear(GL_COLOR_BUFFER_BIT);

                glBegin(GL_TRIANGLES);
                glVertex2f(x - 0.4f, y - 0.4f);
                glVertex2f(x + 0.4f, y - 0.4f);
                glVertex2f(x + 0.0f, y + 0.4f);
                glEnd();

                SDL_GL_SwapBuffers();
            }
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

static void on_key_movement(linted_controller controller,
                            enum linted_controller_direction direction,
                            bool moving)
{
    int request_status;
    do {
        request_status =
            linted_controller_send_movement(controller, direction, moving);
    } while (-1 == request_status && EINTR == errno);
    if (-1 == request_status) {
        exit(errno);
    }
}
