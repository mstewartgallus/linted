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
#include "linted/simulator_loop.h"
#include "linted/util.h"

#include "SDL.h"
#include "SDL_opengl.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

enum message_type {
    GUI_SHUTDOWN,
    GUI_UPDATE
};

struct message_data {
    enum message_type type;
    int32_t x_position;
    int32_t y_position;
};

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

static void on_key_movement(linted_controller_t controller,
                            enum linted_controller_direction direction,
                            bool moving);

int linted_gui_pair(linted_gui_t gui[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    return linted_mq_pair(gui, &attr, 0, 0);
}

int linted_gui_send_shutdown(linted_gui_t const gui)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = GUI_SHUTDOWN;

    return mq_send(gui, (char const *)&message_data, sizeof message_data, 0);
}

int linted_gui_send_update(linted_gui_t const gui, int32_t const x, int32_t const y)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = GUI_UPDATE;
    message_data.x_position = x;
    message_data.y_position = y;

    return mq_send(gui, (char const *)&message_data, sizeof message_data, 0);
}

int linted_gui_close(linted_gui_t const gui)
{
    return mq_close(gui);
}

int linted_gui_run(linted_gui_t gui, linted_controller_t controller,
                   linted_main_loop_t main_loop)
{
    if (-1 == SDL_Init(SDL_INIT_EVENTTHREAD | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE)) {
        LINTED_ERROR("Could not initialize the GUI: %s", SDL_GetError());
    }

    SDL_WM_SetCaption(PACKAGE_NAME, NULL);

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(attribute_values); ++ii) {
        struct attribute_value_pair const pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            LINTED_ERROR("Could not set a double buffer attribute: %s", SDL_GetError());
        }
    }

    Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    unsigned width = 640, height = 800;

    /* Initialize SDL */
    if (NULL == SDL_SetVideoMode(width, height, 0, sdl_flags)) {
        LINTED_ERROR("Could not set the video mode: %s", SDL_GetError());
    }

    /* Get actual window size, and not requested window size */
    SDL_VideoInfo const *const video_info = SDL_GetVideoInfo();
    width = video_info->current_w;
    height = video_info->current_h;

    float x = 0;
    float y = 0;

 setup_window:;
    SDL_Surface *const video_surface = SDL_SetVideoMode(width, height,
                                                        0, sdl_flags);
    if (NULL == video_surface) {
        LINTED_ERROR("Could not set the video mode: %s", SDL_GetError());
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
            case SDLK_ESCAPE:{
                    int request_status;
                    do {
                        request_status = linted_main_loop_send_close_request(main_loop);
                    } while (-1 == request_status && EINTR == errno);
                    if (-1 == request_status) {
                        LINTED_ERROR("Could not send main loop request to close: %s",
                                     linted_error_string_alloc(errno));
                    }
                    break;
                }

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

            default: break;
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

            default: break;
            }
            break;

        case SDL_QUIT:{
                int request_status;
                do {
                    request_status = linted_main_loop_send_close_request(main_loop);
                } while (-1 == request_status && EINTR == errno);
                if (-1 == request_status) {
                    LINTED_ERROR("Could not send main loop request to close: %s",
                                 linted_error_string_alloc(errno));
                }
                break;
            }
        }

        bool had_gui_command = false;
        {
            struct message_data message_data;
            struct timespec timespec = {
                .tv_sec = 0,
                .tv_nsec = 0
            };

            ssize_t bytes_read;
            do {
                bytes_read = mq_timedreceive(gui,
                                             (char *)&message_data,
                                             sizeof message_data, NULL, &timespec);
            } while (-1 == bytes_read && EINTR == errno);
            if (-1 == bytes_read) {
                if (errno != ETIMEDOUT) {
                    LINTED_ERROR("Could not read from gui connection: %s",
                                 linted_error_string_alloc(errno));
                }
            } else {
                switch (message_data.type) {
                case GUI_SHUTDOWN:
                    goto exit_main_loop;

                case GUI_UPDATE:{
                        x = ((float)message_data.x_position) / 255;
                        y = ((float)message_data.y_position) / 255;
                        break;
                    }

                default:
                    LINTED_ERROR("Received unexpected request type: %i",
                                 message_data.type);
                }

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

 exit_main_loop:
    SDL_Quit();

    return 0;
}

static void on_key_movement(linted_controller_t controller,
                            enum linted_controller_direction direction,
                            bool moving)
{
    int request_status;
    do {
        request_status = linted_controller_send_movement(controller, direction,
                                                        moving);
    } while (-1 == request_status && EINTR == errno);
    if (-1 == request_status) {
        if (errno != EAGAIN) {
            LINTED_ERROR("Could not send movement command to controller: %s",
                         linted_error_string_alloc(errno));
        }
    }
}
