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
#include "linted/simulator.h"
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
    uint8_t x_position;
    uint8_t y_position;
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

static int gui_run(linted_spawner_t const spawner, int const inboxes[]);

linted_gui_t linted_gui_spawn(linted_spawner_t const spawner,
                              linted_main_loop_t main_loop)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    mqd_t gui_mqs[2];
    if (-1 == linted_mq_pair(gui_mqs, &attr, 0)) {
        return -1;
    }

    if (-1 == linted_spawner_spawn(spawner, gui_run, (int[]) {
                                   gui_mqs[0], main_loop, -1})) {
        goto error_and_close_mqueues;
    }

    mq_close(gui_mqs[0]);

    return gui_mqs[1];

 error_and_close_mqueues:
    mq_close(gui_mqs[0]);

    mq_close(gui_mqs[1]);

    return -1;
}

int linted_gui_send_shutdown(linted_gui_t const gui)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = GUI_SHUTDOWN;

    return mq_send(gui, (char const *)&message_data, sizeof message_data, 0);
}

int linted_gui_send_update(linted_gui_t const gui, uint8_t const x, uint8_t const y)
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

static int gui_run(linted_spawner_t const spawner, int const inboxes[])
{
    if (-1 == linted_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %s", linted_error_string_alloc(errno));
    }

    int const inbox = inboxes[0];
    linted_main_loop_t const main_loop = inboxes[1];

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
                        request_status = linted_main_loop_request_close(main_loop);
                    } while (-1 == request_status && EINTR == errno);
                    if (-1 == request_status) {
                        LINTED_ERROR("Could not send main loop request to close: %s",
                                     linted_error_string_alloc(errno));
                    }
                    break;
                }
            default:
                break;
            }
            break;

        case SDL_QUIT:{
                int request_status;
                do {
                    request_status = linted_main_loop_request_close(main_loop);
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
                bytes_read = mq_timedreceive(inbox,
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
                    LINTED_ERROR("Received unexpected request type: %d",
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

    if (-1 == mq_close(inbox)) {
        LINTED_ERROR("Could not close simulator inbox: %s",
                     linted_error_string_alloc(errno));
    }

    if (-1 == linted_main_loop_close(main_loop)) {
        LINTED_ERROR("Could not close main loop handle: %s",
                     linted_error_string_alloc(errno));
    }

    return EXIT_SUCCESS;
}
