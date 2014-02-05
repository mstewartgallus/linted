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
#include "linted/io.h"
#include "linted/simulator.h"
#include "linted/util.h"

#include "SDL.h"
#include "SDL_opengl.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

enum request_type {
    GUI_UPDATE
};

struct message_data {
    enum request_type type;
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

static int gui_run(linted_task_spawner_t const spawner, int inbox);

int linted_gui_spawn(linted_gui_t * const gui, linted_task_spawner_t const spawner)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof (struct message_data);

    int const gui_mq = linted_io_anonymous_mq(&attr, 0);
    if (-1 == gui_mq) {
        return -1;
    }

    if (-1 == linted_task_spawn(&gui->_task, spawner, gui_run, gui_mq)) {
        goto error_and_close_mqueue;
    }

    gui->_server = gui_mq;

    return 0;

 error_and_close_mqueue:
    close(gui_mq);

    return -1;
}

int linted_gui_send_update(linted_gui_t const gui, uint8_t const x, uint8_t const y)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = GUI_UPDATE;
    message_data.x_position = x;
    message_data.y_position = y;

    int send_status;
    do {
        send_status = mq_send(gui._server,
                              (char const *) &message_data, sizeof message_data,
                              0);
    } while (-1 == send_status && EINTR == EINTR);
    if (-1 == send_status) {
        return -1;
    }

    return 0;
}

int linted_gui_close(linted_gui_t const gui)
{
    return mq_close(gui._server);
}

static int gui_run(linted_task_spawner_t const spawner, int const inbox)
{
    if (-1 == linted_task_spawner_close(spawner)) {
        LINTED_ERROR("Could not close spawner: %m", errno);
    }

    if (-1 == SDL_Init(SDL_INIT_EVENTTHREAD | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE)) {
        LINTED_ERROR("Could not initialize the GUI: %s\n", SDL_GetError());
    }

    SDL_WM_SetCaption(PACKAGE_NAME, NULL);

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(attribute_values); ++ii) {
        struct attribute_value_pair const pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            LINTED_ERROR("Could not set a double buffer attribute: %s\n", SDL_GetError());
        }
    }

    Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    unsigned width = 640, height = 800;

    /* Initialize SDL */
    if (NULL == SDL_SetVideoMode(width, height, 0, sdl_flags)) {
        LINTED_ERROR("Could not set the video mode: %s\n", SDL_GetError());
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
        LINTED_ERROR("Could not set the video mode: %s\n", SDL_GetError());
    }

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
                /* TODO: Something, linted_simulator_send_close_request(simulator_chan); */
                break;
            default:
                break;
            }
            break;

        case SDL_QUIT:
            /* TODO: something, linted_simulator_send_close_request(simulator_chan); */
            break;
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
                                             (char *) &message_data,
                                             sizeof message_data,
                                             NULL,
                                             &timespec);
            } while (-1 == bytes_read && EINTR == errno);
            if (-1 == bytes_read) {
                if (errno != ETIMEDOUT) {
                    LINTED_ERROR("Could not read from gui connection: %m", errno);
                }
            } else {
                if (0 == bytes_read) {
                    break;
                }

                switch (message_data.type) {
                case GUI_UPDATE:{
                    x = ((float)message_data.x_position) / 255;
                    y = ((float)message_data.y_position) / 255;
                    break;
                }

                default:
                    LINTED_ERROR
                        ("Received unexpected request type: %d.\n", message_data.type);
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

    SDL_Quit();

    if (-1 == close(inbox)) {
        LINTED_ERROR("Could not close simulator inbox: %m", errno);
    }

    return EXIT_SUCCESS;
}
