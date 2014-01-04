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

#include "linted/actor.h"
#include "linted/gui.h"
#include "linted/simulator.h"
#include "linted/util.h"

#include "SDL.h"
#include "SDL_opengl.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { linted_actor_port x; } linted_gui_port;
static linted_gui_port linted_gui_port_from_fildes(int fildes);
static linted_gui_command linted_gui_recv(linted_gui_port gui);

struct attribute_value_pair {
    SDL_GLattr attribute;
    int value;
};

enum { ATTRIBUTE_AMOUNT = 12 };

static struct attribute_value_pair const attribute_values[ATTRIBUTE_AMOUNT];

int linted_gui_run(int const gui_fifo, int const simulator_fifo) {
    linted_simulator_t const simulator_chan = linted_simulator_from_fildes(simulator_fifo);
    linted_gui_port const gui_port = linted_gui_port_from_fildes(gui_fifo);

    int const sdl_init_status = SDL_Init(SDL_INIT_EVENTTHREAD | SDL_INIT_VIDEO
                                         | SDL_INIT_NOPARACHUTE);
    if (-1 == sdl_init_status) {
        LINTED_ERROR("Could not initialize the GUI: %s\n", SDL_GetError());
    }

    SDL_WM_SetCaption(PACKAGE_NAME, NULL);

    for (size_t ii = 0; ii < ATTRIBUTE_AMOUNT; ++ii) {
        struct attribute_value_pair const pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            LINTED_ERROR("Could not set a double buffer attribute: %s\n",
                         SDL_GetError());
        }
    }

    Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    unsigned width = 640, height = 800;

    /* Initialize SDL */
    {
        SDL_Surface * const video_surface = SDL_SetVideoMode(width, height, 0,
                                                             sdl_flags);
        if (NULL == video_surface) {
            LINTED_ERROR("Could not set the video mode: %s\n", SDL_GetError());
        }
    }
    /* Get actual window size, and not requested window size */
    SDL_VideoInfo const * const video_info = SDL_GetVideoInfo();
    width = video_info->current_w;
    height = video_info->current_h;

    float x = 0;
    float y = 0;

 setup_window:;
    SDL_Surface * const video_surface = SDL_SetVideoMode(width, height,
                                                         0, sdl_flags);
    if (NULL == video_surface) {
        LINTED_ERROR("Could not set the video mode: %s\n", SDL_GetError());
    }

    glClearColor(1.0f, 0.2f, 0.3f, 0.0f);
    glViewport(0, 0, width, height);

    bool should_resize = false;
    Uint32 next_tick = SDL_GetTicks();
    for (;;) {
        {
            Uint32 const now = SDL_GetTicks();
            if (now >= next_tick) {
                next_tick += 1000 / 60;
                linted_simulator_send_tick_request(simulator_chan);
            }
        }

        /* Handle events first before rendering */
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
                linted_simulator_send_close_request(simulator_chan);
                break;
            default: break;
            }
            break;

        case SDL_QUIT:
            linted_simulator_send_close_request(simulator_chan);
            break;
        }

        struct pollfd fifo_fd = { .fd = gui_fifo, .events = POLLIN };
        int poll_status;
        bool had_gui_command;
        {
            int error_code;
            do {
                poll_status = poll(&fifo_fd, 1, 0);
            } while (-1 == poll_status
                     && (error_code = errno, error_code != EINTR));
            if (-1 == poll_status) {
                LINTED_ERROR("Error polling file descriptors %s\n",
                             strerror(errno));
            }
            had_gui_command = poll_status > 0;
        }
        if (had_gui_command) {
            linted_gui_command const command = linted_gui_recv(gui_port);
            switch (command.type) {
            case LINTED_GUI_COMMAND_TICK_CHANGE:
                x = ((float) command.tick_change.x) / 255;
                y = ((float) command.tick_change.y) / 255;
                break;

            case LINTED_GUI_COMMAND_SHUTDOWN:
                goto shutdown_gui;

            default:
                LINTED_ERROR("Received unknown command: %d\n", command.type);
                break;
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

 shutdown_gui:
    linted_simulator_send_gui_closed(simulator_chan);

    SDL_Quit();

    {
        int error_status;
        int error_code;
        do {
            error_status = close(simulator_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close simulator fifo %s\n",
                         strerror(error_code));
        }
    }

    {
        int error_status;
        int error_code;
        do {
            error_status = close(gui_fifo);
        } while (-1 == error_status && (error_code = errno, error_code != EINTR));
        if (-1 == error_status) {
            LINTED_ERROR("Could not close gui fifo %s\n",
                         strerror(error_code));
        }
    }

    return EXIT_SUCCESS;
}

static linted_gui_port linted_gui_port_from_fildes(int fildes) {
    return (linted_gui_port) { .x = (linted_actor_port) { .x = fildes } };
}

static linted_gui_command linted_gui_recv(linted_gui_port gui) {
    uint8_t message[LINTED_GUI_MESSAGE_SIZE];
    linted_actor_recv(gui.x, message, LINTED_GUI_MESSAGE_SIZE);
    return (linted_gui_command) {
        .type = message[0],
        .tick_change = (linted_gui_tick_change) {
            .x = message[1],
            .y = message[2]
        }
    };
}

static struct attribute_value_pair const attribute_values[ATTRIBUTE_AMOUNT] = {
    { SDL_GL_RED_SIZE, 5 },
    { SDL_GL_GREEN_SIZE, 5 },
    { SDL_GL_BLUE_SIZE, 5 },

    { SDL_GL_DOUBLEBUFFER, 1 },

    { SDL_GL_BUFFER_SIZE, 16 },
    { SDL_GL_DEPTH_SIZE, 16 },

    /* The following are unused */
    { SDL_GL_ALPHA_SIZE, 0 },
    { SDL_GL_STENCIL_SIZE, 0 },

    { SDL_GL_ACCUM_RED_SIZE, 0 },
    { SDL_GL_ACCUM_GREEN_SIZE, 0 },
    { SDL_GL_ACCUM_BLUE_SIZE, 0 },
    { SDL_GL_ACCUM_ALPHA_SIZE, 0 }
};
