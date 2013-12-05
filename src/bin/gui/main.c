/*
 * Copyright 2013 Steven Stewart-Gallus
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

#include "linted/base/stdio.h"

#include <SDL.h>
#include <SDL_opengl.h>

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " " LINTED_GUI_NAME " GUI_PIPE SIMULATOR_PIPE\n"\
    "Run the gui\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

typedef struct { linted_actor_port x; } linted_gui_port;
static linted_gui_port linted_gui_port_from_file(FILE * file);
static linted_gui_command linted_gui_recv(linted_gui_port gui);

static linted_gui_port linted_gui_port_from_file(FILE * file) {
    return (linted_gui_port) { .x = (linted_actor_port) { .x = file } };
}

static linted_gui_command linted_gui_recv(linted_gui_port gui) {
    const linted_actor_byte_fast type = linted_actor_recv_byte(gui.x);
    const linted_actor_byte_fast data = linted_actor_recv_byte(gui.x);
    return (linted_gui_command) { .type = type, .data = data };
}

typedef struct {
    SDL_GLattr attribute;
    int value;
}  attribute_value_pair;

enum { ATTRIBUTE_AMOUNT = 12 };

static const attribute_value_pair attribute_values[ATTRIBUTE_AMOUNT];


int linted_gui_main(int argc, char * argv[]) {
    if (argc != 4) {
        linted_fprintf(stderr,
                       PACKAGE_TARNAME
                       " "
                       LINTED_GUI_NAME
                       " did not understand the input\n");
        linted_fputs(USAGE_TEXT, stderr);
        linted_fflush(stderr);
        return EXIT_FAILURE;
    }

    const char * const gui_string = argv[2];
    const char * const simulator_string = argv[3];

    FILE * const simulator_fifo = linted_fdopen(atoi(simulator_string), "wb");

    const int gui_fd = atoi(gui_string);
    FILE * const gui_fifo = linted_fdopen(gui_fd, "rb");

    const linted_simulator_chan simulator_chan = linted_simulator_chan_from_file(simulator_fifo);
    const linted_gui_port gui_port = linted_gui_port_from_file(gui_fifo);

    if (-1 == SDL_Init(SDL_INIT_EVENTTHREAD | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE)) {
        LINTED_ERROR("Could not initialize the GUI: %s\n", SDL_GetError());
    }

    SDL_WM_SetCaption(PACKAGE_NAME, NULL);


    for (size_t ii = 0; ii < ATTRIBUTE_AMOUNT; ++ii) {
        const attribute_value_pair pair = attribute_values[ii];
        if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
            LINTED_ERROR("Could not set a double buffer attribute: %s\n",
                         SDL_GetError());
        }
    }

    const Uint32 sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
    unsigned width = 640, height = 800;

    /* Initialize SDL */
    SDL_SetVideoMode(width, height, 0, sdl_flags);

    /* Get actual window size, and not requested window size */
    const SDL_VideoInfo * const video_info = SDL_GetVideoInfo();
    width = video_info->current_w;
    height = video_info->current_h;

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
    float x = 0;
    for (;;) {
        {
            const Uint32 now = SDL_GetTicks();
            if (now >= next_tick) {
                next_tick += 1000 / 60;
                linted_simulator_send(simulator_chan, (linted_simulator_command) {
                        .type = (linted_actor_byte_fast) {
                            .x = LINTED_SIMULATOR_TICK_REQUEST
                        }
                    });
            }
        }

        /* Handle events first before rendering */
        SDL_Event sdl_event;
        const bool had_sdl_event = SDL_PollEvent(&sdl_event);
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
                linted_simulator_send(simulator_chan, (linted_simulator_command) {
                        .type = (linted_actor_byte_fast) {
                            .x = LINTED_SIMULATOR_CLOSE_REQUEST
                        }
                    });
                break;
            default: break;
            }
            break;

        case SDL_QUIT:
            linted_simulator_send(simulator_chan, (linted_simulator_command) {
                    .type = (linted_actor_byte_fast) {
                        .x = LINTED_SIMULATOR_CLOSE_REQUEST
                    }
                });
            break;
        }

        struct pollfd fifo_fd = { .fd = gui_fd, .events = POLLIN };
      retry:;
        const int poll_status = poll(&fifo_fd, 1, 0);
        if (-1 == poll_status) {
            if (errno == EINTR) {
                goto retry;
            }
            LINTED_ERROR("Error polling file descriptors %s\n", strerror(errno));
        }
        const bool had_gui_command = poll_status > 0;
        if (had_gui_command) {
            linted_gui_command command = linted_gui_recv(gui_port);
            switch (command.type.x) {
            case LINTED_GUI_COMMAND_TICK_CHANGE:
                x = ((float) command.data.x) / 255;
                break;

            case LINTED_GUI_COMMAND_SHUTDOWN:
                goto shutdown_gui;

            default:
                LINTED_ERROR("Received unknown command: %d\n", command.type.x);
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
            glVertex2f(x - 0.4f, -0.4f);
            glVertex2f(x + 0.4f, -0.4f);
            glVertex2f(x + 0.0f,  0.4f);
            glEnd();
            
            SDL_GL_SwapBuffers();
        }
    }

 shutdown_gui:
    SDL_Quit();

    linted_fclose(simulator_fifo);
    linted_fclose(gui_fifo);

    return EXIT_SUCCESS;
}

static const attribute_value_pair attribute_values[ATTRIBUTE_AMOUNT] = {
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
