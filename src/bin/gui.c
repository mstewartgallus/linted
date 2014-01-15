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

struct request_data {
	enum request_type type;
	uint8_t x_position;
	uint8_t y_position;
};

struct reply_data {
	char dummy;
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
	int sockets[2];
	if (-1 == linted_io_create_local_server(sockets)) {
		return -1;
	}

	int const gui_reader = sockets[0];
	int const gui_writer = sockets[1];

	if (-1 == linted_task_spawn(&gui->_task, spawner, gui_run, gui_reader)) {
		goto error_and_close_sockets;
	}

	if (-1 == close(gui_reader)) {
		goto error_and_close_socket;
	}

	gui->_server = gui_writer;

	return 0;

 error_and_close_sockets:
	close(gui_reader);

 error_and_close_socket:
	close(gui_writer);

	return -1;
}

int linted_gui_send_update(linted_gui_t const gui, uint8_t const x, uint8_t const y)
{
	int error_status = -1;

	int const connection = linted_io_connect_to_local_socket(gui._server);
	if (-1 == connection) {
		goto finish;
	}

	{
		struct request_data request_data;
		memset(&request_data, 0, sizeof request_data);
		request_data.type = GUI_UPDATE;
		request_data.x_position = x;
		request_data.y_position = y;

		ssize_t bytes_written;
		do {
			bytes_written = write(connection,
					      &request_data, sizeof request_data);
		} while (-1 == bytes_written && EINTR == EINTR);
		if (-1 == bytes_written) {
			goto finish_and_close_connection;
		}
	}

	{
		struct reply_data reply_data;
		ssize_t bytes_read;
		do {
			bytes_read = read(connection, &reply_data, sizeof reply_data);
		} while (-1 == bytes_read && EINTR == EINTR);
		if (-1 == bytes_read) {
			goto finish_and_close_connection;
		}

		/* This is just to confirm the server updated */
	}
	error_status = 0;

 finish_and_close_connection:
	if (-1 == close(connection)) {
		error_status = -1;
	}

 finish:
	return error_status;
}

int linted_gui_close(linted_gui_t const gui)
{
	return close(gui._server);
}

static int gui_run(linted_task_spawner_t const spawner, int const inbox)
{
	int const spawner_close_status = linted_task_spawner_close(spawner);
	if (-1 == spawner_close_status) {
		LINTED_ERROR("Could not close spawner: %m", errno);
	}

	int const sdl_init_status =
	    SDL_Init(SDL_INIT_EVENTTHREAD | SDL_INIT_VIDEO | SDL_INIT_NOPARACHUTE);
	if (-1 == sdl_init_status) {
		LINTED_ERROR("Could not initialize the GUI: %s\n", SDL_GetError());
	}

	SDL_WM_SetCaption(PACKAGE_NAME, NULL);

	for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(attribute_values); ++ii) {
		struct attribute_value_pair const pair = attribute_values[ii];
		if (-1 == SDL_GL_SetAttribute(pair.attribute, pair.value)) {
			LINTED_ERROR
			    ("Could not set a double buffer attribute: %s\n",
			     SDL_GetError());
		}
	}

	Uint32 const sdl_flags = SDL_OPENGL | SDL_RESIZABLE;
	unsigned width = 640, height = 800;

	/* Initialize SDL */
	{
		SDL_Surface *const video_surface = SDL_SetVideoMode(width, height, 0,
								    sdl_flags);
		if (NULL == video_surface) {
			LINTED_ERROR("Could not set the video mode: %s\n",
				     SDL_GetError());
		}
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

		struct pollfd fifo_fd = {.fd = inbox,.events = POLLIN };
		int poll_status;
		bool had_gui_command;
		{
			int error_code;
			do {
				poll_status = poll(&fifo_fd, 1, 0);
			} while (-1 == poll_status
				 && (error_code = errno, error_code != EINTR));
			if (-1 == poll_status) {
				LINTED_ERROR("Error polling file descriptors %m", errno);
			}
			had_gui_command = poll_status > 0;
		}

		if (had_gui_command) {
			/* TODO: Handle multiple connections */
			int const connection = linted_io_recv_socket(inbox);
			if (-1 == connection) {
				if (0 == errno) {
					break;
				}

				LINTED_ERROR("Could not accept gui connection: %m",
					     errno);
			}

			struct request_data request_data;
			ssize_t bytes_read;
			do {
				bytes_read = read(connection,
						  &request_data, sizeof request_data);
			} while (-1 == bytes_read && EINTR == errno);
			if (-1 == bytes_read) {
				LINTED_ERROR("Could not read from gui connection: %m",
					     errno);
			}

			switch (request_data.type) {
			case GUI_UPDATE:{
					x = ((float)request_data.x_position) / 255;
					y = ((float)request_data.y_position) / 255;
					struct reply_data const reply_data = {
						.dummy = 0
					};
					ssize_t bytes_written;
					do {
						bytes_written =
						    write(connection,
							  &reply_data, sizeof reply_data);
					} while (-1 == bytes_written && errno == EINTR);
					if (-1 == bytes_written) {
						LINTED_ERROR
						    ("Could not read from gui inbox: %m",
						     errno);
					}
					break;
				}

			default:
				LINTED_ERROR
				    ("Received unexpected request type: %d.\n",
				     request_data.type);
			}
			if (-1 == close(connection)) {
				LINTED_ERROR("Could not close connection: %m", errno);
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

	int const inbox_close_status = close(inbox);
	if (-1 == inbox_close_status) {
		LINTED_ERROR("Could not close simulator inbox: %m", errno);
	}

	return EXIT_SUCCESS;
}
