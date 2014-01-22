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
#include "linted/sandbox.h"
#include "linted/simulator.h"
#include "linted/supervisor.h"
#include "linted/util.h"

#include "uv.h"

#include <errno.h>
#include <stdlib.h>

struct timer_data {
	linted_gui_t gui;
	linted_simulator_t simulator;
};

static void timer_callback(uv_timer_t * handle, int status);

int linted_supervisor_run(linted_task_spawner_t spawner)
{
	linted_gui_t gui;
	if (-1 == linted_gui_spawn(&gui, spawner)) {
		LINTED_ERROR("Could not spawn gui: %m", errno);
	}

	linted_simulator_t simulator;
	if (-1 == linted_simulator_spawn(&simulator, spawner)) {
		LINTED_ERROR("Could not spawn simulator: %m", errno);
	}

	uv_loop_t *const loop = uv_default_loop();

	uv_timer_t timer;
	int const timer_status = uv_timer_init(loop, &timer);
	if (timer_status < 0) {
		LINTED_ERROR("Could not initialize timer: %s",
			     uv_strerror(-timer_status));
	}

	struct timer_data timer_data = {
		.simulator = simulator,
		.gui = gui
	};
	timer.data = &timer_data;
	int const start_status = uv_timer_start(&timer, timer_callback, 0, 1000L / 60);
	if (start_status < 0) {
		LINTED_ERROR("Could not start timer: %s", uv_strerror(-start_status));
	}

	uv_run(loop, UV_RUN_DEFAULT);

	if (-1 == linted_simulator_close(simulator)) {
		LINTED_ERROR("Could not close simulator handle: %m", errno);
	}

	if (-1 == linted_gui_close(gui)) {
		LINTED_ERROR("Could not close gui handle: %m", errno);
	}

	return EXIT_SUCCESS;
}

static void timer_callback(uv_timer_t * const handle, int const status)
{
	struct timer_data *const timer_data = handle->data;
	linted_gui_t const gui = timer_data->gui;
	linted_simulator_t const simulator = timer_data->simulator;

	struct linted_simulator_tick_results tick_results;
	if (-1 == linted_simulator_send_tick(&tick_results, simulator)) {
		LINTED_ERROR("Could not send tick message to simulator: %m", errno);
	}

	if (-1 == linted_gui_send_update(gui,
					 tick_results.x_position,
					 tick_results.y_position)) {
		LINTED_ERROR("Could not send update message to gui: %m", errno);
	}
}
