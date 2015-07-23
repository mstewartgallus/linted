/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "simulator.h"

#include "linted/asynch.h"
#include "linted/controller.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/sched.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>

#define ROTATION_SPEED 512U
#define DEAD_ZONE (LINTED_SIM_UINT_MAX / 8)

enum { ON_READ_TIMER,
       ON_RECEIVE_CONTROLLER_EVENT,
       ON_SENT_UPDATER_EVENT,
       MAX_TASKS };

struct action_state {
	linted_sim_int x_tilt;
	linted_sim_int y_tilt;

	signed int x : 2U;
	signed int z : 2U;

	bool jumping : 1U;
};

struct differentiable {
	linted_sim_int value;
	linted_sim_int old;
};

struct sim_state {
	struct differentiable position[3U];

	linted_sim_angle x_rotation;
	linted_sim_angle y_rotation;

	bool update_pending : 1U;
	bool write_in_progress : 1U;
};

struct tick_data {
	struct linted_asynch_pool *pool;
	struct linted_updater_task_send *updater_task;
	struct action_state const *action_state;
	struct sim_state *sim_state;
	linted_ko updater;
};

struct controller_data {
	struct linted_asynch_pool *pool;
	struct action_state *action_state;
};

struct updater_data {
	struct sim_state *sim_state;
	struct linted_asynch_pool *pool;
	linted_ko updater;
};

static unsigned char linted_sim_start(char const *const process_name,
                                      size_t argc,
                                      char const *const argv[]);
static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error
on_read_timer(struct linted_asynch_task *completed_task);
static linted_error
on_controller_receive(struct linted_asynch_task *task);
static linted_error
on_sent_update(struct linted_asynch_task *completed_task);

static void maybe_update(linted_updater updater,
                         struct sim_state *sim_state,
                         struct linted_updater_task_send *updater_task,
                         struct linted_asynch_pool *pool);

static void simulate_tick(struct sim_state *sim_state,
                          struct action_state const *action_state);
static linted_sim_angle tilt_rotation(linted_sim_angle rotation,
                                      linted_sim_int tilt);
static linted_sim_angle tilt_clamped_rotation(linted_sim_angle rotation,
                                              linted_sim_int tilt);

static linted_sim_uint absolute(linted_sim_int x);
static linted_sim_int min_int(linted_sim_int x, linted_sim_int y);
static linted_sim_int sign(linted_sim_int x);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-simulator",
    .start = linted_sim_start};

static unsigned char linted_sim_start(char const *const process_name,
                                      size_t argc,
                                      char const *const argv[])
{
	linted_error err;

	if (argc < 3U) {
		linted_log(LINTED_LOG_ERROR,
		           "missing some of 2 file operands");
		return EXIT_FAILURE;
	}

	char const *controller_path = argv[1U];
	char const *updater_path = argv[2U];

	linted_controller controller;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     controller_path, LINTED_KO_RDWR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		controller = xx;
	}

	linted_ko updater;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, updater_path,
		                     LINTED_KO_RDWR);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_ko_open: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		updater = xx;
	}

	struct action_state action_state = {
	    .x = 0, .z = 0, .jumping = false};

	struct sim_state sim_state = {
	    .update_pending = true, /* Initialize the gui at start */
	    .write_in_progress = false,
	    .position = {{.value = 0, .old = 0},
	                 {.value = 0, .old = 0},
	                 {.value = 3 * 1024, .old = 3 * 1024}},
	    .x_rotation = LINTED_SIM_ANGLE(1U, 2U),
	    .y_rotation = LINTED_SIM_ANGLE(0U, 1U)};

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		err = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (err != 0)
			goto exit;
		pool = xx;
	}

	struct tick_data timer_data;
	struct controller_data controller_data;
	struct updater_data updater_data;

	struct linted_sched_task_sleep_until *tick_task;
	struct linted_controller_task_receive *controller_task;
	struct linted_updater_task_send *updater_task;

	{
		struct linted_sched_task_sleep_until *xx;
		err = linted_sched_task_sleep_until_create(&xx,
		                                           &timer_data);
		if (err != 0)
			goto destroy_pool;
		tick_task = xx;
	}

	{
		struct linted_controller_task_receive *xx;
		err = linted_controller_task_receive_create(
		    &xx, &controller_data);
		if (err != 0)
			goto destroy_pool;
		controller_task = xx;
	}

	{
		struct linted_updater_task_send *xx;
		err =
		    linted_updater_task_send_create(&xx, &updater_data);
		if (err != 0)
			goto destroy_pool;
		updater_task = xx;
	}

	timer_data.pool = pool;
	timer_data.updater_task = updater_task;
	timer_data.action_state = &action_state;
	timer_data.sim_state = &sim_state;
	timer_data.updater = updater;

	controller_data.pool = pool;
	controller_data.action_state = &action_state;

	{
		struct timespec now;
		err = linted_sched_time(&now);
		if (err != 0)
			goto stop_pool;

		linted_sched_task_sleep_until_prepare(
		    tick_task, ON_READ_TIMER, &now);
	}
	linted_asynch_pool_submit(
	    pool, linted_sched_task_sleep_until_to_asynch(tick_task));

	linted_controller_task_receive_prepare(
	    controller_task, ON_RECEIVE_CONTROLLER_EVENT, controller);
	linted_asynch_pool_submit(
	    pool,
	    linted_controller_task_receive_to_asynch(controller_task));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		err = dispatch(completed_task);
		if (err != 0)
			goto stop_pool;
	}

stop_pool:
	linted_asynch_task_cancel(
	    linted_sched_task_sleep_until_to_asynch(tick_task));
	linted_asynch_task_cancel(
	    linted_controller_task_receive_to_asynch(controller_task));
	linted_asynch_task_cancel(
	    linted_updater_task_send_to_asynch(updater_task));

	for (;;) {
		struct linted_asynch_task *task;
		linted_error poll_err;
		{
			struct linted_asynch_task *xx;
			poll_err = linted_asynch_pool_poll(pool, &xx);
			if (LINTED_ERROR_AGAIN == poll_err)
				break;
			task = xx;
		}

		linted_error dispatch_err =
		    linted_asynch_task_err(task);
		if (0 == err && dispatch_err != LINTED_ERROR_CANCELLED)
			err = dispatch_err;
	}

destroy_pool : {
	linted_error destroy_err = linted_asynch_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;
}

	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)tick_task;
	(void)controller_task;
	(void)updater_task;

exit:
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error dispatch(struct linted_asynch_task *completed_task)
{
	switch (linted_asynch_task_action(completed_task)) {
	case ON_READ_TIMER:
		return on_read_timer(completed_task);

	case ON_RECEIVE_CONTROLLER_EVENT:
		return on_controller_receive(completed_task);

	case ON_SENT_UPDATER_EVENT:
		return on_sent_update(completed_task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_read_timer(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (err != 0)
		return err;

	struct linted_sched_task_sleep_until *timer_task =
	    linted_sched_task_sleep_until_from_asynch(task);
	struct tick_data *timer_data =
	    linted_sched_task_sleep_until_data(timer_task);

	struct linted_asynch_pool *pool = timer_data->pool;
	linted_ko updater = timer_data->updater;
	struct linted_updater_task_send *updater_task =
	    timer_data->updater_task;
	struct action_state const *action_state =
	    timer_data->action_state;
	struct sim_state *sim_state = timer_data->sim_state;

	struct timespec last_tick_time;
	{
		struct timespec xx;
		linted_sched_task_sleep_until_time(timer_task, &xx);
		last_tick_time = xx;
	}
	time_t last_tick_sec = last_tick_time.tv_sec;
	long last_tick_nsec = last_tick_time.tv_nsec;

	long const second = 1000000000;

	time_t next_tick_sec = last_tick_sec;
	long next_tick_nsec = last_tick_nsec + (second / 60) / 2;
	if (next_tick_nsec >= second) {
		next_tick_nsec -= second;
		next_tick_sec += 1;
	}

	struct timespec next_tick_time = {.tv_sec = next_tick_sec,
	                                  .tv_nsec = next_tick_nsec};

	{
		struct timespec xx = next_tick_time;
		linted_sched_task_sleep_until_prepare(
		    timer_task, ON_READ_TIMER, &xx);
	}
	linted_asynch_pool_submit(pool, task);

	simulate_tick(sim_state, action_state);

	maybe_update(updater, sim_state, updater_task, pool);

	return 0;
}

static linted_error
on_controller_receive(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (err != 0)
		return err;

	struct linted_controller_task_receive *controller_task =
	    linted_controller_task_receive_from_asynch(task);
	struct controller_data *controller_data =
	    linted_controller_task_receive_data(controller_task);

	struct linted_asynch_pool *pool = controller_data->pool;
	struct action_state *action_state =
	    controller_data->action_state;

	struct linted_controller_message message;
	err = linted_controller_decode(controller_task, &message);
	if (err != 0)
		return err;

	linted_asynch_pool_submit(pool, task);

	signed int x = message.right - message.left;
	signed int z = message.back - message.forward;

	linted_sim_int x_tilt = -message.x_tilt;
	linted_sim_int y_tilt = -message.y_tilt;

	bool jumping = message.jumping;

	action_state->x = x;
	action_state->z = z;

	action_state->x_tilt = x_tilt;
	action_state->y_tilt = y_tilt;

	action_state->jumping = jumping;

	return 0;
}

static linted_error on_sent_update(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (ENOENT == err)
		err = 0;
	if (ECONNREFUSED == err)
		err = 0;
	if (err != 0)
		return err;

	struct linted_updater_task_send *updater_task =
	    linted_updater_task_send_from_asynch(task);

	struct updater_data *updater_data =
	    linted_updater_task_send_data(updater_task);
	struct linted_asynch_pool *pool = updater_data->pool;
	linted_ko updater = updater_data->updater;
	struct sim_state *sim_state = updater_data->sim_state;

	sim_state->write_in_progress = false;

	maybe_update(updater, sim_state, updater_task, pool);

	return 0;
}

static void maybe_update(linted_updater updater,
                         struct sim_state *sim_state,
                         struct linted_updater_task_send *updater_task,
                         struct linted_asynch_pool *pool)
{
	bool update_pending = sim_state->update_pending;
	bool write_in_progress = sim_state->write_in_progress;

	struct differentiable const *position = sim_state->position;

	linted_sim_int x_position = position[0U].value;
	linted_sim_int y_position = position[1U].value;
	linted_sim_int z_position = position[2U].value;

	linted_sim_angle x_rotation = sim_state->x_rotation;
	linted_sim_angle y_rotation = sim_state->y_rotation;

	if (!update_pending)
		return;

	if (write_in_progress)
		return;

	struct updater_data *updater_data =
	    linted_updater_task_send_data(updater_task);
	updater_data->sim_state = sim_state;
	updater_data->pool = pool;
	updater_data->updater = updater;

	linted_updater_angle update_x_rotation = LINTED_UPDATER_ANGLE(
	    x_rotation._value, (uintmax_t)LINTED_SIM_UINT_MAX + 1U);

	linted_updater_angle update_y_rotation = LINTED_UPDATER_ANGLE(
	    y_rotation._value, (uintmax_t)LINTED_SIM_UINT_MAX + 1U);

	struct linted_updater_update update = {
	    .x_position = x_position,
	    .y_position = y_position,
	    .z_position = z_position,
	    .x_rotation = update_x_rotation,
	    .y_rotation = update_y_rotation};

	{
		struct linted_updater_update xx = update;
		linted_updater_task_send_prepare(
		    updater_task, ON_SENT_UPDATER_EVENT, updater, &xx);
	}
	linted_asynch_pool_submit(
	    pool, linted_updater_task_send_to_asynch(updater_task));

	sim_state->update_pending = false;
	sim_state->write_in_progress = true;
}

static linted_sim_int downscale(linted_sim_int x)
{
	return (INTMAX_C(16) * x) / LINTED_SIM_INT_MAX;
}

static void simulate_tick(struct sim_state *sim_state,
                          struct action_state const *action_state)
{

	linted_sim_angle x_rotation = sim_state->x_rotation;
	linted_sim_angle y_rotation = sim_state->y_rotation;

	struct differentiable *positions = sim_state->position;
	size_t dimensions = LINTED_ARRAY_SIZE(sim_state->position);

	linted_sim_int x = action_state->x;
	linted_sim_int z = action_state->z;
	linted_sim_int jumping = action_state->jumping;
	linted_sim_int x_tilt = action_state->x_tilt;
	linted_sim_int y_tilt = action_state->y_tilt;

	linted_sim_int cos_x = downscale(linted_sim_cos(x_rotation));
	linted_sim_int sin_x = downscale(linted_sim_sin(x_rotation));

	linted_sim_int x_thrust[3U] = {z * -sin_x, 0, z * -cos_x};
	linted_sim_int y_thrust[3U] = {x * -cos_x, 0, x * sin_x};
	linted_sim_int z_thrust[3U] = {
	    0, jumping * downscale(-LINTED_SIM_INT_MAX), 0};

	linted_sim_int thrusts[3U];
	for (size_t ii = 0U; ii < dimensions; ++ii)
		thrusts[ii] =
		    x_thrust[ii] + y_thrust[ii] + z_thrust[ii];

	linted_sim_int gravity[3U] = {0, 10, 0};

	linted_sim_int normal_force[3U] = {
	    0, (positions[1U].value >= 0) * -20, 0};

	linted_sim_int forces[3U];
	for (size_t ii = 0U; ii < dimensions; ++ii)
		forces[ii] =
		    gravity[ii] + normal_force[ii] + thrusts[ii];

	for (size_t ii = 0U; ii < dimensions; ++ii) {
		struct differentiable *pos = &positions[ii];

		linted_sim_int position = pos->value;
		linted_sim_int old_position = pos->old;

		linted_sim_int old_velocity = position - old_position;
		linted_sim_int force = forces[ii];

		linted_sim_int guess_velocity =
		    linted_sim_isatadd(force, old_velocity);

		linted_sim_int friction =
		    min_int(absolute(guess_velocity), 5 /* = μ Fₙ */) *
		    -sign(guess_velocity);

		linted_sim_int new_velocity =
		    linted_sim_isatadd(guess_velocity, friction);

		linted_sim_int new_position =
		    linted_sim_isatadd(position, new_velocity);

		pos->value = new_position;
		pos->old = position;
	}

	linted_sim_angle new_x_rotation =
	    tilt_rotation(x_rotation, x_tilt);
	linted_sim_angle new_y_rotation =
	    tilt_clamped_rotation(y_rotation, y_tilt);

	sim_state->x_rotation = new_x_rotation;
	sim_state->y_rotation = new_y_rotation;

	sim_state->update_pending = true;
}

static linted_sim_angle tilt_rotation(linted_sim_angle rotation,
                                      linted_sim_int tilt)
{

	linted_sim_angle increment =
	    LINTED_SIM_ANGLE(1, ROTATION_SPEED);

	return linted_sim_angle_add((absolute(tilt) > DEAD_ZONE) *
	                                sign(tilt),
	                            rotation, increment);
}

static linted_sim_angle tilt_clamped_rotation(linted_sim_angle rotation,
                                              linted_sim_int tilt)
{
	linted_sim_int tilt_sign = sign(tilt);

	if (absolute(tilt) <= DEAD_ZONE) {
		return rotation;
	}

	linted_sim_angle minimum = LINTED_SIM_ANGLE(15U, 16U);
	linted_sim_angle maximum = LINTED_SIM_ANGLE(3U, 16U);
	linted_sim_angle increment =
	    LINTED_SIM_ANGLE(1U, ROTATION_SPEED);

	return linted_sim_angle_add_clamped(tilt_sign, minimum, maximum,
	                                    rotation, increment);
}

static linted_sim_int min_int(linted_sim_int x, linted_sim_int y)
{
	return x < y ? x : y;
}

static linted_sim_int sign(linted_sim_int x)
{
	return x > 0 ? 1 : 0 == x ? 0 : -1;
}

static linted_sim_uint absolute(linted_sim_int x)
{
	if (LINTED_SIM_INT_MIN == x) {
		/* Avoid tricky arithmetic overflow possibilities */
		return ((linted_sim_uint) - (LINTED_SIM_INT_MIN + 1)) +
		       1U;
	} else if (x < 0) {
		return -x;
	} else {
		return x;
	}
}
