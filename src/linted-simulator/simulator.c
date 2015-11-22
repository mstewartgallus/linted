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

#include "sim.h"

#include "linted/async.h"
#include "linted/controller.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/prctl.h"
#include "linted/sched.h"
#include "linted/start.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

/**
 * @file
 *
 * @todo Fix collision handling. I want to prevent interpenetration
 *       and properly handle discontinuities in velocity. I need
 *       proper collision response and not the hacky spring like thing
 *       I have currently.
 *       Resources:
 *       <ul>
 *       <li>http://www.gdcvault.com/play/1018239/Physics-for-Game-Programmers-Continuous</li>
 *       <li>http://gafferongames.com/virtualgo/collision-response-and-coulomb-friction</li>
 *       <li>http://www.codezealot.org/archives/55</li>
 *       <li>http://www.codezealot.org/archives/88</li>
 *       <li>http://mollyrocket.com/849</li>
 *       <li>http://chrishecker.com/images/d/df/Gdmphys1.pdf</li>
 *       <li>http://chrishecker.com/images/c/c2/Gdmphys2.pdf</li>
 *       <li>http://chrishecker.com/images/e/e7/Gdmphys3.pdf</li>
 *       <li>http://chrishecker.com/images/b/bb/Gdmphys4.pdf</li>
 *       <li>http://www.pixar.com/companyinfo/research/pbm2001/pdf/notesg.pdf</li>
 *       <li>http://www.wildbunny.co.uk/blog/2011/04/06/physics-engines-for-dummies</li>
 *       <li>http://www.bulletphysics.com/ftp/pub/test/physics/papers/IterativeDynamics.pdf</li>
 *       </ul>
 */

#define ROTATION_SPEED 512U
#define DEAD_ZONE (SIM_UINT_MAX / 8)

enum { ON_READ_TIMER,
       ON_RECEIVE_CONTROLLER_EVENT,
       ON_SENT_UPDATER_EVENT,
       MAX_TASKS };

struct intent {
	sim_int look_sideways;
	sim_int look_up_or_down;

	signed int strafe : 2U;
	signed int retreat_or_go_forth : 2U;

	bool jump_up : 1U;
};

struct differentiable {
	sim_int value;
	sim_int old;
};

struct state {
	struct differentiable position[3U];

	sim_angle z_rotation;
	sim_angle x_rotation;

	bool update_pending : 1U;
	bool write_in_progress : 1U;
};

struct sim {
	struct state state;
	struct intent intent;
	struct linted_async_pool *pool;
	struct linted_updater_task_send *updater_task;
	struct linted_controller_task_recv *controller_task;
	struct linted_sched_task_sleep_until *tick_task;
	linted_ko updater;
	linted_ko controller;
};

static linted_error sim_init(struct sim *sim,
                             struct linted_async_pool *pool,
                             char const *controller_path,
                             char const *updater_path);
static linted_error sim_stop(struct sim *sim);
static linted_error sim_destroy(struct sim *sim);

static linted_error dispatch(struct sim *sim, union linted_async_ck ck,
                             void *userstate, linted_error err);
static linted_error
sim_on_tick(struct sim *sim,
            struct linted_sched_task_sleep_until *timer_task,
            linted_error err);
static linted_error sim_on_controller_event(
    struct sim *sim,
    struct linted_controller_task_recv *controller_task,
    linted_error err);
static linted_error
sim_on_update(struct sim *sim,
              struct linted_updater_task_send *updater_task,
              linted_error err);

static void maybe_update(struct sim *sim, linted_updater updater,
                         struct state *state,
                         struct linted_updater_task_send *updater_task,
                         struct linted_async_pool *pool);

static void simulate_tick(struct state *state,
                          struct intent const *intent);
static sim_angle tilt_rotation(sim_angle rotation, sim_int tilt);
static sim_angle tilt_clamped_rotation(sim_angle rotation,
                                       sim_int tilt);

static sim_int downscale(sim_int x, sim_int y);
static sim_uint absolute(sim_int x);
static sim_int min_int(sim_int x, sim_int y);
static sim_int sign(sim_int x);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-simulator", 0};

static unsigned char linted_start_main(char const *const process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	if (argc < 3U) {
		linted_log(LINTED_LOG_ERROR,
		           "missing some of 2 file operands");
		return EXIT_FAILURE;
	}

	char const *controller_path = argv[1U];
	char const *updater_path = argv[2U];

	err = linted_prctl_set_timerslack(1U);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_prctl_set_timerslack: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	struct linted_async_pool *pool;
	{
		struct linted_async_pool *xx;
		err = linted_async_pool_create(&xx, MAX_TASKS);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_async_pool_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	static struct sim sim = {0};
	err = sim_init(&sim, pool, controller_path, updater_path);
	if (err != 0)
		goto destroy_pool;

	for (;;) {
		struct linted_async_result result;
		{
			struct linted_async_result xx;
			linted_async_pool_wait(pool, &xx);
			result = xx;
		}

		err = dispatch(&sim, result.task_ck, result.userstate,
		               result.err);
		if (err != 0)
			goto stop_simulator;
	}
stop_simulator:
	sim_stop(&sim);

	for (;;) {
		struct linted_async_result result;
		linted_error poll_err;
		{
			struct linted_async_result xx;
			poll_err = linted_async_pool_poll(pool, &xx);
			if (LINTED_ERROR_AGAIN == poll_err)
				break;
			result = xx;
		}

		linted_error dispatch_err = result.err;
		if (0 == err && dispatch_err != LINTED_ERROR_CANCELLED)
			err = dispatch_err;
	}

	sim_destroy(&sim);

destroy_pool:
	;
	linted_error destroy_err = linted_async_pool_destroy(pool);
	if (0 == err)
		err = destroy_err;

	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "%s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static linted_error sim_init(struct sim *sim,
                             struct linted_async_pool *pool,
                             char const *controller_path,
                             char const *updater_path)
{
	linted_error err = 0;

	linted_controller controller;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD,
		                     controller_path, LINTED_KO_RDWR);
		if (err != 0)
			return err;
		controller = xx;
	}

	linted_ko updater;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, updater_path,
		                     LINTED_KO_RDWR);
		if (err != 0)
			goto close_controller;
		updater = xx;
	}

	struct linted_sched_task_sleep_until *tick_task;
	{
		struct linted_sched_task_sleep_until *xx;
		err = linted_sched_task_sleep_until_create(&xx, 0);
		if (err != 0)
			goto close_updater;
		tick_task = xx;
	}

	struct linted_controller_task_recv *controller_task;
	{
		struct linted_controller_task_recv *xx;
		err = linted_controller_task_recv_create(&xx, 0);
		if (err != 0)
			goto destroy_tick_task;
		controller_task = xx;
	}

	struct linted_updater_task_send *updater_task;
	{
		struct linted_updater_task_send *xx;
		err = linted_updater_task_send_create(&xx, 0);
		if (err != 0)
			goto destroy_controller_task;
		updater_task = xx;
	}

	sim->pool = pool;
	sim->updater = updater;
	sim->updater_task = updater_task;
	sim->controller_task = controller_task;
	sim->tick_task = tick_task;
	sim->controller = controller;
	sim->state = (struct state){
	    .update_pending = true, /* Initialize the gui at start */
	    .write_in_progress = false,
	    .position = {{.value = 0, .old = 0},
	                 {.value = 10 * 1024, .old = 10 * 1024},
	                 {.value = 0, .old = 0}},
	    .z_rotation = SIM_ANGLE(0U, 1U),
	    .x_rotation = SIM_ANGLE(3U, 16U)};

	sim->intent = (struct intent){
	    .strafe = 0, .retreat_or_go_forth = 0, .jump_up = false};

	{
		struct timespec now;
		err = linted_sched_time(&now);
		if (err != 0)
			goto destroy_updater_task;

		linted_async_pool_submit(
		    pool,
		    linted_sched_task_sleep_until_prepare(
		        tick_task,
		        (union linted_async_ck){.u64 = ON_READ_TIMER},
		        tick_task, &now));
	}

	linted_async_pool_submit(
	    pool, linted_controller_task_recv_prepare(
	              controller_task,
	              (union linted_async_ck){
	                  .u64 = ON_RECEIVE_CONTROLLER_EVENT},
	              controller_task, controller));

	return 0;

destroy_updater_task:
	linted_updater_task_send_destroy(updater_task);

destroy_controller_task:
	linted_controller_task_recv_destroy(controller_task);

destroy_tick_task:
	linted_sched_task_sleep_until_destroy(tick_task);

close_updater:
	linted_ko_close(updater);

close_controller:
	linted_ko_close(controller);

	return err;
}

static linted_error sim_stop(struct sim *sim)
{
	struct linted_updater_task_send *updater_task =
	    sim->updater_task;
	struct linted_controller_task_recv *controller_task =
	    sim->controller_task;
	struct linted_sched_task_sleep_until *tick_task =
	    sim->tick_task;

	linted_async_task_cancel(
	    linted_sched_task_sleep_until_to_async(tick_task));
	linted_async_task_cancel(
	    linted_controller_task_recv_to_async(controller_task));
	linted_async_task_cancel(
	    linted_updater_task_send_to_async(updater_task));

	return 0;
}

static linted_error sim_destroy(struct sim *sim)
{
	struct linted_updater_task_send *updater_task =
	    sim->updater_task;
	struct linted_controller_task_recv *controller_task =
	    sim->controller_task;
	struct linted_sched_task_sleep_until *tick_task =
	    sim->tick_task;

	linted_ko updater = sim->updater;
	linted_ko controller = sim->controller;

	linted_updater_task_send_destroy(updater_task);

	linted_controller_task_recv_destroy(controller_task);

	linted_sched_task_sleep_until_destroy(tick_task);

	linted_ko_close(updater);

	linted_ko_close(controller);

	return 0;
}

static linted_error dispatch(struct sim *sim,
                             union linted_async_ck task_ck,
                             void *userstate, linted_error err)
{
	switch (task_ck.u64) {
	case ON_READ_TIMER:
		return sim_on_tick(sim, userstate, err);

	case ON_RECEIVE_CONTROLLER_EVENT:
		return sim_on_controller_event(sim, userstate, err);

	case ON_SENT_UPDATER_EVENT:
		return sim_on_update(sim, userstate, err);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error
sim_on_tick(struct sim *sim,
            struct linted_sched_task_sleep_until *timer_task,
            linted_error err)
{
	struct linted_async_pool *pool = sim->pool;
	linted_ko updater = sim->updater;
	struct linted_updater_task_send *updater_task =
	    sim->updater_task;
	struct intent const *intent = &sim->intent;
	struct state *state = &sim->state;

	if (err != 0)
		return err;

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

		linted_async_pool_submit(
		    pool,
		    linted_sched_task_sleep_until_prepare(
		        timer_task,
		        (union linted_async_ck){.u64 = ON_READ_TIMER},
		        timer_task, &xx));
	}

	simulate_tick(state, intent);

	maybe_update(sim, updater, state, updater_task, pool);

	return 0;
}

static linted_error sim_on_controller_event(
    struct sim *sim,
    struct linted_controller_task_recv *controller_task,
    linted_error err)
{
	struct linted_async_pool *pool = sim->pool;
	linted_ko controller = sim->controller;
	struct intent *intent = &sim->intent;

	if (err != 0)
		return err;

	struct linted_controller_message message;
	err = linted_controller_decode(controller_task, &message);
	if (err != 0)
		return err;

	linted_async_pool_submit(
	    pool, linted_controller_task_recv_prepare(
	              controller_task,
	              (union linted_async_ck){
	                  .u64 = ON_RECEIVE_CONTROLLER_EVENT},
	              controller_task, controller));

	signed int x = message.left - message.right;
	signed int y = message.back - message.forward;

	sim_int look_sideways = message.z_tilt;
	sim_int look_up_or_down = -message.x_tilt;

	bool jump_up = message.jumping;

	intent->strafe = x;
	intent->retreat_or_go_forth = y;

	intent->look_sideways = look_sideways;
	intent->look_up_or_down = look_up_or_down;

	intent->jump_up = jump_up;

	return 0;
}

static linted_error
sim_on_update(struct sim *sim,
              struct linted_updater_task_send *updater_task,
              linted_error err)
{
	struct linted_async_pool *pool = sim->pool;
	linted_ko updater = sim->updater;
	struct state *state = &sim->state;

	if (err != 0)
		return err;

	state->write_in_progress = false;

	maybe_update(sim, updater, state, updater_task, pool);

	return 0;
}

static void maybe_update(struct sim *sim, linted_updater updater,
                         struct state *state,
                         struct linted_updater_task_send *updater_task,
                         struct linted_async_pool *pool)
{
	bool update_pending = state->update_pending;
	bool write_in_progress = state->write_in_progress;

	struct differentiable const *position = state->position;

	sim_int x_position = position[0U].value;
	sim_int y_position = position[1U].value;
	sim_int z_position = position[2U].value;

	sim_angle z_rotation = state->z_rotation;
	sim_angle x_rotation = state->x_rotation;

	if (!update_pending)
		return;

	if (write_in_progress)
		return;

	linted_updater_angle update_z_rotation = LINTED_UPDATER_ANGLE(
	    z_rotation._value, (uintmax_t)SIM_UINT_MAX + 1U);

	linted_updater_angle update_x_rotation = LINTED_UPDATER_ANGLE(
	    x_rotation._value, (uintmax_t)SIM_UINT_MAX + 1U);

	struct linted_updater_update update = {
	    .x_position = x_position,
	    .y_position = y_position,
	    .z_position = z_position,
	    .z_rotation = update_z_rotation,
	    .x_rotation = update_x_rotation};

	{
		struct linted_updater_update xx = update;

		linted_async_pool_submit(
		    pool, linted_updater_task_send_prepare(
		              updater_task,
		              (union linted_async_ck){
		                  .u64 = ON_SENT_UPDATER_EVENT},
		              updater_task, updater, &xx));
	}

	state->update_pending = false;
	state->write_in_progress = true;
}

static void simulate_tick(struct state *state,
                          struct intent const *intent)
{
	sim_angle z_rotation = state->z_rotation;
	sim_angle x_rotation = state->x_rotation;

	struct differentiable *positions = state->position;
	size_t dimensions = LINTED_ARRAY_SIZE(state->position);

	sim_int x = intent->strafe;
	sim_int y = intent->retreat_or_go_forth;
	sim_int jump_up = intent->jump_up;
	sim_int look_sideways = intent->look_sideways;
	sim_int look_up_or_down = intent->look_up_or_down;

	bool contacting_ground = positions[2U].value >= 0;

	sim_int cos_z = downscale(sim_cos(z_rotation), 32);
	sim_int sin_z = downscale(sim_sin(z_rotation), 32);

	sim_int y_thrust[3U] = {contacting_ground * y * sin_z,
	                        contacting_ground * y * cos_z, 0};
	sim_int x_thrust[3U] = {contacting_ground * x * cos_z,
	                        contacting_ground * x * -sin_z, 0};
	sim_int z_thrust[3U] = {0, 0, contacting_ground * jump_up *
	                                  downscale(-SIM_INT_MAX, 512)};

	sim_int thrusts[3U];
	for (size_t ii = 0U; ii < dimensions; ++ii)
		thrusts[ii] =
		    x_thrust[ii] + y_thrust[ii] + z_thrust[ii];

	sim_int gravity[3U] = {0, 0, 10};

	sim_int normal_force[3U] = {0, 0, -contacting_ground};

	sim_int forces[3U];
	for (size_t ii = 0U; ii < dimensions; ++ii)
		forces[ii] =
		    gravity[ii] + normal_force[ii] + thrusts[ii];

	for (size_t ii = 0U; ii < dimensions; ++ii) {
		struct differentiable *pos = &positions[ii];

		sim_int position = pos->value;
		sim_int old_position = pos->old;

		sim_int old_velocity = position - old_position;
		sim_int force = forces[ii];

		sim_int guess_velocity =
		    sim_isatadd(force, old_velocity);

		sim_int mu;
		if (0U == ii || 1U == ii) {
			mu = contacting_ground * 5;
		} else {
			mu = 5;
		}
		sim_int friction =
		    min_int(absolute(guess_velocity), mu) *
		    -sign(guess_velocity);

		sim_int new_velocity =
		    sim_isatadd(guess_velocity, friction);

		if (2U == ii && contacting_ground && new_velocity > 0) {
			new_velocity = 0U;
		}

		sim_int new_position =
		    sim_isatadd(position, new_velocity);

		pos->value = new_position;
		pos->old = position;
	}

	sim_angle new_z_rotation =
	    tilt_rotation(z_rotation, look_sideways);
	sim_angle new_x_rotation =
	    tilt_clamped_rotation(x_rotation, look_up_or_down);

	state->z_rotation = new_z_rotation;
	state->x_rotation = new_x_rotation;

	state->update_pending = true;
}

static sim_angle tilt_rotation(sim_angle rotation, sim_int tilt)
{
	sim_angle increment = SIM_ANGLE(1, ROTATION_SPEED);

	return sim_angle_add((absolute(tilt) > DEAD_ZONE) * sign(tilt),
	                     rotation, increment);
}

static sim_angle tilt_clamped_rotation(sim_angle rotation, sim_int tilt)
{
	sim_int tilt_sign = sign(tilt);

	if (absolute(tilt) <= DEAD_ZONE) {
		return rotation;
	}

	sim_angle minimum = SIM_ANGLE(3U, 16U);
	sim_angle maximum = SIM_ANGLE(5U, 16U);
	sim_angle increment = SIM_ANGLE(1U, ROTATION_SPEED);

	return sim_angle_add_clamped(tilt_sign, minimum, maximum,
	                             rotation, increment);
}

static sim_int downscale(sim_int x, sim_int y)
{
	return (((intmax_t)y) * x) / SIM_INT_MAX;
}

static sim_int min_int(sim_int x, sim_int y)
{
	return x < y ? x : y;
}

static sim_int sign(sim_int x)
{
	return x > 0 ? 1 : 0 == x ? 0 : -1;
}

static sim_uint absolute(sim_int x)
{
	if (SIM_INT_MIN == x) {
		/* Avoid tricky arithmetic overflow possibilities */
		return ((sim_uint) - (SIM_INT_MIN + 1)) + 1U;
	} else if (x < 0) {
		return -x;
	} else {
		return x;
	}
}
