/*
 * Copyright 2015 Steven Stewart-Gallus
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
#include "config.h"

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/util.h"

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

/* Deliberately overflow in cases such as 1/1 */
#define SIM_ANGLE(X, Y)                                                \
	(sim_angle)                                                    \
	{                                                              \
		(sim_uint)((((uintmax_t)SIM_UINT_MAX) + 1U) / (Y)) *   \
		    (X)                                                \
	}

#define SIM_INT_MAX INT32_MAX
#define SIM_INT_MIN INT32_MIN

#define SIM_UINT_MAX UINT32_MAX

#define SIM_Id PRId32

#define ROTATION_SPEED 512U
#define DEAD_ZONE (SIM_UINT_MAX / 8)

module LntdSimulatorC
{
	uses interface LntdMainLoop;
	uses interface LntdLogger;
	uses interface LntdStdio;

	uses interface LntdTimer as Timer;
	uses interface LntdControllerReader as ControllerReader;
	uses interface LntdUpdateWriter as UpdateWriter;
}
implementation
{
	typedef uint_least32_t sim_uint;
	typedef int_least32_t sim_int;

	struct sim__angle;
	typedef struct sim__angle sim_angle;

	struct sim__angle {
		sim_uint _value;
	};

	struct differentiable {
		sim_int value;
		sim_int old;
	};

	sim_int look_sideways;
	sim_int look_up_or_down;

	int strafe;
	int retreat_or_go_forth;

	bool jump_up;

	struct differentiable position[3U];

	sim_angle z_rotation;
	sim_angle x_rotation;

	struct timespec next_tick;
	uint64_t tick = 0U;

	bool pending_update;
	bool is_updating;

	lntd_ko update_ko;
	lntd_ko controller_ko;

	lntd_error simulate_tick(void);
	void maybe_update(void);
	void cancelall(void);
	void finish(lntd_error err);
	void update_tick(void);

	sim_angle tilt_rotation(sim_angle rotation, sim_int tilt);
	sim_angle tilt_clamped_rotation(sim_angle rotation,
	                                sim_int tilt);

	sim_int downscale(sim_int x, sim_int y);
	sim_uint absolute(sim_int x);
	sim_int min_int(sim_int x, sim_int y);
	sim_int sign(sim_int x);

	sim_angle sim_angle_add(int sign, sim_angle theta,
	                        sim_angle phi);

	sim_angle sim_angle_add_clamped(int sign, sim_angle min,
	                                sim_angle max, sim_angle theta,
	                                sim_angle phi);

	sim_int sin_quarter(sim_uint theta);
	sim_int sim_sin(sim_angle angle);
	sim_int sim_cos(sim_angle angle);
	sim_int sim_isatadd(sim_int x, sim_int y);

	sim_int saturate(int_fast64_t x);

	event void LntdMainLoop.boot(size_t argc,
	                             char const *const *argv)
	{
		lntd_error err = 0;

		char const *updater_path;
		char const *controller_path;

		if (argc < 3U) {
			call LntdLogger.log(
			    LNTD_LOGGER_ERROR,
			    "missing some of 2 file operands!");
			call LntdMainLoop.exit(EXIT_FAILURE);
			return;
		}

		controller_path = argv[1U];
		updater_path = argv[2U];

		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   updater_path, LNTD_KO_RDWR);
			if (err != 0) {
				finish(err);
				return;
			}
			update_ko = xx;
		}

		{
			lntd_ko xx;
			err =
			    lntd_ko_open(&xx, LNTD_KO_CWD,
			                 controller_path, LNTD_KO_RDWR);
			if (err != 0) {
				finish(err);
				return;
			}
			controller_ko = xx;
		}

		position[1U].value = 10 * 1024;
		position[1U].old = 10 * 1024;

		z_rotation = SIM_ANGLE(0U, 1U);
		x_rotation = SIM_ANGLE(3U, 16U);

		call ControllerReader.start(controller_ko);

		if (-1 == clock_gettime(CLOCK_MONOTONIC, &next_tick)) {
			finish(errno);
			return;
		}

		update_tick();
		call Timer.execute(&next_tick);
	}

	event int LntdMainLoop.shutdown(lntd_error err)
	{
		lntd_ko_close(update_ko);
		lntd_ko_close(controller_ko);

		if (err != 0)
			return EXIT_FAILURE;

		return EXIT_SUCCESS;
	}

	event void LntdMainLoop.recv_cancel(void)
	{
		cancelall();
	}

	event void Timer.tick_done(lntd_error err)
	{
		if (LNTD_ERROR_CANCELLED == err) {
			finish(0);
			return;
		}

		if (err != 0) {
			finish(err);
			return;
		}

		err = simulate_tick();
		if (err != 0) {
			finish(err);
			return;
		}

		pending_update = true;
		maybe_update();

		++tick;

		update_tick();
		call Timer.execute(&next_tick);
	}

	event void ControllerReader.read_input(
	    lntd_error err,
	    struct lntd_controller_reader_input const *input)
	{
		int_fast32_t z_tilt;
		int_fast32_t x_tilt;

		bool left;
		bool right;
		bool forward;
		bool back;

		bool jumping;

		if (err != 0) {
			finish(err);
			return;
		}

		z_tilt = input->z_tilt;
		x_tilt = input->x_tilt;

		left = input->left;
		right = input->right;
		forward = input->forward;
		back = input->back;

		jumping = input->jumping;

		look_sideways = z_tilt;
		look_up_or_down = -x_tilt;

		strafe = left - right;
		retreat_or_go_forth = back - forward;

		jump_up = jumping;
	}

	event void UpdateWriter.write_done(lntd_error err)
	{
		if (err != 0) {
			finish(err);
			return;
		}

		is_updating = false;

		maybe_update();
	}

	void maybe_update(void)
	{
		struct lntd_update_writer_update update;

		update.x_position = position[0U].value;
		update.y_position = position[1U].value;
		update.z_position = position[2U].value;

		update.z_rotation = z_rotation._value;
		update.x_rotation = x_rotation._value;

		call UpdateWriter.write(update_ko, &update);
	}

	lntd_error simulate_tick(void)
	{
		struct differentiable *positions = position;
		size_t dimensions = LNTD_ARRAY_SIZE(position);

		sim_int x = strafe;
		sim_int y = retreat_or_go_forth;

		bool contacting_ground = positions[2U].value >= 0;

		sim_int cos_z = downscale(sim_cos(z_rotation), 32);
		sim_int sin_z = downscale(sim_sin(z_rotation), 32);

		sim_int y_thrust[3U] = {contacting_ground * y * sin_z,
		                        contacting_ground * y * cos_z,
		                        0};
		sim_int x_thrust[3U] = {contacting_ground * x * cos_z,
		                        contacting_ground * x * -sin_z,
		                        0};
		sim_int z_thrust[3U] = {
		    0, 0, contacting_ground * jump_up *
		              downscale(-SIM_INT_MAX, 512)};

		sim_int thrusts[3U];

		sim_int gravity[3U] = {0, 0, 10};

		sim_int normal_force[3U] = {0, 0, -contacting_ground};

		sim_int forces[3U];

		{
			size_t ii;
			for (ii = 0U; ii < dimensions; ++ii)
				thrusts[ii] = x_thrust[ii] +
				              y_thrust[ii] +
				              z_thrust[ii];
		}

		{
			size_t ii;
			for (ii = 0U; ii < dimensions; ++ii)
				forces[ii] = gravity[ii] +
				             normal_force[ii] +
				             thrusts[ii];
		}

		{
			size_t ii;
			for (ii = 0U; ii < dimensions; ++ii) {
				struct differentiable *pos =
				    &positions[ii];

				sim_int position = pos->value;
				sim_int old_position = pos->old;

				sim_int old_velocity =
				    position - old_position;
				sim_int force = forces[ii];

				sim_int guess_velocity =
				    sim_isatadd(force, old_velocity);

				sim_int mu;
				sim_int friction;
				sim_int new_velocity;
				sim_int new_position;

				if (0U == ii || 1U == ii) {
					mu = contacting_ground * 5;
				} else {
					mu = 5;
				}
				friction =
				    min_int(absolute(guess_velocity),
				            mu) *
				    -sign(guess_velocity);

				new_velocity = sim_isatadd(
				    guess_velocity, friction);

				if (2U == ii && contacting_ground &&
				    new_velocity > 0) {
					new_velocity = 0U;
				}

				new_position =
				    sim_isatadd(position, new_velocity);

				pos->value = new_position;
				pos->old = position;
			}
		}

		z_rotation = tilt_rotation(z_rotation, look_sideways);
		x_rotation =
		    tilt_clamped_rotation(x_rotation, look_up_or_down);

		return 0;
	}

	void update_tick(void)
	{
		time_t last_tick_sec = next_tick.tv_sec;
		long last_tick_nsec = next_tick.tv_nsec;

		long const second = 1000000000;

		time_t next_tick_sec = last_tick_sec;
		long next_tick_nsec =
		    last_tick_nsec + (second / 60) / 2;
		if (next_tick_nsec >= second) {
			next_tick_nsec -= second;
			next_tick_sec += 1;
		}

		next_tick.tv_sec = next_tick_sec;
		next_tick.tv_nsec = next_tick_nsec;
	}

	void cancelall(void)
	{
		call Timer.cancel();
		call ControllerReader.stop();
		call UpdateWriter.cancel();
	}

	void finish(lntd_error err)
	{
		if (err != 0) {
			char *errmsg;

			errno = err;
			if (-1 == asprintf(&errmsg, "%m")) {
				return;
			}

			call LntdLogger.log(LNTD_LOGGER_ERROR, errmsg);

			free(errmsg);
		}

		call LntdMainLoop.exit(err);
	}

	sim_angle tilt_rotation(sim_angle rotation, sim_int tilt)
	{
		sim_angle increment = SIM_ANGLE(1, ROTATION_SPEED);

		return sim_angle_add((absolute(tilt) > DEAD_ZONE) *
		                         sign(tilt),
		                     rotation, increment);
	}

	sim_angle tilt_clamped_rotation(sim_angle rotation,
	                                sim_int tilt)
	{
		sim_int tilt_sign = sign(tilt);

		sim_angle minimum = SIM_ANGLE(3U, 16U);
		sim_angle maximum = SIM_ANGLE(5U, 16U);
		sim_angle increment = SIM_ANGLE(1U, ROTATION_SPEED);

		if (absolute(tilt) <= DEAD_ZONE) {
			return rotation;
		}

		return sim_angle_add_clamped(
		    tilt_sign, minimum, maximum, rotation, increment);
	}

	sim_int downscale(sim_int x, sim_int y)
	{
		return (((intmax_t)y) * x) / SIM_INT_MAX;
	}

	sim_int min_int(sim_int x, sim_int y)
	{
		return x < y ? x : y;
	}

	sim_int sign(sim_int x)
	{
		return x > 0 ? 1 : 0 == x ? 0 : -1;
	}

	sim_uint absolute(sim_int x)
	{
		if (SIM_INT_MIN == x) {
			/* Avoid tricky arithmetic overflow
			 * possibilities */
			return ((sim_uint) - (SIM_INT_MIN + 1)) + 1U;
		} else if (x < 0) {
			return -x;
		} else {
			return x;
		}
	}

	sim_angle sim_angle_add(int sign, sim_angle theta,
	                        sim_angle phi)
	{
		sim_angle angle;
		angle._value =
		    (theta._value + sign * (int_fast64_t)phi._value) %
		    SIM_UINT_MAX;
		return angle;
	}

	inline sim_angle sim_angle_add_clamped(
	    int sign, sim_angle min, sim_angle max, sim_angle theta,
	    sim_angle phi)
	{
		sim_uint result;

		LNTD_ASSERT(max._value <= SIM_UINT_MAX / 2U);
		LNTD_ASSERT(min._value <= SIM_UINT_MAX / 2U);

		result =
		    (theta._value + sign * (int_fast64_t)phi._value) %
		    SIM_UINT_MAX;
		switch ((sign > 0) |
		        (theta._value > SIM_UINT_MAX / 2U) << 1U) {
		case 1U | (1U << 1U):
			result =
			    result > max._value ? max._value : result;
			break;

		case 1U | (0U << 1U):
			result =
			    result > max._value ? max._value : result;
			break;

		case 0U | (1U << 1U):
			result =
			    result > min._value ? result : min._value;
			break;

		case 0U | (0U << 1U):
			result =
			    result > min._value ? result : min._value;
			break;
		}
		return (sim_angle){result};
	}

	/**
	 * @todo Use a proper fixed point implementation of sin.
	 *
	 * [0, 2³²) → (-(2³¹ - 1), 2³¹ - 1)
	 */
	inline sim_int sin_quarter(sim_uint theta)
	{
		uintmax_t above_max = ((uintmax_t)SIM_UINT_MAX) + 1U;

		double dval =
		    theta * ((6.2831853071795864769252867665590 / 4.0) /
		             above_max);

		return sin(dval) * SIM_INT_MAX;
	}

	inline sim_int sim_sin(sim_angle angle)
	{
		sim_uint value = angle._value;

		uintmax_t above_max = ((uintmax_t)SIM_UINT_MAX) + 1U;

		uintmax_t rem = value % (above_max / 4U);

		unsigned char ii = value / (above_max / 4U);

		signed char rfactor = 2 * (int)(1U - (ii / 2U)) - 1;
		signed char ifactor = 2 * (int)(1U - ii % 2U) - 1;
		unsigned offset = ii % 2U;

		sim_uint theta = offset * (SIM_UINT_MAX / 4U) +
		                 ifactor * (intmax_t)rem;
		return rfactor *
		       sin_quarter((SIM_UINT_MAX * (uintmax_t)theta) /
		                   (SIM_UINT_MAX / 4U));
	}

	inline sim_int sim_cos(sim_angle angle)
	{
		sim_uint value = angle._value;

		uintmax_t above_max = ((uintmax_t)SIM_UINT_MAX) + 1U;

		uintmax_t rem = value % (above_max / 4U);

		unsigned char ii = (value / (above_max / 4U) + 1U) % 4U;

		signed char rfactor = 2 * (int)(1U - (ii / 2U)) - 1;
		signed char ifactor = 2 * (int)(1U - ii % 2U) - 1;
		unsigned offset = ii % 2U;

		sim_uint theta = offset * (SIM_UINT_MAX / 4U) +
		                 ifactor * (intmax_t)rem;
		return rfactor *
		       sin_quarter((SIM_UINT_MAX * (uintmax_t)theta) /
		                   (SIM_UINT_MAX / 4U));
	}

	inline sim_int sim_isatadd(sim_int x, sim_int y)
	{
		return saturate((int_fast64_t)x + y);
	}

	sim_int saturate(int_fast64_t x)
	{
		if (x > SIM_INT_MAX)
			return SIM_INT_MAX;

		if (x < SIM_INT_MIN)
			return SIM_INT_MIN;

		return x;
	}
}
