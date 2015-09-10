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
#ifndef LINTED_SIM_SIMULATOR_H
#define LINTED_SIM_SIMULATOR_H

#include "linted/util.h"

#include <inttypes.h>
#include <math.h>
#include <stdint.h>

typedef uint_least32_t sim_uint;
typedef int_least32_t sim_int;

#define SIM_INT_MAX INT32_MAX
#define SIM_INT_MIN INT32_MIN

#define SIM_UINT_MAX UINT32_MAX

#define SIM_Id PRId32

struct sim__angle;
typedef struct sim__angle sim_angle;

/* Deliberately overflow in cases such as 1/1 */
#define SIM_ANGLE(X, Y)                                                \
	{                                                              \
		._value =                                              \
		    (sim_uint)((((uintmax_t)SIM_UINT_MAX) + 1U) /      \
		               (Y)) *                                  \
		    (X)                                                \
	}

static sim_angle sim_angle_add(int sign, sim_angle theta,
                               sim_angle phi);

static sim_angle sim_angle_add_clamped(int sign, sim_angle min,
                                       sim_angle max, sim_angle theta,
                                       sim_angle phi);

static sim_int sim__sin_quarter(sim_uint theta);
static sim_int sim_sin(sim_angle angle);
static sim_int sim_cos(sim_angle angle);
static sim_int sim_isatadd(sim_int x, sim_int y);

static sim_int sim__saturate(int_fast64_t x);

struct sim__angle {
	sim_uint _value;
};

static inline sim_angle sim_angle_add(int sign, sim_angle theta,
                                      sim_angle phi)
{
	sim_angle angle;
	angle._value =
	    (theta._value + sign * (int_fast64_t)phi._value) %
	    SIM_UINT_MAX;
	return angle;
}

static inline sim_angle sim_angle_add_clamped(int sign, sim_angle min,
                                              sim_angle max,
                                              sim_angle theta,
                                              sim_angle phi)
{
	LINTED_ASSERT(max._value <= SIM_UINT_MAX / 2U);
	LINTED_ASSERT(min._value <= SIM_UINT_MAX / 2U);

	sim_uint result =
	    (theta._value + sign * (int_fast64_t)phi._value) %
	    SIM_UINT_MAX;
	switch ((sign > 0) | (theta._value > SIM_UINT_MAX / 2U) << 1U) {
	case 1U | (1U << 1U):
		result = result > max._value ? max._value : result;
		break;

	case 1U | (0U << 1U):
		result = result > max._value ? max._value : result;
		break;

	case 0U | (1U << 1U):
		result = result > min._value ? result : min._value;
		break;

	case 0U | (0U << 1U):
		result = result > min._value ? result : min._value;
		break;
	}
	sim_angle angle;
	angle._value = result;
	return angle;
}

/**
 * @todo Use a proper fixed point implementation of sin.
 *
 * [0, 2³²) → (-(2³¹ - 1), 2³¹ - 1)
 */
static inline sim_int sim__sin_quarter(sim_uint theta)
{
	uintmax_t above_max = ((uintmax_t)SIM_UINT_MAX) + 1U;

	double dval =
	    theta *
	    ((6.2831853071795864769252867665590 / 4.0) / above_max);

	return sin(dval) * SIM_INT_MAX;
}

static inline sim_int sim_sin(sim_angle angle)
{
	sim_uint value = angle._value;

	uintmax_t above_max = ((uintmax_t)SIM_UINT_MAX) + 1U;

	uintmax_t rem = value % (above_max / 4U);

	unsigned char ii = value / (above_max / 4U);

	signed char rfactor = 2 * (int)(1U - (ii / 2U)) - 1;
	signed char ifactor = 2 * (int)(1U - ii % 2U) - 1;
	unsigned offset = ii % 2U;

	sim_uint theta =
	    offset * (SIM_UINT_MAX / 4U) + ifactor * (intmax_t)rem;
	return rfactor *
	       sim__sin_quarter((SIM_UINT_MAX * (uintmax_t)theta) /
	                        (SIM_UINT_MAX / 4U));
}

static inline sim_int sim_cos(sim_angle angle)
{
	sim_uint value = angle._value;

	uintmax_t above_max = ((uintmax_t)SIM_UINT_MAX) + 1U;

	uintmax_t rem = value % (above_max / 4U);

	unsigned char ii = (value / (above_max / 4U) + 1U) % 4U;

	signed char rfactor = 2 * (int)(1U - (ii / 2U)) - 1;
	signed char ifactor = 2 * (int)(1U - ii % 2U) - 1;
	unsigned offset = ii % 2U;

	sim_uint theta =
	    offset * (SIM_UINT_MAX / 4U) + ifactor * (intmax_t)rem;
	return rfactor *
	       sim__sin_quarter((SIM_UINT_MAX * (uintmax_t)theta) /
	                        (SIM_UINT_MAX / 4U));
}

static inline sim_int sim_isatadd(sim_int x, sim_int y)
{
	return sim__saturate((int_fast64_t)x + y);
}

static sim_int sim__saturate(int_fast64_t x)
{
	if (x > SIM_INT_MAX)
		return SIM_INT_MAX;

	if (x < SIM_INT_MIN)
		return SIM_INT_MIN;

	return x;
}
#endif /* LINTED_SIM_SIMULATOR_H */
