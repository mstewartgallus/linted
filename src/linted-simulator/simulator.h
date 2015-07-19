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

#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stdint.h>

typedef uint_least32_t linted_sim_uint;
typedef int_least32_t linted_sim_int;

#define LINTED_SIM_INT_MAX INT32_MAX
#define LINTED_SIM_INT_MIN INT32_MIN

#define LINTED_SIM_UINT_MAX UINT32_MAX

#define LINTED_SIM_Id PRId32

struct linted_sim__angle;
typedef struct linted_sim__angle linted_sim_angle;

/* Deliberately overflow in cases such as 1/1 */
#define LINTED_SIM_ANGLE(X, Y)                                         \
	{                                                              \
		._value =                                              \
		    (linted_sim_uint)(                                 \
		        (((uintmax_t)LINTED_SIM_UINT_MAX) + 1U) /      \
		        (Y)) *                                         \
		    (X)                                                \
	}

static linted_sim_angle linted_sim_angle_add(int sign,
                                             linted_sim_angle theta,
                                             linted_sim_angle phi);

static linted_sim_angle linted_sim_angle_add_clamped(
    int sign, linted_sim_angle min, linted_sim_angle max,
    linted_sim_angle theta, linted_sim_angle phi);

static linted_sim_int linted_sim__sin_quarter(linted_sim_uint theta);
static linted_sim_int linted_sim_sin(linted_sim_angle angle);
static linted_sim_int linted_sim_cos(linted_sim_angle angle);
static linted_sim_int linted_sim_isatadd(linted_sim_int x,
                                         linted_sim_int y);

static linted_sim_int linted_sim__saturate(int_fast64_t x);

struct linted_sim__angle {
	linted_sim_uint _value;
};

static inline linted_sim_angle
linted_sim_angle_add(int sign, linted_sim_angle theta,
                     linted_sim_angle phi)
{
	linted_sim_angle angle;
	angle._value =
	    (theta._value + sign * (int_fast64_t)phi._value) %
	    LINTED_SIM_UINT_MAX;
	return angle;
}

static inline linted_sim_angle linted_sim_angle_add_clamped(
    int sign, linted_sim_angle min, linted_sim_angle max,
    linted_sim_angle theta, linted_sim_angle phi)
{
	assert(max._value <= LINTED_SIM_UINT_MAX / 2U);
	assert(LINTED_SIM_UINT_MAX / 2U < min._value);

	linted_sim_uint result =
	    (theta._value + sign * (int_fast64_t)phi._value) %
	    LINTED_SIM_UINT_MAX;
	switch ((sign > 0) |
	        (theta._value > LINTED_SIM_UINT_MAX / 2U) << 1U) {
	case 1U | (1U << 1U):
		break;

	case 1U | (0U << 1U):
		result = result > max._value ? max._value : result;
		break;

	case 0U | (1U << 1U):
		result = result > min._value ? result : min._value;
		break;

	case 0U | (0U << 1U):
		break;
	}
	linted_sim_angle angle;
	angle._value = result;
	return angle;
}

/**
 * @todo Use a proper fixed point implementation of sin.
 *
 * [0, 2³²) → (-(2³¹ - 1), 2³¹ - 1)
 */
static inline linted_sim_int
linted_sim__sin_quarter(linted_sim_uint theta)
{
	uintmax_t above_max = ((uintmax_t)LINTED_SIM_UINT_MAX) + 1U;

	double dval =
	    theta *
	    ((6.2831853071795864769252867665590 / 4.0) / above_max);

	return sin(dval) * LINTED_SIM_INT_MAX;
}

static inline linted_sim_int linted_sim_sin(linted_sim_angle angle)
{
	linted_sim_uint value = angle._value;

	uintmax_t above_max = ((uintmax_t)LINTED_SIM_UINT_MAX) + 1U;

	uintmax_t rem = value % (above_max / 4U);

	unsigned char ii = value / (above_max / 4U);

	signed char rfactor = 2 * (int)(1U - (ii / 2U)) - 1;
	signed char ifactor = 2 * (int)(1U - ii % 2U) - 1;
	unsigned offset = ii % 2U;

	linted_sim_uint theta = offset * (LINTED_SIM_UINT_MAX / 4U) +
	                        ifactor * (intmax_t)rem;
	return rfactor * linted_sim__sin_quarter(
	                     (LINTED_SIM_UINT_MAX * (uintmax_t)theta) /
	                     (LINTED_SIM_UINT_MAX / 4U));
}

static inline linted_sim_int linted_sim_cos(linted_sim_angle angle)
{
	linted_sim_uint value = angle._value;

	uintmax_t above_max = ((uintmax_t)LINTED_SIM_UINT_MAX) + 1U;

	uintmax_t rem = value % (above_max / 4U);

	unsigned char ii = (value / (above_max / 4U) + 1U) % 4U;

	signed char rfactor = 2 * (int)(1U - (ii / 2U)) - 1;
	signed char ifactor = 2 * (int)(1U - ii % 2U) - 1;
	unsigned offset = ii % 2U;

	linted_sim_uint theta = offset * (LINTED_SIM_UINT_MAX / 4U) +
	                        ifactor * (intmax_t)rem;
	return rfactor * linted_sim__sin_quarter(
	                     (LINTED_SIM_UINT_MAX * (uintmax_t)theta) /
	                     (LINTED_SIM_UINT_MAX / 4U));
}

static inline linted_sim_int linted_sim_isatadd(linted_sim_int x,
                                                linted_sim_int y)
{
	return linted_sim__saturate((int_fast64_t)x + y);
}

static linted_sim_int linted_sim__saturate(int_fast64_t x)
{
	if (x > LINTED_SIM_INT_MAX)
		return LINTED_SIM_INT_MAX;

	if (x < LINTED_SIM_INT_MIN)
		return LINTED_SIM_INT_MIN;

	return x;
}
#endif /* LINTED_SIM_SIMULATOR_H */
