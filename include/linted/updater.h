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
#ifndef LINTED_UPDATER_H
#define LINTED_UPDATER_H

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/mq.h"
#include "linted/rpc.h"

#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stdint.h>

/**
 * @file
 *
 * Exposes a protocol for updating a GUI on the progress of a
 * simulator.
 */

/**
 * A handle to access the updater. Is safe to share between processes.
 */
typedef linted_mq linted_updater;

typedef uint_least32_t linted_updater_uint;
typedef int_least32_t linted_updater_int;

typedef struct linted_updater__angle
{
    linted_updater_uint _value;
} linted_updater_angle;

struct linted_updater_update
{
    linted_updater_int x_position;
    linted_updater_int y_position;
    linted_updater_int z_position;

    linted_updater_angle x_rotation;
    linted_updater_angle y_rotation;
};

struct linted_updater_task_send
{
    struct linted_asynch_task_mq_send parent;
    char message
        [LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE
         + LINTED_RPC_UINT32_SIZE + LINTED_RPC_UINT32_SIZE];
};

struct linted_updater_task_receive
{
    struct linted_asynch_task_mq_receive parent;
    char message
        [LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE
         + LINTED_RPC_UINT32_SIZE + LINTED_RPC_UINT32_SIZE];
};

#define LINTED_UPDATER_UINT_MAX UINT32_MAX

#define LINTED_UPDATER_INT_MAX INT32_MAX
#define LINTED_UPDATER_INT_MIN INT32_MIN

#define LINTED_UPDATER_Id PRIdLEAST32

#define LINTED_UPDATER_ANGLE(X, Y)                                             \
    {                                                                          \
        ._value = (((uintmax_t)LINTED_UPDATER_UINT_MAX) / (Y)) * (X)           \
    }

static linted_updater_int linted_updater__sin_first_half(linted_updater_uint x);
static linted_updater_int
linted_updater__sin_first_quadrant(linted_updater_uint angle);

static linted_updater_int linted_updater__saturate(int_fast64_t x);

linted_error linted_updater_create(linted_updater *updaterp, int flags);

void linted_updater_send(struct linted_updater_task_send *task, int task_id,
                         linted_updater updater,
                         struct linted_updater_update const *update);

void linted_updater_receive(struct linted_updater_task_receive *task,
                            int task_id, linted_updater updater);

void linted_updater_decode(struct linted_updater_task_receive const *task,
                           struct linted_updater_update *update);

static inline float linted_updater_angle_to_float(linted_updater_angle theta)
{
    return theta._value * (2 * 3.14159265358979323846264338327 / UINT32_MAX);
}

static inline linted_updater_angle
linted_updater_angle_add(int sign, linted_updater_angle theta,
                         linted_updater_angle phi)
{
    linted_updater_angle angle;
    angle._value = (theta._value + sign * (int_fast64_t)phi._value)
                   % UINT32_MAX;
    return angle;
}

static inline linted_updater_angle linted_updater_angle_add_clamped(
    int sign, linted_updater_angle min, linted_updater_angle max,
    linted_updater_angle theta, linted_updater_angle phi)
{
    assert(max._value <= LINTED_UPDATER_UINT_MAX / 2u);
    assert(LINTED_UPDATER_UINT_MAX / 2u < min._value);

    linted_updater_uint result
        = (theta._value + sign * (int_fast64_t)phi._value)
          % LINTED_UPDATER_UINT_MAX;
    switch ((sign > 0) | (theta._value > LINTED_UPDATER_UINT_MAX / 2u) << 1u) {
    case 1u | (1u << 1u) :
        break;

    case 1u | (0u << 1u) :
        result = result > max._value ? max._value : result;
        break;

    case 0u | (1u << 1u) :
        result = result > min._value ? result : min._value;
        break;

    case 0u | (0u << 1u) :
        break;
    }
    linted_updater_angle angle;
    angle._value = result;
    return angle;
}

/**
 * @bug Key points such as 3/4 aren't quite correct
 */
static inline linted_updater_int linted_updater_sin(linted_updater_angle angle)
{
    linted_updater_uint x = angle._value;

    if (x >= LINTED_UPDATER_UINT_MAX / 2u) {
        return
            -linted_updater__sin_first_half(x - LINTED_UPDATER_UINT_MAX / 2u);
    }
    return linted_updater__sin_first_half(x);
}

static inline linted_updater_int linted_updater_cos(linted_updater_angle angle)
{
    linted_updater_angle x
        = { ._value = angle._value + LINTED_UPDATER_UINT_MAX / 4u };
    return linted_updater_sin(x);
}

static inline linted_updater_int
linted_updater__sin_first_half(linted_updater_uint x)
{
    if (x > LINTED_UPDATER_UINT_MAX / 4u) {
        x = LINTED_UPDATER_UINT_MAX / 2u - x;
    }

    return linted_updater__sin_first_quadrant(x);
}

// This should always be positive
static inline linted_updater_int
linted_updater__sin_first_quadrant(linted_updater_uint angle)
{
    uintmax_t x = angle * 6u;
    uintmax_t max = LINTED_UPDATER_INT_MAX;

    /* Approximate with a Taylor series */
    return x - (x * x * x) / (6u * max * max)
           + (x * x * x * x * x) / (120u * max * max * max * max)
           - (x * x * x * x * x * x * x)
             / (5040u * max * max * max * max * max * max);
}

static inline linted_updater_int linted_updater_isatadd(linted_updater_int x,
                                                        linted_updater_int y)
{
    return linted_updater__saturate((int_fast64_t)x + y);
}

static linted_updater_int linted_updater__saturate(int_fast64_t x)
{
    if (x > LINTED_UPDATER_INT_MAX) {
        return LINTED_UPDATER_INT_MAX;
    }

    if (x < LINTED_UPDATER_INT_MIN) {
        return LINTED_UPDATER_INT_MIN;
    }

    return x;
}

#endif /* LINTED_UPDATER_H */
