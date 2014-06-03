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
#include <math.h>
#include <stdint.h>

/**
 * @file
 *
 * Exposes a protocol for updating a GUI on the progress of a
 * simulator.
 */

#define LINTED_UPDATER_UINT_MAX UINT32_MAX

#define LINTED_UPDATER_INT_MAX INT32_MAX
#define LINTED_UPDATER_INT_MIN INT32_MIN

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
    char message[LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes)];
};

struct linted_updater_task_receive
{
    struct linted_asynch_task_mq_receive parent;
    char message[LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes) +
                 LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes)];
};

static linted_updater_int linted_updater__saturate(int_fast64_t x);

linted_error linted_updater_pair(linted_updater updater[2], int flags);

void linted_updater_send(struct linted_updater_task_send *task, int task_id,
                         linted_updater updater,
                         struct linted_updater_update const *update);

void linted_updater_receive(struct linted_updater_task_receive *task,
                            int task_id, linted_updater updater);

void linted_updater_decode(struct linted_updater_task_receive const *task,
                           struct linted_updater_update *update);

static inline float linted_updater_angle_to_float(linted_updater_angle theta)
{
    return theta._value * (2 * acosf(-1.0f) / UINT32_MAX);
}

static inline linted_updater_angle linted_updater_angle_add(int sign,
                                                            linted_updater_angle theta,
                                                            linted_updater_angle phi)
{
    linted_updater_angle angle;
    angle._value = (theta._value + sign * (int_fast64_t)phi._value) % UINT32_MAX;
    return angle;
}

static inline linted_updater_angle linted_updater_angle_add_clamped(int sign,
                                                                    linted_updater_angle min,
                                                                    linted_updater_angle max,
                                                                    linted_updater_angle theta,
                                                                    linted_updater_angle phi)
{
    assert(max._value <= LINTED_UPDATER_UINT_MAX / 2u);
    assert(LINTED_UPDATER_UINT_MAX / 2u < min._value);

    linted_updater_uint result = (theta._value + sign * (int_fast64_t)phi._value) % LINTED_UPDATER_UINT_MAX;
    switch ((sign > 0) | (theta._value > LINTED_UPDATER_UINT_MAX / 2u) << 1u) {
    case 1u | (1u << 1u):
        break;

    case 1u | (0u << 1u):
        result = result > max._value ? max._value : result;
        break;

    case 0u | (1u << 1u):
        result = result > min._value ? result : min._value;
        break;

    case 0u | (0u << 1u):
        break;
    }
    linted_updater_angle angle;
    angle._value = result;
    return angle;
}

static inline linted_updater_angle linted_updater_angle_from_frac(linted_updater_uint x,
                                                                  linted_updater_uint y)
{
    linted_updater_angle angle;
    angle._value = (LINTED_UPDATER_UINT_MAX / y) * x;
    return angle;
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
