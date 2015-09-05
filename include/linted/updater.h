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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LINTED_UPDATER_H
#define LINTED_UPDATER_H

#include "linted/async.h"
#include "linted/error.h"
#include "linted/ko.h"

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
typedef linted_ko linted_updater;

typedef int_least32_t linted_updater_int;
typedef uint_least32_t linted_updater_uint;

#define LINTED_UPDATER_UINT_MAX UINT32_MAX

struct linted_updater__angle;
typedef struct linted_updater__angle linted_updater_angle;

struct linted_updater_update;
struct linted_updater_task_send;
struct linted_updater_task_receive;

/* Deliberately overflow in cases such as 1/1 */
#define LINTED_UPDATER_ANGLE(X, Y)                                     \
	{                                                              \
		._value =                                              \
		    (linted_updater_uint)(                             \
		        (((uintmax_t)LINTED_UPDATER_UINT_MAX) + 1U) /  \
		        (Y)) *                                         \
		    (X)                                                \
	}

static double
linted_updater_angle_to_double(linted_updater_angle theta);

linted_error linted_updater_task_receive_create(
    struct linted_updater_task_receive **taskp, void *data);
void linted_updater_task_receive_destroy(
    struct linted_updater_task_receive *task);

void linted_updater_task_receive_prepare(
    struct linted_updater_task_receive *task,
    union linted_async_ck task_ck, linted_ko updater);
struct linted_async_task *linted_updater_task_receive_to_async(
    struct linted_updater_task_receive *task);
struct linted_updater_task_receive *
linted_updater_task_receive_from_async(struct linted_async_task *task);
void *linted_updater_task_receive_data(
    struct linted_updater_task_receive *task);

linted_error
linted_updater_task_send_create(struct linted_updater_task_send **taskp,
                                void *data);
void linted_updater_task_send_destroy(
    struct linted_updater_task_send *task);

void linted_updater_task_send_prepare(
    struct linted_updater_task_send *task,
    union linted_async_ck task_ck, linted_ko updater,
    struct linted_updater_update const *update);
struct linted_async_task *linted_updater_task_send_to_async(
    struct linted_updater_task_send *task);
struct linted_updater_task_send *
linted_updater_task_send_from_async(struct linted_async_task *task);
void *
linted_updater_task_send_data(struct linted_updater_task_send *task);

void linted_updater_decode(
    struct linted_updater_task_receive const *task,
    struct linted_updater_update *update);

struct linted_updater__angle {
	linted_updater_uint _value;
};

struct linted_updater_update {
	linted_updater_int x_position;
	linted_updater_int y_position;
	linted_updater_int z_position;

	linted_updater_angle z_rotation;
	linted_updater_angle x_rotation;
};

static inline double
linted_updater_angle_to_double(linted_updater_angle theta)
{
	uintmax_t above_max = ((uintmax_t)LINTED_UPDATER_UINT_MAX) + 1U;
	return theta._value *
	       (6.2831853071795864769252867665590 / above_max);
}

#endif /* LINTED_UPDATER_H */
