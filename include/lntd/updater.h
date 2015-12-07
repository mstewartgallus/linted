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
#ifndef LNTD_UPDATER_H
#define LNTD_UPDATER_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stdint.h>

/**
 * @file
 *
 * Exposes a protocol for updating a GUI on the progress of a
 * simulator.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

/**
 * A handle to access the updater. Is safe to share between processes.
 */
typedef lntd_ko lntd_updater;

typedef int_least32_t lntd_updater_int;
typedef uint_least32_t lntd_updater_uint;

struct lntd_updater__angle;
typedef struct lntd_updater__angle lntd_updater_angle;

struct lntd_updater_update;
struct lntd_updater_task_send;
struct lntd_updater_task_recv;

static double lntd_updater_angle_to_double(lntd_updater_angle theta);

lntd_error
lntd_updater_task_recv_create(struct lntd_updater_task_recv **taskp,
                              void *data);
void lntd_updater_task_recv_destroy(
    struct lntd_updater_task_recv *task);

struct lntd_async_task *
lntd_updater_task_recv_prepare(struct lntd_updater_task_recv *task,
                               union lntd_async_ck task_ck,
                               void *userstate, lntd_ko updater);
struct lntd_async_task *
lntd_updater_task_recv_to_async(struct lntd_updater_task_recv *task);
void *lntd_updater_task_recv_data(struct lntd_updater_task_recv *task);

lntd_error
lntd_updater_task_send_create(struct lntd_updater_task_send **taskp,
                              void *data);
void lntd_updater_task_send_destroy(
    struct lntd_updater_task_send *task);

struct lntd_async_task *lntd_updater_task_send_prepare(
    struct lntd_updater_task_send *task, union lntd_async_ck task_ck,
    void *userstate, lntd_ko updater,
    struct lntd_updater_update const *update);
struct lntd_async_task *
lntd_updater_task_send_to_async(struct lntd_updater_task_send *task);
void *lntd_updater_task_send_data(struct lntd_updater_task_send *task);

void lntd_updater_decode(struct lntd_updater_task_recv const *task,
                         struct lntd_updater_update *update);

#define LNTD_UPDATER_UINT_MAX UINT32_MAX

struct lntd_updater__angle {
	lntd_updater_uint _value;
};

/* Deliberately overflow in cases such as 1/1 */
#define LNTD_UPDATER_ANGLE(X, Y)                                       \
	{                                                              \
		._value =                                              \
		    (lntd_updater_uint)(                               \
		        (((uintmax_t)LNTD_UPDATER_UINT_MAX) + 1U) /    \
		        (Y)) *                                         \
		    (X)                                                \
	}

struct lntd_updater_update {
	lntd_updater_int x_position;
	lntd_updater_int y_position;
	lntd_updater_int z_position;

	lntd_updater_angle z_rotation;
	lntd_updater_angle x_rotation;
};

static inline double
lntd_updater_angle_to_double(lntd_updater_angle theta)
{
	uintmax_t above_max = ((uintmax_t)LNTD_UPDATER_UINT_MAX) + 1U;
	return theta._value *
	       (6.2831853071795864769252867665590 / above_max);
}

#endif /* LNTD_UPDATER_H */
