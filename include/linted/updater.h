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

#include "linted/error.h"
#include "linted/rpc.h"

#include <mqueue.h>
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
typedef mqd_t linted_updater;

typedef uint_fast32_t linted_updater_uint_fast;
typedef int_fast32_t linted_updater_int_fast;

struct linted_updater_update
{
    linted_updater_int_fast x_position;
    linted_updater_int_fast y_position;
    linted_updater_int_fast z_position;

    linted_updater_uint_fast x_rotation;
    linted_updater_uint_fast y_rotation;
};

struct linted_updater_task
{
    union linted_asynch_task asynch_task;
    char message[LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes)
                 + LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes)
                 + LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes)
                 + LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes)
                 + LINTED_SIZEOF_MEMBER(struct linted_rpc_uint32, bytes)];
};

linted_error linted_updater_pair(linted_updater updater[2], int rflags,
                                 int wflags);

void linted_updater_send(struct linted_asynch_pool* pool, int task_id,
                         linted_updater updater,
                         struct linted_updater_update const* update,
                         struct linted_updater_task* task);

void linted_updater_receive(struct linted_asynch_pool* pool, int task_id,
                            linted_updater updater,
                            struct linted_updater_task* task);

void linted_updater_decode(struct linted_updater_task const* task,
                           struct linted_updater_update* update);

linted_error linted_updater_close(linted_updater updater);

#endif /* LINTED_UPDATER_H */
