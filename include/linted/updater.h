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
#include "linted/ko.h"
#include "linted/sim.h"

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

struct linted_updater_update
{
	linted_sim_int x_position;
	linted_sim_int y_position;
	linted_sim_int z_position;

	linted_sim_angle x_rotation;
	linted_sim_angle y_rotation;
};

struct linted_updater_task_send;
struct linted_updater_task_receive;

linted_error
linted_updater_task_receive_create(struct linted_updater_task_receive **taskp,
                                   void *data);
void
linted_updater_task_receive_destroy(struct linted_updater_task_receive *task);

void
linted_updater_task_receive_prepare(struct linted_updater_task_receive *task,
                                    unsigned task_action, linted_ko updater);
struct linted_asynch_task *
linted_updater_task_receive_to_asynch(struct linted_updater_task_receive *task);
struct linted_updater_task_receive *
linted_updater_task_receive_from_asynch(struct linted_asynch_task *task);
void *
linted_updater_task_receive_data(struct linted_updater_task_receive *task);

linted_error
linted_updater_task_send_create(struct linted_updater_task_send **taskp,
                                void *data);
void linted_updater_task_send_destroy(struct linted_updater_task_send *task);

void
linted_updater_task_send_prepare(struct linted_updater_task_send *task,
                                 unsigned task_action, linted_ko updater,
                                 struct linted_updater_update const *update);
struct linted_asynch_task *
linted_updater_task_send_to_asynch(struct linted_updater_task_send *task);
struct linted_updater_task_send *
linted_updater_task_send_from_asynch(struct linted_asynch_task *task);
void *linted_updater_task_send_data(struct linted_updater_task_send *task);

void linted_updater_decode(struct linted_updater_task_receive const *task,
                           struct linted_updater_update *update);

#endif /* LINTED_UPDATER_H */
