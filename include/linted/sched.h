/*
 * Copyright 2014 Steven Stewart-Gallus
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
#ifndef LINTED_SCHED_H
#define LINTED_SCHED_H

#include "linted/asynch.h"
#include "linted/error.h"

#if _POSIX_C_SOURCE >= 199309L
#include <signal.h>
#endif

#include <sys/types.h>

#if _POSIX_C_SOURCE >= 199309L
#include <time.h>
#endif

/**
 * @file
 *
 * Task scheduling.
 */

struct linted_sched_task_idle;
struct linted_sched_task_sleep_until;

linted_error
linted_sched_task_idle_create(struct linted_sched_task_idle **taskp,
                              void *data);
void linted_sched_task_idle_destroy(struct linted_sched_task_idle *task);

void *linted_sched_task_idle_data(struct linted_sched_task_idle *task);
struct linted_asynch_task *
linted_sched_task_idle_to_asynch(struct linted_sched_task_idle *task);
struct linted_sched_task_idle *
linted_sched_task_idle_from_asynch(struct linted_asynch_task *task);
void linted_sched_task_idle_prepare(struct linted_sched_task_idle *task,
                                    unsigned task_action);

linted_error linted_sched_task_sleep_until_create(
    struct linted_sched_task_sleep_until **taskp, void *data);
void linted_sched_task_sleep_until_destroy(
    struct linted_sched_task_sleep_until *task);

#if _POSIX_C_SOURCE >= 199309L
void linted_sched_task_sleep_until_prepare(
    struct linted_sched_task_sleep_until *task, unsigned task_action, int flags,
    struct timespec const *req);
void linted_sched_task_sleep_until_request(
    struct linted_sched_task_sleep_until *task, struct timespec *req);
#endif
void *
linted_sched_task_sleep_until_data(struct linted_sched_task_sleep_until *task);
struct linted_asynch_task *linted_sched_task_sleep_until_to_asynch(
    struct linted_sched_task_sleep_until *task);
struct linted_sched_task_sleep_until *
linted_sched_task_sleep_until_from_asynch(struct linted_asynch_task *task);

void linted_sched_do_idle(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);
void linted_sched_do_sleep_until(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);
#endif /* LINTED_SCHED_H */