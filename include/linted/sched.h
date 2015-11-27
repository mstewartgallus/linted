/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#ifndef LINTED_SCHED_H
#define LINTED_SCHED_H

#include "linted/error.h"

/**
 * @file
 *
 * Task scheduling.
 */

struct linted_async_pool;
struct linted_async_task;
union linted_async_ck;

typedef int linted_sched_priority;

struct linted_async_pool;
struct linted_async_task;

struct linted_sched_task_idle;
struct linted_sched_task_sleep_until;

struct timespec;

linted_error linted_sched_getpriority(linted_sched_priority *priorityp);

linted_error linted_sched_time(struct timespec *data);

linted_error
linted_sched_task_idle_create(struct linted_sched_task_idle **taskp,
                              void *data);
void linted_sched_task_idle_destroy(
    struct linted_sched_task_idle *task);

void *linted_sched_task_idle_data(struct linted_sched_task_idle *task);
struct linted_async_task *
linted_sched_task_idle_to_async(struct linted_sched_task_idle *task);
struct linted_async_task *
linted_sched_task_idle_prepare(struct linted_sched_task_idle *task,
                               union linted_async_ck task_ck,
                               void *userstate);

linted_error linted_sched_task_sleep_until_create(
    struct linted_sched_task_sleep_until **taskp, void *data);
void linted_sched_task_sleep_until_destroy(
    struct linted_sched_task_sleep_until *task);

struct linted_async_task *linted_sched_task_sleep_until_prepare(
    struct linted_sched_task_sleep_until *task,
    union linted_async_ck task_ck, void *userstate,
    struct timespec const *req);
void linted_sched_task_sleep_until_time(
    struct linted_sched_task_sleep_until *task, struct timespec *time);

void *linted_sched_task_sleep_until_data(
    struct linted_sched_task_sleep_until *task);
struct linted_async_task *linted_sched_task_sleep_until_to_async(
    struct linted_sched_task_sleep_until *task);

void linted_sched_do_idle(struct linted_async_pool *pool,
                          struct linted_async_task *task);
void linted_sched_do_sleep_until(struct linted_async_pool *pool,
                                 struct linted_async_task *task);

static inline void linted_sched_light_yield(void);

#if defined __i386__ || defined __amd64__
static inline void linted_sched_light_yield(void)
{
	__asm__ __volatile__("pause");
}
#elif defined __arm__ || defined __aarch64__
static inline void linted_sched_light_yield(void)
{
	__asm__ __volatile__("yield");
}
#elif defined __powerpc__
static inline void linted_sched_light_yield(void)
{
	/* Do nothing */
}
#endif

#endif /* LINTED_SCHED_H */
