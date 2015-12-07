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
#ifndef LNTD_SCHED_H
#define LNTD_SCHED_H

#include "lntd/error.h"

/**
 * @file
 *
 * Task scheduling.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

typedef int lntd_sched_priority;

struct lntd_async_pool;
struct lntd_async_task;

struct lntd_sched_task_idle;
struct lntd_sched_task_sleep_until;

struct timespec;

lntd_error lntd_sched_getpriority(lntd_sched_priority *priorityp);

lntd_error lntd_sched_time(struct timespec *data);

lntd_error
lntd_sched_task_idle_create(struct lntd_sched_task_idle **taskp,
                            void *data);
void lntd_sched_task_idle_destroy(struct lntd_sched_task_idle *task);

void *lntd_sched_task_idle_data(struct lntd_sched_task_idle *task);
struct lntd_async_task *
lntd_sched_task_idle_to_async(struct lntd_sched_task_idle *task);
struct lntd_async_task *
lntd_sched_task_idle_prepare(struct lntd_sched_task_idle *task,
                             union lntd_async_ck task_ck,
                             void *userstate);

lntd_error lntd_sched_task_sleep_until_create(
    struct lntd_sched_task_sleep_until **taskp, void *data);
void lntd_sched_task_sleep_until_destroy(
    struct lntd_sched_task_sleep_until *task);

struct lntd_async_task *lntd_sched_task_sleep_until_prepare(
    struct lntd_sched_task_sleep_until *task,
    union lntd_async_ck task_ck, void *userstate,
    struct timespec const *req);
void lntd_sched_task_sleep_until_time(
    struct lntd_sched_task_sleep_until *task, struct timespec *time);

void *lntd_sched_task_sleep_until_data(
    struct lntd_sched_task_sleep_until *task);
struct lntd_async_task *lntd_sched_task_sleep_until_to_async(
    struct lntd_sched_task_sleep_until *task);

void lntd_sched_do_idle(struct lntd_async_pool *pool,
                        struct lntd_async_task *task);
void lntd_sched_do_sleep_until(struct lntd_async_pool *pool,
                               struct lntd_async_task *task);

static inline void lntd_sched_light_yield(void);

#if defined __i386__ || defined __amd64__
static inline void lntd_sched_light_yield(void)
{
	__asm__ __volatile__("pause");
}
#elif defined __arm__ || defined __aarch64__
static inline void lntd_sched_light_yield(void)
{
	__asm__ __volatile__("yield");
}
#elif defined __powerpc__
static inline void lntd_sched_light_yield(void)
{
	/* Do nothing */
}
#endif

#endif /* LNTD_SCHED_H */
