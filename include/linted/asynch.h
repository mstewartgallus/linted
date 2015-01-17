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
#ifndef LINTED_ASYNCH_H
#define LINTED_ASYNCH_H

#include "linted/error.h"
#include "linted/ko.h"

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asynchronously.
 */

/* This can't be a POSIX real-time signal as those queue up so we can
 * end up queuing a barrage of signals that trap the thread were
 * waiting in signal handling.
 */
#define LINTED_ASYNCH_SIGNO SIGUSR1

struct linted_asynch_pool;

enum {
	LINTED_ASYNCH_TASK_IDLE,
	LINTED_ASYNCH_TASK_POLL,
	LINTED_ASYNCH_TASK_READ,
	LINTED_ASYNCH_TASK_WRITE,
	LINTED_ASYNCH_TASK_SIGWAITINFO,
	LINTED_ASYNCH_TASK_WAITID,
	LINTED_ASYNCH_TASK_SLEEP_UNTIL,
	LINTED_ASYNCH_TASK_RECV,
	LINTED_ASYNCH_TASK_SENDTO,
	LINTED_ASYNCH_TASK_ACCEPT
};

struct linted_asynch_task;
struct linted_asynch_waiter;

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks);
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool);

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task);
void linted_asynch_pool_resubmit(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);
void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task,
                                 linted_error errnum);

void linted_asynch_pool_wait_on_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_waiter *waiter,
                                     struct linted_asynch_task *task,
                                     linted_ko ko, short flags);

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp);

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp);

linted_error linted_asynch_waiter_create(struct linted_asynch_waiter **waiterp);
void linted_asynch_waiter_destroy(struct linted_asynch_waiter *waiter);
short linted_asynch_waiter_revents(struct linted_asynch_waiter *waiter);

linted_error linted_asynch_task_create(struct linted_asynch_task **taskp,
                                       void *data, unsigned type);
void linted_asynch_task_destroy(struct linted_asynch_task *task);

void linted_asynch_task_cancel(struct linted_asynch_task *task);
void linted_asynch_task_prepare(struct linted_asynch_task *task,
                                unsigned task_action);
unsigned linted_asynch_task_action(struct linted_asynch_task *task);
linted_error linted_asynch_task_errnum(struct linted_asynch_task *task);

void *linted_asynch_task_data(struct linted_asynch_task *task);

#endif /* LINTED_ASYNCH_H */
