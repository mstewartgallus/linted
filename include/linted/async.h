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
#ifndef LINTED_ASYNCH_H
#define LINTED_ASYNCH_H

#include "linted/error.h"
#include "linted/ko.h"

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asyncronously.
 */

/* This can't be a POSIX real-time signal as those queue up so we can
 * end up queuing a barrage of signals that trap the thread were
 * waiting in signal handling.
 */
#define LINTED_ASYNCH_SIGNO SIGUSR1

struct linted_async_pool;

enum { LINTED_ASYNCH_TASK_IDLE,
       LINTED_ASYNCH_TASK_POLL,
       LINTED_ASYNCH_TASK_READ,
       LINTED_ASYNCH_TASK_WRITE,
       LINTED_ASYNCH_TASK_SIGNAL_WAIT,
       LINTED_ASYNCH_TASK_SLEEP_UNTIL };
typedef unsigned char linted_async_type;

struct linted_async_task;
struct linted_async_waiter;

union linted_async_ck {
	void *ptr;
	/* By C standards at least 8 bits long */
	unsigned char u8;
	/* By C standards at least 16 bits long */
	unsigned short u16;
	/* By C standards at least 32 bits long */
	unsigned long u32;
	/* By C standards at least 64 bits long */
	unsigned long long u64;
};

struct linted_async_result {
	union linted_async_ck task_ck;
	struct linted_async_task *task;
	linted_error err;
};

linted_error linted_async_pool_create(struct linted_async_pool **poolp,
                                      unsigned max_tasks);
linted_error linted_async_pool_destroy(struct linted_async_pool *pool);

void linted_async_pool_submit(struct linted_async_pool *pool,
                              struct linted_async_task *task);
void linted_async_pool_resubmit(struct linted_async_pool *pool,
                                struct linted_async_task *task);
void linted_async_pool_complete(struct linted_async_pool *pool,
                                struct linted_async_task *task,
                                linted_error err);

void linted_async_pool_wait_on_poll(struct linted_async_pool *pool,
                                    struct linted_async_waiter *waiter,
                                    struct linted_async_task *task,
                                    linted_ko ko, short flags);

linted_error
linted_async_pool_wait(struct linted_async_pool *pool,
                       struct linted_async_result *resultp);

linted_error
linted_async_pool_poll(struct linted_async_pool *pool,
                       struct linted_async_result *resultp);

linted_error
linted_async_waiter_create(struct linted_async_waiter **waiterp);
void linted_async_waiter_destroy(struct linted_async_waiter *waiter);
short linted_async_waiter_revents(struct linted_async_waiter *waiter);

linted_error linted_async_task_create(struct linted_async_task **taskp,
                                      void *data,
                                      linted_async_type type);
void linted_async_task_destroy(struct linted_async_task *task);

void linted_async_task_cancel(struct linted_async_task *task);
struct linted_async_task *
linted_async_task_prepare(struct linted_async_task *task,
                          union linted_async_ck task_ck);

void *linted_async_task_data(struct linted_async_task *task);

#endif /* LINTED_ASYNCH_H */
