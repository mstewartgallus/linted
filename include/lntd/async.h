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
#ifndef LNTD_ASYNCH_H
#define LNTD_ASYNCH_H

#include "lntd/error.h"
#include "lntd/ko.h"

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asyncronously.
 */

/* This can't be a POSIX real-time signal as those queue up so we can
 * end up queuing a barrage of signals that trap the thread were
 * waiting in signal handling.
 */
#define LNTD_ASYNCH_SIGNO SIGUSR1

struct lntd_async_pool;

enum { LNTD_ASYNCH_TASK_IDLE,
       LNTD_ASYNCH_TASK_POLL,
       LNTD_ASYNCH_TASK_READ,
       LNTD_ASYNCH_TASK_WRITE,
       LNTD_ASYNCH_TASK_SIGNAL_WAIT,
       LNTD_ASYNCH_TASK_SLEEP_UNTIL };
typedef unsigned char lntd_async_type;

struct lntd_async_task;
struct lntd_async_waiter;

union lntd_async_ck {
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

struct lntd_async_result {
	union lntd_async_ck task_ck;
	void *userstate;
	lntd_error err;
};

lntd_error lntd_async_pool_create(struct lntd_async_pool **poolp,
                                  unsigned max_tasks);
lntd_error lntd_async_pool_destroy(struct lntd_async_pool *pool);

void lntd_async_pool_resubmit(struct lntd_async_pool *pool,
                              struct lntd_async_task *task);
void lntd_async_pool_complete(struct lntd_async_pool *pool,
                              struct lntd_async_task *task,
                              lntd_error err);

void lntd_async_pool_wait_on_poll(struct lntd_async_pool *pool,
                                  struct lntd_async_waiter *waiter,
                                  struct lntd_async_task *task,
                                  lntd_ko ko, short flags);

lntd_error lntd_async_pool_wait(struct lntd_async_pool *pool,
                                struct lntd_async_result *resultp);

lntd_error lntd_async_pool_poll(struct lntd_async_pool *pool,
                                struct lntd_async_result *resultp);

lntd_error lntd_async_waiter_create(struct lntd_async_waiter **waiterp);
void lntd_async_waiter_destroy(struct lntd_async_waiter *waiter);
short lntd_async_waiter_revents(struct lntd_async_waiter *waiter);

lntd_error lntd_async_task_create(struct lntd_async_task **taskp,
                                  void *data, lntd_async_type type);
void lntd_async_task_destroy(struct lntd_async_task *task);

void lntd_async_task_cancel(struct lntd_async_task *task);
void lntd_async_task_submit(struct lntd_async_pool *pool,
                            struct lntd_async_task *task,
                            union lntd_async_ck task_ck,
                            void *userstate);

void *lntd_async_task_data(struct lntd_async_task *task);

#endif /* LNTD_ASYNCH_H */
