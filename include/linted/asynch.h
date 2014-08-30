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
#include "linted/queue.h"

#include <sys/types.h>

#if _POSIX_C_SOURCE >= 199309L
#include <time.h>
#endif

#if _POSIX_C_SOURCE >= 200809L
#include <sys/wait.h>
#endif

/**
 * @file
 *
 * Schedule tasks on kernel objects to be completed asynchronously.
 */

enum {
	LINTED_ASYNCH_TASK_POLL,
	LINTED_ASYNCH_TASK_READ,
	LINTED_ASYNCH_TASK_WRITE,
	LINTED_ASYNCH_TASK_MQ_RECEIVE,
	LINTED_ASYNCH_TASK_MQ_SEND,
	LINTED_ASYNCH_TASK_WAITID,
	LINTED_ASYNCH_TASK_ACCEPT,
	LINTED_ASYNCH_TASK_SLEEP_UNTIL
};

struct linted_asynch_pool;

struct linted_asynch_task
{
	struct linted_queue_node parent;
	linted_error errnum;
	unsigned type;
	unsigned task_action;
};

#if _POSIX_C_SOURCE >= 200809L
struct linted_asynch_task_waitid
{
	struct linted_asynch_task parent;
	siginfo_t info;
	idtype_t idtype;
	id_t id;
	int options;
};
#endif

#if _POSIX_C_SOURCE >= 199309L
struct linted_asynch_task_sleep_until
{
	struct linted_asynch_task parent;
	int flags;
	struct timespec request;
};
#endif

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks);
linted_error linted_asynch_pool_stop(struct linted_asynch_pool *pool);
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool);

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task);

void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);

linted_error linted_asynch_pool_wait(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp);

linted_error linted_asynch_pool_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_task **completionp);

void linted_asynch_task(struct linted_asynch_task *task, unsigned type,
                        unsigned task_action);

#if _POSIX_C_SOURCE >= 200809L
void linted_asynch_task_waitid(struct linted_asynch_task_waitid *task,
                               unsigned task_action, idtype_t idtype, id_t id,
                               int options);
#endif

#if _POSIX_C_SOURCE >= 199309L
void linted_asynch_task_sleep_until(struct linted_asynch_task_sleep_until *task,
                                    unsigned task_action, int flags,
                                    struct timespec const *request);
#endif

#endif /* LINTED_ASYNCH_H */
