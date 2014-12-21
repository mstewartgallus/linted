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

#if _POSIX_C_SOURCE >= 199309L
#include <signal.h>
#endif

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

struct linted_asynch_pool;

enum {
	LINTED_ASYNCH_TASK_POLL,
	LINTED_ASYNCH_TASK_READ,
	LINTED_ASYNCH_TASK_WRITE,
	LINTED_ASYNCH_TASK_SIGWAITINFO,
	LINTED_ASYNCH_TASK_MQ_RECEIVE,
	LINTED_ASYNCH_TASK_MQ_SEND,
	LINTED_ASYNCH_TASK_WAITID,
	LINTED_ASYNCH_TASK_SLEEP_UNTIL,
	LINTED_ASYNCH_TASK_RECV,
	LINTED_ASYNCH_TASK_ACCEPT
};

struct linted_asynch_task;
struct linted_asynch_task_waitid;
struct linted_asynch_task_sigwaitinfo;
struct linted_asynch_task_sleep_until;

struct linted_asynch_waiter;

linted_error linted_asynch_pool_create(struct linted_asynch_pool **poolp,
                                       unsigned max_tasks);
linted_error linted_asynch_pool_stop(struct linted_asynch_pool *pool);
linted_error linted_asynch_pool_destroy(struct linted_asynch_pool *pool);

void linted_asynch_pool_submit(struct linted_asynch_pool *pool,
                               struct linted_asynch_task *task);
void linted_asynch_pool_complete(struct linted_asynch_pool *pool,
                                 struct linted_asynch_task *task);
void linted_asynch_pool_wait_on_poll(struct linted_asynch_pool *pool,
                                     struct linted_asynch_waiter *waiter,
                                     struct linted_asynch_task *task, int ko,
                                     unsigned short flags);

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
void linted_asynch_task_seterrnum(struct linted_asynch_task *task,
                                  linted_error errnum);
void *linted_asynch_task_data(struct linted_asynch_task *task);

linted_error
linted_asynch_task_waitid_create(struct linted_asynch_task_waitid **taskp,
                                 void *data);
void linted_asynch_task_waitid_destroy(struct linted_asynch_task_waitid *task);

#if _POSIX_C_SOURCE >= 200809L
void linted_asynch_task_waitid_prepare(struct linted_asynch_task_waitid *task,
                                       unsigned task_action, idtype_t type,
                                       id_t id, int options);
void linted_asynch_task_waitid_info(struct linted_asynch_task_waitid *task,
                                    siginfo_t *info);
#endif
void *linted_asynch_task_waitid_data(struct linted_asynch_task_waitid *task);
struct linted_asynch_task *
linted_asynch_task_waitid_to_asynch(struct linted_asynch_task_waitid *task);
struct linted_asynch_task_waitid *
linted_asynch_task_waitid_from_asynch(struct linted_asynch_task *task);

linted_error linted_asynch_task_sigwaitinfo_create(
    struct linted_asynch_task_sigwaitinfo **taskp, void *data);
void linted_asynch_task_sigwaitinfo_destroy(
    struct linted_asynch_task_sigwaitinfo *task);

#if _POSIX_C_SOURCE >= 199309L
void linted_asynch_task_sigwaitinfo_prepare(
    struct linted_asynch_task_sigwaitinfo *task, unsigned task_action,
    sigset_t const *set);
#endif
void *linted_asynch_task_sigwaitinfo_data(
    struct linted_asynch_task_sigwaitinfo *task);
int linted_asynch_task_sigwaitinfo_signo(
    struct linted_asynch_task_sigwaitinfo *task);
struct linted_asynch_task *linted_asynch_task_sigwaitinfo_to_asynch(
    struct linted_asynch_task_sigwaitinfo *task);
struct linted_asynch_task_sigwaitinfo *
linted_asynch_task_sigwaitinfo_from_asynch(struct linted_asynch_task *task);

linted_error linted_asynch_task_sleep_until_create(
    struct linted_asynch_task_sleep_until **taskp, void *data);
void linted_asynch_task_sleep_until_destroy(
    struct linted_asynch_task_sleep_until *task);

#if _POSIX_C_SOURCE >= 199309L
void linted_asynch_task_sleep_until_prepare(
    struct linted_asynch_task_sleep_until *task, unsigned task_action,
    int flags, struct timespec const *req);
void linted_asynch_task_sleep_until_request(
    struct linted_asynch_task_sleep_until *task, struct timespec *req);
#endif
void *linted_asynch_task_sleep_until_data(
    struct linted_asynch_task_sleep_until *task);
struct linted_asynch_task *linted_asynch_task_sleep_until_to_asynch(
    struct linted_asynch_task_sleep_until *task);
struct linted_asynch_task_sleep_until *
linted_asynch_task_sleep_until_from_asynch(struct linted_asynch_task *task);

#endif /* LINTED_ASYNCH_H */
