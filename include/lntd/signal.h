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
#ifndef LNTD_SIGNAL_H
#define LNTD_SIGNAL_H

#include "lntd/error.h"

/**
 * @file
 *
 * Signal handling.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

struct lntd_signal_task_wait;

lntd_error lntd_signal_init(void);

lntd_error
lntd_signal_task_wait_create(struct lntd_signal_task_wait **taskp,
                             void *data);
void lntd_signal_task_wait_destroy(struct lntd_signal_task_wait *task);

struct lntd_async_task *
lntd_signal_task_wait_prepare(struct lntd_signal_task_wait *task,
                              union lntd_async_ck task_ck,
                              void *userstate);

void *lntd_signal_task_wait_data(struct lntd_signal_task_wait *task);
int lntd_signal_task_wait_signo(struct lntd_signal_task_wait *task);
struct lntd_async_task *
lntd_signal_task_wait_to_async(struct lntd_signal_task_wait *task);

void lntd_signal_do_wait(struct lntd_async_pool *pool,
                         struct lntd_async_task *task);

void lntd_signal_listen_to_sigchld(void);
void lntd_signal_listen_to_sighup(void);
void lntd_signal_listen_to_sigint(void);
void lntd_signal_listen_to_sigquit(void);
void lntd_signal_listen_to_sigterm(void);

char const *lntd_signal_string(int signo);

#endif /* LNTD_SIGNAL_H */
