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
#ifndef LINTED_SIGNAL_H
#define LINTED_SIGNAL_H

#include "linted/asynch.h"
#include "linted/error.h"

#include <stddef.h>

/**
 * @file
 *
 * Signal handling.
 */

struct linted_asynch_pool;
struct linted_asynch_task;

struct linted_signal_task_wait;

linted_error linted_signal_init(void);

linted_error
linted_signal_task_wait_create(struct linted_signal_task_wait **taskp,
                               void *data);
void linted_signal_task_wait_destroy(
    struct linted_signal_task_wait *task);

void linted_signal_task_wait_prepare(
    struct linted_signal_task_wait *task,
    union linted_asynch_action task_action);

void *
linted_signal_task_wait_data(struct linted_signal_task_wait *task);
int linted_signal_task_wait_signo(struct linted_signal_task_wait *task);
struct linted_asynch_task *
linted_signal_task_wait_to_asynch(struct linted_signal_task_wait *task);
struct linted_signal_task_wait *
linted_signal_task_wait_from_asynch(struct linted_asynch_task *task);

void linted_signal_do_wait(struct linted_asynch_pool *pool,
                           struct linted_asynch_task *task);

void linted_signal_listen_to_sighup(void);
void linted_signal_listen_to_sigint(void);
void linted_signal_listen_to_sigquit(void);
void linted_signal_listen_to_sigterm(void);

char const *linted_signal_string(int signo);

#endif /* LINTED_SIGNAL_H */
