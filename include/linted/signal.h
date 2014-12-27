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
#ifndef LINTED_SIGNAL_H
#define LINTED_SIGNAL_H

#include "linted/asynch.h"
#include "linted/error.h"

#if _POSIX_C_SOURCE >= 199309L
#include <signal.h>
#endif

/**
 * @file
 *
 * Signal handling.
 */

struct linted_signal_task_sigwaitinfo;

linted_error linted_signal_task_sigwaitinfo_create(
    struct linted_signal_task_sigwaitinfo **taskp, void *data);
void linted_signal_task_sigwaitinfo_destroy(
    struct linted_signal_task_sigwaitinfo *task);

#if _POSIX_C_SOURCE >= 199309L
void linted_signal_task_sigwaitinfo_prepare(
    struct linted_signal_task_sigwaitinfo *task, unsigned task_action,
    sigset_t const *set);
#endif
void *linted_signal_task_sigwaitinfo_data(
    struct linted_signal_task_sigwaitinfo *task);
int linted_signal_task_sigwaitinfo_signo(
    struct linted_signal_task_sigwaitinfo *task);
struct linted_asynch_task *linted_signal_task_sigwaitinfo_to_asynch(
    struct linted_signal_task_sigwaitinfo *task);
struct linted_signal_task_sigwaitinfo *
linted_signal_task_sigwaitinfo_from_asynch(struct linted_asynch_task *task);

void linted_signal_do_sigwaitinfo(struct linted_asynch_pool *pool,
                                  struct linted_asynch_task *task);

#endif /* LINTED_SIGNAL_H */
