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
#ifndef LINTED_PID_H
#define LINTED_PID_H

#include "linted/error.h"

#if _POSIX_C_SOURCE >= 200809L
#include <sys/wait.h>
#endif

/**
 * @file
 *
 * System processes.
 */

struct linted_asynch_pool;
struct linted_asynch_task;

struct linted_pid_task_waitid;

linted_error
linted_pid_task_waitid_create(struct linted_pid_task_waitid **taskp,
                              void *data);
void linted_pid_task_waitid_destroy(struct linted_pid_task_waitid *task);

#if _POSIX_C_SOURCE >= 200809L
void linted_pid_task_waitid_prepare(struct linted_pid_task_waitid *task,
                                    unsigned task_action, idtype_t type,
                                    id_t id, int options);
void linted_pid_task_waitid_info(struct linted_pid_task_waitid *task,
                                 siginfo_t *info);
#endif
void *linted_pid_task_waitid_data(struct linted_pid_task_waitid *task);
struct linted_asynch_task *
linted_pid_task_waitid_to_asynch(struct linted_pid_task_waitid *task);
struct linted_pid_task_waitid *
linted_pid_task_waitid_from_asynch(struct linted_asynch_task *task);

void linted_pid_do_waitid(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);

#endif /* LINTED_PID_H */
