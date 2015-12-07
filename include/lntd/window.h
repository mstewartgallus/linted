/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#ifndef LNTD_WINDOW_H
#define LNTD_WINDOW_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stdint.h>

/**
 * @file
 *
 * Communicates user input to another process.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

typedef lntd_ko lntd_window;
typedef lntd_ko lntd_window_notifier;

struct lntd_window_task_notify;
struct lntd_window_task_watch;

lntd_error lntd_window_read(lntd_window window, uint_fast32_t *outp);
lntd_error lntd_window_write(lntd_window window, uint_fast32_t in);

lntd_error
lntd_window_task_watch_create(struct lntd_window_task_watch **taskp,
                              void *data);
void lntd_window_task_watch_destroy(
    struct lntd_window_task_watch *task);

struct lntd_async_task *
lntd_window_task_watch_prepare(struct lntd_window_task_watch *task,
                               union lntd_async_ck task_ck,
                               void *userstate, lntd_ko notifier);
struct lntd_async_task *
lntd_window_task_watch_to_async(struct lntd_window_task_watch *task);
void *lntd_window_task_watch_data(struct lntd_window_task_watch *task);

lntd_error
lntd_window_task_notify_create(struct lntd_window_task_notify **taskp,
                               void *data);
void lntd_window_task_notify_destroy(
    struct lntd_window_task_notify *task);

struct lntd_async_task *
lntd_window_task_notify_prepare(struct lntd_window_task_notify *task,
                                union lntd_async_ck task_ck,
                                void *userstate, lntd_ko notifier);
struct lntd_async_task *
lntd_window_task_notify_to_async(struct lntd_window_task_notify *task);
void *
lntd_window_task_notify_data(struct lntd_window_task_notify *task);

#endif /* LNTD_WINDOW_H */
