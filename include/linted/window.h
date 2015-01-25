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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_WINDOW_H
#define LINTED_WINDOW_H

#include "linted/error.h"
#include "linted/ko.h"

#include <stdint.h>

/**
 * @file
 *
 * Communicates user input to another process.
 */

struct linted_asynch_task;

typedef linted_ko linted_window;
typedef linted_ko linted_window_notifier;

struct linted_window_task_notify;
struct linted_window_task_watch;

linted_error linted_window_read(linted_window window, uint_fast32_t *outp);
linted_error linted_window_write(linted_window window, uint_fast32_t in);

linted_error
linted_window_task_watch_create(struct linted_window_task_watch **taskp,
                                void *data);
void linted_window_task_watch_destroy(struct linted_window_task_watch *task);

void linted_window_task_watch_prepare(struct linted_window_task_watch *task,
                                      unsigned task_action, linted_ko notifier);
struct linted_window_task_watch *
linted_window_task_watch_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_window_task_watch_to_asynch(struct linted_window_task_watch *task);
void *linted_window_task_watch_data(struct linted_window_task_watch *task);

linted_error
linted_window_task_notify_create(struct linted_window_task_notify **taskp,
                                 void *data);
void linted_window_task_notify_destroy(struct linted_window_task_notify *task);

void linted_window_task_notify_prepare(struct linted_window_task_notify *task,
                                       unsigned task_action,
                                       linted_ko notifier);
struct linted_window_task_notify *
linted_window_task_notify_from_asynch(struct linted_asynch_task *task);
struct linted_asynch_task *
linted_window_task_notify_to_asynch(struct linted_window_task_notify *task);
void *linted_window_task_notify_data(struct linted_window_task_notify *task);

#endif /* LINTED_WINDOW_H */
