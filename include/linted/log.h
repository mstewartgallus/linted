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
#ifndef LINTED_LOG_H
#define LINTED_LOG_H

#include "linted/error.h"
#include "linted/mq.h"

#include <stddef.h>

/**
 * @file
 *
 * Exposes a protocol for logging events.
 */

#define LINTED_LOG_MAX 1024U

/**
 * A handle to access the log. Is safe to share between processes.
 */
typedef linted_mq linted_log;

struct linted_log_task_receive;

linted_error
linted_log_task_receive_create(struct linted_log_task_receive **taskp,
                               void *data);
void linted_log_task_receive_destroy(struct linted_log_task_receive *task);

struct linted_asynch_task *
linted_log_task_receive_to_asynch(struct linted_log_task_receive *task);
struct linted_log_task_receive *
linted_log_task_receive_from_asynch(struct linted_asynch_task *task);
void *linted_log_task_receive_data(struct linted_log_task_receive *task);
void linted_log_task_receive_prepare(struct linted_log_task_receive *task,
                                     unsigned task_action, linted_log log,
                                     char msg_ptr[static LINTED_LOG_MAX]);
size_t linted_log_task_receive_bytes_read(struct linted_log_task_receive *task);
char *linted_log_task_receive_buf(struct linted_log_task_receive *task);

linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len);

#endif /* LINTED_LOG_H */
