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
#ifndef LINTED_LOGGER_H
#define LINTED_LOGGER_H

#include "linted/error.h"
#include "linted/mq.h"

#include <stddef.h>

/**
 * @file
 *
 * Exposes a protocol for logging events.
 */

#define LINTED_LOGGER_LOG_MAX 1024

/**
 * A handle to access the logger. Is safe to share between processes.
 */
typedef linted_mq linted_logger;

struct linted_logger_task
{
    struct linted_mq_task_receive parent;
};

linted_error linted_logger_create(linted_logger * restrict loggerp,
                                  unsigned long flags);

linted_error linted_logger_close(linted_logger logger);

linted_error linted_logger_log(linted_logger logger, char const *msg_ptr,
                               size_t msg_len);

void linted_logger_receive(struct linted_logger_task *task, unsigned task_id,
                           linted_logger logger,
                           char msg_ptr[static LINTED_LOGGER_LOG_MAX]);

#endif /* LINTED_LOGGER_H */
