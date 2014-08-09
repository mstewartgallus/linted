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
#include "config.h"

#include "linted/logger.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <string.h>

linted_error linted_logger_create(linted_logger *loggerp, int flags)
{
    if (flags != 0) {
        return EINVAL;
    }

    struct linted_mq_attr attr = {0};

    attr.maxmsg = 10;
    attr.msgsize = LINTED_LOGGER_LOG_MAX;

    return linted_mq_create(loggerp, "/logger", &attr, 0);
}

/**
 * @todo Make asynchronous
 */
linted_error linted_logger_log(linted_logger logger, char const *msg_ptr,
                               size_t msg_len)
{
    struct linted_mq_task_send send_task;

    linted_mq_task_send(&send_task, 0, logger, msg_ptr, msg_len);
    linted_asynch_pool_submit(NULL, LINTED_UPCAST(&send_task));

    return LINTED_UPCAST(&send_task)->errnum;
}

void linted_logger_receive(struct linted_logger_task *task, unsigned task_id,
                           linted_logger logger,
                           char msg_ptr[static LINTED_LOGGER_LOG_MAX])
{
    linted_mq_task_receive(LINTED_UPCAST(task), task_id, logger, msg_ptr,
                           LINTED_LOGGER_LOG_MAX);
}
