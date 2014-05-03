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

#include <string.h>

errno_t linted_logger_pair(linted_logger logger[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = LINTED_LOGGER_LOG_MAX;

    return linted_mq_pair(logger, &attr, 0, 0);
}

errno_t linted_logger_close(linted_logger logger)
{
    return mq_close(logger);
}

errno_t linted_logger_log(linted_logger logger, char const* msg_ptr,
                          size_t msg_len)
{
    return -1 == mq_send(logger, msg_ptr, msg_len, 0) ? errno : 0;
}

errno_t linted_logger_recv_log(linted_logger logger,
                               char msg_ptr[static LINTED_LOGGER_LOG_MAX],
                               size_t* msg_len)
{
    ssize_t size = mq_receive(logger, msg_ptr, LINTED_LOGGER_LOG_MAX, NULL);
    if (-1 == size) {
        return errno;
    }

    *msg_len = size;
    return 0;
}
