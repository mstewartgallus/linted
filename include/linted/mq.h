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
#ifndef LINTED_MQ_H
#define LINTED_MQ_H

#include "linted/error.h"
#include "linted/ko.h"

#include <stddef.h>

/**
 * @file
 *
 * Implements message queues.
 */

typedef linted_ko linted_mq;

struct linted_mq_attr
{
    size_t maxmsg;
    size_t msgsize;
};

/**
 * The linted_mq_pair call creates an unnamed pair of message queues.
 *
 * @param mqdes Returns two message queue descriptors. mqdes[0] refers
 *              to the read end. mqdes[1] refers to the write end.
 *
 * @param attr The attributes for the created message queue.
 *
 * @param flags Must be zero.
 *
 * @returns Zero on success or an error code on error.
 *
 * @error EMFILE The process already has the maximum number of message
 *               queues open.
 *
 * @error ENFILE The system already has the maximum number of message
 *               queues open.
 *
 * @error ENOMEM Insufficient memory.
 *
 * @error ENOSPC Insufficient space.
 */
linted_error linted_mq_pair(linted_mq mq[2], struct linted_mq_attr *attr,
                            int flags);

#endif /* LINTED_MQ_H */
