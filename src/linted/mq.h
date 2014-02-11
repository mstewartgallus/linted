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

#include <mqueue.h>

/**
 * The linted_mq_pair call creates an unnamed pair of message queues
 * with the specified attributes attr and flags oflag.
 *
 * @param mqd Returns two message queue descriptors. mqd[0] refers to
 *            the read end. mqd[1] refers to the write end.
 *
 * @param oflaga Can be O_NONBLOCK to open the read end in nonblocking
 *               mode.
 *
 * @param oflagb Can be O_NONBLOCK to open the write end in
 *               nonblocking mode.
 *
 * @returns Zero on success. -1 on error, and errno is set
 *          appropriately.
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
int linted_mq_pair(mqd_t mqs[], struct mq_attr *attr, int oflaga, int oflagb);

#endif                          /* LINTED_MQ_H */
