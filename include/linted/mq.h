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
#include "linted/util.h"

#include <stddef.h>

/**
 * @file
 *
 * Implements message queues.
 */

typedef linted_ko linted_mq;

struct linted_mq_task_receive;
struct linted_mq_task_send;

/**
 * The linted_mq_pair call creates an unnamed pair of message queues.
 *
 * @warning There is a race where a process with equal user and group
 *          but different capabilities can open these while they are
 *          being created.
 *
 * @param mqp Returns a message queue.
 *
 * @param debugpath A name to associate with the queue that is used
 *                  for debugging.  Must start with /.
 *
 * @param maxmsg The maximum amount of messages.
 *
 * @param msgsize The maximum size of a message.
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
linted_error linted_mq_create(linted_mq *mqp, char const *debugpath,
                              size_t maxmsg, size_t msgsize,
                              unsigned long flags);

linted_error
linted_mq_task_receive_create(struct linted_mq_task_receive **taskp,
                              void *data);
void linted_mq_task_receive_destroy(struct linted_mq_task_receive *task);

void linted_mq_task_receive_prepare(struct linted_mq_task_receive *task,
                                    unsigned task_action, linted_ko ko,
                                    char *buf, size_t msglen);
struct linted_asynch_task *
linted_mq_task_receive_to_asynch(struct linted_mq_task_receive *task);
struct linted_mq_task_receive *
linted_mq_task_receive_from_asynch(struct linted_asynch_task *task);
void *linted_mq_task_receive_data(struct linted_mq_task_receive *task);
size_t linted_mq_task_receive_bytes_read(struct linted_mq_task_receive *task);

linted_error linted_mq_task_send_create(struct linted_mq_task_send **taskp,
                                        void *data);
void linted_mq_task_send_destroy(struct linted_mq_task_send *task);

void linted_mq_task_send_prepare(struct linted_mq_task_send *task,
                                 unsigned task_action, linted_ko ko, char *buf,
                                 size_t msglen);
struct linted_asynch_task *
linted_mq_task_send_to_asynch(struct linted_mq_task_send *task);
struct linted_mq_task_send *
linted_mq_task_send_from_asynch(struct linted_asynch_task *task);
void *linted_mq_task_send_data(struct linted_mq_task_send *task);

void linted_mq_do_receive(struct linted_asynch_pool *pool,
                          struct linted_asynch_task *task);

void linted_mq_do_send(struct linted_asynch_pool *pool,
                       struct linted_asynch_task *task);

#endif /* LINTED_MQ_H */
