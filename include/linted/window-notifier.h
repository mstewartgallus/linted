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
#ifndef LINTED_WINDOW_NOTIFIER_H
#define LINTED_WINDOW_NOTIFIER_H

#include "linted/error.h"
#include "linted/mq.h"
#include "linted/rpc.h"
#include "linted/util.h"

#include <stdint.h>

/**
 * @file
 *
 * Communicates user input to another process.
 */

typedef linted_mq linted_window_notifier;

struct linted_window_notifier_task_send
{
	struct linted_mq_task_send parent;
	char message[LINTED_RPC_UINT32_SIZE];
};

struct linted_window_notifier_task_receive
{
	struct linted_mq_task_receive parent;
	char message[LINTED_RPC_UINT32_SIZE];
};

#define LINTED_WINDOW_NOTIFIER_SEND_UPCAST(X)                                  \
	LINTED_MQ_SEND_UPCAST(LINTED_UPCAST(X))
#define LINTED_WINDOW_NOTIFIER_SEND_DOWNCAST(X)                                \
	LINTED_DOWNCAST(struct linted_window_notifier_task_send,               \
	                LINTED_MQ_SEND_DOWNCAST(X))

#define LINTED_WINDOW_NOTIFIER_RECEIVE_UPCAST(X)                               \
	LINTED_MQ_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define LINTED_WINDOW_NOTIFIER_RECEIVE_DOWNCAST(X)                             \
	LINTED_DOWNCAST(struct linted_window_notifier_task_receive,            \
	                LINTED_MQ_RECEIVE_DOWNCAST(X))

void linted_window_notifier_send(struct linted_window_notifier_task_send *task,
                                 unsigned task_id,
                                 linted_window_notifier notifier,
                                 uint_fast32_t window);

void
linted_window_notifier_receive(struct linted_window_notifier_task_receive *task,
                               unsigned task_id,
                               linted_window_notifier notifier);

uint_fast32_t linted_window_notifier_decode(
    struct linted_window_notifier_task_receive const *task);

#endif /* LINTED_WINDOW_NOTIFIER_H */
