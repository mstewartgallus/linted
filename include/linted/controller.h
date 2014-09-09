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
#ifndef LINTED_CONTROLLER_H
#define LINTED_CONTROLLER_H

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

typedef linted_mq linted_controller;

struct linted_controller_message
{
	int_fast32_t x_tilt;
	int_fast32_t y_tilt;

	_Bool left : 1U;
	_Bool right : 1U;
	_Bool forward : 1U;
	_Bool back : 1U;

	_Bool jumping : 1U;
};

struct linted_controller_task_send
{
	struct linted_mq_task_send parent;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE + 1U];
};

struct linted_controller_task_receive
{
	struct linted_mq_task_receive parent;
	char message[LINTED_RPC_INT32_SIZE + LINTED_RPC_INT32_SIZE + 1U];
};

#define LINTED_CONTROLLER_SEND_UPCAST(X) LINTED_MQ_SEND_UPCAST(LINTED_UPCAST(X))
#define LINTED_CONTROLLER_SEND_DOWNCAST(X)                                     \
	LINTED_DOWNCAST(struct linted_controller_task_send,                    \
	                LINTED_MQ_SEND_DOWNCAST(X))

#define LINTED_CONTROLLER_RECEIVE_UPCAST(X)                                    \
	LINTED_MQ_RECEIVE_UPCAST(LINTED_UPCAST(X))
#define LINTED_CONTROLLER_RECEIVE_DOWNCAST(X)                                  \
	LINTED_DOWNCAST(struct linted_controller_task_receive,                 \
	                LINTED_MQ_RECEIVE_DOWNCAST(X))

linted_error linted_controller_create(linted_controller *controllerp,
                                      unsigned long flags);

void linted_controller_send(struct linted_controller_task_send *task,
                            unsigned task_id, linted_controller controller,
                            struct linted_controller_message const *message);

void linted_controller_receive(struct linted_controller_task_receive *task,
                               unsigned task_id, linted_controller controller);

linted_error
linted_controller_decode(struct linted_controller_task_receive const *task,
                         struct linted_controller_message *message);

#endif /* LINTED_CONTROLLER_H */
