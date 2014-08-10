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

#include <stdbool.h>
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

    bool left : 1;
    bool right : 1;
    bool forward : 1;
    bool back : 1;

    bool jumping : 1;
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

linted_error linted_controller_create(linted_controller * restrict controllerp,
                                      unsigned long flags);

void linted_controller_send(struct linted_controller_task_send *task,
                            unsigned task_id, linted_controller controller,
                            struct linted_controller_message const * restrict message);

void linted_controller_receive(struct linted_controller_task_receive *task,
                               unsigned task_id, linted_controller controller);

linted_error
linted_controller_decode(struct linted_controller_task_receive const *task,
                         struct linted_controller_message * restrict message);

#endif /* LINTED_CONTROLLER_H */
