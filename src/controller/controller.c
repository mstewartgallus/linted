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
#include "config.h"

#include "linted/controller.h"

#include "linted/io.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <stdint.h>
#include <string.h>

linted_error linted_controller_create(linted_controller * restrict controllerp,
                                      unsigned long flags)
{
    if (flags != 0U) {
        return EINVAL;
    }

    struct linted_mq_attr attr = { 0 };

    attr.maxmsg = 1;
    attr.msgsize
        = LINTED_SIZEOF_MEMBER(struct linted_controller_task_send, message);

    return linted_mq_create(controllerp, "/controller", &attr, 0);
}

void linted_controller_send(struct linted_controller_task_send *task,
                            int task_id, linted_controller controller,
                            struct linted_controller_message const * restrict message)
{
    linted_mq_task_send(LINTED_UPCAST(task), task_id, controller, task->message,
                        sizeof task->message);

    char *tip = task->message;

    linted_rpc_pack(message->x_tilt, tip);
    tip += LINTED_RPC_INT32_SIZE;

    linted_rpc_pack(message->y_tilt, tip);
    tip += LINTED_RPC_INT32_SIZE;

    unsigned char bitfield
        = ((uintmax_t)message->forward) | ((uintmax_t)message->back) << 1U
          | ((uintmax_t)message->right) << 2U | ((uintmax_t)message->left) << 3U
          | ((uintmax_t)message->jumping) << 4U;
    memcpy(tip, &bitfield, sizeof bitfield);
}

void linted_controller_receive(struct linted_controller_task_receive *task,
                               int task_id, linted_controller controller)
{
    linted_mq_task_receive(LINTED_UPCAST(task), task_id, controller,
                           task->message, sizeof task->message);
}

linted_error
linted_controller_decode(struct linted_controller_task_receive const *task,
                         struct linted_controller_message * restrict message)
{
    char const *tip = task->message;

    message->x_tilt = linted_rpc_unpack(tip);
    tip += LINTED_RPC_INT32_SIZE;

    message->y_tilt = linted_rpc_unpack(tip);
    tip += LINTED_RPC_INT32_SIZE;

    unsigned char bitfield;
    memcpy(&bitfield, tip, sizeof bitfield);

    if ((bitfield & ~(1U | 1U << 1U | 1U << 2U | 1U << 3U | 1U << 4U)) != 0U) {
        return EPROTO;
    }

    message->forward = bitfield & 1U;
    message->back = (bitfield & (1U << 1U)) != 0U;
    message->right = (bitfield & (1U << 2U)) != 0U;
    message->left = (bitfield & (1U << 3U)) != 0U;

    message->jumping = (bitfield & (1U << 4U)) != 0U;

    return 0;
}
