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

#include "linted/updater.h"

#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <stddef.h>
#include <string.h>

linted_error linted_updater_create(linted_updater * restrict updaterp,
                                   unsigned long flags)
{
    if (flags != 0U) {
        return EINVAL;
    }

    struct linted_mq_attr attr = { 0 };

    attr.maxmsg = 1;
    attr.msgsize
        = LINTED_SIZEOF_MEMBER(struct linted_updater_task_send, message);

    return linted_mq_create(updaterp, "/updater", &attr, 0);
}

void linted_updater_send(struct linted_updater_task_send *task,
                         unsigned task_id,
                         linted_updater updater,
                         struct linted_updater_update const *update)
{
    linted_mq_task_send(LINTED_UPCAST(task), task_id, updater, task->message,
                        sizeof task->message);

    char *tip = task->message;

    linted_rpc_pack(update->x_position, tip);
    tip += LINTED_RPC_INT32_SIZE;

    linted_rpc_pack(update->y_position, tip);
    tip += LINTED_RPC_INT32_SIZE;

    linted_rpc_pack(update->z_position, tip);
    tip += LINTED_RPC_INT32_SIZE;

    linted_rpc_pack_uint32(update->x_rotation._value, tip);
    tip += LINTED_RPC_UINT32_SIZE;

    linted_rpc_pack_uint32(update->y_rotation._value, tip);
}

void linted_updater_receive(struct linted_updater_task_receive *task,
                            unsigned task_id, linted_updater updater)
{
    linted_mq_task_receive(LINTED_UPCAST(task), task_id, updater, task->message,
                           sizeof task->message);
}

void linted_updater_decode(struct linted_updater_task_receive const *task,
                           struct linted_updater_update * restrict update)
{
    char const *tip = task->message;

    update->x_position = linted_rpc_unpack(tip);
    tip += LINTED_RPC_INT32_SIZE;

    update->y_position = linted_rpc_unpack(tip);
    tip += LINTED_RPC_INT32_SIZE;

    update->z_position = linted_rpc_unpack(tip);
    tip += LINTED_RPC_INT32_SIZE;

    update->x_rotation._value = linted_rpc_unpack_uint32(tip);
    tip += LINTED_RPC_UINT32_SIZE;

    update->y_rotation._value = linted_rpc_unpack_uint32(tip);
}
