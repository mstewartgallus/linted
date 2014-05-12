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

#include <stddef.h>
#include <string.h>

linted_error linted_updater_pair(linted_updater updater[2], int rflags,
                                 int wflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 1;
    attr.mq_msgsize
        = LINTED_SIZEOF_MEMBER(struct linted_updater_task, message);

    return linted_mq_pair(updater, &attr, rflags, wflags);
}

void linted_updater_send(struct linted_asynch_pool* pool, int task_id,
                         linted_updater updater,
                         struct linted_updater_update const *update,
                         struct linted_updater_task* task)
{
    char* tip = task->message;

    struct linted_rpc_int32 x_position = linted_rpc_pack(update->x_position);
    memcpy(tip, x_position.bytes, sizeof x_position.bytes);
    tip += sizeof x_position.bytes;

    struct linted_rpc_int32 y_position = linted_rpc_pack(update->y_position);
    memcpy(tip, y_position.bytes, sizeof y_position.bytes);
    tip += sizeof y_position.bytes;

    struct linted_rpc_int32 z_position = linted_rpc_pack(update->z_position);
    memcpy(tip, z_position.bytes, sizeof z_position.bytes);
    tip += sizeof z_position.bytes;

    struct linted_rpc_uint32 x_rotation
        = linted_rpc_pack_uint32(update->x_rotation);
    memcpy(tip, x_rotation.bytes, sizeof x_rotation.bytes);
    tip += sizeof x_rotation.bytes;

    struct linted_rpc_uint32 y_rotation
        = linted_rpc_pack_uint32(update->y_rotation);
    memcpy(tip, y_rotation.bytes, sizeof y_rotation.bytes);

    linted_io_mq_send(pool, task_id, updater, task->message,
                      sizeof task->message,
                      &task->asynch_task);
}

void linted_updater_receive(struct linted_asynch_pool* pool, int task_id,
                            linted_updater updater,
                            struct linted_updater_task* task)
{
    linted_io_mq_receive(pool, task_id, updater, task->message,
                         sizeof task->message,
                         &task->asynch_task);
}

void linted_updater_decode(struct linted_updater_task const* task,
                           struct linted_updater_update* update)
{
    char const* tip = task->message;

    struct linted_rpc_int32 x_position;
    memcpy(x_position.bytes, tip, sizeof x_position.bytes);
    update->x_position = linted_rpc_unpack(x_position);
    tip += sizeof x_position.bytes;

    struct linted_rpc_int32 y_position;
    memcpy(y_position.bytes, tip, sizeof y_position.bytes);
    update->y_position = linted_rpc_unpack(y_position);
    tip += sizeof y_position.bytes;

    struct linted_rpc_int32 z_position;
    memcpy(z_position.bytes, tip, sizeof z_position.bytes);
    update->z_position = linted_rpc_unpack(z_position);
    tip += sizeof z_position.bytes;

    struct linted_rpc_uint32 x_rotation;
    memcpy(x_rotation.bytes, tip, sizeof x_rotation.bytes);
    update->x_rotation = linted_rpc_unpack_uint32(x_rotation);
    tip += sizeof x_rotation.bytes;

    struct linted_rpc_uint32 y_rotation;
    memcpy(y_rotation.bytes, tip, sizeof y_rotation.bytes);
    update->y_rotation = linted_rpc_unpack_uint32(y_rotation);
}

linted_error linted_updater_close(linted_updater const updater)
{
    return -1 == mq_close(updater) ? errno : 0;
}
