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

#include "linted/mq.h"
#include "linted/rpc.h"
#include "linted/util.h"

#include <stdint.h>
#include <string.h>

#define MESSAGE_SIZE                                                           \
    (LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes)                      \
     + LINTED_SIZEOF_MEMBER(struct linted_rpc_int32, bytes) + 1)

typedef char message_type[MESSAGE_SIZE];

linted_error linted_controller_pair(linted_controller controller[2], int readflags,
                               int writeflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 1;
    attr.mq_msgsize = sizeof(message_type);

    return linted_mq_pair(controller, &attr, readflags, writeflags);
}

linted_error linted_controller_send(linted_controller controller,
                               struct linted_controller_message const* message)
{
    message_type raw_message;
    char* tip = raw_message;

    struct linted_rpc_int32 x_tilt = linted_rpc_pack(message->x_tilt);
    memcpy(tip, x_tilt.bytes, sizeof x_tilt.bytes);
    tip += sizeof x_tilt.bytes;

    struct linted_rpc_int32 y_tilt = linted_rpc_pack(message->y_tilt);
    memcpy(tip, y_tilt.bytes, sizeof y_tilt.bytes);
    tip += sizeof y_tilt.bytes;

    unsigned char bitfield
        = ((uintmax_t)message->forward) | ((uintmax_t)message->back) << 1u
          | ((uintmax_t)message->right) << 2u | ((uintmax_t)message->left) << 3u
          | ((uintmax_t)message->jumping) << 4u;
    memcpy(tip, &bitfield, sizeof bitfield);

    return -1 == mq_send(controller, raw_message, sizeof raw_message, 0) ? errno
                                                                         : 0;
}

linted_error linted_controller_close(linted_controller controller)
{
    return -1 == mq_close(controller) ? errno : 0;
}

linted_error linted_controller_receive(linted_controller queue,
                                  struct linted_controller_message* message)
{
    message_type raw_message;
    ssize_t recv_status
        = mq_receive(queue, raw_message, sizeof raw_message, NULL);
    if (-1 == recv_status) {
        return errno;
    }

    size_t bytes_read = recv_status;
    if (bytes_read != sizeof raw_message) {
        return EPROTO;
    }

    char* tip = raw_message;

    struct linted_rpc_int32 x_tilt;
    memcpy(x_tilt.bytes, tip, sizeof x_tilt.bytes);
    message->x_tilt = linted_rpc_unpack(x_tilt);
    tip += sizeof x_tilt.bytes;

    struct linted_rpc_int32 y_tilt;
    memcpy(y_tilt.bytes, tip, sizeof y_tilt.bytes);
    message->y_tilt = linted_rpc_unpack(y_tilt);
    tip += sizeof y_tilt.bytes;

    unsigned char bitfield;
    memcpy(&bitfield, tip, sizeof bitfield);

    if ((bitfield & ~(1u | 1u << 1u | 1u << 2u | 1u << 3u | 1u << 4u)) != 0u) {
        return EPROTO;
    }

    message->forward = bitfield & 1u;
    message->back = (bitfield & (1u << 1u)) != 0u;
    message->right = (bitfield & (1u << 2u)) != 0u;
    message->left = (bitfield & (1u << 3u)) != 0u;

    message->jumping = (bitfield & (1u << 4u)) != 0u;

    return 0;
}
