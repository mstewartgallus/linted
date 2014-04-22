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
#include "linted/util.h"

#include <stdint.h>
#include <string.h>

#define MESSAGE_SIZE 1

typedef char message_type[MESSAGE_SIZE];

errno_t linted_controller_pair(linted_controller controller[2],
                               int readflags, int writeflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 1;
    attr.mq_msgsize = sizeof(message_type);

    return linted_mq_pair(controller, &attr, readflags, writeflags);
}

errno_t linted_controller_send(linted_controller controller,
                               struct linted_controller_message const *message)
{
    unsigned char bitfield = ((uintmax_t) message->up)
        | ((uintmax_t) message->down) << 1u
        | ((uintmax_t) message->right) << 2u
        | ((uintmax_t) message->left) << 3u;

    message_type raw_message;
    memcpy(raw_message, &bitfield, sizeof bitfield);
    return -1 == mq_send(controller, raw_message, sizeof raw_message, 0)
        ? errno : 0;
}

errno_t linted_controller_close(linted_controller controller)
{
    return -1 == mq_close(controller) ? errno : 0;
}

errno_t linted_controller_receive(linted_controller queue,
                                  struct linted_controller_message * message)
{
    message_type raw_message;
    ssize_t recv_status = mq_receive(queue, raw_message, sizeof raw_message,
                                     NULL);
    if (-1 == recv_status) {
        return errno;
    }

    size_t bytes_read = recv_status;
    if (bytes_read != sizeof raw_message) {
        return EPROTO;
    }

    unsigned char bitfield;
    memcpy(&bitfield, raw_message, sizeof bitfield);

    message->up = bitfield & 1u;
    message->down = (bitfield & (1u << 1u)) != 0u;
    message->right = (bitfield & (1u << 2u)) != 0u;
    message->left = (bitfield & (1u << 3u)) != 0u;

    return 0;
}
