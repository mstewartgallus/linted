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
#include <stdio.h>
#include <string.h>

#define INT_MIN (-(intmax_t) (UINTMAX_C(1) << 31u))

struct int32 { char bytes[4]; };

#define MESSAGE_SIZE (                              \
    LINTED_SIZEOF_MEMBER(struct int32, bytes)       \
    + LINTED_SIZEOF_MEMBER(struct int32, bytes))

typedef char message_type[MESSAGE_SIZE];

static struct int32 pack(int_fast32_t fast) {
    uint_fast32_t positive = ((int_fast64_t) fast) + INT_MIN;

    unsigned char bytes[LINTED_SIZEOF_MEMBER(struct int32, bytes)] = {
        ((uintmax_t) positive) & 0xFFu,
        (((uintmax_t) positive) >> 8u) & 0xFFu,
        (((uintmax_t) positive) >> 16u) & 0xFFu,
        (((uintmax_t) positive) >> 24u) & 0xFFu
    };

    struct int32 raw;
    memcpy(raw.bytes, &bytes, sizeof raw.bytes);
    return raw;
}

static int_fast32_t unpack(struct int32 raw) {
    unsigned char pos_bytes[sizeof raw.bytes];
    memcpy(&pos_bytes, raw.bytes, sizeof raw.bytes);

    uint_fast32_t positive = ((uintmax_t) pos_bytes[0])
        | (((uintmax_t) pos_bytes[1]) << 8u)
        | (((uintmax_t) pos_bytes[2]) << 16u)
        | (((uintmax_t) pos_bytes[3]) << 24u);

    return ((int_fast64_t) positive) - INT_MIN;
}

int linted_updater_pair(linted_updater updater[2], int rflags, int wflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 1;
    attr.mq_msgsize = sizeof(message_type);

    return linted_mq_pair(updater, &attr, rflags, wflags);
}

int linted_updater_send_update(linted_updater updater,
                               struct linted_updater_update const *update)
{
    message_type message;
    char *tip = message;

    struct int32 x_position = pack(update->x_position);
    memcpy(tip, x_position.bytes, sizeof x_position.bytes);
    tip += sizeof x_position.bytes;

    struct int32 y_position = pack(update->y_position);
    memcpy(tip, y_position.bytes, sizeof y_position.bytes);

    return mq_send(updater, message, sizeof message, 0);
}

int linted_updater_receive_update(linted_updater updater,
                                  struct linted_updater_update *update)
{
    message_type message;
    int receive_status = mq_receive(updater, message, sizeof message, NULL);
    if (receive_status != -1) {
        char *tip = message;

        struct int32 x_position;
        memcpy(x_position.bytes, tip, sizeof x_position.bytes);
        update->x_position = unpack(x_position);
        tip += sizeof x_position.bytes;

        struct int32 y_position;
        memcpy(y_position.bytes, tip, sizeof y_position.bytes);
        update->y_position = unpack(y_position);
    }
    return receive_status;
}

int linted_updater_close(linted_updater const updater)
{
    return mq_close(updater);
}
