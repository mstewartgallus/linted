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

#include "linted/mq.h"
#include "linted/move.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

struct message_data {
    enum linted_move_direction direction;
    bool moving;
};

int linted_move_queue_pair(linted_move_queue_t move[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct message_data);

    return linted_mq_pair(move, &attr, 0, 0);
}

int linted_move_queue_send(linted_move_queue_t move,
                           enum linted_move_direction direction,
                           bool moving)
{
    struct message_data message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.direction = direction;
    message_data.moving = moving;

    return mq_send(move, (char const *)&message_data, sizeof message_data, 0);
}

int linted_move_queue_notify(linted_move_queue_t queue,
                             struct sigevent const * sevp)
{
    return mq_notify(queue, sevp);
}

int linted_move_queue_receive(linted_move_queue_t queue,
                              enum linted_move_direction * direction,
                              bool * moving)
{
    struct message_data message_data;
    ssize_t received  = mq_receive(queue, (char *) &message_data, sizeof message_data, NULL);
    if (-1 == received) {
        return -1;
    }
    *direction = message_data.direction;
    *moving = message_data.moving;
    return 0;
}

int linted_move_close(linted_move_queue_t const move)
{
    return mq_close(move);
}
