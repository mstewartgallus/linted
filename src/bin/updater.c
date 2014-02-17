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

#define SIZEOF_MEMBER(type, member) (sizeof ((type *) 0)->member)

typedef char message_type[SIZEOF_MEMBER(struct linted_updater_update, x_position)
                     + SIZEOF_MEMBER(struct linted_updater_update, y_position)];

int linted_updater_pair(linted_updater updater[2], int rflags, int wflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(message_type);

    return linted_mq_pair(updater, &attr, rflags, wflags);
}

int linted_updater_send_update(linted_updater updater,
                               struct linted_updater_update const *update)
{
    message_type message;
    char * tip = message;

    memcpy(tip, &update->x_position, sizeof update->x_position);
    tip += sizeof update->x_position;

    memcpy(tip, &update->y_position, sizeof update->y_position);

    return mq_send(updater, message, sizeof message, 0);
}

int linted_updater_receive_update(linted_updater updater,
                                  struct linted_updater_update *update)
{
    message_type message;
    int receive_status = mq_receive(updater, message, sizeof message, NULL);

    if (receive_status != -1) {
        char * tip = message;

        memcpy(&update->x_position, tip, sizeof update->x_position);
        tip += sizeof update->x_position;

        memcpy(&update->y_position, tip, sizeof update->y_position);
    }

    return receive_status;
}

int linted_updater_close(linted_updater const updater)
{
    return mq_close(updater);
}
