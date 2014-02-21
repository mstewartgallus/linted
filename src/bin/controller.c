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

#include "linted/gui.h"
#include "linted/mq.h"
#include "linted/controller.h"
#include "linted/util.h"

#include <string.h>

typedef char message_type[sizeof(uint8_t)
                          + sizeof(uint8_t)
                          + sizeof(uint8_t)];

int linted_controller_pair(linted_controller controller[2],
                           int readflags, int writeflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(message_type);

    return linted_mq_pair(controller, &attr, readflags, writeflags);
}

int linted_controller_send_movement(linted_controller controller,
                                    enum linted_controller_direction direction,
                                    bool moving)
{
    message_type message;

    uint8_t raw_type = LINTED_CONTROLLER_MOVEMENT;
    uint8_t raw_direction = direction;
    uint8_t raw_moving = moving;

    char *tip = message;

    memcpy(tip, &raw_type, sizeof raw_type);
    tip += sizeof raw_type;

    memcpy(tip, &raw_direction, sizeof raw_direction);
    tip += sizeof raw_direction;

    memcpy(tip, &raw_moving, sizeof raw_moving);

    return mq_send(controller, message, sizeof message, 0);
}

int linted_controller_close(linted_controller const controller)
{
    return mq_close(controller);
}

int linted_controller_receive(linted_controller queue,
                              struct linted_controller_message *message)
{
    /*
     * TODO: Return an error if we receive incorrectly formatted
     * data
     */
    message_type raw_message;

    int receive_status = mq_receive(queue, raw_message, sizeof raw_message,
                                    NULL);

    if (receive_status != -1) {
        uint8_t raw_type;
        uint8_t raw_direction;
        uint8_t raw_moving;

        char *tip = raw_message;

        memcpy(&raw_type, tip, sizeof raw_type);
        tip += sizeof raw_type;

        memcpy(&raw_direction, tip, sizeof raw_direction);
        tip += sizeof raw_direction;

        memcpy(&raw_moving, tip, sizeof raw_moving);

        message->type = raw_type;
        message->direction = raw_direction;
        message->moving = raw_moving;
    }

    return receive_status;
}

int linted_controller_notify(linted_controller controller,
                             struct sigevent const *sevp)
{
    return mq_notify(controller, sevp);
}
