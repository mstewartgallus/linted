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

#include <errno.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

int linted_controller_pair(linted_controller_t controller[2])
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct linted_controller_message);

    return linted_mq_pair(controller, &attr, 0);
}

int linted_controller_send_movement(linted_controller_t controller,
                                   enum linted_controller_direction direction,
                                   bool moving)
{
    struct linted_controller_message message_data;
    memset(&message_data, 0, sizeof message_data);

    message_data.type = LINTED_CONTROLLER_MOVEMENT;
    message_data.direction = direction;
    message_data.moving = moving;

    return mq_send(controller, (char const *)&message_data, sizeof message_data, 0);
}

int linted_controller_send_tick(linted_controller_t controller)
{
    struct linted_controller_message message_data;
    memset(&message_data, 0, sizeof message_data);
    message_data.type = LINTED_CONTROLLER_TICK;
    return mq_send(controller, (char const *)&message_data, sizeof message_data, 0);
}

int linted_controller_send_shutdown(linted_controller_t const controller)
{
    struct linted_controller_message message_data;
    memset(&message_data, 0, sizeof message_data);
    message_data.type = LINTED_CONTROLLER_SHUTDOWN;
    return mq_send(controller, (char const *)&message_data, sizeof message_data, 0);
}

int linted_controller_close(linted_controller_t const controller)
{
    return mq_close(controller);
}

int linted_controller_receive(linted_controller_t queue,
                              struct linted_controller_message * message)
{
    return mq_receive(queue, (char *) message, sizeof *message, NULL);
}
