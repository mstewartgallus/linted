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
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

int linted_updater_pair(linted_updater_t updater[2], int rflags, int wflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 10;
    attr.mq_msgsize = sizeof(struct linted_updater_update);

    return linted_mq_pair(updater, &attr, rflags, wflags);
}

int linted_updater_send_update(linted_updater_t updater,
                               struct linted_updater_update update)
{
    return mq_send(updater, (char const *)&update, sizeof update, 0);
}

int linted_updater_receive_update(linted_updater_t updater,
                                  struct linted_updater_update * update)
{
    return mq_receive(updater, (char *)update, sizeof *update, NULL);
}

int linted_updater_close(linted_updater_t const updater)
{
    return mq_close(updater);
}
