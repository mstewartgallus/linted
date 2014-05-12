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

#include "linted/shutdowner.h"

#include "linted/error.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

struct message
{
    char dummy;
};

linted_error linted_shutdowner_pair(linted_shutdowner shutdowner[2], int rflags,
                                    int wflags)
{
    struct mq_attr attr;
    memset(&attr, 0, sizeof attr);

    attr.mq_maxmsg = 1;
    attr.mq_msgsize = 1;

    return linted_mq_pair(shutdowner, &attr, rflags, wflags);
}

linted_error linted_shutdowner_send_shutdown(linted_shutdowner shutdowner)
{
    char dummy = 0;
    return -1 == mq_send(shutdowner, &dummy, sizeof dummy, 0) ? errno : 0;
}

linted_error linted_shutdowner_receive(struct linted_asynch_pool* pool,
                                       int task_id,
                                       linted_shutdowner shutdowner,
                                       struct linted_shutdowner_event * event)
{
    return linted_io_mq_receive(pool, task_id, shutdowner, &event->dummy[0], 1);
}

linted_error linted_shutdowner_close(linted_shutdowner shutdowner)
{
    return -1 == mq_close(shutdowner) ? errno : 0;
}
