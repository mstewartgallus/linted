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

linted_error linted_shutdowner_pair(linted_shutdowner shutdowner[2], int flags)
{
    if (flags != 0) {
        return EINVAL;
    }

    struct linted_mq_attr attr;

    attr.maxmsg = 1;
    attr.msgsize = 1;

    return linted_mq_pair(shutdowner, &attr, 0);
}

linted_error linted_shutdowner_send_shutdown(linted_shutdowner shutdowner)
{
    char dummy = 0;
    struct linted_asynch_task_mq_send send_task;

    linted_asynch_mq_send(&send_task, 0, shutdowner, &dummy, 1);
    linted_asynch_pool_submit(NULL, LINTED_UPCAST(&send_task));

    return LINTED_UPCAST(&send_task)->errnum;
}

void linted_shutdowner_receive(struct linted_shutdowner_task *task, int task_id,
                               linted_shutdowner shutdowner)
{
    linted_asynch_mq_receive(LINTED_UPCAST(task), task_id, shutdowner,
                             &task->dummy[0], 1);
}
