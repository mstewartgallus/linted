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
#ifndef LINTED_SHUTDOWNER_H
#define LINTED_SHUTDOWNER_H

#include "linted/asynch.h"
#include "linted/error.h"

#include <mqueue.h>

/**
 * @file
 *
 * Exposes a protocol for shutting down a service.
 */

typedef mqd_t linted_shutdowner;

struct linted_shutdowner_event
{
    char dummy[1];
};

linted_error linted_shutdowner_pair(linted_shutdowner queues[2], int rflags,
                                    int wflags);

linted_error linted_shutdowner_close(linted_shutdowner move);

linted_error linted_shutdowner_send_shutdown(linted_shutdowner queue);

linted_error linted_shutdowner_receive(struct linted_asynch_pool* pool,
                                       int task_id,
                                       linted_shutdowner shutdowner,
                                       struct linted_shutdowner_event* event);

#endif /* LINTED_SHUTDOWNER_H */
