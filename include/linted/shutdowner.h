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

#include <errno.h>
#include <mqueue.h>

typedef mqd_t linted_shutdowner;

errno_t linted_shutdowner_pair(linted_shutdowner queues[2],
                               int rflags, int wflags);

errno_t linted_shutdowner_close(linted_shutdowner move);

errno_t linted_shutdowner_send_shutdown(linted_shutdowner queue);

int linted_shutdowner_notify(linted_shutdowner queue,
                             struct sigevent const *sevp);

int linted_shutdowner_receive(linted_shutdowner queue);

#endif                          /* LINTED_SHUTDOWNER_H */
