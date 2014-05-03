/*
 * Copyright 2014 Steven Stewart-Gallus
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
#ifndef LINTED_WAITER_H
#define LINTED_WAITER_H

#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <sys/types.h>

struct linted_waiter_data;

struct linted_waiter_message {
    siginfo_t exit_info;
    errno_t errnum;
};

struct linted_waiter {
    pthread_t pthread;
    struct linted_waiter_data* waiter_data;
    int init_wait_fd;
    int waiter_wait_fd;
};

errno_t linted_waiter_init(struct linted_waiter* waiter, pid_t pid);
int linted_waiter_fd(struct linted_waiter const* waiter);
errno_t linted_waiter_destroy(struct linted_waiter const* waiter);

#endif /* LINTED_WAITER_H */
