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
#ifndef LINTED_MANAGER_H
#define LINTED_MANAGER_H

#include <signal.h>

enum {
    LINTED_MANAGER_NUMBER
};

struct linted_manager_message {
    int type;
    int number;
};

int linted_manager_send_signal(void);
int linted_manager_wait_signal(void);

int linted_manager_receive_message(siginfo_t * info,
                                   struct linted_manager_message *message);

int linted_manager_send_message(pid_t pid,
                                struct linted_manager_message const *message);

#endif                          /* LINTED_MANAGER_H */
