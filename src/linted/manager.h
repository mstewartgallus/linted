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

struct linted_manager_arguments {
    int number;
};

struct linted_manager_reply {
    int number;
};

struct linted_manager_request {
    struct linted_manager_arguments arguments;
    struct linted_manager_reply reply;
};

int linted_manager_send_signal(void);
int linted_manager_wait_signal(void);

int linted_manager_receive_request(pid_t pid,
                                   struct linted_manager_request const *request,
                                   struct linted_manager_arguments *arguments);
int linted_manager_send_reply(pid_t pid,
                              struct linted_manager_request *request,
                              struct linted_manager_reply const * reply);
int linted_manager_finish_reply(pid_t pid, int errnum);

int linted_manager_send_request(pid_t pid,
                                struct linted_manager_request *request);

#endif                          /* LINTED_MANAGER_H */
