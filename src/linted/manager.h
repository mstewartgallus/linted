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

#include <stdbool.h>
#include <signal.h>

enum {
    LINTED_MANAGER_START
#define LINTED_MANAGER_START LINTED_MANAGER_START
};

enum {
    LINTED_MANAGER_SERVICE_GUI,
    LINTED_MANAGER_SERVICE_SIMULATOR
};

struct linted_manager_req;

struct linted_manager_start_args {
    unsigned service;
};

struct linted_manager_start_reply {
    bool is_up;
};


struct linted_manager_start_req {
    unsigned type;
    struct linted_manager_start_args args;
    struct linted_manager_start_reply reply;
};

int linted_manager_send_signal(void);
int linted_manager_wait_signal(void);

int linted_manager_req_type(pid_t pid,
                            struct linted_manager_req const *request);

int linted_manager_start_req_args(pid_t pid,
                                  struct linted_manager_req const *request,
                                  struct linted_manager_start_args *args);

int linted_manager_start_req_reply(pid_t pid,
                                   struct linted_manager_req *request,
                                   struct linted_manager_start_reply const *reply);

int linted_manager_finish_reply(pid_t pid, int errnum);

int linted_manager_send_request(pid_t pid,
                                struct linted_manager_req *request);

#endif                          /* LINTED_MANAGER_H */
