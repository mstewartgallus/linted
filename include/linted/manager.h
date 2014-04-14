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

#include <errno.h>
#include <stdbool.h>
#include <signal.h>

enum {
    LINTED_MANAGER_START
#define LINTED_MANAGER_START LINTED_MANAGER_START
};

enum {
    LINTED_MANAGER_SERVICE_GUI,
#define LINTED_MANAGER_SERVICE_GUI LINTED_MANAGER_SERVICE_GUI

    LINTED_MANAGER_SERVICE_SIMULATOR
#define LINTED_MANAGER_SERVICE_SIMULATOR LINTED_MANAGER_SERVICE_SIMULATOR
};

struct linted_manager_start_reply {
    bool is_up;
};

struct linted_manager_start_request {
    unsigned type;
    unsigned service;
};

union linted_manager_request {
    unsigned type;
    struct linted_manager_start_request start;
    char padding[120];
};

union linted_manager_reply {
    struct linted_manager_start_reply start;
    char padding[120];
};

errno_t linted_manager_recv_request(int manager,
                                    union linted_manager_request *request);

errno_t linted_manager_send_reply(int manager,
                                  union linted_manager_reply const *reply);

errno_t linted_manager_send_request(int manager,
                                    union linted_manager_request const
                                    *request);

errno_t linted_manager_recv_reply(int manager,
                                  union linted_manager_reply *reply);

#endif                          /* LINTED_MANAGER_H */
