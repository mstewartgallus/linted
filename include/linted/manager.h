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
#include <sys/un.h>

#define LINTED_MANAGER_PATH_MAX                     \
    (sizeof(struct sockaddr_un) - sizeof(sa_family_t))

typedef int linted_manager;

enum linted_manager_type {
    LINTED_MANAGER_STATUS,
    LINTED_MANAGER_STOP
};

enum linted_manager_service {
    LINTED_MANAGER_SERVICE_GUI,
    LINTED_MANAGER_SERVICE_SIMULATOR
};

struct linted_manager_status_request {
    enum linted_manager_type type;
    enum linted_manager_service service;
};

struct linted_manager_status_reply {
    bool is_up;
};

struct linted_manager_stop_request {
    enum linted_manager_type type;
    enum linted_manager_service service;
};

struct linted_manager_stop_reply {
    bool was_up;
};

union linted_manager_request {
    enum linted_manager_type type;
    struct linted_manager_status_request status;
    struct linted_manager_stop_request stop;
};

union linted_manager_reply {
    struct linted_manager_status_reply status;
    struct linted_manager_stop_reply stop;
};

errno_t linted_manager_bind(linted_manager * manager, int backlog,
                            char const *path, size_t path_len);

errno_t linted_manager_connect(linted_manager * manager,
                               char const *path, size_t path_len);

errno_t linted_manager_close(linted_manager manager);

errno_t linted_manager_path(linted_manager manager,
                            char buf[static LINTED_MANAGER_PATH_MAX],
                            size_t * len);

errno_t linted_manager_recv_request(linted_manager manager,
                                    union linted_manager_request *request,
                                    size_t * size);

errno_t linted_manager_send_reply(linted_manager manager,
                                  union linted_manager_reply const *reply);

errno_t linted_manager_send_request(linted_manager manager,
                                    union linted_manager_request const
                                    *request);

errno_t linted_manager_recv_reply(linted_manager manager,
                                  union linted_manager_reply *reply,
                                  size_t * size);

#endif                          /* LINTED_MANAGER_H */
