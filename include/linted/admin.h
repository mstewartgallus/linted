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
#ifndef LINTED_ADMIN_H
#define LINTED_ADMIN_H

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/unit.h"

#include <sys/un.h>

/**
 * @file
 *
 * Monitor, probe and control an init process.
 */

#define LINTED_ADMIN_PATH_MAX (sizeof(struct sockaddr_un) - sizeof(sa_family_t))

typedef linted_ko linted_admin;

enum linted_admin_type {
	LINTED_ADMIN_STATUS,
	LINTED_ADMIN_STOP
};

struct linted_admin_status_request
{
	enum linted_admin_type type;
	size_t size;
	char name[LINTED_UNIT_NAME_MAX];
};

struct linted_admin_status_reply
{
	_Bool is_up;
};

struct linted_admin_stop_request
{
	enum linted_admin_type type;
	size_t size;
	char name[LINTED_UNIT_NAME_MAX];
};

struct linted_admin_stop_reply
{
	_Bool was_up;
};

union linted_admin_request
{
	enum linted_admin_type type;
	struct linted_admin_status_request status;
	struct linted_admin_stop_request stop;
};

union linted_admin_reply
{
	struct linted_admin_status_reply status;
	struct linted_admin_stop_reply stop;
};

struct linted_admin_task_accept
{
	struct linted_ko_task_accept parent;
};

struct linted_admin_task_recv_request
{
	struct linted_ko_task_read parent;
	union linted_admin_request request;
};

struct linted_admin_task_send_reply
{
	struct linted_ko_task_write parent;
	union linted_admin_reply reply;
};

linted_error linted_admin_bind(linted_admin *admin, int backlog,
                               char const *path, size_t path_len);

void linted_admin_accept(struct linted_admin_task_accept *task,
                         unsigned task_action, linted_admin admin);

linted_error linted_admin_connect(linted_admin *admin, char const *path,
                                  size_t path_len);

linted_error linted_admin_path(linted_admin admin,
                               char buf[static LINTED_ADMIN_PATH_MAX],
                               size_t *len);

void linted_admin_recv_request(struct linted_admin_task_recv_request *task,
                               unsigned task_action, linted_admin admin);

void linted_admin_send_reply(struct linted_admin_task_send_reply *task,
                             unsigned task_action, linted_admin admin,
                             union linted_admin_reply const *reply);

linted_error
linted_admin_send_request(linted_admin admin,
                          union linted_admin_request const *request);

linted_error linted_admin_recv_reply(linted_admin admin,
                                     union linted_admin_reply *reply,
                                     size_t *size);

#endif /* LINTED_ADMIN_H */
