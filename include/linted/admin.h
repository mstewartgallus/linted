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

struct linted_admin_task_accept;
struct linted_admin_task_recv_request;
struct linted_admin_task_send_reply;

linted_error linted_admin_bind(linted_admin *admin, int backlog,
                               char const *path, size_t path_len);

linted_error linted_admin_connect(linted_admin *admin, char const *path,
                                  size_t path_len);

linted_error linted_admin_path(linted_admin admin,
                               char buf[static LINTED_ADMIN_PATH_MAX],
                               size_t *len);

void linted_admin_accept(struct linted_admin_task_accept *task,
                         unsigned task_action, linted_admin admin);

linted_error
linted_admin_task_accept_create(struct linted_admin_task_accept **taskp,
                                void *data);
void linted_admin_task_accept_destroy(struct linted_admin_task_accept *task);

linted_admin
linted_admin_task_accept_returned_ko(struct linted_admin_task_accept *task);
void linted_admin_task_accept_prepare(struct linted_admin_task_accept *task,
                                      unsigned task_action, linted_ko ko);
void *linted_admin_task_accept_data(struct linted_admin_task_accept *task);
struct linted_asynch_task *
linted_admin_task_accept_to_asynch(struct linted_admin_task_accept *task);
struct linted_admin_task_accept *
linted_admin_task_accept_from_asynch(struct linted_asynch_task *task);

linted_error linted_admin_task_recv_request_create(
    struct linted_admin_task_recv_request **taskp, void *data);
void linted_admin_task_recv_request_destroy(
    struct linted_admin_task_recv_request *task);

linted_admin
linted_admin_task_recv_request_ko(struct linted_admin_task_recv_request *task);
union linted_admin_request const *linted_admin_task_recv_request_request(
    struct linted_admin_task_recv_request *task);
void *linted_admin_task_recv_request_data(
    struct linted_admin_task_recv_request *task);
void linted_admin_task_recv_request_prepare(
    struct linted_admin_task_recv_request *task, unsigned task_action,
    linted_ko ko);
struct linted_asynch_task *linted_admin_task_recv_request_to_asynch(
    struct linted_admin_task_recv_request *task);
struct linted_admin_task_recv_request *
linted_admin_task_recv_request_from_asynch(struct linted_asynch_task *task);

linted_error
linted_admin_task_send_reply_create(struct linted_admin_task_send_reply **taskp,
                                    void *data);
void
linted_admin_task_send_reply_destroy(struct linted_admin_task_send_reply *task);

void
linted_admin_task_send_reply_prepare(struct linted_admin_task_send_reply *task,
                                     unsigned task_action, linted_ko ko,
                                     union linted_admin_reply const *reply);
void *
linted_admin_task_send_reply_data(struct linted_admin_task_send_reply *task);
struct linted_asynch_task *linted_admin_task_send_reply_to_asynch(
    struct linted_admin_task_send_reply *task);
struct linted_admin_task_send_reply *
linted_admin_task_send_reply_from_asynch(struct linted_asynch_task *task);

linted_error
linted_admin_send_request(linted_admin admin,
                          union linted_admin_request const *request);

linted_error linted_admin_recv_reply(linted_admin admin,
                                     union linted_admin_reply *reply,
                                     size_t *size);

#endif /* LINTED_ADMIN_H */
