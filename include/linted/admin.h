/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LINTED_ADMIN_H
#define LINTED_ADMIN_H

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/unit.h"

#include <stddef.h>

/**
 * @file
 *
 * Monitor, probe and control an init process.
 */

struct linted_async_pool;
struct linted_async_task;

typedef linted_ko linted_admin_in;
typedef linted_ko linted_admin_out;

enum { LINTED_ADMIN_ADD_UNIT, LINTED_ADMIN_STATUS, LINTED_ADMIN_STOP };
typedef unsigned char linted_admin_type;

struct linted_admin_add_unit_request {
	linted_admin_type type;

	char const *name;

	char const *const *command;

	_Bool has_priority : 1U;
	_Bool has_limit_no_file : 1U;
	_Bool has_limit_msgqueue : 1U;
	_Bool has_limit_locks : 1U;

	_Bool clone_newuser : 1U;
	_Bool clone_newpid : 1U;
	_Bool clone_newipc : 1U;
	_Bool clone_newnet : 1U;
	_Bool clone_newns : 1U;
	_Bool clone_newuts : 1U;

	_Bool no_new_privs : 1U;
};

struct linted_admin_add_unit_reply {
	linted_admin_type type;
};

struct linted_admin_status_request {
	linted_admin_type type;
	char const *name;
};

struct linted_admin_status_reply {
	linted_admin_type type;
	_Bool is_up;
};

struct linted_admin_stop_request {
	linted_admin_type type;
	char const *name;
};

struct linted_admin_stop_reply {
	linted_admin_type type;
	_Bool was_up;
};

union linted_admin_request {
	linted_admin_type type;
	struct linted_admin_add_unit_request add_unit;
	struct linted_admin_status_request status;
	struct linted_admin_stop_request stop;
};

union linted_admin_reply {
	linted_admin_type type;
	struct linted_admin_add_unit_reply add_unit;
	struct linted_admin_status_reply status;
	struct linted_admin_stop_reply stop;
};

struct linted_admin_in_task_recv;
struct linted_admin_out_task_send;

linted_error linted_admin_in_task_recv_create(
    struct linted_admin_in_task_recv **taskp, void *data);
void linted_admin_in_task_recv_destroy(
    struct linted_admin_in_task_recv *task);

linted_error linted_admin_in_task_recv_request(
    union linted_admin_request **outp,
    struct linted_admin_in_task_recv *task);

void linted_admin_request_free(union linted_admin_request *outp);

void *
linted_admin_in_task_recv_data(struct linted_admin_in_task_recv *task);
void linted_admin_in_task_recv_prepare(
    struct linted_admin_in_task_recv *task,
    union linted_async_ck task_ck, linted_ko ko);
struct linted_async_task *linted_admin_in_task_recv_to_async(
    struct linted_admin_in_task_recv *task);
struct linted_admin_in_task_recv *
linted_admin_in_task_recv_from_async(struct linted_async_task *task);

linted_error linted_admin_out_task_send_create(
    struct linted_admin_out_task_send **taskp, void *data);
void linted_admin_out_task_send_destroy(
    struct linted_admin_out_task_send *task);

void linted_admin_out_task_send_prepare(
    struct linted_admin_out_task_send *task,
    union linted_async_ck task_ck, linted_ko ko,
    union linted_admin_reply const *reply);
void *linted_admin_out_task_send_data(
    struct linted_admin_out_task_send *task);
struct linted_async_task *linted_admin_out_task_send_to_async(
    struct linted_admin_out_task_send *task);
struct linted_admin_out_task_send *
linted_admin_out_task_send_from_async(struct linted_async_task *task);

linted_error
linted_admin_in_send(linted_admin_in admin,
                     union linted_admin_request const *request);

linted_error linted_admin_out_recv(linted_admin_out admin,
                                   union linted_admin_reply *reply);

#endif /* LINTED_ADMIN_H */
