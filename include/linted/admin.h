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

#include <stddef.h>
#include <stdint.h>

/**
 * @file
 *
 * Monitor, probe and control an init process.
 */

struct linted_async_pool;
struct linted_async_task;
union linted_async_ck;

typedef int_least32_t linted_admin_bool;
typedef int_least32_t linted_admin_enum;

typedef linted_ko linted_admin_in;
typedef linted_ko linted_admin_out;

enum { LINTED_ADMIN_ADD_UNIT,
       LINTED_ADMIN_ADD_SOCKET,
       LINTED_ADMIN_STATUS,
       LINTED_ADMIN_STOP };
typedef linted_admin_enum linted_admin_type;

struct linted_admin_request_add_unit {
	char *name;
	char *fstab;
	char *chdir_path;

	struct {
		uint_least32_t command_len;
		char **command_val;
	} command;

	struct {
		uint_least32_t environment_len;
		char **environment_val;
	} environment;

	int_least64_t *timer_slack_nsec;
	int_least64_t *priority;
	int_least64_t *limit_no_file;
	int_least64_t *limit_msgqueue;
	int_least64_t *limit_locks;
	int_least64_t *limit_memlock;

	linted_admin_bool clone_newuser;
	linted_admin_bool clone_newpid;
	linted_admin_bool clone_newipc;
	linted_admin_bool clone_newnet;
	linted_admin_bool clone_newns;
	linted_admin_bool clone_newuts;

	linted_admin_bool no_new_privs : 1U;
};

struct linted_admin_request_add_socket {
	char *name;
	char *path;
	int_least32_t fifo_size;
	int_least32_t sock_type;
};

struct linted_admin_request_status {
	char *name;
};

struct linted_admin_request_stop {
	char *name;
};

struct linted_admin_request {
	linted_admin_type type;
	union {
		struct linted_admin_request_add_unit add_unit;
		struct linted_admin_request_add_socket add_socket;
		struct linted_admin_request_status status;
		struct linted_admin_request_stop stop;
	} linted_admin_request_u;
};

struct linted_admin_reply_add_unit {
	char dummy;
};

struct linted_admin_reply_add_socket {
	char dummy;
};

struct linted_admin_reply_status {
	linted_admin_bool is_up;
};

struct linted_admin_reply_stop {
	linted_admin_bool was_up;
};

struct linted_admin_reply {
	linted_admin_type type;
	union {
		struct linted_admin_reply_add_unit add_unit;
		struct linted_admin_reply_add_socket add_socket;
		struct linted_admin_reply_status status;
		struct linted_admin_reply_stop stop;
	} linted_admin_reply_u;
};

struct linted_admin_in_task_recv;
struct linted_admin_out_task_send;

linted_error linted_admin_in_task_recv_create(
    struct linted_admin_in_task_recv **taskp, void *data);
void linted_admin_in_task_recv_destroy(
    struct linted_admin_in_task_recv *task);

linted_error linted_admin_in_task_recv_request(
    struct linted_admin_request **outp,
    struct linted_admin_in_task_recv *task);

void linted_admin_request_free(struct linted_admin_request *outp);

void *
linted_admin_in_task_recv_data(struct linted_admin_in_task_recv *task);
struct linted_async_task *linted_admin_in_task_recv_prepare(
    struct linted_admin_in_task_recv *task,
    union linted_async_ck task_ck, void *userstate, linted_ko ko);
struct linted_async_task *linted_admin_in_task_recv_to_async(
    struct linted_admin_in_task_recv *task);

linted_error linted_admin_out_task_send_create(
    struct linted_admin_out_task_send **taskp, void *data);
void linted_admin_out_task_send_destroy(
    struct linted_admin_out_task_send *task);

struct linted_async_task *linted_admin_out_task_send_prepare(
    struct linted_admin_out_task_send *task,
    union linted_async_ck task_ck, void *userstate, linted_ko ko,
    struct linted_admin_reply const *reply);
void *linted_admin_out_task_send_data(
    struct linted_admin_out_task_send *task);
struct linted_async_task *linted_admin_out_task_send_to_async(
    struct linted_admin_out_task_send *task);

linted_error
linted_admin_in_send(linted_admin_in admin,
                     struct linted_admin_request const *request);

linted_error linted_admin_out_recv(linted_admin_out admin,
                                   struct linted_admin_reply *reply);

#endif /* LINTED_ADMIN_H */
