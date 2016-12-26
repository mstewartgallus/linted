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
#ifndef LNTD_ADMIN_H
#define LNTD_ADMIN_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stddef.h>
#include <stdint.h>

/**
 * @file
 *
 * Monitor, probe and control an init process.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

typedef int_least32_t lntd_admin_bool;
typedef int_least32_t lntd_admin_enum;

typedef lntd_ko lntd_admin_in;
typedef lntd_ko lntd_admin_out;

enum { LNTD_ADMIN_ADD_UNIT,
       LNTD_ADMIN_ADD_SOCKET,
       LNTD_ADMIN_STATUS,
       LNTD_ADMIN_STOP };
typedef lntd_admin_enum lntd_admin_type;

struct lntd_admin_request_add_unit {
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

	lntd_admin_bool clone_newuser;
	lntd_admin_bool clone_newcgroup;
	lntd_admin_bool clone_newpid;
	lntd_admin_bool clone_newipc;
	lntd_admin_bool clone_newnet;
	lntd_admin_bool clone_newns;
	lntd_admin_bool clone_newuts;

	lntd_admin_bool no_new_privs;
	lntd_admin_bool seccomp;
};

struct lntd_admin_request_add_socket {
	char *name;
	char *path;
	int_least32_t fifo_size;
	int_least32_t sock_type;
};

struct lntd_admin_request_status {
	char *name;
};

struct lntd_admin_request_stop {
	char *name;
};

struct lntd_admin_request {
	lntd_admin_type type;
	union {
		struct lntd_admin_request_add_unit add_unit;
		struct lntd_admin_request_add_socket add_socket;
		struct lntd_admin_request_status status;
		struct lntd_admin_request_stop stop;
	} lntd_admin_request_u;
};

struct lntd_admin_reply_add_unit {
	char dummy;
};

struct lntd_admin_reply_add_socket {
	char dummy;
};

struct lntd_admin_reply_status {
	lntd_admin_bool is_up;
};

struct lntd_admin_reply_stop {
	lntd_admin_bool was_up;
};

struct lntd_admin_reply {
	lntd_admin_type type;
	union {
		struct lntd_admin_reply_add_unit add_unit;
		struct lntd_admin_reply_add_socket add_socket;
		struct lntd_admin_reply_status status;
		struct lntd_admin_reply_stop stop;
	} lntd_admin_reply_u;
};

struct lntd_admin_in_task_recv;
struct lntd_admin_out_task_send;

lntd_error
lntd_admin_in_task_recv_create(struct lntd_admin_in_task_recv **taskp,
                               void *data);
void lntd_admin_in_task_recv_destroy(
    struct lntd_admin_in_task_recv *task);

lntd_error
lntd_admin_in_task_recv_request(struct lntd_admin_request **outp,
                                struct lntd_admin_in_task_recv *task);

void lntd_admin_request_free(struct lntd_admin_request *outp);

void *
lntd_admin_in_task_recv_data(struct lntd_admin_in_task_recv *task);
void lntd_admin_in_task_recv_submit(
    struct lntd_async_pool *pool, struct lntd_admin_in_task_recv *task,
    union lntd_async_ck task_ck, void *userstate, lntd_ko ko);
void lntd_admin_in_task_recv_cancel(
    struct lntd_admin_in_task_recv *task);

lntd_error
lntd_admin_out_task_send_create(struct lntd_admin_out_task_send **taskp,
                                void *data);
void lntd_admin_out_task_send_destroy(
    struct lntd_admin_out_task_send *task);

void lntd_admin_out_task_send_submit(
    struct lntd_async_pool *pool, struct lntd_admin_out_task_send *task,
    union lntd_async_ck task_ck, void *userstate, lntd_ko ko,
    struct lntd_admin_reply const *reply);
void *
lntd_admin_out_task_send_data(struct lntd_admin_out_task_send *task);
void lntd_admin_out_task_send_cancel(
    struct lntd_admin_out_task_send *task);

lntd_error lntd_admin_in_send(lntd_admin_in admin,
                              struct lntd_admin_request const *request);

lntd_error lntd_admin_out_recv(lntd_admin_out admin,
                               struct lntd_admin_reply *reply);

#endif /* LNTD_ADMIN_H */
