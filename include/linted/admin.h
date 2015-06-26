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

struct linted_asynch_pool;
struct linted_asynch_task;

typedef linted_ko linted_admin_in;
typedef linted_ko linted_admin_out;

enum { LINTED_ADMIN_STATUS, LINTED_ADMIN_STOP };
typedef unsigned char linted_admin_type;
struct linted_admin_status_request
{
	linted_admin_type type;
	size_t size;
	char name[LINTED_UNIT_NAME_MAX];
};

struct linted_admin_status_reply
{
	_Bool is_up;
};

struct linted_admin_stop_request
{
	linted_admin_type type;
	size_t size;
	char name[LINTED_UNIT_NAME_MAX];
};

struct linted_admin_stop_reply
{
	_Bool was_up;
};

union linted_admin_request
{
	linted_admin_type type;
	struct linted_admin_status_request status;
	struct linted_admin_stop_request stop;
};

union linted_admin_reply
{
	struct linted_admin_status_reply status;
	struct linted_admin_stop_reply stop;
};

struct linted_admin_in_task_read;
struct linted_admin_out_task_write;

linted_error linted_admin_in_task_read_create(
    struct linted_admin_in_task_read **taskp, void *data);
void linted_admin_in_task_read_destroy(
    struct linted_admin_in_task_read *task);

linted_admin_in
linted_admin_in_task_read_ko(struct linted_admin_in_task_read *task);
union linted_admin_request const *linted_admin_in_task_read_request(
    struct linted_admin_in_task_read *task);
void *
linted_admin_in_task_read_data(struct linted_admin_in_task_read *task);
void linted_admin_in_task_read_prepare(
    struct linted_admin_in_task_read *task, unsigned task_action,
    linted_ko ko);
struct linted_asynch_task *linted_admin_in_task_read_to_asynch(
    struct linted_admin_in_task_read *task);
struct linted_admin_in_task_read *
linted_admin_in_task_read_from_asynch(struct linted_asynch_task *task);

linted_error linted_admin_out_task_write_create(
    struct linted_admin_out_task_write **taskp, void *data);
void linted_admin_out_task_write_destroy(
    struct linted_admin_out_task_write *task);

void linted_admin_out_task_write_prepare(
    struct linted_admin_out_task_write *task, unsigned task_action,
    linted_ko ko, union linted_admin_reply const *reply);
void *linted_admin_out_task_write_data(
    struct linted_admin_out_task_write *task);
struct linted_asynch_task *linted_admin_out_task_write_to_asynch(
    struct linted_admin_out_task_write *task);
struct linted_admin_out_task_write *
linted_admin_out_task_write_from_asynch(
    struct linted_asynch_task *task);

linted_error
linted_admin_in_write(linted_admin_in admin,
                      union linted_admin_request const *request);

linted_error linted_admin_out_read(linted_admin_out admin,
                                   union linted_admin_reply *reply);

#endif /* LINTED_ADMIN_H */
