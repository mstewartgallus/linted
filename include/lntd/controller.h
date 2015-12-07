/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#ifndef LNTD_CONTROLLER_H
#define LNTD_CONTROLLER_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <stdint.h>

/**
 * @file
 *
 * Communicates user input to another process.
 */

struct lntd_async_pool;
struct lntd_async_task;
union lntd_async_ck;

typedef lntd_ko lntd_controller;

struct lntd_controller_message {
	int_fast32_t z_tilt;
	int_fast32_t x_tilt;

	_Bool left : 1U;
	_Bool right : 1U;
	_Bool forward : 1U;
	_Bool back : 1U;

	_Bool jumping : 1U;
};

struct lntd_controller_task_send;
struct lntd_controller_task_recv;

lntd_error lntd_controller_task_send_create(
    struct lntd_controller_task_send **taskp, void *data);
void lntd_controller_task_send_destroy(
    struct lntd_controller_task_send *task);

struct lntd_async_task *lntd_controller_task_send_to_async(
    struct lntd_controller_task_send *task);
void *
lntd_controller_task_send_data(struct lntd_controller_task_send *task);
struct lntd_async_task *lntd_controller_task_send_prepare(
    struct lntd_controller_task_send *task, union lntd_async_ck task_ck,
    void *userstate, lntd_controller controller,
    struct lntd_controller_message const *message);

lntd_error lntd_controller_task_recv_create(
    struct lntd_controller_task_recv **taskp, void *data);
void lntd_controller_task_recv_destroy(
    struct lntd_controller_task_recv *task);

struct lntd_async_task *lntd_controller_task_recv_to_async(
    struct lntd_controller_task_recv *task);
void *
lntd_controller_task_recv_data(struct lntd_controller_task_recv *task);
struct lntd_async_task *lntd_controller_task_recv_prepare(
    struct lntd_controller_task_recv *task, union lntd_async_ck task_ck,
    void *userstate, lntd_controller controller);

lntd_error
lntd_controller_decode(struct lntd_controller_task_recv const *task,
                       struct lntd_controller_message *message);

#endif /* LNTD_CONTROLLER_H */
