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
#ifndef LINTED_CONTROLLER_H
#define LINTED_CONTROLLER_H

#include "linted/error.h"
#include "linted/ko.h"

#include <stdint.h>

/**
 * @file
 *
 * Communicates user input to another process.
 */

typedef linted_ko linted_controller;

struct linted_controller_message {
	int_fast32_t z_tilt;
	int_fast32_t x_tilt;

	_Bool left : 1U;
	_Bool right : 1U;
	_Bool forward : 1U;
	_Bool back : 1U;

	_Bool jumping : 1U;
};

struct linted_controller_task_send;
struct linted_controller_task_receive;

linted_error linted_controller_task_send_create(
    struct linted_controller_task_send **taskp, void *data);
void linted_controller_task_send_destroy(
    struct linted_controller_task_send *task);

struct linted_asynch_task *linted_controller_task_send_to_asynch(
    struct linted_controller_task_send *task);
struct linted_controller_task_send *
linted_controller_task_send_from_asynch(
    struct linted_asynch_task *task);
void *linted_controller_task_send_data(
    struct linted_controller_task_send *task);
void linted_controller_task_send_prepare(
    struct linted_controller_task_send *task, unsigned task_action,
    linted_controller controller,
    struct linted_controller_message const *message);

linted_error linted_controller_task_receive_create(
    struct linted_controller_task_receive **taskp, void *data);
void linted_controller_task_receive_destroy(
    struct linted_controller_task_receive *task);

struct linted_asynch_task *linted_controller_task_receive_to_asynch(
    struct linted_controller_task_receive *task);
struct linted_controller_task_receive *
linted_controller_task_receive_from_asynch(
    struct linted_asynch_task *task);
void *linted_controller_task_receive_data(
    struct linted_controller_task_receive *task);
void linted_controller_task_receive_prepare(
    struct linted_controller_task_receive *task, unsigned task_action,
    linted_controller controller);

linted_error linted_controller_decode(
    struct linted_controller_task_receive const *task,
    struct linted_controller_message *message);

#endif /* LINTED_CONTROLLER_H */
