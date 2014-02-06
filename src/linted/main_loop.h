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
#ifndef LINTED_MAIN_LOOP_H
#define LINTED_MAIN_LOOP_H

#include "linted/task.h"

#include <mqueue.h>

typedef mqd_t linted_main_loop_t;

int linted_main_loop_run(linted_task_spawner_t spawner);

int linted_main_loop_request_close(linted_main_loop_t main_loop);

int linted_main_loop_close(linted_main_loop_t main_loop);

#endif                          /* LINTED_MAIN_LOOP_H */
