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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_SIMULATOR_LOOP_H
#define LINTED_SIMULATOR_LOOP_H

#include "linted/controller.h"
#include "linted/gui.h"
#include "linted/spawner.h"

#include <mqueue.h>

/**
 * A handle to access the simulator_loop. Is safe to share between processes.
 */
typedef mqd_t linted_simulator_loop_t;

int linted_simulator_loop_pair(linted_simulator_loop_t simulator_loop[2]);

int linted_simulator_loop_run(linted_simulator_loop_t simulator_loop,
                              linted_controller_t simulator);

int linted_simulator_loop_send_shutdown(linted_simulator_loop_t simulator_loop);

int linted_simulator_loop_close(linted_simulator_loop_t simulator_loop);

#endif                          /* LINTED_SIMULATOR_LOOP_H */
