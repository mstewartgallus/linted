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
#ifndef LINTED_GUI_H
#define LINTED_GUI_H

#include "linted/main_loop.h"
#include "linted/task.h"

#include <mqueue.h>
#include <stdint.h>

/**
 * A handle to access the gui. Is safe to share between processes.
 */
typedef mqd_t linted_gui_t;

linted_gui_t linted_gui_spawn(linted_task_spawner_t spawner,
                              linted_main_loop_t main_loop);

int linted_gui_send_update(linted_gui_t gui, uint8_t x, uint8_t y);

int linted_gui_close(linted_gui_t gui);

#endif                          /* LINTED_GUI_H */
