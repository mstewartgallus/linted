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
#ifndef LINTED_SPAWNER_H
#define LINTED_SPAWNER_H

#include "linted/server.h"

/**
 * Is a spawner. Is shareable.
 */
typedef linted_server_t linted_spawner_t;

typedef void (*linted_spawner_task_t) (linted_spawner_t spawner, int const fildes[]);

/**
 * Runs a spawner process and starts main_loop in a separate
 * process. Spawned processes will return from this function.
 *
 * The spawner captures the current state (open files etc..) and forks
 * off copies of this state. This is useful for capturing a copy of a
 * known good process startup state.
 *
 * @returns -1 on error and a value in errno.
 */
int linted_spawner_run(linted_spawner_task_t main_loop, int const fildes[]);

/**
 * Spawns a task. The task may or may not be spawned in a seperate
 * address space. Returns -1 on error and a value in errno.
 *
 * @param spawner The spawner.
 * @param func The function to execute.
 * @param inbox A file descriptor to pass to the spawned task.
 */
int linted_spawner_spawn(linted_spawner_t spawner,
                         linted_spawner_task_t func, int const fildes[]);

/**
 * Closes a spawner. Returns -1 on error and the error in errno.
 */
int linted_spawner_close(linted_spawner_t spawner);

#endif                          /* LINTED_SPAWNER_H */
