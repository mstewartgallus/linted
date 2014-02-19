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
#ifndef LINTED_PROCESS_SPAWNER_H
#define LINTED_PROCESS_SPAWNER_H

#include "linted/spawner.h"

#include <stddef.h>

/**
 * Runs a spawner process and starts main_loop in a separate
 * process. Spawned processes will return from this function.
 *
 * The process_spawner captures the current state (open files etc..)
 * and forks off copies of this state. This is useful for capturing a
 * copy of a known good process startup state.
 *
 * @returns -1 on error and a value in errno.
 */
int linted_process_spawner_run(linted_spawner spawner,
                               int const preserved_fildes[],
                               size_t preserved_fildes_size,
                               linted_spawner_task main_loop,
                               int const fildes[], size_t fildes_size);

#endif                          /* LINTED_PROCESS_SPAWNER_H */
