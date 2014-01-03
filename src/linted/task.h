/*
 * Copyright 2013 Steven Stewart-Gallus
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
#ifndef LINTED_TASK_H
#define LINTED_TASK_H

#include <sys/types.h>

typedef struct _linted_task_t {
    pid_t _process_id;
} linted_task_t;

/**
 * Spawns a task. The task may or may not be spawned in a seperate
 * address space.
 *
 * @param task The output task info (on success).
 * @param binary_name The value of argv[0].
 * @param subcommand The subcommand to execute.
 * @param fildes The file descriptors to pass to the subcommand and
 *               leave open (they are duplicated into the process see
 *               dup).
 */
int linted_task_spawn(linted_task_t * task, char * binary_name,
                      char const * subcommand, int const fildes[]);

#endif /* LINTED_TASK_H */
