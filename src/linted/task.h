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

typedef struct _linted_task {
    pid_t _pid;
} linted_task_t;

typedef struct _linted_task_spawner {
    int _request_writer;
} linted_task_spawner_t;

typedef int (*linted_task_func_t)(linted_task_spawner_t spawner, int inbox);

/**
 * Spawns a task. The task may or may not be spawned in a seperate
 * address space. Returns -1 on error and a value in errno.
 *
 * @param task The output task info (on success).
 * @param spawner The spawner.
 * @param func The function to execute.
 * @param inbox A file descriptor to pass to the spawned task.
 */
int linted_task_spawn(linted_task_t * task, linted_task_spawner_t spawner,
                      linted_task_func_t func, int inbox);

/**
 * Closes a spawner. Returns -1 on error and the error in errno.
 */
int linted_task_spawner_close(linted_task_spawner_t spawner);

#endif /* LINTED_TASK_H */
