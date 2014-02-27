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

#include <stddef.h>
#include <sys/types.h>

#define LINTED_SPAWNER_MAX_FILDES_COUNT (20)

/**
 * Is a spawner. Is shareable.
 */
typedef int linted_spawner;

typedef int linted_spawner_future;

/**
 * A spawner task exits succesfully with 0 or unsuccessfully with -1
 * and an error value in errno.
 */
typedef int (*linted_spawner_task) (void * context, int const fildes[]);

struct linted_spawner_request {
    int fildes[LINTED_SPAWNER_MAX_FILDES_COUNT];
    size_t fildes_count;
    linted_spawner_task task;
};

/**
 * Spawns a task. The task may or may not be spawned in a seperate
 * address space. Returns -1 on error and a value in errno.
 *
 * @param spawner The spawner.
 * @param func The function to execute.
 */
int linted_spawner_spawn(linted_spawner spawner, linted_spawner_task func,
                         int const fildes[], size_t fildes_size);

int linted_spawner_pair(linted_spawner spawners[2]);

/**
 * Closes a spawner. Returns -1 on error and the error in errno.
 */
int linted_spawner_close(linted_spawner spawner);

ssize_t linted_spawner_recv_future(linted_spawner spawner,
                                   linted_spawner_future * future);

int linted_spawner_recv_request(linted_spawner_future future,
                                struct linted_spawner_request *request);

int linted_spawner_deny_request(linted_spawner_future future, int errnum);

#endif                          /* LINTED_SPAWNER_H */
