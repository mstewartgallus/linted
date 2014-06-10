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
#ifndef LINTED_SPAWN_H
#define LINTED_SPAWN_H

#include "linted/error.h"

#include <sys/types.h>

/**
 * @file
 *
 * Safely, spawn a new process.
 */

struct linted_spawn_file_actions;
struct linted_spawn_attr;

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp);
void linted_spawn_attr_setpgroup(struct linted_spawn_attr *attr, pid_t pgroup);
void linted_spawn_attr_destroy(struct linted_spawn_attr *attr);

linted_error linted_spawn_file_actions_init(
    struct linted_spawn_file_actions **file_actionsp);
linted_error linted_spawn_file_actions_adddup2(
    struct linted_spawn_file_actions **file_actionsp, linted_ko oldko,
    linted_ko newko);
void linted_spawn_file_actions_destroy(
    struct linted_spawn_file_actions *file_actions);

linted_error linted_spawn(pid_t *child, linted_ko dirko, char const *path,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char *const argv[], char *const envp[]);

#endif /* LINTED_SPAWN_H */
