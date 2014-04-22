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

#include "linted/str.h"

#include <spawn.h>

struct linted_spawn_file_actions;
struct linted_spawn_attr;

errno_t linted_spawn_attr_init(struct linted_spawn_attr ** attrp);
void linted_spawn_attr_setpgroup(struct linted_spawn_attr * attr, pid_t pgroup);
void linted_spawn_attr_destroy(struct linted_spawn_attr * attr);

errno_t linted_spawn_file_actions_init(struct linted_spawn_file_actions ** file_actionsp);
errno_t linted_spawn_file_actions_adddup2(struct linted_spawn_file_actions ** file_actionsp,
                                          int oldfildes,
                                          int newfildes);
void linted_spawn_file_actions_destroy(struct linted_spawn_file_actions * file_actions);

errno_t linted_spawn(pid_t * child, char const * path,
                     struct linted_spawn_file_actions const * file_actions,
                     struct linted_spawn_attr const * attr,
                     char * const argv[], char * const envp[]);

#endif                          /* LINTED_SPAWN_H */
