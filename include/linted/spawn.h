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
#include "linted/ko.h"

#include <signal.h>

/**
 * @file
 *
 * Safely, spawn a new process.
 */

struct linted_spawn_file_actions;
struct linted_spawn_attr;

linted_error linted_spawn_attr_init(struct linted_spawn_attr **attrp);
void linted_spawn_attr_destroy(struct linted_spawn_attr *attr);

void linted_spawn_attr_setmask(struct linted_spawn_attr *attr,
                               sigset_t const *set);
void linted_spawn_attr_setdeparent(struct linted_spawn_attr *attr,
                                   _Bool deparent);
void linted_spawn_attr_setcloneflags(struct linted_spawn_attr *attr, int flags);
void linted_spawn_attr_setchrootdir(struct linted_spawn_attr *attr,
                                    char const *chrootdir);
linted_error linted_spawn_attr_setmount(struct linted_spawn_attr *attr,
                                        char const *source, char const *target,
                                        char const *filesystemtype,
                                        _Bool mkdir_flag, _Bool touch_flag,
                                        unsigned long mountflags,
                                        char const *data);

linted_error linted_spawn_file_actions_init(
    struct linted_spawn_file_actions **file_actionsp);
linted_error linted_spawn_file_actions_adddup2(
    struct linted_spawn_file_actions **file_actionsp, linted_ko oldko,
    linted_ko newko);
void linted_spawn_file_actions_destroy(
    struct linted_spawn_file_actions *file_actions);

/**
 * Spawns a ptraced child
 */
linted_error linted_spawn(pid_t *childp, linted_ko dirko, char const *path,
                          struct linted_spawn_file_actions const *file_actions,
                          struct linted_spawn_attr const *attr,
                          char const *const argv[], char const *const envp[]);

#endif /* LINTED_SPAWN_H */
