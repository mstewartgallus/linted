/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_SPAWN_H
#define LNTD_SPAWN_H

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/pid.h"

/**
 * @file
 *
 * Safely, spawn a new process.
 */

struct lntd_spawn_file_actions;
struct lntd_spawn_attr;

lntd_error lntd_spawn_attr_init(struct lntd_spawn_attr **attrp);
void lntd_spawn_attr_destroy(struct lntd_spawn_attr *attr);

lntd_error lntd_spawn_file_actions_init(
    struct lntd_spawn_file_actions **file_actionsp);
void lntd_spawn_file_actions_destroy(
    struct lntd_spawn_file_actions *file_actions);

void lntd_spawn_attr_set_die_on_parent_death(
    struct lntd_spawn_attr *attrp);

void lntd_spawn_file_actions_set_stdin(
    struct lntd_spawn_file_actions *file_actions, lntd_ko newko);
void lntd_spawn_file_actions_set_stdout(
    struct lntd_spawn_file_actions *file_actions, lntd_ko newko);
void lntd_spawn_file_actions_set_stderr(
    struct lntd_spawn_file_actions *file_actions, lntd_ko newko);

lntd_error
lntd_spawn(lntd_pid *childp, lntd_ko dirko, char const *path,
           struct lntd_spawn_file_actions const *file_actions,
           struct lntd_spawn_attr const *attr, char const *const argv[],
           char const *const envp[]);

#endif /* LNTD_SPAWN_H */
