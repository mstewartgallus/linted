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
#ifndef LINTED_SPAWN_H
#define LINTED_SPAWN_H

#include <sys/types.h>

/**
 * Spawns a process.
 *
 * @param pid The output process id (on success).
 * @param binary_name The value of argv[0].
 * @param subcommand The subcommand to execute.
 * @param fildes The file descriptors to pass to the subcommand and
 *               leave open.
 */
int linted_spawn(pid_t * const pid, char * const binary_name,
                 char const * const subcommand, int const fildes[const]);

#endif /* LINTED_SPAWN_H */
