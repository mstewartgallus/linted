
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
#include "config.h"

#include "linted/spawn.h"
#include "linted/util.h"

#include <errno.h>
#include <spawn.h>
#include <string.h>

posix_spawn_file_actions_t linted_spawn_file_actions(void) {
    posix_spawn_file_actions_t file_actions;
    const int error_code = posix_spawn_file_actions_init(&file_actions);
    if (-1 == error_code) {
        LINTED_ERROR("Could not create spawn file actions object: %s\n", strerror(errno));
    }
    return file_actions;
}

posix_spawnattr_t linted_spawnattr(void) {
    posix_spawnattr_t attrp;
    const int error_code = posix_spawnattr_init(&attrp);
    if (-1 == error_code) {
        LINTED_ERROR("Could not create spawn attributes object: %s\n", strerror(errno));
    }
    return attrp;
}

void linted_spawn_file_actions_addclose(posix_spawn_file_actions_t * file_actions,
                                        int fildes) {
    const int error_code = posix_spawn_file_actions_addclose(file_actions, fildes);
    if (-1 == error_code) {
        LINTED_ERROR("Could not add close file descriptor action: %s\n", strerror(errno));
    }
}

void linted_spawn_file_actions_destroy(posix_spawn_file_actions_t file_actions) {
    const int error_code = posix_spawn_file_actions_destroy(&file_actions);
    if (-1 == error_code) {
        LINTED_ERROR("Could not destroy spawn file actions object: %s\n", strerror(errno));
    }
}

void linted_spawnattr_destroy(posix_spawnattr_t attrp) {
    const int error_code = posix_spawnattr_destroy(&attrp);
    if (-1 == error_code) {
        LINTED_ERROR("Could not destroy spawn attributes object: %s\n", strerror(errno));
    }
}

pid_t linted_spawn(char * path,
                   posix_spawn_file_actions_t file_actions,
                   posix_spawnattr_t attrp,
                   char *const argv[], char *const envp[]) {
    pid_t pid;
    int error_code;
    do {
        error_code = posix_spawn(&pid, path, &file_actions, &attrp, argv, envp);
    } while (-1 == error_code && errno == EINTR);
    if (-1 == error_code) {
        LINTED_ERROR("Could not spawn process: %s\n", strerror(errno));
    }
    return pid;
}
