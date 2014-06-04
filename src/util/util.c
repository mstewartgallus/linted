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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/util.h"

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

extern char **environ;

static linted_error close_fds_except(int const *kept_fds, size_t size);

linted_error linted_util_sanitize_environment(void)
{
    /* Sanitize the environment */
    for (char **env = environ; *env != NULL; ++env) {
        memset(*env, '\0', strlen(*env));
    }
    environ = NULL;

    return 0;
}
