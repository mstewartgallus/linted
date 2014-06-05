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

#include "linted/util.h"

#include <string.h>

extern char **environ;

linted_error linted_util_sanitize_environment(void)
{
#ifndef __linux__
/* This error is important because some platforms have the environment
 * variable hidden in other places as well. For example, Windows has a
 * local cache of the environment variables AND an Operating System
 * list of environment variables.
 */
#error sanitizing environment variables has not been implemented for this platform
#endif

    /* Sanitize the environment */
    for (char **env = environ; *env != NULL; ++env) {
        memset(*env, '\0', strlen(*env));
    }
    environ = NULL;

    return 0;
}
