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
#include "config.h"

#include "linted/service.h"
#include "linted/util.h"

#include <errno.h>
#include <stddef.h>
#include <string.h>

struct pair
{
    char const *name;
    enum linted_service service;
};

struct pair const pairs[] = { { "init", LINTED_SERVICE_INIT },
                              { "gui", LINTED_SERVICE_GUI },
                              { "simulator", LINTED_SERVICE_SIMULATOR },
                              { "stdin", LINTED_SERVICE_STDIN },
                              { "stdout", LINTED_SERVICE_STDOUT },
                              { "stderr", LINTED_SERVICE_STDERR },
                              { "logger", LINTED_SERVICE_LOGGER },
                              { "controller", LINTED_SERVICE_CONTROLLER },
                              { "updater", LINTED_SERVICE_UPDATER },
                              { "shutdowner", LINTED_SERVICE_SHUTDOWNER } };

linted_error linted_service_for_name(enum linted_service *servicep,
                                     char const *name)
{
    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(pairs); ++ii) {
        if (0 == strcmp(name, pairs[ii].name)) {
            *servicep = pairs[ii].service;
            return 0;
        }
    }
    return EINVAL;
}
