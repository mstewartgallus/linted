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
#ifndef LINTED_SERVICE_H
#define LINTED_SERVICE_H

#include "linted/error.h"

/**
 * @file
 *
 * Services.
 */

enum linted_service {
    LINTED_SERVICE_INIT,
    LINTED_SERVICE_STDIN,
    LINTED_SERVICE_STDOUT,
    LINTED_SERVICE_STDERR,
    LINTED_SERVICE_GUI,
    LINTED_SERVICE_SIMULATOR,
    LINTED_SERVICE_LOGGER,
    LINTED_SERVICE_CONTROLLER,
    LINTED_SERVICE_UPDATER,
    LINTED_SERVICE_SHUTDOWNER
};

linted_error linted_service_for_name(enum linted_service *service,
                                     char const *name);

#endif /* LINTED_SERVICE_H */
