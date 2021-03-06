/*
 * Copyright 2015 Steven Stewart-Gallus
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
#ifndef LNTD_ENV_H
#define LNTD_ENV_H

#include "lntd/error.h"

/**
 * @file
 *
 * Manipulate a process environment.
 */

/**
 * @todo Deprecated `lntd_env_set` as it is racy in multithreaded
 * environments.
 */
lntd_error lntd_env_set(char const *key, char const *value,
                        unsigned char overwrite);

lntd_error lntd_env_get(char const *key, char **valuep);

#endif /* LNTD_ENV_H */
