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
 *implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_STR_H
#define LINTED_STR_H

#include "linted/error.h"
#include "linted/util.h"

#include <stddef.h>
#include <stdint.h>

/**
 * @file
 *
 */

linted_error linted_str_append(char **bufp, size_t *capp, size_t *sizep,
                               char const *str, size_t strsize);

linted_error linted_str_append_cstring(char **bufp, size_t *capp,
                                       size_t *sizep, char const *str);

linted_error linted_str_append_format(char **bufp, size_t *capp,
                                      size_t *sizep,
                                      char const *formatstr, ...)
    LINTED_FORMAT(__printf__, 4, 5);

#endif /* LINTED_STR_H */
