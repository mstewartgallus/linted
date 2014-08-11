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
#ifndef LINTED_STR_H
#define LINTED_STR_H

#include "linted/error.h"

#include <stddef.h>
#include <stdint.h>

/**
 * @file
 *
 * Not null terminated strings.
 */

struct linted_str
{
    char const *bytes;
    uint_least16_t size;
};

#define LINTED_STR(Str)                                                        \
    (struct linted_str)                                                        \
    {                                                                          \
        .size = sizeof Str - 1U, .bytes = Str                                  \
    }

linted_error linted_str_append(char **restrict bufp, size_t *restrict capp,
                               size_t *restrict sizep, char const *str,
                               size_t strsize);

linted_error linted_str_append_str(char **restrict bufp, size_t *restrict capp,
                                   size_t *restrict sizep,
                                   struct linted_str str);

linted_error linted_str_append_cstring(char **restrict bufp,
                                       size_t *restrict capp,
                                       size_t *restrict sizep, char const *str);

linted_error linted_str_append_format(char **restrict bufp,
                                      size_t *restrict capp,
                                      size_t *restrict sizep,
                                      char const *formatstr, ...);

#endif /* LINTED_STR_H */
