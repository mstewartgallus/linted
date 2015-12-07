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
#ifndef LNTD_STR_H
#define LNTD_STR_H

#include "lntd/error.h"
#include "lntd/util.h"

#include <stddef.h>

/**
 * @file
 *
 */

lntd_error lntd_str_dup(char **result, char const *input);

lntd_error lntd_str_dup_len(char **result, char const *input, size_t n);

lntd_error lntd_str_append(char **bufp, size_t *capp, size_t *sizep,
                           char const *str, size_t strsize);

lntd_error lntd_str_append_cstring(char **bufp, size_t *capp,
                                   size_t *sizep, char const *str);

lntd_error lntd_str_append_format(char **bufp, size_t *capp,
                                  size_t *sizep, char const *formatstr,
                                  ...) LNTD_FORMAT(__printf__, 4, 5);

lntd_error lntd_str_format(char **strp, char const *format, ...)
    LNTD_FORMAT(__printf__, 2, 3);

#endif /* LNTD_STR_H */
