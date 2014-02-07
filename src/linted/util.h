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
#ifndef LINTED_UTIL_H
#define LINTED_UTIL_H

#include <stdlib.h>
#include <syslog.h>

#define LINTED_ARRAY_SIZE(array) ((sizeof (array)) / sizeof ((array)[0]))

#define LINTED_ERROR(format_string, ...)                                \
    do {                                                                \
        syslog(LOG_ERR, "Error in file %s, function %s, and line %d: " format_string, \
               __FILE__, __func__, __LINE__,  __VA_ARGS__);             \
        exit(EXIT_FAILURE);                                             \
    } while (0)

char const *linted_error_string_alloc(int errnum);

void linted_error_string_free(char const *error_string);

#endif                          /* LINTED_UTIL_H */
