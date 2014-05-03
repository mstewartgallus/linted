/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#ifndef LINTED_LOCALE_H
#define LINTED_LOCALE_H

#include <errno.h>

#include "linted/str.h"

errno_t linted_locale_missing_process_name(int fildes,
                                           struct linted_str package_name);

errno_t linted_locale_on_bad_option(int fildes, char const* program_name,
                                    char const* bad_option);

errno_t linted_locale_try_for_more_help(int fildes, char const* program_name,
                                        struct linted_str help_option);

errno_t linted_locale_version(int fildes, struct linted_str package_string,
                              struct linted_str copyright_year);

#endif /* LINTED_LOCALE_H */
