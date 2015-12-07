/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
#ifndef LNTD_LOCALE_H
#define LNTD_LOCALE_H

#include "lntd/error.h"
#include "lntd/ko.h"

/**
 * @file
 *
 * Abstracts over various localizations of common text.
 */

lntd_error lntd_locale_on_bad_option(lntd_ko ko,
                                     char const *process_name,
                                     char const *bad_option);

lntd_error lntd_locale_try_for_more_help(lntd_ko ko,
                                         char const *process_name,
                                         char const *help_option);

lntd_error lntd_locale_version(lntd_ko ko, char const *package_string,
                               char const *copyright_year);

#endif /* LNTD_LOCALE_H */
