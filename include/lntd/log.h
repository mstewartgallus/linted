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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_LOG_H
#define LNTD_LOG_H

#include "lntd/util.h"

/**
 * @file
 *
 * Abstracts over the system logger.
 */

enum { LNTD_LOG_ERROR = 3, LNTD_LOG_WARNING = 4, LNTD_LOG_INFO = 6 };
typedef unsigned char lntd_log_level;

void lntd_log_open(char const *ident);

/**
 * @file
 *
 * @bug Does not sanitize strange UTF-8 or terminal control
 *      characters.  If an attacker controlled string is inserted into
 *      the system log they can gain privileges.  See also,
 *      http://marc.info/?l=bugtraq&m=104612710031920&q=p3 .
 */
void lntd_log(lntd_log_level log_level, char const *format, ...)
    LNTD_FORMAT(__printf__, 2, 3);

#endif /* LNTD_LOG_H */
