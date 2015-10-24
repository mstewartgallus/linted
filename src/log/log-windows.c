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
#include "linted/io.h"
#include "linted/log.h"

#include <stdarg.h>

/**
 * @file
 *
 * @bug Windows NT: provide Event Tracing events.
 */

void linted_log_open(char const *ident)
{
}

void linted_log(linted_log_level log_level, char const *format, ...)
{

	va_list ap;
	va_start(ap, format);

	linted_io_write_va_list(LINTED_KO_STDERR, 0, format, ap);
	linted_io_write_string(LINTED_KO_STDERR, 0, "\n");

	va_end(ap);
}
