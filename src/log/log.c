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
#define _GNU_SOURCE

#include "config.h"

#include "linted/log.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <syslog.h>
#include <unistd.h>

void linted_log_open(char const *ident)
{
	/* For now, don't use LOG_PID because syslog is confused by
	 * CLONE_NEWPID.
	 */

	openlog(ident, LOG_CONS | LOG_NDELAY | LOG_PERROR, LOG_USER);
}

void linted_log(unsigned log_level, char const *format, ...)
{
	int priority;
	switch (log_level) {
	case LINTED_LOG_ERR:
		priority = LOG_ERR;
		break;

	default:
		assert(false);
		return;
	}

	va_list ap;
	va_start(ap, format);
	vsyslog(priority, format, ap);
	va_end(ap);
}
