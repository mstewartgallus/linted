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
#define _GNU_SOURCE

#include "config.h"

#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/util.h"

#include <stdarg.h>
#include <stdbool.h>
#include <syslog.h>

static linted_ko tty;
static bool tty_init;

static void do_syslog(int prio, char const *format, va_list args)
    LINTED_FORMAT(__printf__, 2, 0);

void linted_log_open(char const *ident)
{
	/* For now, don't use LOG_PID because syslog is confused by
	 * CLONE_NEWPID.
	 */

	openlog(ident, LOG_CONS | LOG_NDELAY, LOG_USER);

	if (0 == linted_ko_open(&tty, LINTED_KO_CWD, "/dev/tty",
	                        LINTED_KO_WRONLY))
		tty_init = true;
}

void linted_log(linted_log_level log_level, char const *format, ...)
{
	int priority;
	switch (log_level) {
	case LINTED_LOG_ERROR:
		priority = LOG_ERR;
		break;

	case LINTED_LOG_WARNING:
		priority = LOG_WARNING;
		break;

	case LINTED_LOG_INFO:
		priority = LOG_INFO;
		break;

	default:
		LINTED_ASSERT(false);
		return;
	}

	va_list ap;
	va_start(ap, format);

	do_syslog(priority, format, ap);

	if (tty_init) {
		linted_io_write_va_list(tty, 0, format, ap);
		linted_io_write_string(tty, 0, "\n");
	}

	va_end(ap);
}

static void do_syslog(int prio, char const *format, va_list args)
{
	va_list cp;
	va_copy(cp, args);

	vsyslog(prio, format, cp);

	va_end(cp);
}
