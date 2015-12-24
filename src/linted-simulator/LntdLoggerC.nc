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
#include "config.h"

#include "logger.h"
#include "lntd/util.h"

#include <stdio.h>
#include <syslog.h>

generic module LntdLoggerC()
{
	provides interface LntdLogger;
	uses interface LntdStdio;
}
implementation
{
	char const *myident = 0;

	command void LntdLogger.init(char const *ident)
	{
		LNTD_ASSERT(myident == 0);

		if (0 == (myident = strdup(ident))) {
			/* Just silently fail */
			return;
		}

		openlog(myident, LOG_CONS | LOG_NDELAY, LOG_USER);
	}

	command void LntdLogger.log(lntd_logger_level level,
	                            char const *buf)
	{
		char *fullstr;

		LNTD_ASSERT(LNTD_LOGGER_ERROR == level);

		{
			char *xx;
			if (-1 ==
			    asprintf(&xx, "%s: %s\n", myident, buf))
				goto log;
			fullstr = xx;
		}

		call LntdStdio.write(LNTD_KO_STDERR, fullstr,
		                     strlen(fullstr));

		free(fullstr);

	log:
		syslog(LOG_ERR, "%s", buf);
	}
}
