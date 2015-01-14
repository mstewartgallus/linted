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
#define _GNU_SOURCE

#include "config.h"

#include "linted/start.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>

#if defined _WIN32 || defined _WIN64
/* Do nothing */
#elif defined __linux__
static void do_nothing(int signo);
#else
#error no IO cancellation implementation for this platform
#endif

int main(int argc, char *argv[])
{
	linted_error errnum = 0;

#if defined _WIN32 || defined _WIN64
/**
 * I do not remember if Windows applications might need sometimes to
 * open up standard IO handles
 */
#elif defined __linux__
	for (;;) {
		linted_ko ko;
		{
			linted_ko xx;
			errnum =
			    linted_ko_open(&xx, LINTED_KO_CWD, "/dev/null", 0);
			if (errnum != 0)
				return EXIT_FAILURE;
			ko = xx;
		}

		if (ko > 3U) {
			linted_ko_close(ko);
			break;
		}
	}
#else
#error no missing standard IO handle fallback implementation for this platform
#endif

	char const *process_name;
	if (argc < 1) {
		process_name = linted_start_config.canonical_process_name;
	} else {
		process_name = argv[0U];
	}

	linted_log_open(process_name);

	if (argc < 1) {
		linted_log(LINTED_LOG_ERR, "missing process name");
		return EXIT_FAILURE;
	}

#if defined _WIN32 || defined _WIN64
/* Do nothing */
#elif defined __linux__
	{
		struct sigaction act = { 0 };
		sigemptyset(&act.sa_mask);
		act.sa_handler = do_nothing;
		act.sa_flags = 0;
		if (-1 == sigaction(LINTED_ASYNCH_SIGNO, &act, 0)) {
			linted_log(LINTED_LOG_ERR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}
#else
#error no IO cancellation implementation for this platform
#endif

	return linted_start(process_name, argc, (char const * const *)argv);
}

#if defined _WIN32 || defined _WIN64
/* Do nothing */
#elif defined __linux__
static void do_nothing(int signo)
{
	/* Do nothing */
}
#else
#error no IO cancellation implementation for this platform
#endif
