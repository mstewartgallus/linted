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
#define _POSIX_C_SOURCE 200809L
#define LINTED_START__NO_MAIN 1

#include "config.h"

#include "linted/start.h"

#include "linted/async.h"
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/signal.h"
#include "linted/str.h"
#include "linted/util.h"

#include <errno.h>
#include <libgen.h>
#include <locale.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static void do_nothing(int signo);

int linted_start__main(struct linted_start_config const *config,
                       int argc, char *argv[])
{
	linted_error err = 0;

	for (;;) {
		linted_ko ko;
		{
			linted_ko xx;
			err =
			    linted_ko_open(&xx, LINTED_KO_CWD,
			                   "/dev/null", LINTED_KO_RDWR);
			if (err != 0)
				return EXIT_FAILURE;
			ko = xx;
		}

		if (ko > 2U) {
			linted_ko_close(ko);
			break;
		}
	}

	char const *process_name = 0;

	bool missing_name = false;

	char const *service;
	{
		char *xx;
		err = linted_environment_get("LINTED_SERVICE", &xx);
		if (err != 0)
			return EXIT_FAILURE;
		service = xx;
	}
	if (service != 0) {
		process_name = service;
	} else if (argc > 0) {
		process_name = argv[0U];
	} else {
		process_name = config->canonical_process_name;
		missing_name = true;
	}

	char *process_basename;
	{
		char *xx;
		err = linted_str_dup(&xx, process_name);
		if (err != 0)
			return EXIT_FAILURE;
		process_basename = xx;
	}
	process_basename = basename(process_basename);

	if (!config->dont_init_logging)
		linted_log_open(process_basename);

	if (missing_name) {
		linted_log(LINTED_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	{
		struct sigaction act = {0};
		sigemptyset(&act.sa_mask);
		act.sa_handler = do_nothing;
		act.sa_flags = 0;
		if (-1 == sigaction(LINTED_ASYNCH_SIGNO, &act, 0)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (!config->dont_init_signals) {
		err = linted_signal_init();
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_signal_init: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (0 == setlocale(LC_ALL, "")) {
		linted_log(LINTED_LOG_ERROR, "setlocale: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	tzset();

	return config->start(process_basename, argc,
	                     (char const *const *)argv);
}

static void do_nothing(int signo)
{
	/* Do nothing */
}
