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
#define LINTED_START_OBJECT

#include "linted/start.h"

#include "linted/async.h"
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/log.h"

#include <errno.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

int linted_start__main(int argc, char *argv[])
{
	linted_error err = 0;

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
		process_name =
		    linted_start_config.canonical_process_name;
		missing_name = true;
	}

	char *process_basename = strdup(process_name);
	if (0 == process_basename)
		return EXIT_FAILURE;
	process_basename = basename(process_basename);

	linted_log_open(process_basename);

	if (missing_name) {
		linted_log(LINTED_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	return linted_start_config.start(process_basename, argc,
	                                 (char const *const *)argv);
}
