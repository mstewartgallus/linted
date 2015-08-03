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

#include "linted/log.h"
#include "linted/start.h"

#include <stdlib.h>
#include <unistd.h>

#include <pulse/context.h>
#include <pulse/error.h>
#include <pulse/mainloop.h>
#include <pulse/proplist.h>

static unsigned char audio_start(char const *const process_name,
                                 size_t argc, char const *const argv[]);

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-audio",
    .start = audio_start};

static unsigned char audio_start(char const *const process_name,
                                 size_t argc, char const *const argv[])
{
	pa_mainloop *mainloop = pa_mainloop_new();
	if (0 == mainloop) {
		linted_log(LINTED_LOG_ERROR, "pa_mainloop_new");
		return EXIT_FAILURE;
	}

	pa_proplist *proplist = pa_proplist_new();
	if (0 == proplist) {
		linted_log(LINTED_LOG_ERROR, "pa_proplist_new");
		return EXIT_FAILURE;
	}

	pa_proplist_sets(proplist, "PULSE_PROP_media.role", "game");

	pa_context *context = pa_context_new_with_proplist(
	    pa_mainloop_get_api(mainloop), process_name, proplist);
	if (0 == context) {
		linted_log(LINTED_LOG_ERROR, "pa_context_new");
		return EXIT_FAILURE;
	}

	int retval;
	{
		int xx;
		int pulse_err = pa_mainloop_run(mainloop, &xx);
		if (pulse_err < 0) {
			linted_log(LINTED_LOG_ERROR,
			           "pa_mainloop_run: %s",
			           pa_strerror(-pulse_err));
			return EXIT_FAILURE;
		}
		retval = xx;
	}

	pa_context_unref(context);
	pa_proplist_free(proplist);
	pa_mainloop_free(mainloop);

	return retval;
}
