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
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "lntd/error.h"
#include "lntd/log.h"
#include "lntd/start.h"

#include <stdlib.h>
#include <windows.h>

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-monitor", 0};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	for (;;) {
		MSG message = {0};
		if (!GetMessage(&message, 0, 0, 0)) {
			err = HRESULT_FROM_WIN32(GetLastError());
			LNTD_ASSUME(err != 0);

			char const *error_str = lntd_error_string(err);
			lntd_log(LNTD_LOG_ERROR, "GetMessage: %s",
			         error_str);
			lntd_error_string_free(error_str);
		}
	}

	return EXIT_SUCCESS;
}
