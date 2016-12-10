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

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/start.h"
#include "lntd/test.h"

#include <stddef.h>
#include <stdlib.h>

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-ko-test", 0};

static unsigned char lntd_start_main(char const *const process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err;

	lntd_ko root;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, "/",
		                   LNTD_KO_DIRECTORY);
		/* Can happen on Windows NT */
		if (LNTD_ERROR_PERMISSION == err)
			return EXIT_SUCCESS;
		if (err != 0)
			LNTD_TEST_FAILURE("err == %s\n",
			                  lntd_error_string(err));
		root = xx;
	}

	err = lntd_ko_close(root);
	if (err != 0)
		LNTD_TEST_FAILURE("err == %s\n",
		                  lntd_error_string(err));

	return EXIT_SUCCESS;
}
