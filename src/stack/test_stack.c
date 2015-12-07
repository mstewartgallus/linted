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
#include "lntd/log.h"
#include "lntd/node.h"
#include "lntd/stack.h"
#include "lntd/start.h"
#include "lntd/util.h"

#include <stddef.h>
#include <stdlib.h>

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-stack-test"};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	struct lntd_stack *stack;
	{
		struct lntd_stack *xx;
		err = lntd_stack_create(&xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_stack_create: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		stack = xx;
	}

	static struct lntd_node nodes[20U];

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(nodes); ++ii) {
		lntd_stack_send(stack, &nodes[ii]);
	}

	for (size_t ii = 0U; ii < LNTD_ARRAY_SIZE(nodes); ++ii) {
		struct lntd_node *xx;
		err = lntd_stack_try_recv(stack, &xx);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_stack_try_recv: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	lntd_stack_destroy(stack);

	return EXIT_SUCCESS;
}
