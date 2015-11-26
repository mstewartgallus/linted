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

#include "linted/error.h"
#include "linted/log.h"
#include "linted/node.h"
#include "linted/stack.h"
#include "linted/start.h"
#include "linted/util.h"

#include <stddef.h>
#include <stdlib.h>

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-stack-test"};

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
                                       char const *const argv[])
{
	linted_error err = 0;

	struct linted_stack *stack;
	{
		struct linted_stack *xx;
		err = linted_stack_create(&xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_stack_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		stack = xx;
	}

	static struct linted_node nodes[20U];

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(nodes); ++ii) {
		linted_stack_send(stack, &nodes[ii]);
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(nodes); ++ii) {
		struct linted_node *xx;
		err = linted_stack_try_recv(stack, &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_stack_try_recv: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	linted_stack_destroy(stack);

	return EXIT_SUCCESS;
}
