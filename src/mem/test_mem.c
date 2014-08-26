/*
 * Copyright 2014 Steven Stewart-Gallus
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
#include "config.h"

#include "linted/mem.h"
#include "linted/util.h"

#include <stdlib.h>

int main(void)
{
	linted_error errnum;
	{
		void *xx;
		errnum = linted_mem_alloc_array(&xx, 0U, 0U);
		linted_mem_free(xx);
	}
	if (errnum != 0) {
		LINTED_IMPOSSIBILITY("errnum == %i\n", errnum);
	}

	{
		void *xx;
		errnum = linted_mem_realloc_array(&xx, NULL, 0U, 0U);
		linted_mem_free(xx);
	}
	if (errnum != 0) {
		LINTED_IMPOSSIBILITY("errnum == %i\n", errnum);
	}

	return EXIT_SUCCESS;
}
