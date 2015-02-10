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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "linted/environment.h"

#include "linted/error.h"
#include "linted/util.h"

#include <errno.h>
#include <stdlib.h>

linted_error linted_environment_set(char const *key, char const *value,
                                    _Bool overwrite)
{
	if (-1 == setenv(key, value, overwrite)) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	return 0;
}

char const *linted_environment_get(char const *key)
{
	return getenv(key);
}