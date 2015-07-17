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
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/environment.h"

#include "linted/error.h"
#include "linted/util.h"

#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

linted_error linted_environment_set(char const *key, char const *value,
                                    _Bool overwrite)
{
	linted_error err = 0;

	pthread_mutex_lock(&mutex);

	if (-1 == setenv(key, value, overwrite)) {
		err = errno;
		LINTED_ASSUME(err != 0);
	}

	pthread_mutex_unlock(&mutex);

	return err;
}

linted_error linted_environment_get(char const *key, char **valuep)
{
	linted_error err = 0;

	pthread_mutex_lock(&mutex);

	char *value_dup;
	{
		char const *value = getenv(key);
		if (0 == value) {
			value_dup = 0;
			goto unlock_mutex;
		}

		value_dup = strdup(value);
		if (0 == value_dup) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}
	}

unlock_mutex:
	pthread_mutex_unlock(&mutex);

	*valuep = value_dup;
	return err;
}
