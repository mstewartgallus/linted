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

#include "lntd/env.h"

#include "lntd/error.h"
#include "lntd/str.h"
#include "lntd/util.h"

#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>

static pthread_mutex_t mutex;

lntd_error lntd_env_set(char const *key, char const *value,
                        unsigned char overwrite)
{
	lntd_error err = 0;

	err = pthread_mutex_lock(&mutex);
	if (err != 0) {
		LNTD_ASSERT(err != EDEADLK);
		LNTD_ASSERT(false);
	}

	if (-1 == setenv(key, value, overwrite)) {
		err = errno;
		LNTD_ASSUME(err != 0);
	}

	{
		lntd_error unlock_err = pthread_mutex_unlock(&mutex);
		if (unlock_err != 0) {
			LNTD_ASSERT(unlock_err != EPERM);
			LNTD_ASSERT(false);
		}
	}

	return err;
}

lntd_error lntd_env_get(char const *key, char **valuep)
{
	lntd_error err = 0;

	err = pthread_mutex_lock(&mutex);
	if (err != 0) {
		LNTD_ASSERT(err != EDEADLK);
		LNTD_ASSERT(false);
	}

	char *value_dup = 0;

	char const *value = getenv(key);
	if (0 == value)
		goto unlock_mutex;

	{
		char *xx;
		err = lntd_str_dup(&xx, value);
		if (err != 0)
			goto unlock_mutex;
		value_dup = xx;
	}

unlock_mutex : {
	lntd_error unlock_err = pthread_mutex_unlock(&mutex);
	if (unlock_err != 0) {
		LNTD_ASSERT(unlock_err != EPERM);
		LNTD_ASSERT(false);
	}
}

	if (0 == err)
		*valuep = value_dup;

	return err;
}

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
