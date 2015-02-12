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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/environment.h"

#include "linted/error.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>
#include <winerror.h>

static void lock(void);
static void unlock(void);

linted_error linted_environment_set(char const *key, char const *value,
                                    _Bool overwrite)
{
	return LINTED_ERROR_UNIMPLEMENTED;
}

/**
 * @todo Convert the key (which should be UTF-1) to UTF-2 and lookup
 *       the unicode environment variable version.
 */
linted_error linted_environment_get(char const *key, char **valuep)
{
	linted_error errnum = 0;

	lock();

	char *value_dup;
	{
		char const *value = getenv(key);
		if (0 == value) {
			value_dup = 0;
			goto unlock_mutex;
		}

		value_dup = strdup(value);
		if (0 == value_dup) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
		}
	}

unlock_mutex:
	unlock();

	*valuep = value_dup;
	return errnum;
}

static INIT_ONCE mutex_init_once = INIT_ONCE_STATIC_INIT;
static CRITICAL_SECTION mutex;

static void lock(void)
{
	bool pending;
	{
		BOOL xx;
		InitOnceBeginInitialize(&mutex_init_once, 0, &xx, 0);
		pending = xx;
	}
	if (!pending)
		goto lock;

	InitializeCriticalSection(&mutex);

	InitOnceComplete(&mutex_init_once, 0, 0);

lock:
	EnterCriticalSection(&mutex);
}

static void unlock(void)
{
	LeaveCriticalSection(&mutex);
}
