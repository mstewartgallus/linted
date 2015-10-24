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
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "linted/environment.h"

#include "linted/error.h"
#include "linted/mem.h"
#include "linted/utf.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <windows.h>
#include <winerror.h>

static void lock(void);
static void unlock(void);

linted_error linted_environment_set(char const *key, char const *value,
                                    _Bool overwrite)
{
	linted_error err = 0;

	wchar_t *key_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(key, &xx);
		if (err != 0)
			return err;
		key_utf2 = xx;
	}

	wchar_t *value_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(value, &xx);
		if (err != 0)
			goto free_key;
		value_utf2 = xx;
	}

	lock();

	if (!overwrite) {
		size_t size;
		{
			size_t xx;
			_wgetenv_s(&xx, 0, 0U, key_utf2);
			size = xx;
		}
		if (size != 0U)
			goto unlock;
	}

	if (_wputenv_s(key_utf2, value_utf2) != 0)
		err = LINTED_ERROR_OUT_OF_MEMORY;

unlock:
	unlock();

	linted_mem_free(value_utf2);

free_key:
	linted_mem_free(key_utf2);
	return err;
}

linted_error linted_environment_get(char const *key, char **valuep)
{
	linted_error err = 0;

	wchar_t *key_utf2;
	{
		wchar_t *xx;
		err = linted_utf_1_to_2(key, &xx);
		if (err != 0)
			return err;
		key_utf2 = xx;
	}

	lock();

	wchar_t *buffer = 0;

	{
		wchar_t *xx = 0;
		size_t yy = 0U;
		errno_t result = _wdupenv_s(&xx, &yy, key_utf2);
		switch (result) {
		case 0:
			break;

		case EINVAL:
			/* Work around a bug in Wine or Windows NT */
			break;

		default:
			err = LINTED_ERROR_OUT_OF_MEMORY;
			goto unlock;
		}
		buffer = xx;
	}

unlock:
	unlock();

	linted_mem_free(key_utf2);

	if (err != 0)
		goto free_buffer;

	char *value;
	if (0 == buffer) {
		value = 0;
	} else {
		char *xx;
		err = linted_utf_2_to_1(buffer, &xx);
		if (err != 0)
			goto free_buffer;
		value = xx;
	}

	*valuep = value;

free_buffer:
	linted_mem_free(buffer);

	return err;
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
