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
#define UNICODE
#define _UNICODE

#define WIN32_LEAN_AND_MEAN


#include "config.h"

#include "linted/db.h"

#include "linted/dir.h"
#include "linted/error.h"
#include "linted/file.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/random.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#define CURRENT_VERSION "0.0.0"

#define GLOBAL_LOCK "global.lock"

#define FIELD_DIR "fields"
#define TEMP_DIR "temp"

#define PAGE_SIZE 4096U

static linted_error prepend(char **result, char const *base, char const *end);

linted_error linted_db_open(linted_db *dbp, linted_ko cwd, char const *pathname,
                            unsigned long flags)
{
	return ENOSYS;
}

linted_error linted_db_close(linted_db db)
{
	return linted_ko_close(db);
}

linted_error linted_db_temp_file(linted_db db, linted_ko *kop, char **pathp)
{
	linted_error errnum = 0;

	static char const field_name[] = "field-XXXXXX.tmp";

	char *temp_path;
	errnum = prepend(&temp_path, TEMP_DIR "/", field_name);
	if (errnum != 0)
		return errnum;

	char *xxxxxx =
	    temp_path + strlen(temp_path) + sizeof "field-" - sizeof field_name;

try_again:
	for (size_t ii = 0U; ii < 6U; ++ii) {
		char random_char;
		for (;;) {
			/* Normally using the modulus would give a bad
			 * distribution but CHAR_MAX + 1U is a power of two
			 */
			random_char = linted_random_fast() % (CHAR_MAX + 1U);

			/* Throw out results and retry for an even
			 * distribution
			 */
			if ((random_char >= 'a' && random_char <= 'z') ||
			    (random_char >= 'A' && random_char <= 'Z') ||
			    (random_char >= '0' && random_char <= '9'))
				break;
		}

		xxxxxx[ii] = random_char;
	}

	linted_ko temp_field;
	errnum = linted_file_create(&temp_field, db, temp_path,
	                            LINTED_FILE_RDWR | LINTED_FILE_SYNC |
	                                LINTED_FILE_EXCL,
	                            S_IRUSR | S_IWUSR);
	if (EEXIST == errnum)
		goto try_again;

	if (errnum != 0) {
		linted_mem_free(temp_path);
		return errnum;
	}

	*pathp = temp_path;
	*kop = temp_field;
	return 0;
}

linted_error linted_db_temp_send(linted_db db, char const *path,
                                 char const *name)
{
	return ENOSYS;
}

static linted_error prepend(char **result, char const *base,
                            char const *pathname)
{
	linted_error errnum;
	size_t base_size = strlen(base);
	size_t pathname_size = strlen(pathname);

	size_t new_path_size = base_size + pathname_size + 1U;

	char *new_path;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, new_path_size);
		if (errnum != 0)
			return errnum;
		new_path = xx;
	}

	new_path[new_path_size - 1U] = '\0';
	memcpy(new_path, base, base_size);
	memcpy(new_path + base_size, pathname, pathname_size);

	*result = new_path;
	return 0;
}
