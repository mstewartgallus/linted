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
#define _GNU_SOURCE

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

static linted_error mutex_lock(linted_ko mutex);
static linted_error mutex_unlock(linted_ko mutex_file);

static linted_error prepend(char **result, char const *base, char const *end);

linted_error linted_db_open(linted_db *dbp, linted_ko cwd, char const *pathname,
                            unsigned long flags)
{
	linted_error errnum;

	if ((flags & ~LINTED_DB_CREAT) != 0U)
		return EINVAL;

	if (0U == flags)
		return EINVAL;

	bool db_creat = (flags & LINTED_DB_CREAT) != 0U;

	linted_dir the_db;
	if (db_creat) {
		linted_dir xx;
		errnum = linted_dir_create(&xx, cwd, pathname, 0U, S_IRWXU);
		if (errnum != 0)
			return errnum;
		the_db = xx;
	} else {
		linted_dir xx;
		errnum =
		    linted_ko_open(&xx, cwd, pathname, LINTED_KO_DIRECTORY);
		if (errnum != 0)
			return errnum;
		the_db = xx;
	}

	/*
	 * This happens at startup and has to be consistent across every
	 * version of the database.
	 */
	linted_ko lock_file;
	linted_ko version_file;

try_to_open_lock_file : {
	linted_ko xx;
	errnum = linted_ko_open(&xx, the_db, GLOBAL_LOCK, LINTED_KO_RDWR);
	switch (errnum) {
	case 0:
		lock_file = xx;
		goto opened_lock_file;

	case ENOENT:
		goto try_to_create_lock_file;

	default:
		goto close_db;
	}
}

try_to_create_lock_file:
	if (-1 == mkfifoat(the_db, GLOBAL_LOCK, S_IRUSR | S_IWUSR)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		if (EEXIST == errnum)
			goto try_to_open_lock_file;
		goto close_db;
	}
	goto try_to_open_lock_file;

opened_lock_file : {
	errnum = mutex_lock(lock_file);
	if (errnum != 0)
		goto close_lock_file;

	/* Sole user of the database now */
	{
		linted_ko xx;
		errnum =
		    linted_ko_open(&xx, the_db, "version", LINTED_KO_RDONLY);
		switch (errnum) {
		case 0:
			version_file = xx;
			goto opened_version_file;

		case ENOENT:
			goto try_to_create_db;

		default:
			goto unlock_db;
		}
	}
}

opened_version_file : {
	/* Opening a created database */
	off_t version_file_size;
	{
		struct stat stats;
		if (-1 == fstat(version_file, &stats)) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto close_version_file;
		}

		version_file_size = stats.st_size;
	}

	/* Maybe we're a 32 bit binary and opening up a 64 bit file */
	if ((uintmax_t)version_file_size > (uintmax_t)SIZE_MAX) {
		errnum = ENOMEM;
		goto close_version_file;
	}

	char *version_text;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, version_file_size);
		if (errnum != 0)
			goto close_version_file;
		version_text = xx;
	}

	errnum = linted_io_read_all(version_file, NULL, version_text,
	                            version_file_size);
	if (errnum != 0)
		goto free_version_text;

	if (version_file_size != sizeof CURRENT_VERSION - 1U) {
		errnum = EINVAL;
		goto free_version_text;
	}

	if (memcmp(version_text, CURRENT_VERSION,
	           sizeof CURRENT_VERSION - 1U) != 0) {
		errnum = EINVAL;
		goto free_version_text;
	}

free_version_text:
	linted_mem_free(version_text);
}

close_version_file : {
	linted_error close_errnum = linted_ko_close(version_file);
	if (0 == errnum)
		errnum = close_errnum;
}
	goto unlock_db;

try_to_create_db : {
	/* Creating the initial database */
	if (!db_creat) {
		errnum = EINVAL;
		goto unlock_db;
	}

	linted_ko version_file_write;
	{
		linted_ko xx;
		errnum = linted_file_create(&xx, the_db, "version",
		                            LINTED_FILE_RDWR | LINTED_FILE_SYNC,
		                            S_IRUSR | S_IWUSR);
		if (errnum != 0)
			goto unlock_db;
		version_file_write = xx;
	}

	errnum = linted_io_write_all(version_file_write, NULL, CURRENT_VERSION,
	                             sizeof CURRENT_VERSION - 1U);

	{
		linted_error close_errnum = linted_ko_close(version_file_write);
		if (0 == errnum)
			errnum = close_errnum;
	}

	if (errnum != 0)
		goto unlock_db;

	if (-1 == mkdirat(the_db, TEMP_DIR, S_IRWXU)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto unlock_db;
	}

	if (-1 == mkdirat(the_db, FIELD_DIR, S_IRWXU)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		goto unlock_db;
	}
}

unlock_db : {
	linted_error unlock_errnum = mutex_unlock(lock_file);
	if (0 == errnum)
		errnum = unlock_errnum;
}

close_lock_file : {
	linted_error close_errnum = linted_ko_close(lock_file);
	if (0 == errnum)
		errnum = close_errnum;
}

	if (errnum != 0)
		goto close_db;

	*dbp = the_db;
	return 0;

close_db:
	linted_db_close(the_db);
	return errnum;
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
	linted_error errnum;

	char *field_path;
	{
		char *xx;
		errnum = prepend(&xx, FIELD_DIR "/", name);
		if (errnum != 0)
			return errnum;
		field_path = xx;
	}

	if (-1 == renameat(db, path, db, field_path)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	linted_mem_free(field_path);

	return errnum;
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

static linted_error mutex_lock(linted_ko mutex_file)
{
	linted_error errnum;

	linted_ko null;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, -1, "/dev/zero", LINTED_KO_RDONLY);
		if (errnum != 0)
			return errnum;
		null = xx;
	}

	if (-1 == splice(null, NULL, mutex_file, NULL, PAGE_SIZE, 0)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	{
		linted_error close_errnum = linted_ko_close(null);
		if (0 == errnum)
			errnum = close_errnum;
	}

	return errnum;
}

static linted_error mutex_unlock(linted_ko mutex_file)
{
	linted_error errnum;

	linted_ko null;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, -1, "/dev/null", LINTED_KO_WRONLY);
		if (errnum != 0)
			return errnum;
		null = xx;
	}

	if (-1 == splice(mutex_file, NULL, null, NULL, PAGE_SIZE,
	                 SPLICE_F_NONBLOCK)) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
	}

	{
		linted_error close_errnum = linted_ko_close(null);
		if (0 == errnum)
			errnum = close_errnum;
	}

	return errnum;
}
