/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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

#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/pid.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <string.h>

struct linted_unit_db {
	size_t size;
	struct linted_unit *list;
};

linted_error linted_unit_db_create(struct linted_unit_db **unitsp)
{
	linted_error err;

	struct linted_unit_db *units;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *units);
		if (err != 0)
			return err;
		units = xx;
	}

	units->size = 0U;
	units->list = 0;

	*unitsp = units;

	return 0;
}

linted_error linted_unit_db_add_unit(struct linted_unit_db *units,
                                     struct linted_unit **unitp)
{
	linted_error err = 0;

	struct linted_unit *list = units->list;
	size_t old_size = units->size;

	size_t new_size = old_size + 1U;
	{
		void *xx;
		err = linted_mem_realloc_array(&xx, list, new_size,
		                               sizeof list[0U]);
		if (err != 0)
			return err;
		list = xx;
	}

	list[old_size].name = 0;
	*unitp = &(list[old_size]);

	units->list = list;
	units->size = new_size;

	return err;
}

void linted_unit_db_destroy(struct linted_unit_db *units)
{
	size_t size = units->size;
	struct linted_unit *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii)
		linted_mem_free(list[ii].name);
	linted_mem_free(list);

	linted_mem_free(units);
}

size_t linted_unit_db_size(struct linted_unit_db *units)
{
	return units->size;
}

struct linted_unit *
linted_unit_db_get_unit(struct linted_unit_db *units, size_t ii)
{
	return &units->list[ii];
}

struct linted_unit *
linted_unit_db_get_unit_by_name(struct linted_unit_db *units,
                                char const *name)
{
	size_t size = units->size;
	struct linted_unit *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_unit *unit = &list[ii];

		if (0 ==
		    strncmp(unit->name, name, LINTED_UNIT_NAME_MAX))
			return unit;
	}

	return 0;
}

linted_error
linted_unit_name(linted_pid pid,
                 char name[static LINTED_UNIT_NAME_MAX + 1U])
{
	linted_error err;

	memset(name, 0, LINTED_UNIT_NAME_MAX + 1U);

	char path[sizeof "/proc/" - 1U +
	          LINTED_NUMBER_TYPE_STRING_SIZE(linted_pid) +
	          sizeof "/environ" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/environ",
	                  (uintmax_t)pid)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	linted_ko ko;
	{
		linted_ko xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, path,
		                     LINTED_KO_RDONLY);
		if (LINTED_ERROR_FILE_NOT_FOUND == err)
			return ESRCH;
		if (err != 0)
			return err;
		ko = xx;
	}

	FILE *file = fdopen(ko, "r");
	if (0 == file) {
		err = errno;
		LINTED_ASSUME(err != 0);

		linted_ko_close(ko);

		return err;
	}

	/* Get the buffer all at once to avoid raciness. */
	char *buf = 0;
	bool eof = false;
	ssize_t zz;
	{
		char *xx = buf;
		size_t yy = 0U;

		errno = 0;
		zz = getline(&xx, &yy, file);
		buf = xx;
	}

	if (-1 == zz) {
		err = errno;
		/* May be zero */
		eof = true;
	}

	if (EOF == fclose(file)) {
		if (0 == err) {
			err = errno;
			LINTED_ASSUME(err != 0);
		}
	}

	if (err != 0)
		goto free_buf;

	memset(name, 0, LINTED_UNIT_NAME_MAX + 1U);

	if (eof)
		goto free_buf;

	char *iter = buf;
	for (;;) {
		if (0 == strncmp("LINTED_SERVICE=", iter,
		                 strlen("LINTED_SERVICE="))) {
			strncpy(name, iter + strlen("LINTED_SERVICE="),
			        LINTED_UNIT_NAME_MAX);
			break;
		}
		iter = strchr(iter, '\0');
		if (0 == iter) {
			err = EINVAL;
			break;
		}
		++iter;
		if ('\0' == *iter)
			break;
		if ('\n' == *iter)
			break;
	}

free_buf:
	linted_mem_free(buf);

	return err;
}

linted_error linted_unit_pid(linted_pid *pidp, linted_pid manager_pid,
                             char const *name)
{
	linted_error err = 0;

	linted_pid *children;
	size_t len;
	{
		linted_pid *xx;
		size_t yy;
		err = linted_pid_children(manager_pid, &xx, &yy);
		if (err != 0)
			return err;
		children = xx;
		len = yy;
	}
	if (0U == len)
		return ESRCH;

	linted_pid child;
	bool found_child = false;
	for (size_t ii = 0U; ii < len; ++ii) {
		child = children[ii];

		char other_name[LINTED_UNIT_NAME_MAX + 1U];
		err = linted_unit_name(child, other_name);
		if (EACCES == err) {
			err = 0U;
			continue;
		}
		if (err != 0)
			goto free_buf;

		if (0 == strcmp(name, other_name)) {
			found_child = true;
			break;
		}
	}

free_buf:
	linted_mem_free(children);

	if (err != 0)
		return err;

	if (!found_child)
		return ESRCH;

	if (pidp != 0)
		*pidp = child;

	return 0;
}
