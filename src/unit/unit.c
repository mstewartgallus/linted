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

#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/mem.h"
#include "lntd/proc.h"
#include "lntd/unit.h"
#include "lntd/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

struct lntd_unit_db {
	size_t size;
	struct lntd_unit *list;
};

lntd_error lntd_unit_db_create(struct lntd_unit_db **unitsp)
{
	lntd_error err;

	struct lntd_unit_db *units;
	{
		void *xx;
		err = lntd_mem_alloc(&xx, sizeof *units);
		if (err != 0)
			return err;
		units = xx;
	}

	units->size = 0U;
	units->list = 0;

	*unitsp = units;

	return 0;
}

lntd_error lntd_unit_db_add_unit(struct lntd_unit_db *units,
                                 struct lntd_unit **unitp)
{
	lntd_error err = 0;

	LNTD_ASSERT(units != 0);
	LNTD_ASSERT(unitp != 0);

	struct lntd_unit *list = units->list;
	size_t old_size = units->size;

	size_t new_size = old_size + 1U;
	LNTD_ASSERT(new_size > 0U);
	{
		void *xx = 0;
		err = lntd_mem_realloc_array(&xx, list, new_size,
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

void lntd_unit_db_destroy(struct lntd_unit_db *units)
{
	size_t size = units->size;
	struct lntd_unit *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii)
		lntd_mem_free(list[ii].name);
	lntd_mem_free(list);

	lntd_mem_free(units);
}

size_t lntd_unit_db_size(struct lntd_unit_db *units)
{
	return units->size;
}

struct lntd_unit *lntd_unit_db_get_unit(struct lntd_unit_db *units,
                                        size_t ii)
{
	return &units->list[ii];
}

struct lntd_unit *
lntd_unit_db_get_unit_by_name(struct lntd_unit_db *units,
                              char const *name)
{
	size_t size = units->size;
	struct lntd_unit *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii) {
		struct lntd_unit *unit = &list[ii];

		if (0 == strncmp(unit->name, name, LNTD_UNIT_NAME_MAX))
			return unit;
	}

	return 0;
}

lntd_error lntd_unit_name(lntd_proc pid,
                          char name[static LNTD_UNIT_NAME_MAX + 1U])
{
	lntd_error err;

	memset(name, 0, LNTD_UNIT_NAME_MAX + 1U);

	char path[sizeof "/proc/" - 1U +
	          LNTD_NUMBER_TYPE_STRING_SIZE(lntd_proc) +
	          sizeof "/environ" - 1U + 1U];
	if (-1 == sprintf(path, "/proc/%" PRIuMAX "/environ",
	                  (uintmax_t)pid)) {
		err = errno;
		LNTD_ASSUME(err != 0);
		return err;
	}

	lntd_ko ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD, path,
		                   LNTD_KO_RDONLY);
		if (LNTD_ERROR_FILE_NOT_FOUND == err)
			return ESRCH;
		if (err != 0)
			return err;
		ko = xx;
	}

	FILE *file = fdopen(ko, "r");
	if (0 == file) {
		err = errno;
		LNTD_ASSUME(err != 0);

		lntd_ko_close(ko);

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
			LNTD_ASSUME(err != 0);
		}
	}

	if (err != 0)
		goto free_buf;

	memset(name, 0, LNTD_UNIT_NAME_MAX + 1U);

	if (eof)
		goto free_buf;

	char *iter = buf;
	for (;;) {
		if (0 == strncmp("LINTED_SERVICE=", iter,
		                 strlen("LINTED_SERVICE="))) {
			strncpy(name, iter + strlen("LINTED_SERVICE="),
			        LNTD_UNIT_NAME_MAX);
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
	lntd_mem_free(buf);

	return err;
}

lntd_error lntd_unit_pid(lntd_proc *pidp, lntd_proc manager_pid,
                         char const *name)
{
	lntd_error err = 0;

	lntd_proc *children;
	size_t len;
	{
		lntd_proc *xx;
		size_t yy;
		err = lntd_proc_children(manager_pid, &xx, &yy);
		if (err != 0)
			return err;
		children = xx;
		len = yy;
	}
	if (0U == len)
		return ESRCH;

	lntd_proc child;
	bool found_child = false;
	for (size_t ii = 0U; ii < len; ++ii) {
		child = children[ii];

		char other_name[LNTD_UNIT_NAME_MAX + 1U];
		err = lntd_unit_name(child, other_name);
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
	lntd_mem_free(children);

	if (err != 0)
		return err;

	if (!found_child)
		return ESRCH;

	if (pidp != 0)
		*pidp = child;

	return 0;
}
