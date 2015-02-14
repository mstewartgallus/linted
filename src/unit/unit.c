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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#include "linted/mem.h"
#include "linted/unit.h"

#include <stddef.h>
#include <string.h>

union unit_union
{
	struct linted_unit common;
	struct linted_unit_service service;
	struct linted_unit_socket socket;
};

struct linted_unit_db
{
	size_t size;
	union unit_union *list;
};

linted_error linted_unit_db_create(struct linted_unit_db **unitsp)
{
	linted_error errnum;

	struct linted_unit_db *units;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *units);
		if (errnum != 0)
			return errnum;
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
	linted_error errnum = 0;

	union unit_union *list = units->list;
	size_t old_size = units->size;

	size_t new_size = old_size + 1U;
	{
		void *xx;
		errnum = linted_mem_realloc_array(&xx, list, new_size,
		                                  sizeof list[0U]);
		if (errnum != 0)
			return errnum;
		list = xx;
	}

	list[old_size].common.name = 0;
	*unitp = &(list[old_size].common);

	units->list = list;
	units->size = new_size;

	return errnum;
}

void linted_unit_db_destroy(struct linted_unit_db *units)
{
	size_t size = units->size;
	union unit_union *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii)
		linted_mem_free(list[ii].common.name);
	linted_mem_free(list);

	linted_mem_free(units);
}

size_t linted_unit_db_size(struct linted_unit_db *units)
{
	return units->size;
}

struct linted_unit *linted_unit_db_get_unit(struct linted_unit_db *units,
                                            size_t ii)
{
	return &units->list[ii].common;
}

struct linted_unit *
linted_unit_db_get_unit_by_name(struct linted_unit_db *units, char const *name)
{
	for (size_t ii = 0U; ii < units->size; ++ii) {
		struct linted_unit *unit = &units->list[ii].common;

		if (0 == strncmp(unit->name, name, LINTED_UNIT_NAME_MAX))
			return unit;
	}

	return 0;
}
