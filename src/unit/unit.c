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
#define _GNU_SOURCE

#include "config.h"

#include "linted/dir.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/fifo.h"
#include "linted/file.h"
#include "linted/pid.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>

union unit_union {
	struct linted_unit common;
	struct linted_unit_service service;
	struct linted_unit_socket socket;
};

struct linted_unit_db {
	size_t size;
	union unit_union *list;
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

	union unit_union *list = units->list;
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

	list[old_size].common.name = 0;
	*unitp = &(list[old_size].common);

	units->list = list;
	units->size = new_size;

	return err;
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

struct linted_unit *
linted_unit_db_get_unit(struct linted_unit_db *units, size_t ii)
{
	return &units->list[ii].common;
}

struct linted_unit *
linted_unit_db_get_unit_by_name(struct linted_unit_db *units,
                                char const *name)
{
	size_t size = units->size;
	union unit_union *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii) {
		struct linted_unit *unit = &list[ii].common;

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

	linted_pid pid;
	bool found_child = false;
	for (size_t ii = 0U; ii < len; ++ii) {
		linted_pid child = children[ii];

		char other_name[LINTED_UNIT_NAME_MAX + 1U];
		err = linted_unit_name(child, other_name);
		if (err != 0)
			goto free_buf;

		if (0 == strcmp(name, other_name)) {
			found_child = true;
			pid = child;
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
		*pidp = pid;

	return 0;
}

linted_error
linted_unit_socket_activate(struct linted_unit_socket *unit)
{
	linted_error err = 0;

	linted_unit_type type = unit->type;
	char const *path = unit->path;

	switch (type) {
	case LINTED_UNIT_SOCKET_TYPE_DIR:
		err = linted_dir_create(0, LINTED_KO_CWD, path, 0U,
		                        S_IRWXU);
		break;

	case LINTED_UNIT_SOCKET_TYPE_FILE:
		err = linted_file_create(0, LINTED_KO_CWD, path, 0U,
		                         S_IRWXU);
		break;

	case LINTED_UNIT_SOCKET_TYPE_FIFO: {
		int fifo_size = unit->fifo_size;

#if defined F_SETPIPE_SZ
		if (fifo_size >= 0) {
			linted_ko fifo;
			{
				linted_ko xx;
				err = linted_fifo_create(
				    &xx, LINTED_KO_CWD, path,
				    LINTED_FIFO_RDWR, S_IRWXU);
				if (err != 0)
					return err;
				fifo = xx;
			}

			if (-1 ==
			    fcntl(fifo, F_SETPIPE_SZ, fifo_size)) {
				err = errno;
				LINTED_ASSUME(err != 0);
			}

			linted_error close_err = linted_ko_close(fifo);
			if (0 == err)
				err = close_err;
		} else
#endif
		{
			err = linted_fifo_create(0, LINTED_KO_CWD, path,
			                         0U, S_IRWXU);
		}

		break;
	}
	}

	return err;
}
