/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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

#include "linted/conf.h"
#include "linted/ko.h"
#include "linted/mem.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
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
	union unit_union list[];
};

static linted_error str_from_strs(char const *const *strs, char const **strp);
static linted_error bool_from_cstring(char const *str, bool *boolp);
static linted_error long_from_cstring(char const *str, long *longp);

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf);
static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf);

linted_error linted_unit_db_create(struct linted_unit_db **unitsp,
                                   struct linted_conf_db *conf_db)
{
	linted_error errnum;
	struct linted_unit_db *units;

	size_t size = linted_conf_db_size(conf_db);

	size_t mem_size = sizeof *units + size * sizeof units->list[0U];
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, mem_size);
		if (errnum != 0)
			return errnum;
		units = xx;
	}
	units->size = 0U;

	*unitsp = units;

	for (size_t ii = 0U; ii < size; ++ii) {
		union unit_union *unit = &units->list[ii];
		struct linted_conf *conf = linted_conf_db_get_conf(conf_db, ii);

		char const *file_name = linted_conf_peek_name(conf);

		char const *dot = strchr(file_name, '.');

		char const *suffix = dot + 1U;

		enum linted_unit_type unit_type;
		if (0 == strcmp(suffix, "socket")) {
			unit_type = UNIT_TYPE_SOCKET;
		} else if (0 == strcmp(suffix, "service")) {
			unit_type = UNIT_TYPE_SERVICE;
		} else {
			errnum = EINVAL;
			goto destroy_units;
		}

		char *unit_name = strndup(file_name, dot - file_name);
		if (NULL == unit_name) {
			errnum = errno;
			LINTED_ASSUME(errnum != 0);
			goto destroy_units;
		}

		unit->common.type = unit_type;
		unit->common.name = unit_name;

		switch (unit_type) {
		case UNIT_TYPE_SERVICE: {
			unit->service.pid = -1;

			errnum = service_create(&unit->service, conf);
			if (errnum != 0)
				goto destroy_units;
			break;
		}

		case UNIT_TYPE_SOCKET: {
			unit->socket.is_open = false;

			errnum = socket_create(&unit->socket, conf);
			if (errnum != 0)
				goto destroy_units;
			break;
		}
		}

		++units->size;
	}

	return 0;

destroy_units:
	linted_unit_db_destroy(units);

	return errnum;
}

void linted_unit_db_destroy(struct linted_unit_db *units)
{
	size_t size = units->size;
	union unit_union *list = units->list;

	for (size_t ii = 0U; ii < size; ++ii)
		linted_mem_free(list[ii].common.name);
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

static linted_error service_create(struct linted_unit_service *unit,
                                   struct linted_conf *conf)
{
	linted_error errnum;

	char const *const *types = linted_conf_find(conf, "Service", "Type");
	char const *const *exec_start =
	    linted_conf_find(conf, "Service", "ExecStart");
	char const *const *no_new_privss =
	    linted_conf_find(conf, "Service", "NoNewPrivileges");
	char const *const *files =
	    linted_conf_find(conf, "Service", "X-Linted-Files");
	char const *const *fstabs =
	    linted_conf_find(conf, "Service", "X-Linted-Fstab");
	char const *const *chdir_paths =
	    linted_conf_find(conf, "Service", "X-Linted-Chdir");
	char const *const *env_whitelist =
	    linted_conf_find(conf, "Service", "X-Linted-Environment-Whitelist");

	char const *type;
	{
		char const *xx;
		errnum = str_from_strs(types, &xx);
		if (errnum != 0)
			return errnum;
		type = xx;
	}

	if (NULL == exec_start)
		return EINVAL;

	char const *no_new_privs;
	{
		char const *xx;
		errnum = str_from_strs(no_new_privss, &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs = xx;
	}

	char const *fstab;
	{
		char const *xx;
		errnum = str_from_strs(fstabs, &xx);
		if (errnum != 0)
			return errnum;
		fstab = xx;
	}

	char const *chdir_path;
	{
		char const *xx;
		errnum = str_from_strs(chdir_paths, &xx);
		if (errnum != 0)
			return errnum;
		chdir_path = xx;
	}

	if (NULL == type) {
		/* simple type of service */
	} else if (0 == strcmp("simple", type)) {
		/* simple type of service */
	} else {
		return EINVAL;
	}

	bool no_new_privs_value = false;
	if (no_new_privs != NULL) {
		bool xx;
		errnum = bool_from_cstring(no_new_privs, &xx);
		if (errnum != 0)
			return errnum;
		no_new_privs_value = xx;
	}

	unit->exec_start = exec_start;
	unit->no_new_privs = no_new_privs_value;
	unit->files = files;
	unit->fstab = fstab;
	unit->chdir_path = chdir_path;
	unit->env_whitelist = env_whitelist;

	return 0;
}

static linted_error socket_create(struct linted_unit_socket *unit,
                                  struct linted_conf *conf)
{
	linted_error errnum;

	char const *const *paths =
	    linted_conf_find(conf, "Socket", "ListenMessageQueue");
	char const *const *maxmsgss =
	    linted_conf_find(conf, "Socket", "MessageQueueMaxMessages");
	char const *const *msgsizes =
	    linted_conf_find(conf, "Socket", "MessageQueueMessageSize");
	char const *const *temps =
	    linted_conf_find(conf, "Socket", "X-Linted-Temporary");

	char const *path;
	{
		char const *xx;
		errnum = str_from_strs(paths, &xx);
		if (errnum != 0)
			return errnum;
		path = xx;
	}

	char const *maxmsgs;
	{
		char const *xx;
		errnum = str_from_strs(maxmsgss, &xx);
		if (errnum != 0)
			return errnum;
		maxmsgs = xx;
	}

	char const *msgsize;
	{
		char const *xx;
		errnum = str_from_strs(msgsizes, &xx);
		if (errnum != 0)
			return errnum;
		msgsize = xx;
	}

	char const *temp;
	{
		char const *xx;
		errnum = str_from_strs(temps, &xx);
		if (errnum != 0)
			return errnum;
		temp = xx;
	}

	long maxmsgs_value;
	{
		long xx;
		errnum = long_from_cstring(maxmsgs, &xx);
		if (errnum != 0)
			return errnum;
		maxmsgs_value = xx;
	}

	long msgsize_value;
	{
		long xx;
		errnum = long_from_cstring(msgsize, &xx);
		if (errnum != 0)
			return errnum;
		msgsize_value = xx;
	}

	bool temp_value;
	{
		bool xx;
		errnum = bool_from_cstring(temp, &xx);
		if (errnum != 0)
			return errnum;
		temp_value = xx;
	}

	if (!temp_value)
		return EINVAL;

	unit->path = path;
	unit->maxmsgs = maxmsgs_value;
	unit->msgsize = msgsize_value;

	return 0;
}

struct linted_unit const *
linted_unit_db_get_unit_by_name(struct linted_unit_db const *units,
                                const char *name)
{
	for (size_t ii = 0U; ii < units->size; ++ii) {
		struct linted_unit const *unit = &units->list[ii].common;

		if (0 == strncmp(unit->name, name, LINTED_UNIT_NAME_MAX))
			return unit;
	}

	return NULL;
}

static linted_error str_from_strs(char const *const *strs, char const **strp)
{
	char const *str;
	if (NULL == strs) {
		str = NULL;
	} else {
		str = strs[0U];

		if (strs[1U] != NULL)
			return EINVAL;
	}

	*strp = str;
	return 0;
}

static linted_error bool_from_cstring(char const *str, bool *boolp)
{
	static char const *const yes_strs[] = { "1", "yes", "true", "on" };
	static char const *const no_strs[] = { "0", "no", "false", "off" };

	bool result;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(yes_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = true;
			goto return_result;
		}
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(no_strs); ++ii) {
		if (0 == strcmp(str, yes_strs[ii])) {
			result = false;
			goto return_result;
		}
	}

	return EINVAL;

return_result:
	*boolp = result;
	return 0;
}

static linted_error long_from_cstring(char const *str, long *longp)
{
	size_t length = strlen(str);
	unsigned long position = 1U;

	if ('0' == str[0U] && length != 1U)
		return EINVAL;

	unsigned long total = 0U;
	for (; length > 0U; --length) {
		char const digit = str[length - 1U];

		if (digit < '0' || digit > '9')
			return EINVAL;

		unsigned long sum =
		    total + ((unsigned)(digit - '0')) * position;
		if (sum > LONG_MAX)
			return ERANGE;

		total = sum;

		unsigned long next_position = 10U * position;
		if (next_position > LONG_MAX)
			return ERANGE;
		position = next_position;
	}

	*longp = total;
	return 0;
}
