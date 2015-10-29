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

#include "linted/environment.h"
#include "linted/error.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/str.h"

#include <errno.h>
#include <libgen.h>
#include <string.h>

linted_error linted_path_package_runtime_dir(char **packagep)
{
	linted_error err = 0;

	char *runtime_dir_path = 0;

	{
		char *xx;
		err = linted_environment_get("XDG_RUNTIME_DIR", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_path;

	{
		char *xx;
		err = linted_environment_get("TMPDIR", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_fallback;

	{
		char *xx;
		err = linted_environment_get("TEMP", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_fallback;

	{
		char *xx;
		err = linted_environment_get("TMP", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_fallback;

	err = linted_str_format(packagep, "%s/%s", "/tmp",
	                        PACKAGE_TARNAME);
	if (err != 0)
		return err;

	linted_log(LINTED_LOG_WARNING,
	           "%s not set, falling back to runtime directory %s",
	           "XDG_RUNTIME_DIR", "/tmp");
	return 0;

got_runtime_dir_fallback:
	linted_log(LINTED_LOG_WARNING,
	           "%s not set, falling back to runtime directory %s",
	           "XDG_RUNTIME_DIR", runtime_dir_path);

got_runtime_dir_path:
	err = linted_str_format(packagep, "%s/%s", runtime_dir_path,
	                        PACKAGE_TARNAME);

	linted_mem_free(runtime_dir_path);

	return err;
}

linted_error linted_path_package_data_home(char **packagep)
{
	linted_error err = 0;

	char *data_home_path;
	{
		char *xx;
		err = linted_environment_get("XDG_DATA_HOME", &xx);
		if (err != 0)
			return err;
		data_home_path = xx;
	}
	if (0 == data_home_path)
		goto fallback;

	err = linted_str_format(packagep, "%s/%s", data_home_path,
	                        PACKAGE_TARNAME);

	linted_mem_free(data_home_path);

	return err;

fallback:
	;
	char *home_path;
	{
		char *xx;
		err = linted_environment_get("HOME", &xx);
		if (err != 0)
			return err;
		home_path = xx;
	}
	if (0 == home_path)
		return EACCES;

	err = linted_str_format(packagep, "%s/%s", home_path,
	                        "local/share");

	linted_mem_free(home_path);

	return err;
}

linted_error linted_path_base(char **basep, char const *str)
{
	linted_error err = 0;

	char *str_dup;
	{
		char *xx;
		err = linted_str_dup(&xx, str);
		if (err != 0)
			return err;
		str_dup = xx;
	}

	size_t base_len;
	{
		char const *base = basename(str_dup);
		base_len = strlen(base);
		memcpy(str_dup, base, base_len);
	}
	str_dup[base_len] = '\0';

	{
		void *xx;
		err = linted_mem_realloc(&xx, str_dup, base_len + 1U);
		if (err != 0) {
			linted_mem_free(str_dup);
			return err;
		}
		str_dup = xx;
	}

	*basep = str_dup;
	return 0;
}

linted_error linted_path_dir(char **dirp, char const *str)
{
	linted_error err = 0;

	char *str_dup;
	{
		char *xx;
		err = linted_str_dup(&xx, str);
		if (err != 0)
			return err;
		str_dup = xx;
	}

	size_t dir_len;
	{
		char const *dir = dirname(str_dup);
		dir_len = strlen(dir);
		memcpy(str_dup, dir, dir_len);
	}
	str_dup[dir_len] = '\0';

	{
		void *xx;
		err = linted_mem_realloc(&xx, str_dup, dir_len + 1U);
		if (err != 0) {
			linted_mem_free(str_dup);
			return err;
		}
		str_dup = xx;
	}

	*dirp = str_dup;
	return 0;
}
