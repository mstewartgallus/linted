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

#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/str.h"

#include <errno.h>
#include <libgen.h>
#include <stddef.h>
#include <string.h>

lntd_error lntd_path_package_runtime_dir(char **packagep)
{
	lntd_error err = 0;

	char *runtime_dir_path = 0;

	{
		char *xx;
		err = lntd_env_get("XDG_RUNTIME_DIR", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_path;

	{
		char *xx;
		err = lntd_env_get("TMPDIR", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_fallback;

	{
		char *xx;
		err = lntd_env_get("TEMP", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_fallback;

	{
		char *xx;
		err = lntd_env_get("TMP", &xx);
		if (err != 0)
			return err;
		runtime_dir_path = xx;
	}
	if (runtime_dir_path != 0)
		goto got_runtime_dir_fallback;

	err =
	    lntd_str_format(packagep, "%s/%s", "/tmp", PACKAGE_TARNAME);
	if (err != 0)
		return err;

	lntd_log(LNTD_LOG_WARNING,
	         "%s not set, falling back to runtime directory %s",
	         "XDG_RUNTIME_DIR", "/tmp");
	return 0;

got_runtime_dir_fallback:
	lntd_log(LNTD_LOG_WARNING,
	         "%s not set, falling back to runtime directory %s",
	         "XDG_RUNTIME_DIR", runtime_dir_path);

got_runtime_dir_path:
	err = lntd_str_format(packagep, "%s/%s", runtime_dir_path,
	                      PACKAGE_TARNAME);

	lntd_mem_free(runtime_dir_path);

	return err;
}

lntd_error lntd_path_package_data_home(char **packagep)
{
	lntd_error err = 0;

	char *data_home_path;
	{
		char *xx;
		err = lntd_env_get("XDG_DATA_HOME", &xx);
		if (err != 0)
			return err;
		data_home_path = xx;
	}
	if (0 == data_home_path)
		goto fallback;

	err = lntd_str_format(packagep, "%s/%s", data_home_path,
	                      PACKAGE_TARNAME);

	lntd_mem_free(data_home_path);

	return err;

fallback:;
	char *home_path;
	{
		char *xx;
		err = lntd_env_get("HOME", &xx);
		if (err != 0)
			return err;
		home_path = xx;
	}
	if (0 == home_path)
		return EACCES;

	err = lntd_str_format(packagep, "%s/%s", home_path,
	                      "local/share");

	lntd_mem_free(home_path);

	return err;
}

lntd_error lntd_path_base(char **basep, char const *str)
{
	lntd_error err = 0;

	char *str_dup;
	{
		char *xx;
		err = lntd_str_dup(&xx, str);
		if (err != 0)
			return err;
		str_dup = xx;
	}

	size_t base_len;
	{
		char const *base = basename(str_dup);
		base_len = strlen(base);

		/* MAY OVERLAP SO USE MEMMOVE! */
		memmove(str_dup, base, base_len);
	}
	str_dup[base_len] = '\0';

	{
		void *xx;
		err = lntd_mem_realloc(&xx, str_dup, base_len + 1U);
		if (err != 0) {
			lntd_mem_free(str_dup);
			return err;
		}
		str_dup = xx;
	}

	*basep = str_dup;
	return 0;
}

lntd_error lntd_path_dir(char **dirp, char const *str)
{
	lntd_error err = 0;

	char *str_dup;
	{
		char *xx;
		err = lntd_str_dup(&xx, str);
		if (err != 0)
			return err;
		str_dup = xx;
	}

	size_t dir_len;
	{
		char const *dir = dirname(str_dup);
		dir_len = strlen(dir);

		/* MAY OVERLAP SO USE MEMMOVE! */
		memmove(str_dup, dir, dir_len);
	}
	str_dup[dir_len] = '\0';

	{
		void *xx;
		err = lntd_mem_realloc(&xx, str_dup, dir_len + 1U);
		if (err != 0) {
			lntd_mem_free(str_dup);
			return err;
		}
		str_dup = xx;
	}

	*dirp = str_dup;
	return 0;
}
