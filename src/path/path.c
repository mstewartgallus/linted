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
#define _GNU_SOURCE

#include "config.h"

#include "linted/environment.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <stdio.h>

/**
 * @todo Fall back to a replacement directory and print a warning
 * message
 */
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

	if (-1 ==
	    asprintf(packagep, "/%s/%s", "/tmp", PACKAGE_TARNAME)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		return err;
	}

	runtime_dir_path = "/tmp";
	linted_log(LINTED_LOG_WARNING,
	           "%s not set, falling back to runtime directory %s",
	           "XDG_RUNTIME_DIR", "/tmp");
	return 0;

got_runtime_dir_fallback:
	linted_log(LINTED_LOG_WARNING,
	           "%s not set, falling back to runtime directory %s",
	           "XDG_RUNTIME_DIR", runtime_dir_path);

got_runtime_dir_path:
	if (-1 == asprintf(packagep, "%s/%s", runtime_dir_path,
	                   PACKAGE_TARNAME)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto free_runtime_dir_path;
	}

free_runtime_dir_path:
	linted_mem_free(runtime_dir_path);

	return err;
}

/**
 * @todo Fall back to ~/.local/share
 */
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
		return EACCES;

	if (-1 == asprintf(packagep, "%s/%s", data_home_path,
	                   PACKAGE_TARNAME)) {
		err = errno;
		LINTED_ASSUME(err != 0);
		goto free_data_home_path;
	}

free_data_home_path:
	linted_mem_free(data_home_path);

	return err;
}
