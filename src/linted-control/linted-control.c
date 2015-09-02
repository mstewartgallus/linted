/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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

#include "linted/admin.h"
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static uint_fast8_t control_start(char const *const process_name,
                                  size_t argc,
                                  char const *const argv[]);

static uint_fast8_t run_status(char const *process_name, size_t argc,
                               char const *const argv[]);
static uint_fast8_t run_stop(char const *process_name, size_t argc,
                             char const *const argv[]);

static linted_error ctl_help(linted_ko ko, char const *process_name,
                             char const *package_name,
                             char const *package_url,
                             char const *package_bugreport);
static linted_error status_help(linted_ko ko, char const *process_name,
                                char const *package_name,
                                char const *package_url,
                                char const *package_bugreport);
static linted_error stop_help(linted_ko ko, char const *process_name,
                              char const *package_name,
                              char const *package_url,
                              char const *package_bugreport);
static linted_error failure(linted_ko ko, char const *process_name,
                            char const *message, linted_error err);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-control",
    .start = control_start};

static uint_fast8_t control_start(char const *const process_name,
                                  size_t argc, char const *const argv[])
{
	bool need_help = false;
	bool need_version = false;

	char const *bad_option = 0;
	char const *command = 0;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			command = argument;
			break;
		}
	}
	++last_index;

	if (need_help) {
		ctl_help(LINTED_KO_STDOUT, process_name, PACKAGE_NAME,
		         PACKAGE_URL, PACKAGE_BUGREPORT);
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR,
		                            process_name, bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT, PACKAGE_STRING,
		                      COPYRIGHT_YEAR);
		return EXIT_SUCCESS;
	}

	if (0 == command) {
		linted_log(LINTED_LOG_ERROR, "missing COMMAND");
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	linted_error err = 0;

	char const *pid;
	{
		char *xx;
		err = linted_environment_get("LINTED_PID", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		pid = xx;
	}
	char const *runtime_dir_path;
	{
		char *xx;
		err = linted_environment_get("XDG_RUNTIME_DIR", &xx);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_environment_get: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		runtime_dir_path = xx;
	}

	if (0 == pid) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "LINTED_PID");
		return EXIT_FAILURE;
	}

	/**
	 * @todo Use fallbacks for missing XDG environment variables.
	 */
	if (0 == runtime_dir_path) {
		linted_log(LINTED_LOG_ERROR,
		           "%s is a required environment variable",
		           "XDG_RUNTIME_HOME");
		return EXIT_FAILURE;
	}

	char *package_runtime_dir_path;
	{
		char *xx;
		if (-1 == asprintf(&xx, "%s/%s", runtime_dir_path,
		                   PACKAGE_TARNAME)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		package_runtime_dir_path = xx;
	}

	char *process_runtime_dir_path;
	{
		char *xx;
		if (-1 == asprintf(&xx, "%s/%s",
		                   package_runtime_dir_path, pid)) {
			linted_log(LINTED_LOG_ERROR, "asprintf: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
		process_runtime_dir_path = xx;
	}

	err = linted_ko_change_directory(process_runtime_dir_path);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_ko_change_directory: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	enum { STATUS, STOP };

	static char const *const commands[] = {[STATUS] = "status",
	                                       [STOP] = "stop"};

	int arg = -1;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(commands); ++ii) {
		if (0 == strcmp(command, commands[ii])) {
			arg = ii;
			break;
		}
	}

	size_t new_argc = argc - last_index + 1U;
	char const *const *new_argv = argv + last_index - 1U;
	switch (arg) {
	case STATUS:
		return run_status(process_name, new_argc, new_argv);

	case STOP:
		return run_stop(process_name, new_argc, new_argv);
	}

	linted_log(LINTED_LOG_ERROR, "urecognized command '%s'",
	           command);
	linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
	                                "--help");
	return EXIT_FAILURE;
}

static uint_fast8_t run_status(char const *process_name, size_t argc,
                               char const *const argv[])
{
	linted_error err;
	bool need_version = false;
	bool need_add_help = false;
	char const *name = 0;
	char const *bad_option = 0;
	char const *bad_argument = 0;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_add_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			if (name != 0) {
				bad_argument = argument;
				break;
			} else {
				name = argument;
			}
		}
	}

	if (need_add_help) {
		status_help(LINTED_KO_STDOUT, process_name,
		            PACKAGE_NAME, PACKAGE_URL,
		            PACKAGE_BUGREPORT);
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR,
		                            process_name, bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	if (bad_argument != 0) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT, PACKAGE_STRING,
		                      COPYRIGHT_YEAR);
		return EXIT_SUCCESS;
	}

	if (0 == name) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: missing SERVICE\n",
		                       process_name);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	size_t name_len = strlen(name);
	if (name_len > LINTED_UNIT_NAME_MAX) {
		failure(LINTED_KO_STDERR, process_name, "SERVICE",
		        EINVAL);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	linted_admin_in admin_in;
	{
		linted_admin_in xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, "admin-in",
		                     LINTED_KO_WRONLY);
		if (err != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        "can not create socket", err);
			return EXIT_FAILURE;
		}
		admin_in = xx;
	}

	linted_admin_out admin_out;
	{
		linted_admin_out xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, "admin-out",
		                     LINTED_KO_RDONLY);
		if (err != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        "can not create socket", err);
			return EXIT_FAILURE;
		}
		admin_out = xx;
	}

	linted_io_write_format(
	    LINTED_KO_STDOUT, 0,
	    "%s: sending the status request for %s\n", process_name,
	    name);

	{
		union linted_admin_request request = {0};

		request.type = LINTED_ADMIN_STATUS;
		request.status.size = name_len;
		memcpy(request.status.name, name, name_len);

		err = linted_admin_in_write(admin_in, &request);
		if (err != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        "can not send request", err);
			return EXIT_FAILURE;
		}
	}

	union linted_admin_reply reply;
	err = linted_admin_out_read(admin_out, &reply);
	if (err != 0) {
		failure(LINTED_KO_STDERR, process_name,
		        "can not read reply", err);
		return EXIT_FAILURE;
	}

	if (reply.status.is_up) {
		linted_io_write_format(LINTED_KO_STDOUT, 0,
		                       "%s: %s is up\n", process_name,
		                       name);
	} else {
		linted_io_write_format(LINTED_KO_STDOUT, 0,
		                       "%s: %s is down\n", process_name,
		                       name);
	}

	return EXIT_SUCCESS;
}

static uint_fast8_t run_stop(char const *process_name, size_t argc,
                             char const *const *const argv)
{
	linted_error err;
	bool need_version = false;
	bool need_add_help = false;
	char const *bad_option = 0;
	char const *bad_argument = 0;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_add_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			bad_argument = argument;
			break;
		}
	}

	if (need_add_help) {
		stop_help(LINTED_KO_STDOUT, process_name, PACKAGE_NAME,
		          PACKAGE_URL, PACKAGE_BUGREPORT);
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR,
		                            process_name, bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	if (bad_argument != 0) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(LINTED_KO_STDERR,
		                                process_name, "--help");
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT, PACKAGE_STRING,
		                      COPYRIGHT_YEAR);
		return EXIT_SUCCESS;
	}

	linted_admin_in admin_in;
	{
		linted_admin_in xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, "admin-in",
		                     LINTED_KO_WRONLY);
		if (err != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        "can not create socket", err);
			return EXIT_FAILURE;
		}
		admin_in = xx;
	}

	linted_admin_out admin_out;
	{
		linted_admin_out xx;
		err = linted_ko_open(&xx, LINTED_KO_CWD, "admin-out",
		                     LINTED_KO_RDONLY);
		if (err != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        "can not create socket", err);
			return EXIT_FAILURE;
		}
		admin_out = xx;
	}

	linted_io_write_format(
	    LINTED_KO_STDOUT, 0,
	    "%s: sending the stop request for the gui\n", process_name);

	{
		union linted_admin_request request = {0};

		request.type = LINTED_ADMIN_STOP;
		request.stop.size = sizeof "linteed-gui" - 1U;
		memcpy(request.stop.name, "linted-gui",
		       sizeof "linted-gui" - 1U);

		err = linted_admin_in_write(admin_in, &request);
	}
	if (err != 0) {
		failure(LINTED_KO_STDERR, process_name,
		        "can send request", err);
		return EXIT_FAILURE;
	}

	bool was_up;
	{
		union linted_admin_reply xx;
		err = linted_admin_out_read(admin_out, &xx);
		if (err != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        "can not read reply", err);
			return EXIT_FAILURE;
		}
		was_up = xx.stop.was_up;
	}

	if (was_up) {
		linted_io_write_format(LINTED_KO_STDOUT, 0,
		                       "%s: gui was killed\n",
		                       process_name);
	} else {
		linted_io_write_format(LINTED_KO_STDOUT, 0,
		                       "%s: the gui was not killed\n",
		                       process_name);
	}

	return EXIT_SUCCESS;
}

static linted_error ctl_help(linted_ko ko, char const *process_name,
                             char const *package_name,
                             char const *package_url,
                             char const *package_bugreport)
{
	linted_error err;

	size_t size = 0U;
	size_t cap = 0U;
	char *buf = 0;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
Usage: ");
	if (err != 0)
		goto free_buf;

	err =
	    linted_str_append_cstring(&buf, &cap, &size, process_name);
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
 [OPTIONS]\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
Run the admin program.\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
  status              request the status of the service\n\
  stop                stop the gui service\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\n");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
Report bugs to <");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size,
	                                package_bugreport);
	if (err)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, ">\n");
	if (err != 0)
		goto free_buf;

	err =
	    linted_str_append_cstring(&buf, &cap, &size, package_name);
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, "\
 home page: <");
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, package_url);
	if (err != 0)
		goto free_buf;

	err = linted_str_append_cstring(&buf, &cap, &size, ">\n");
	if (err != 0)
		goto free_buf;

	err = linted_io_write_all(ko, 0, buf, size);
	if (err != 0)
		goto free_buf;

free_buf:
	linted_mem_free(buf);
	return err;
}

static linted_error status_help(linted_ko ko, char const *process_name,
                                char const *package_name,
                                char const *package_url,
                                char const *package_bugreport)
{
	linted_error err;

	err = linted_io_write_string(
	    ko, 0, "Usage: LINTED_ADMIN_SOCKET=SOCKET ");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, process_name);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0,
	                             " status [OPTIONS] SERVICE\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
Report the status.\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
  SOCKET              the socket to connect to\n\
  SERVICE             the service to get the status of\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "Report bugs to <");
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, package_bugreport);
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, package_name);
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, " home page: <");
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, package_url);
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	return 0;
}

static linted_error stop_help(linted_ko ko, char const *process_name,
                              char const *package_name,
                              char const *package_url,
                              char const *package_bugreport)
{
	linted_error err;

	err = linted_io_write_string(
	    ko, 0, "Usage: LINTED_ADMIN_SOCKET=SOCKET ");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, process_name);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, " stop [OPTIONS]\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "Stop a service.\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\
  SOCKET              the socket to connect to\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "Report bugs to <");
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, package_bugreport);
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, package_name);
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, " home page: <");
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, package_url);
	if (err != 0)
		return err;
	err = linted_io_write_string(ko, 0, ">\n");
	if (err != 0)
		return err;

	return 0;
}

static linted_error failure(linted_ko ko, char const *process_name,
                            char const *message, linted_error error)
{
	linted_error err;

	err = linted_io_write_string(ko, 0, process_name);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, ": ");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, message);
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, ": ");
	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, linted_error_string(error));

	if (err != 0)
		return err;

	err = linted_io_write_string(ko, 0, "\n");
	if (err != 0)
		return err;

	return 0;
}
