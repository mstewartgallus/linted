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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "config.h"

#include "linted/admin.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/unit.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

static uint_fast8_t control_start(char const *const process_name, size_t argc,
                                  char const *const argv[]);

static uint_fast8_t run_status(char const *process_name, size_t argc,
                               char const *const argv[]);
static uint_fast8_t run_stop(char const *process_name, size_t argc,
                             char const *const argv[]);

static linted_error ctl_help(linted_ko ko, char const *process_name,
                             struct linted_str package_name,
                             struct linted_str package_url,
                             struct linted_str package_bugreport);
static linted_error status_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);
static linted_error stop_help(linted_ko ko, char const *process_name,
                              struct linted_str package_name,
                              struct linted_str package_url,
                              struct linted_str package_bugreport);
static linted_error failure(linted_ko ko, char const *process_name,
                            struct linted_str message, linted_error errnum);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-control",
	.start = control_start
};

static uint_fast8_t control_start(char const *const process_name, size_t argc,
                                  char const *const argv[])
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
		ctl_help(LINTED_KO_STDOUT, process_name,
		         LINTED_STR(PACKAGE_NAME), LINTED_STR(PACKAGE_URL),
		         LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT,
		                      LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	if (0 == command) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: missing COMMAND\n", process_name);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	enum { STATUS, STOP };

	static char const *const commands[] = {[STATUS] = "status",
		                               [STOP] = "stop" };

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

	linted_io_write_format(LINTED_KO_STDERR, 0,
	                       "%s: unrecognized command '%s'\n", process_name,
	                       command);
	linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
	                                LINTED_STR("--help"));
	return EXIT_FAILURE;
}

static uint_fast8_t run_status(char const *process_name, size_t argc,
                               char const *const argv[])
{
	linted_error errnum;
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
		            LINTED_STR(PACKAGE_NAME), LINTED_STR(PACKAGE_URL),
		            LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (bad_argument != 0) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT,
		                      LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	char const *path = getenv("LINTED_ADMIN_SOCKET");
	if (0 == path) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: missing LINTED_ADMIN_SOCKET\n",
		                       process_name);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (0 == name) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: missing SERVICE\n", process_name);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t name_len = strlen(name);
	if (name_len > LINTED_UNIT_NAME_MAX) {
		failure(LINTED_KO_STDERR, process_name, LINTED_STR("SERVICE"),
		        EINVAL);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t path_len = strlen(path);
	linted_admin admin;
	{
		linted_admin xx;
		errnum = linted_admin_connect(&xx, path, path_len);
		if (errnum != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        LINTED_STR("can not create socket"), errnum);
			return EXIT_FAILURE;
		}
		admin = xx;
	}

	linted_io_write_format(LINTED_KO_STDOUT, 0,
	                       "%s: sending the status request for %s\n",
	                       process_name, name);

	{
		union linted_admin_request request = { 0 };

		request.type = LINTED_ADMIN_STATUS;
		request.status.size = name_len;
		memcpy(request.status.name, name, name_len);

		errnum = linted_admin_send_request(admin, &request);
		if (errnum != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        LINTED_STR("can not send request"), errnum);
			return EXIT_FAILURE;
		}
	}

	union linted_admin_reply reply;
	size_t bytes_read;
	{
		size_t xx;
		errnum = linted_admin_recv_reply(admin, &reply, &xx);
		if (errnum != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        LINTED_STR("can not read reply"), errnum);
			return EXIT_FAILURE;
		}
		bytes_read = xx;
	}

	if (0U == bytes_read) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: socket hung up\n", process_name);
		return EXIT_FAILURE;
	}

	/* Sent malformed input */
	if (bytes_read != sizeof reply) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: reply was too small: %" PRIuMAX
		                       "\n",
		                       process_name, (uintmax_t)bytes_read);
		return EXIT_FAILURE;
	}

	if (reply.status.is_up) {
		linted_io_write_format(LINTED_KO_STDOUT, 0, "%s: %s is up\n",
		                       process_name, name);
	} else {
		linted_io_write_format(LINTED_KO_STDOUT, 0, "%s: %s is down\n",
		                       process_name, name);
	}

	return EXIT_SUCCESS;
}

static uint_fast8_t run_stop(char const *process_name, size_t argc,
                             char const *const argv[const])
{
	linted_error errnum;
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
		stop_help(LINTED_KO_STDOUT, process_name,
		          LINTED_STR(PACKAGE_NAME), LINTED_STR(PACKAGE_URL),
		          LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != 0) {
		linted_locale_on_bad_option(LINTED_KO_STDERR, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (bad_argument != 0) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(LINTED_KO_STDOUT,
		                      LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	char const *path = getenv("LINTED_ADMIN_SOCKET");
	if (0 == path) {
		linted_io_write_format(LINTED_KO_STDERR, 0,
		                       "%s: missing LINTED_ADMIN_SOCKET\n",
		                       process_name);
		linted_locale_try_for_more_help(LINTED_KO_STDERR, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t path_len = strlen(path);
	linted_admin admin;
	{
		linted_admin xx;
		errnum = linted_admin_connect(&xx, path, path_len);
		if (errnum != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        LINTED_STR("can not create socket"), errnum);
			return EXIT_FAILURE;
		}
		admin = xx;
	}

	linted_io_write_format(LINTED_KO_STDOUT, 0,
	                       "%s: sending the stop request for the gui\n",
	                       process_name);

	{
		union linted_admin_request request = { 0 };

		request.type = LINTED_ADMIN_STOP;
		request.stop.size = sizeof "gui" - 1U;
		memcpy(request.stop.name, "gui", sizeof "gui" - 1U);

		errnum = linted_admin_send_request(admin, &request);
	}
	if (errnum != 0) {
		failure(LINTED_KO_STDERR, process_name,
		        LINTED_STR("can send request"), errnum);
		return EXIT_FAILURE;
	}

	bool was_up;
	size_t bytes_read;
	{
		union linted_admin_reply xx;
		size_t yy;
		errnum = linted_admin_recv_reply(admin, &xx, &yy);
		if (errnum != 0) {
			failure(LINTED_KO_STDERR, process_name,
			        LINTED_STR("can not read reply"), errnum);
			return EXIT_FAILURE;
		}
		bytes_read = yy;

		if (0U == bytes_read) {
			linted_io_write_format(LINTED_KO_STDERR, 0,
			                       "%s: socket hung up\n",
			                       process_name);
			return EXIT_FAILURE;
		}

		was_up = xx.stop.was_up;
	}

	if (was_up) {
		linted_io_write_format(LINTED_KO_STDOUT, 0,
		                       "%s: gui was killed\n", process_name);
	} else {
		linted_io_write_format(LINTED_KO_STDOUT, 0,
		                       "%s: the gui was not killed\n",
		                       process_name);
	}

	return EXIT_SUCCESS;
}

static linted_error ctl_help(linted_ko ko, char const *process_name,
                             struct linted_str package_name,
                             struct linted_str package_url,
                             struct linted_str package_bugreport)
{
	linted_error errnum;

	size_t size = 0U;
	size_t cap = 0U;
	char *buf = 0;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
Usage: "));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_cstring(&buf, &cap, &size, process_name);
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
 [OPTIONS]\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
Run the admin program.\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
  status              request the status of the service\n\
  stop                stop the gui service\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
Report bugs to <"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, package_bugreport);
	if (errnum)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR(">\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, package_name);
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR("\
 home page: <"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, package_url);
	if (errnum != 0)
		goto free_buf;

	errnum = linted_str_append_str(&buf, &cap, &size, LINTED_STR(">\n"));
	if (errnum != 0)
		goto free_buf;

	errnum = linted_io_write_all(ko, 0, buf, size);
	if (errnum != 0)
		goto free_buf;

free_buf:
	linted_mem_free(buf);
	return errnum;
}

static linted_error status_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(
	    ko, 0, LINTED_STR("Usage: LINTED_ADMIN_SOCKET=SOCKET "));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_string(ko, 0, process_name);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0,
	                             LINTED_STR(" status [OPTIONS] SERVICE\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\
Report the status.\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\
  SOCKET              the socket to connect to\n\
  SERVICE             the service to get the status of\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("Report bugs to <"));
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, package_bugreport);
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, LINTED_STR(">\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, package_name);
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, LINTED_STR(" home page: <"));
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, package_url);
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, LINTED_STR(">\n"));
	if (errnum != 0)
		return errnum;

	return 0;
}

static linted_error stop_help(linted_ko ko, char const *process_name,
                              struct linted_str package_name,
                              struct linted_str package_url,
                              struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(
	    ko, 0, LINTED_STR("Usage: LINTED_ADMIN_SOCKET=SOCKET "));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_string(ko, 0, process_name);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR(" stop [OPTIONS]\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("Stop a service.\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\
  SOCKET              the socket to connect to\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("Report bugs to <"));
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, package_bugreport);
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, LINTED_STR(">\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, package_name);
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, LINTED_STR(" home page: <"));
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, package_url);
	if (errnum != 0)
		return errnum;
	errnum = linted_io_write_str(ko, 0, LINTED_STR(">\n"));
	if (errnum != 0)
		return errnum;

	return 0;
}

static linted_error failure(linted_ko ko, char const *process_name,
                            struct linted_str message, linted_error error)
{
	linted_error errnum;

	errnum = linted_io_write_string(ko, 0, process_name);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR(": "));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, message);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR(": "));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_string(ko, 0, linted_error_string(error));

	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	return 0;
}
