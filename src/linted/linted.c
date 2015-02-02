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

#include "settings.h"

#include "linted/error.h"
#include "linted/io.h"
#include "linted/locale.h"
#include "linted/log.h"
#include "linted/start.h"
#include "linted/str.h"
#include "linted/util.h"

#include <errno.h>
#include <inttypes.h>
#include <libgen.h>
#include <locale.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/auxv.h>
#include <unistd.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

struct envvar
{
	char const *key;
	char const *value;
};

enum { HELP, VERSION_OPTION };

extern char **environ;

static unsigned char main_start(char const *const process_name, size_t argc,
                                char const *const argv[const]);

static linted_error exec_init(char const *init);

static bool is_privileged(void);
static bool was_privileged(void);

static linted_error do_help(linted_ko ko, char const *process_name,
                            struct linted_str package_name,
                            struct linted_str package_url,
                            struct linted_str package_bugreport);

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-linted", .start = main_start
};

static struct envvar const default_envvars[] = {
	{ "LINTED_PROCESS_NAME", "linted" },
	{ "LINTED_UNIT_PATH", PKGUNITSDIR ":" PKGDEFAULTUNITSDIR },
	{ "LINTED_INIT", PKGLIBEXECDIR "/linted-init" EXEEXT },
	{ "LINTED_MONITOR", PKGLIBEXECDIR "/linted-monitor" EXEEXT },
	{ "LINTED_SANDBOX", PKGLIBEXECDIR "/linted-sandbox" EXEEXT },
	{ "LINTED_WAITER", PKGLIBEXECDIR "/linted-waiter" EXEEXT },
	{ "LINTED_GUI", PKGLIBEXECDIR "/linted-gui" EXEEXT },
	{ "LINTED_GUI_FSTAB", PKGDEFAULTCONFDIR "/gui-fstab" },
	{ "LINTED_SIMULATOR", PKGLIBEXECDIR "/linted-simulator" EXEEXT },
	{ "LINTED_SIMULATOR_FSTAB", PKGDEFAULTCONFDIR "/simulator-fstab" },
	{ "LINTED_DRAWER", PKGLIBEXECDIR "/linted-drawer" EXEEXT },
	{ "LINTED_DRAWER_FSTAB", PKGDEFAULTCONFDIR "/drawer-fstab" },
	{ "LINTED_WINDOW", PKGLIBEXECDIR "/linted-window" EXEEXT }
};

static char const *const argstrs[] = {[HELP] = "--help",
	                              [VERSION_OPTION] = "--version" };

static unsigned char main_start(char const *const process_name, size_t argc,
                                char const *const argv[const])
{
	if (is_privileged()) {
		linted_log(LINTED_LOG_ERROR,
		           "%s should not be run with high privileges",
		           PACKAGE_NAME);
		return EPERM;
	}

	if (0 == setlocale(LC_ALL, "")) {
		linted_log(LINTED_LOG_ERROR, "linted_spawn_attr_init: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(default_envvars); ++ii) {
		struct envvar const *envvar = &default_envvars[ii];
		if (-1 == setenv(envvar->key, envvar->value, false)) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_spawn_attr_init: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	char const *init = getenv("LINTED_INIT");

	bool need_help = false;
	bool need_version = false;

	char const *bad_option = 0;

	for (size_t ii = 1U; ii < argc; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(argstrs); ++jj) {
			if (0 == strcmp(argument, argstrs[jj])) {
				arg = jj;
				break;
			}
		}

		switch (arg) {
		case -1:
			bad_option = argument;
			break;

		case HELP:
			need_help = true;
			break;

		case VERSION_OPTION:
			need_version = true;
			break;
		}
	}

	if (need_help) {
		do_help(LINTED_KO_STDOUT, process_name,
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

	linted_error errnum;

	fprintf(stdout, "LINTED_PID=%" PRIuMAX "\n", (uintmax_t)getpid());

	errnum = exec_init(init);
	if (errnum != 0) {
		linted_log(LINTED_LOG_ERROR, "exec_init: %s",
		           linted_error_string(errnum));
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

static linted_error exec_init(char const *init)
{
	linted_error errnum;

	char *init_dup = strdup(init);
	if (0 == init_dup) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}
	char *init_base = basename(init_dup);

	char const *const init_argv[] = { init_base, 0 };
	execve(init, (char * const *)init_argv, environ);
	errnum = errno;
	LINTED_ASSUME(errnum != 0);

	return errnum;
}

static bool is_privileged(void)
{
	uid_t uid = getuid();
	if (0 == uid)
		return true;

	gid_t gid = getgid();
	if (0 == gid)
		return true;

	return was_privileged();
}

#ifdef __linux__
static bool was_privileged(void)
{
	return getauxval(AT_SECURE);
}
#else
#error "was privileged" check has not been implemented for this system yet
#endif

static linted_error do_help(linted_ko ko, char const *process_name,
                            struct linted_str package_name,
                            struct linted_str package_url,
                            struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("Usage: "));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_string(ko, 0, process_name);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR(" [OPTIONS]\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, 0, LINTED_STR("Play the game.\n"));
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
  LINTED_UNIT_PATH    a `:' separated list of directories units are from\n\
  LINTED_LOGGER_FSTAB the location of the logger fstab\n\
  LINTED_LOGGER       the location of the logger executable\n\
  LINTED_GUI_FSTAB    the location of the GUI fstab\n\
  LINTED_GUI          the location of the GUI executable\n\
  LINTED_SIMULATOR_FSTAB the location of the simulator fstab\n\
  LINTED_SIMULATOR    the location of the simulator executable\n"));
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
