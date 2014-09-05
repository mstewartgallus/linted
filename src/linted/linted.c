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
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "settings.h"

#include "linted/io.h"
#include "linted/locale.h"
#include "linted/start.h"
#include "linted/util.h"

#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern char **environ;

enum {
	HELP,
	VERSION_OPTION
};

static char const *const argstrs[] = {[HELP] = "--help",
	                              [VERSION_OPTION] = "--version" };

struct envvar
{
	char const *key;
	char const *value;
};

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-linted",
	.kos_size = 0U,
	.kos = NULL
};

static struct envvar const default_envvars[] = {
	{ "LINTED_PROCESS_NAME", "linted" },
	{ "LINTED_UNIT_PATH", PKGDEFAULTUNITSDIR },
	{ "LINTED_CHROOT", CHROOTDIR },
	{ "LINTED_INIT", PKGLIBEXECDIR "/init" EXEEXT },
	{ "LINTED_LOGGER", PKGLIBEXECDIR "/logger" EXEEXT },
	{ "LINTED_LOGGER_FSTAB", PKGDEFAULTCONFDIR "/logger-fstab" },
	{ "LINTED_GUI", PKGLIBEXECDIR "/gui" EXEEXT },
	{ "LINTED_GUI_FSTAB", PKGDEFAULTCONFDIR "/gui-fstab" },
	{ "LINTED_SIMULATOR", PKGLIBEXECDIR "/simulator" EXEEXT },
	{ "LINTED_SIMULATOR_FSTAB", PKGDEFAULTCONFDIR "/simulator-fstab" }
};

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);

unsigned char linted_start(char const *const process_name, size_t argc,
                           char const *const argv[const])
{
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(default_envvars); ++ii) {
		struct envvar const *envvar = &default_envvars[ii];
		if (-1 == setenv(envvar->key, envvar->value, false)) {
			perror("setenv");
			return EXIT_FAILURE;
		}
	}

	char const *init = getenv("LINTED_INIT");

	bool need_help = false;
	bool need_version = false;

	char const *bad_option = NULL;

	for (size_t ii = 1U; ii < argc; ++ii) {
		char const *argument = argv[ii];

		int arg = -1;
		for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(argstrs); ++jj) {
			if (0 == strncmp(argument, argstrs[jj],
			                 strlen(argstrs[jj]))) {
				arg = jj;
				break;
			}
		}

		switch (arg) {
		bad_argument:
		case -1:
			bad_option = argument;
			break;

		case HELP:
			if (argument[strlen(argstrs[HELP])] != '\0')
				goto bad_argument;
			need_help = true;
			break;

		case VERSION_OPTION:
			if (argument[strlen(argstrs[VERSION_OPTION])] != '\0')
				goto bad_argument;
			need_version = true;
			break;
		}
	}

	if (need_help) {
		linted_help(STDOUT_FILENO, process_name,
		            LINTED_STR(PACKAGE_NAME), LINTED_STR(PACKAGE_URL),
		            LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != NULL) {
		linted_locale_on_bad_option(STDERR_FILENO, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	linted_ko stdfiles[] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(stdfiles); ++ii) {
		linted_ko ko = stdfiles[ii];

		int flags = fcntl(ko, F_GETFD);
		if (-1 == flags) {
			perror("fcntl");
			return EXIT_FAILURE;
		}

		if (-1 == fcntl(ko, F_SETFD, (long)flags & !FD_CLOEXEC)) {
			perror("fcntl");
			return EXIT_FAILURE;
		}
	}

	char const * const init_argv[] = {init, NULL};
	execve(init, (char * const *)init_argv, environ);
	perror("execve");

	return EXIT_FAILURE;
}

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("Usage: "));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_string(ko, NULL, process_name);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(" [OPTIONS]\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("Play the game.\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  LINTED_UNIT_PATH    a `:' separated list of directories units are from\n\
  LINTED_CHROOT       the directory the chroot is mounted to\n\
  LINTED_LOGGER_FSTAB the location of the logger fstab\n\
  LINTED_LOGGER       the location of the logger executable\n\
  LINTED_GUI_FSTAB    the location of the GUI fstab\n\
  LINTED_GUI          the location of the GUI executable\n\
  LINTED_SIMULATOR_FSTAB the location of the simulator fstab\n\
  LINTED_SIMULATOR    the location of the simulator executable\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("Report bugs to <"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, package_bugreport);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, package_name);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(" home page: <"));
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, package_url);
	if (errnum != 0)
		return errnum;

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0)
		return errnum;

	return 0;
}
