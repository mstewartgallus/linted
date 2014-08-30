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
#define _GNU_SOURCE

#include "config.h"

#include "binaries.h"
#include "init.h"
#include "init_config.h"

#include "linted/io.h"
#include "linted/locale.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <sys/wait.h>

#include <linux/sched.h>

enum {
	HELP,
	VERSION_OPTION,
	CHROOTDIR_OPTION,
	LOGGER_FSTAB,
	SIMULATOR_FSTAB,
	GUI_FSTAB,
	LOGGER,
	SIMULATOR,
	GUI
};

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-init",
	.open_current_working_directory = true,
	.kos_size = 0U,
	.kos = NULL
};

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);

unsigned char linted_start(linted_ko cwd, char const *const process_name,
                           size_t argc, char const *const argv[const])
{
	linted_error errnum;

	bool need_help = false;
	bool need_version = false;

	char const *bad_option = NULL;

	char const *logger_path = PKGLIBEXECDIR "/logger" EXEEXT;
	char const *simulator_path = PKGLIBEXECDIR "/simulator" EXEEXT;
	char const *gui_path = PKGLIBEXECDIR "/gui" EXEEXT;

	char const *logger_fstab_path = PKGDEFAULTCONFDIR "/logger-fstab";
	char const *simulator_fstab_path = PKGDEFAULTCONFDIR "/simulator-fstab";
	char const *gui_fstab_path = PKGDEFAULTCONFDIR "/gui-fstab";

	char const *chrootdir_path = CHROOTDIR;

	for (size_t ii = 1U; ii < argc; ++ii) {
		char const *argument = argv[ii];

		static char const *const arguments[] =
		    {[HELP] = "--help",
		     [VERSION_OPTION] = "--version",
		     [CHROOTDIR_OPTION] = "--chrootdir",
		     [LOGGER_FSTAB] = "--logger-fstab",
		     [SIMULATOR_FSTAB] = "--simulator-fstab",
		     [GUI_FSTAB] = "--gui-fstab",
		     [LOGGER] = "--logger",
		     [SIMULATOR] = "--simulator",
		     [GUI] = "--gui" };

		int arg = -1;
		for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(arguments); ++jj) {
			if (0 == strncmp(argument, arguments[jj],
			                 strlen(arguments[jj]))) {
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
			if (argument[strlen(arguments[HELP])] != '\0')
				goto bad_argument;
			need_help = true;
			break;

		case VERSION_OPTION:
			if (argument[strlen(arguments[VERSION_OPTION])] != '\0')
				goto bad_argument;
			need_version = true;
			break;

		case CHROOTDIR_OPTION:
			if (argument[strlen(arguments[CHROOTDIR_OPTION])] !=
			    '=')
				goto bad_argument;
			chrootdir_path = argument + strlen("--chrootdir=");
			break;

		case LOGGER_FSTAB:
			if (argument[strlen(arguments[LOGGER_FSTAB])] != '=')
				goto bad_argument;
			logger_fstab_path =
			    argument + strlen("--logger-fstab=");
			break;

		case SIMULATOR_FSTAB:
			if (argument[strlen(arguments[SIMULATOR_FSTAB])] != '=')
				goto bad_argument;
			simulator_fstab_path =
			    argument + strlen("--simulator-fstab=");
			break;

		case GUI_FSTAB:
			if (argument[strlen(arguments[GUI_FSTAB])] != '=')
				goto bad_argument;
			gui_fstab_path = argument + strlen("--gui-fstab=");
			break;

		case LOGGER:
			if (argument[strlen(arguments[LOGGER])] != '=')
				goto bad_argument;
			logger_path = argument + strlen("--logger=");
			break;

		case SIMULATOR:
			if (argument[strlen(arguments[SIMULATOR])] != '=')
				goto bad_argument;
			simulator_path = argument + strlen("--simulator=");
			break;

		case GUI:
			gui_path = argument + strlen("--gui=");
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

	gid_t gid = getgid();
	uid_t uid = getuid();

	/* Clone off a child in a new PID namespace. CLONE_NEWUSER is
	 * needed to allow the permissions to work.
	 */
	pid_t child;
	{
		child = syscall(__NR_clone,
		                SIGCHLD | CLONE_NEWUSER | CLONE_NEWPID, NULL);
		if (-1 == child) {
			linted_io_write_format(
			    STDERR_FILENO, NULL,
			    "%s: can't clone unprivileged process: %s\n",
			    process_name, linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (child != 0) {
		siginfo_t info;
		do {
			errnum = -1 == waitid(P_PID, child, &info, WEXITED)
			             ? errno
			             : 0;
		} while (EINTR == errnum);
		if (errnum != 0) {
			assert(errnum != EINVAL);
			assert(errnum != ECHILD);
			LINTED_ASSUME_UNREACHABLE();
		}
		return info.si_status;
	}

	/* Stupidly, uid_map and gid_map aren't writable in these when
	 * the binary is not dumpable.
	 */
	if (-1 == prctl(PR_SET_DUMPABLE, 1L, 0L, 0L, 0L)) {
		perror("prctl");
		return EXIT_FAILURE;
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum =
			    linted_ko_open(&xx, AT_FDCWD, "/proc/self/uid_map",
			                   LINTED_KO_WRONLY);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_open");
				return EXIT_FAILURE;
			}
			file = xx;
		}

		errnum =
		    linted_io_write_format(file, NULL, "%i %i 1\n", uid, uid);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_io_write_format");
			return EXIT_FAILURE;
		}

		errnum = linted_ko_close(file);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_ko_close");
			return EXIT_FAILURE;
		}
	}

	{
		linted_ko file;
		{
			linted_ko xx;
			errnum =
			    linted_ko_open(&xx, AT_FDCWD, "/proc/self/gid_map",
			                   LINTED_KO_WRONLY);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_ko_open");
				return EXIT_FAILURE;
			}
			file = xx;
		}

		errnum =
		    linted_io_write_format(file, NULL, "%i %i 1\n", gid, gid);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_io_write_format");
			return EXIT_FAILURE;
		}

		errnum = linted_ko_close(file);
		if (errnum != 0) {
			perror("linted_ko_close");
			return EXIT_FAILURE;
		}
	}

	if (-1 == prctl(PR_SET_DUMPABLE, 0L, 0L, 0L, 0L)) {
		perror("prctl");
		return EXIT_FAILURE;
	}

	struct linted_init_config config = { .chrootdir_path = chrootdir_path,
		                             .logger_fstab_path =
		                                 logger_fstab_path,
		                             .simulator_fstab_path =
		                                 simulator_fstab_path,
		                             .gui_fstab_path = gui_fstab_path,
		                             .logger_path = logger_path,
		                             .simulator_path = simulator_path,
		                             .gui_path = gui_path };
	return linted_init_init(cwd, &config);
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
  --chrootdir         the directory the chroot is mounted to\n\
  --fstab             the location of the chroot mount instructions\n\
  --simulator         the location of the simulator executable\n\
  --gui               the location of the gui executable\n"));
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
