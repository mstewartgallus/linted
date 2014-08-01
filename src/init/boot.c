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

#include "linted/io.h"
#include "linted/locale.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/wait.h>

#include <linux/sched.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define CHROOTDIR_OPTION "--chrootdir"
#define FSTAB_OPTION "--fstab"
#define SIMULATOR_OPTION "--simulator"
#define GUI_OPTION "--gui"

#ifndef PR_SET_CHILD_SUBREAPER
#define PR_SET_CHILD_SUBREAPER 36UL
#endif

struct linted_start_config const linted_start_config
    = { .canonical_process_name = PACKAGE_NAME "-init",
        .open_current_working_directory = true,
        .kos_size = 0U,
        .kos = NULL };

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);

uint_fast8_t linted_start(int cwd, char const *const process_name, size_t argc,
                          char const *const argv[const])
{
    linted_error errnum;

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    char const *simulator_path = PKGLIBEXECDIR "/simulator" EXEEXT;
    char const *gui_path = PKGLIBEXECDIR "/gui" EXEEXT;
    char const *fstab_path = PKGCONFDIR "/fstab";
    char const *chrootdir_path = CHROOTDIR;

    for (size_t ii = 1U; ii < argc; ++ii) {
        char const *argument = argv[ii];

        if (0 == strcmp(argument, HELP_OPTION)) {
            need_help = true;
        } else if (0 == strcmp(argument, VERSION_OPTION)) {
            need_version = true;
        } else if (0 == strncmp(argument, CHROOTDIR_OPTION "=",
                                strlen(CHROOTDIR_OPTION "="))) {
            chrootdir_path = argument + strlen(CHROOTDIR_OPTION "=");
        } else if (0 == strncmp(argument, FSTAB_OPTION "=",
                                strlen(FSTAB_OPTION "="))) {
            fstab_path = argument + strlen(FSTAB_OPTION "=");
        } else if (0 == strncmp(argument, SIMULATOR_OPTION "=",
                                strlen(SIMULATOR_OPTION "="))) {
            simulator_path = argument + strlen(SIMULATOR_OPTION "=");
        } else if (0 == strncmp(argument, GUI_OPTION "=",
                                strlen(GUI_OPTION "="))) {
            gui_path = argument + strlen(GUI_OPTION "=");
        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        linted_help(STDOUT_FILENO, process_name, LINTED_STR(PACKAGE_NAME),
                    LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(STDERR_FILENO, process_name, bad_option);
        linted_locale_try_for_more_help(STDERR_FILENO, process_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    char const *display_orig = getenv("DISPLAY");
    if (NULL == display_orig) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing DISPLAY environment variable\n",
                               process_name);
        linted_locale_try_for_more_help(STDERR_FILENO, process_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    char const *display = strdup(display_orig);
    if (NULL == display) {
        perror("strdup");
        return EXIT_FAILURE;
    }

    if ((errnum = linted_util_sanitize_environment()) != 0) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
    %s: can not sanitize the environment: %s\n",
                               process_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    /* Clone off a child in a new PID namespace. CLONE_NEWUSER is
     * needed to allow the permissions to work.
     */
    pid_t child;
    {
        child
            = syscall(__NR_clone, SIGCHLD | CLONE_NEWUSER | CLONE_NEWPID, NULL);
        if (-1 == child) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: can't clone unprivileged process: %s\n",
                                   process_name,
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }

        if (0 == child) {
            return linted_init_init(cwd, display, chrootdir_path, fstab_path,
                                    simulator_path, gui_path);
        }
    }

    {
        siginfo_t info;
        do {
            errnum = -1 == waitid(P_PID, child, &info, WEXITED) ? errno : 0;
        } while (EINTR == errnum);
        if (errnum != 0) {
            assert(errnum != EINVAL);
            assert(errnum != ECHILD);
            assert(false);
        }
        return info.si_status;
    }
}

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport)
{
    linted_error errnum;

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(ko, NULL, process_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(" [OPTIONS]\n")))
        != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Play the game.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --chrootdir         the directory the chroot is mounted to\n\
  --fstab             the location of the chroot mount instructions\n\
  --simulator         the location of the simulator executable\n\
  --gui               the location of the gui executable\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(ko, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}
