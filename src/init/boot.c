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

enum {
    HELP,
    VERSION_OPTION,
    CHROOTDIR_OPTION,
    SIMULATOR_FSTAB,
    GUI_FSTAB,
    SIMULATOR,
    GUI
};

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
    char const *simulator_fstab_path = PKGCONFDIR "/simulator-fstab";
    char const *gui_fstab_path = PKGCONFDIR "/gui-fstab";
    char const *chrootdir_path = CHROOTDIR;

    for (size_t ii = 1U; ii < argc; ++ii) {
        char const *argument = argv[ii];

        static char const *const arguments[]
            = {[HELP] = "--help",
               [VERSION_OPTION] = "--version",
               [CHROOTDIR_OPTION] = "--chrootdir",
               [SIMULATOR_FSTAB] = "--simulator-fstab",
               [GUI_FSTAB] = "--gui-fstab",
               [SIMULATOR] = "--simulator",
               [GUI] = "--gui" };

        int arg = -1;
        for (size_t jj = 0U; jj < LINTED_ARRAY_SIZE(arguments); ++jj) {
            if (0 == strncmp(argument, arguments[jj], strlen(arguments[jj]))) {
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
            if (argument[strlen(arguments[HELP])] != '\0') {
                goto bad_argument;
            }
            need_help = true;
            break;

        case VERSION_OPTION:
            if (argument[strlen(arguments[VERSION_OPTION])] != '\0') {
                goto bad_argument;
            }
            need_version = true;
            break;

        case CHROOTDIR_OPTION:
            if (argument[strlen(arguments[CHROOTDIR_OPTION])] != '=') {
                goto bad_argument;
            }
            chrootdir_path = argument + strlen("--chrootdir=");
            break;

        case SIMULATOR_FSTAB:
            if (argument[strlen(arguments[SIMULATOR_FSTAB])] != '=') {
                goto bad_argument;
            }
            simulator_fstab_path = argument + strlen("--simulator-fstab=");
            break;

        case GUI_FSTAB:
            if (argument[strlen(arguments[GUI_FSTAB])] != '=') {
                goto bad_argument;
            }
            gui_fstab_path = argument + strlen("--gui-fstab=");
            break;

        case SIMULATOR:
            if (argument[strlen(arguments[SIMULATOR])] != '=') {
                goto bad_argument;
            }
            simulator_path = argument + strlen("--simulator=");
            break;

        case GUI:
            gui_path = argument + strlen("--gui=");
            break;
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
                        SIGCHLD | CLONE_NEWUSER | CLONE_NEWPID | CLONE_NEWUTS,
                        NULL);
        if (-1 == child) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: can't clone unprivileged process: %s\n",
                                   process_name,
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }

        if (0 == child) {
            {
                linted_ko file;
                {
                    linted_ko xx;
                    if ((errnum = linted_ko_open(&xx, -1, "/proc/self/uid_map",
                                                 LINTED_KO_WRONLY)) != 0) {
                        errno = errnum;
                        perror("open");
                        return EXIT_FAILURE;
                    }
                    file = xx;
                }

                if ((errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
                                                     uid, uid)) != 0) {
                    errno = errnum;
                    perror("linted_io_write_format");
                    return EXIT_FAILURE;
                }

                if (-1 == linted_ko_close(file)) {
                    perror("linted_ko_close");
                    return EXIT_FAILURE;
                }
            }

            {
                linted_ko file;
                {
                    linted_ko xx;
                    if ((errnum = linted_ko_open(&xx, -1, "/proc/self/gid_map",
                                                 LINTED_KO_WRONLY)) != 0) {
                        errno = errnum;
                        perror("open");
                        return EXIT_FAILURE;
                    }
                    file = xx;
                }

                if ((errnum = linted_io_write_format(file, NULL, "%i %i 1\n",
                                                     gid, gid)) != 0) {
                    errno = errnum;
                    perror("linted_io_write_format");
                    return EXIT_FAILURE;
                }

                if (-1 == linted_ko_close(file)) {
                    perror("linted_ko_close");
                    return EXIT_FAILURE;
                }
            }

            if (-1
                == sethostname(PACKAGE_TARNAME, sizeof PACKAGE_TARNAME - 1U)) {
                perror("sethostname");
                return EXIT_FAILURE;
            }

            return linted_init_init(cwd, chrootdir_path, simulator_fstab_path,
                                    gui_fstab_path, simulator_path, gui_path);
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
            LINTED_ASSUME_UNREACHABLE();
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
