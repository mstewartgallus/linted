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
#include "config.h"

#include "linted/binaries.h"
#include "linted/controller.h"
#include "linted/io.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

#define USAGE_TEXT \
    "Usage: " PACKAGE_TARNAME " [OPTIONS]\n"\
    "Play the " PACKAGE_NAME " game\n"\
    "\n"\
    "The following OPTIONS are accepted:\n"\
    "--version\t\tJust print version info and return\n"\
    "--help\t\tPrint this usage information\n"\
    "\n"\
    "Report bugs to " PACKAGE_BUGREPORT "\n"

#define VERSION_TEXT \
    PACKAGE_STRING "\n"\
    "Copyright (C) 2013 Steven Stewart-Gallus\n"\
    PACKAGE_NAME " comes with ABSOLUTELY NO WARRANTY.\n"\
    "You may redistribute copies of Linted\n"\
    "under the terms of the Apache License.\n"\
    "For more information about these matters, see the file named COPYING.\n"

extern char **environ;

static int run_game(char const *simulator_path, int simulator_binary,
                    char const *gui_path, int gui_binary);

int main(int argc, char **argv)
{
    stdin = NULL;
    stdout = NULL;
    stderr = NULL;

    /* First we check if we are run with proper security */
    uid_t const uid = getuid();
    uid_t const euid = geteuid();
    if (0 == euid || 0 == uid) {
        linted_io_write_string(STDERR_FILENO, NULL, "\
Bad administrator!\n\
It is insecure to run a game as root!\n");
        return EXIT_FAILURE;
    }

    if (argc < 1) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: missing process name\n",
                               PACKAGE_TARNAME);
        return EXIT_FAILURE;
    }

    char const * const program_name = argv[0];

    bool show_help = false;
    bool show_version = false;
    char const *simulator_path = PKGLIBEXECDIR "/simulator" EXEEXT;
    char const *gui_path = PKGLIBEXECDIR "/gui" EXEEXT;

    for (unsigned ii = 1; ii < (unsigned)argc; ++ii) {
        char const *argument = argv[ii];
        if (0 == strcmp(argument, "--help")) {
            show_help = true;
        } else if (0 == strcmp(argument, "--version")) {
            show_version = true;
        } else if (0 == strncmp(argument, "--simulator=",
                                sizeof "--simulator=" - 1)) {
            simulator_path = argument + sizeof "--simulator=" - 1;
        } else if (0 == strncmp(argument, "--gui=", sizeof "--gui=" - 1)) {
            gui_path = argument + sizeof "--gui=" - 1;
        } else {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: urecognized option '%s'\n",
                                   program_name, argument);
            linted_io_write_format(STDERR_FILENO, NULL, "\
Try `%s --help' for more information.\n",
                                          program_name);
            return EXIT_FAILURE;
        }
    }

    int const simulator_binary = open(simulator_path, O_RDONLY | O_CLOEXEC);
    if (-1 == simulator_binary) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: %s: %s\n", program_name,
                               simulator_path,
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    int const gui_binary = open(gui_path, O_RDONLY | O_CLOEXEC);
    if (-1 == gui_binary) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: %s: %s\n", program_name,
                               gui_path,
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    openlog(PACKAGE_TARNAME, LOG_PERROR /* So the user can see this */
            | LOG_CONS          /* So we know there is an error */
            | LOG_PID           /* Because we fork several times */
            , LOG_USER          /* This is a game and is a user program */
        );

    int succesfully_executing = 0;
    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        succesfully_executing = -1;

        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not drop ability to raise privileges: %s",
               error_string);
        linted_error_string_free(error_string);
    }

    /* Sanitize open files */
    fd_set essential_fds;
    FD_ZERO(&essential_fds);
    FD_SET(STDERR_FILENO, &essential_fds);
    FD_SET(STDOUT_FILENO, &essential_fds);
    FD_SET(simulator_binary, &essential_fds);
    FD_SET(gui_binary, &essential_fds);
    if (-1 == linted_io_close_fds_except(&essential_fds)) {
        succesfully_executing = -1;

        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not close unneeded files: %s", error_string);
        linted_error_string_free(error_string);
    }

    /* Sanitize the environment */
    char **env = environ;
    for (; *env != NULL; ++env) {
        /* TODO: Only pass the display variable in for the gui task */
        size_t const env_length = strlen(*env);
        char const needle[] = "DISPLAY=";
        size_t const needle_length = sizeof needle - 1;
        if (env_length >= needle_length) {
            if (0 == memcmp(needle, *env, needle_length)) {
                continue;
            }
        }

        char *sensitive_data = strchr(*env, '=') + 1;

        memset(sensitive_data, '\0', env_length - (sensitive_data - *env));
    }

    if (0 == succesfully_executing) {
        if (show_help || show_version) {
            if (show_help) {
                linted_io_write_string(STDOUT_FILENO, NULL, USAGE_TEXT);
            } else if (show_version) {
                linted_io_write_string(STDOUT_FILENO, NULL, VERSION_TEXT);
            }

            if (-1 == close(STDOUT_FILENO)) {
                succesfully_executing = -1;
                char const *error_string = linted_error_string_alloc(errno);
                syslog(LOG_ERR, "could not close standard output: %s",
                       error_string);
                linted_error_string_free(error_string);
            }
        } else {
            if (-1 == close(STDOUT_FILENO)) {
                succesfully_executing = -1;
                char const *error_string = linted_error_string_alloc(errno);
                syslog(LOG_ERR, "could not close standard output: %s",
                       error_string);
                linted_error_string_free(error_string);
            }

            if (-1 == run_game(simulator_path, simulator_binary,
                               gui_path, gui_binary)) {
                succesfully_executing = -1;
                char const *error_string = linted_error_string_alloc(errno);
                syslog(LOG_ERR, "could not run the game: %s", error_string);
                linted_error_string_free(error_string);
            }
        }
    }

    if (-1 == close(STDERR_FILENO)) {
        succesfully_executing = -1;
        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not close standard error: %s", error_string);
        linted_error_string_free(error_string);
    }

    closelog();

    return (-1 == succesfully_executing) ? EXIT_FAILURE : EXIT_SUCCESS;
}

static int run_game(char const *simulator_path, int simulator_binary,
                    char const *gui_path, int gui_binary)
{
    int exit_status = -1;

    linted_updater updater_mqs[2];
    linted_controller controller_mqs[2];
    linted_shutdowner simulator_shutdowner_mqs[2];

    linted_updater updater_read;
    linted_updater updater_write;

    linted_controller controller_read;
    linted_controller controller_write;

    linted_shutdowner simulator_shutdowner_read;
    linted_shutdowner simulator_shutdowner_write;

    if (-1 == linted_updater_pair(updater_mqs, O_NONBLOCK, O_NONBLOCK)) {
        return -1;
    }

    updater_read = updater_mqs[0];
    updater_write = updater_mqs[1];

    if (-1 == linted_controller_pair(controller_mqs, O_NONBLOCK, O_NONBLOCK)) {
        goto cleanup_updater_pair;
    }

    controller_read = controller_mqs[0];
    controller_write = controller_mqs[1];

    if (-1 == linted_shutdowner_pair(simulator_shutdowner_mqs, O_NONBLOCK, 0)) {
        goto cleanup_controller_pair;
    }

    simulator_shutdowner_read = simulator_shutdowner_mqs[0];
    simulator_shutdowner_write = simulator_shutdowner_mqs[1];

    pid_t live_processes[] = { -1, -1 };

    pid_t gui = fork();
    if (-1 == gui) {
        goto cleanup_shutdowner_pair;
    }
    live_processes[0] = gui;

    if (0 == gui) {
        int new_updater_read = dup(updater_read);
        if (-1 == new_updater_read) {
            _Exit(errno);
        }

        int new_simulator_shutdowner_write = dup(simulator_shutdowner_write);
        if (-1 == new_simulator_shutdowner_write) {
            _Exit(errno);
        }

        int new_controller_write = dup(controller_write);
        if (-1 == new_controller_write) {
            _Exit(errno);
        }

        char updater_string[] = "--updater=" INT_STRING_PADDING;
        sprintf(updater_string, "--updater=%i", new_updater_read);

        char shutdowner_string[] = "--shutdowner=" INT_STRING_PADDING;
        sprintf(shutdowner_string, "--shutdowner=%i",
                new_simulator_shutdowner_write);

        char controller_string[] = "--controller=" INT_STRING_PADDING;
        sprintf(controller_string, "--controller=%i", new_controller_write);

        char *args[] = {
            (char *)gui_path,
            updater_string,
            shutdowner_string,
            controller_string,
            NULL
        };
        fexecve(gui_binary, args, environ);
        _Exit(errno);
    }

    pid_t simulator = fork();
    if (-1 == simulator) {
        goto cleanup_processes;
    }
    live_processes[1] = simulator;

    if (0 == simulator) {
        int new_updater_write = dup(updater_write);
        if (-1 == new_updater_write) {
            _Exit(errno);
        }

        int new_simulator_shutdowner_read = dup(simulator_shutdowner_read);
        if (-1 == new_simulator_shutdowner_read) {
            _Exit(errno);
        }

        int new_controller_read = dup(controller_read);
        if (-1 == new_controller_read) {
            _Exit(errno);
        }

        char updater_string[] = "--updater=" INT_STRING_PADDING;
        sprintf(updater_string, "--updater=%i", new_updater_write);

        char shutdowner_string[] = "--shutdowner=" INT_STRING_PADDING;
        sprintf(shutdowner_string, "--shutdowner=%i",
                new_simulator_shutdowner_read);

        char controller_string[] = "--controller=" INT_STRING_PADDING;
        sprintf(controller_string, "--controller=%i", new_controller_read);

        char *args[] = {
            (char *)simulator_path,
            updater_string,
            shutdowner_string,
            controller_string,
            NULL
        };
        fexecve(simulator_binary, args, environ);
        _Exit(errno);
    }

    sigset_t sigchld_set;
    sigemptyset(&sigchld_set);
    sigaddset(&sigchld_set, SIGCHLD);

    sigset_t sigold_set;
    pthread_sigmask(SIG_BLOCK, &sigchld_set, &sigold_set);

    for (;;) {
        int info;
        sigwait(&sigchld_set, &info);

        pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);
        /* Let the signal be handled like normal */
        pthread_sigmask(SIG_BLOCK, &sigchld_set, NULL);

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(live_processes); ++ii) {
            /* Poll for our processes */
            if (-1 == live_processes[ii]) {
                continue;
            }

            siginfo_t exit_info;
            exit_info.si_pid = 0;
            waitid(P_PID, live_processes[ii], &exit_info, WEXITED | WNOHANG);

            if (0 == exit_info.si_pid) {
                continue;
            }

            live_processes[ii] = -1;

            switch (exit_info.si_code) {
            case CLD_DUMPED:
            case CLD_KILLED:
                raise(exit_info.si_status);
                goto restore_sigmask;

            case CLD_EXITED:
                if (exit_info.si_status != 0) {
                    errno = exit_info.si_status;
                    goto restore_sigmask;
                }
                break;
            }
        }

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(live_processes); ++ii) {
            if (live_processes[ii] != -1) {
                goto got_live;
            }
        }

        break;

 got_live:;
    }

    exit_status = 0;

 restore_sigmask:
    pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);

 cleanup_processes:
    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(live_processes); ++ii) {
        if (live_processes[ii] != -1) {
            int errnum = errno;
            int kill_status = kill(live_processes[ii], SIGQUIT);
            if (-1 == kill_status) {
                /* errno == ESRCH is fine */
                assert(errno != EINVAL);
                assert(errno != EPERM);
            }

            errno = errnum;
        }
    }

 cleanup_shutdowner_pair:
    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(simulator_shutdowner_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_shutdowner_close(simulator_shutdowner_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 cleanup_controller_pair:
    {
        int errnum = errno;
        int close_status = linted_controller_close(controller_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_controller_close(controller_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

 cleanup_updater_pair:
    {
        int errnum = errno;
        int close_status = linted_updater_close(updater_read);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    {
        int errnum = errno;
        int close_status = linted_updater_close(updater_write);
        if (-1 == exit_status) {
            errno = errnum;
        }
        if (-1 == close_status) {
            exit_status = -1;
        }
    }

    return exit_status;
}
