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
#include "linted/manager.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <unistd.h>

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define SIMULATOR_OPTION "--simulator"
#define GUI_OPTION "--gui"

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

static int run_game(char const *simulator_path, char const *gui_path,
                    char const *display);

int main(int argc, char **argv)
{
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
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing process name\n", PACKAGE_TARNAME);
        return EXIT_FAILURE;
    }

    char const *const program_name = argv[0];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    char const *simulator_path = PKGLIBEXECDIR "/simulator" EXEEXT;
    char const *gui_path = PKGLIBEXECDIR "/gui" EXEEXT;

    for (unsigned ii = 1; ii < (unsigned)argc; ++ii) {
        char const *argument = argv[ii];

        if (0 == strcmp(argument, HELP_OPTION)) {
            need_help = true;
        } else if (0 == strcmp(argument, VERSION_OPTION)) {
            need_version = true;

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
        linted_io_write_format(STDOUT_FILENO, NULL, "Usage: %s [OPTIONS]\n",
                               program_name);

        linted_io_write_format(STDOUT_FILENO, NULL, "Play the %s game.\n",
                               PACKAGE_NAME);

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  --help              display this help and exit\n\
  --version           display version information and exit\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\
  --simulator         the location of the simulator executable\n\
  --gui               the location of the gui executable\n");

        linted_io_write_string(STDOUT_FILENO, NULL, "\n");

        linted_io_write_format(STDOUT_FILENO, NULL, "Report bugs to <%s>\n",
                               PACKAGE_BUGREPORT);
        linted_io_write_format(STDOUT_FILENO, NULL, "%s home page: <%s>\n",
                               PACKAGE_NAME, PACKAGE_URL);

        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: urecognized option '%s'\n",
                               program_name, bad_option);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_io_write_string(STDERR_FILENO, NULL, PACKAGE_STRING);

        linted_io_write_string(STDERR_FILENO, NULL, "\n\n");

        linted_io_write_format(STDERR_FILENO, NULL, "\
Copyright (C) %d Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n", COPYRIGHT_YEAR);

        return EXIT_SUCCESS;
    }

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: no DISPLAY environment variable\n",
                               program_name);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "Try `%s %s' for more information.\n",
                               program_name, HELP_OPTION);
        return EXIT_FAILURE;
    }

    size_t display_value_length = strlen(original_display);
    size_t display_string_length =
        strlen("DISPLAY=") + display_value_length + 1;
    char *display = malloc(display_string_length);
    if (NULL == display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: can't allocate DISPLAY string: %s\n",
                               program_name, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }
    memcpy(display, "DISPLAY=", strlen("DISPLAY="));
    memcpy(display + strlen("DISPLAY="), original_display,
           display_value_length);
    display[display_string_length - 1] = 0;

    {
        fd_set essential_fds;
        FD_ZERO(&essential_fds);

        FD_SET(STDERR_FILENO, &essential_fds);
        FD_SET(STDIN_FILENO, &essential_fds);
        FD_SET(STDOUT_FILENO, &essential_fds);

        if (-1 == linted_util_sanitize_environment(&essential_fds)) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s", program_name, linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }
    }

    /* Sandbox */
    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        assert(errno != EINVAL);

        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not drop ability to raise privileges: %s", program_name, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    openlog(PACKAGE_TARNAME, LOG_PERROR /* So the user can see this */
            | LOG_CONS          /* So we know there is an error */
            | LOG_PID           /* Because we fork several times */
            , LOG_USER          /* This is a game and is a user program */
        );

    int succesfully_executing = 0;

    if (0 == succesfully_executing) {
        if (-1 == run_game(simulator_path, gui_path, display)) {
            succesfully_executing = -1;
            char const *error_string = linted_error_string_alloc(errno);
            syslog(LOG_ERR, "could not run the game: %s", error_string);
            linted_error_string_free(error_string);
        }
    }

    if (-1 == linted_io_close(STDERR_FILENO)) {
        succesfully_executing = -1;
        char const *const error_string = linted_error_string_alloc(errno);
        syslog(LOG_ERR, "could not close standard error: %s", error_string);
        linted_error_string_free(error_string);
    }

    closelog();

    return (-1 == succesfully_executing) ? EXIT_FAILURE : EXIT_SUCCESS;
}

static int run_game(char const *simulator_path, char const *gui_path,
                    char const *display)
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

    sigset_t sig_set;
    sigemptyset(&sig_set);

    sigaddset(&sig_set, SIGCHLD);
    sigaddset(&sig_set, linted_manager_send_signal());

    sigset_t sigold_set;
    pthread_sigmask(SIG_BLOCK, &sig_set, &sigold_set);

    pid_t live_processes[] = { -1, -1 };

    /* Create placeholder file descriptors to be overwritten later on */
    {
        int create_processes_succesfully = -1;

        int updater_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == updater_placeholder) {
            goto restore_sigmask;
        }

        int shutdowner_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == shutdowner_placeholder) {
            goto close_updater_placeholder;
        }

        int controller_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == controller_placeholder) {
            goto close_shutdowner_placeholder;
        }

        char updater_string[] = "--updater=" INT_STRING_PADDING;
        sprintf(updater_string, "--updater=%i", updater_placeholder);

        char shutdowner_string[] = "--shutdowner=" INT_STRING_PADDING;
        sprintf(shutdowner_string, "--shutdowner=%i", shutdowner_placeholder);

        char controller_string[] = "--controller=" INT_STRING_PADDING;
        sprintf(controller_string, "--controller=%i", controller_placeholder);

        {
            char *args[] = {
                (char *)gui_path,
                updater_string,
                shutdowner_string,
                controller_string,
                NULL
            };
            char *envp[] = { (char *)display, NULL };

            posix_spawn_file_actions_t file_actions;
            if (-1 == posix_spawn_file_actions_init(&file_actions)) {
                goto cleanup_processes;
            }

            int spawned_okay = -1;
            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       updater_read,
                                                       updater_placeholder)) {
                goto destroy_gui_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       simulator_shutdowner_write,
                                                       shutdowner_placeholder))
            {
                goto destroy_gui_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       controller_write,
                                                       controller_placeholder))
            {
                goto destroy_gui_file_actions;
            }

            if (-1 == posix_spawn(&live_processes[0], gui_path,
                                  &file_actions, NULL, args, envp)) {
                goto destroy_gui_file_actions;
            }

            spawned_okay = 0;

 destroy_gui_file_actions:
            if (-1 == posix_spawn_file_actions_destroy(&file_actions)) {
                goto close_controller_placeholder;
            }
            if (-1 == spawned_okay) {
                goto close_controller_placeholder;
            }
        }

        {
            char *args[] = {
                (char *)simulator_path,
                updater_string,
                shutdowner_string,
                controller_string,
                NULL
            };
            char *envp[] = { NULL };

            posix_spawn_file_actions_t file_actions;
            if (-1 == posix_spawn_file_actions_init(&file_actions)) {
                goto cleanup_processes;
            }

            int spawned_okay = -1;
            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       updater_write,
                                                       updater_placeholder)) {
                goto destroy_sim_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       simulator_shutdowner_read,
                                                       shutdowner_placeholder))
            {
                goto destroy_sim_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       controller_read,
                                                       controller_placeholder))
            {
                goto destroy_sim_file_actions;
            }

            if (-1 == posix_spawn(&live_processes[1], simulator_path,
                                  &file_actions, NULL, args, envp)) {
                goto destroy_sim_file_actions;
            }

            spawned_okay = 0;

 destroy_sim_file_actions:
            if (-1 == posix_spawn_file_actions_destroy(&file_actions)) {
                goto close_controller_placeholder;
            }
            if (-1 == spawned_okay) {
                goto close_controller_placeholder;
            }
        }

        create_processes_succesfully = 0;

 close_controller_placeholder:
        {
            int errnum = errno;
            int close_status = linted_io_close(controller_placeholder);
            if (-1 == create_processes_succesfully) {
                errno = errnum;
            }
            if (-1 == close_status) {
                create_processes_succesfully = -1;
            }
        }
 close_shutdowner_placeholder:
        {
            int errnum = errno;
            int close_status = linted_io_close(shutdowner_placeholder);
            if (-1 == create_processes_succesfully) {
                errno = errnum;
            }
            if (-1 == close_status) {
                create_processes_succesfully = -1;
            }
        }
 close_updater_placeholder:
        {
            int errnum = errno;
            int close_status = linted_io_close(updater_placeholder);
            if (-1 == create_processes_succesfully) {
                errno = errnum;
            }
            if (-1 == close_status) {
                create_processes_succesfully = -1;
            }
        }

        if (-1 == create_processes_succesfully) {
            goto cleanup_processes;
        }
    }

    for (;;) {
        siginfo_t info;
        int signal_number;
        do {
            signal_number = sigtimedwait(&sig_set, &info, NULL);
        } while (-1 == signal_number && EINTR == errno);
        if (-1 == signal_number) {
            goto cleanup_processes;
        }

        if (linted_manager_send_signal() == signal_number) {
            struct linted_manager_message message;
            memset(&message, 0, sizeof message);

            if (-1 == linted_manager_receive_message(&info, &message)) {
                char const *err = linted_error_string_alloc(errno);
                linted_io_write_format(STDERR_FILENO, NULL,
                                       "got bad message: %s!\n", err);
                linted_error_string_free(err);
            } else {
                linted_io_write_format(STDERR_FILENO, NULL,
                                       "got message: %i!\n", message.dummy);
            }
        } else if (SIGCHLD == signal_number) {
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(live_processes); ++ii) {
                /* Poll for our processes */
                if (-1 == live_processes[ii]) {
                    continue;
                }

                siginfo_t exit_info;
                exit_info.si_pid = 0;
                waitid(P_PID, live_processes[ii], &exit_info,
                       WEXITED | WNOHANG);

                if (0 == exit_info.si_pid) {
                    continue;
                }

                live_processes[ii] = -1;

                switch (exit_info.si_code) {
                case CLD_DUMPED:
                case CLD_KILLED:
                    raise(exit_info.si_status);
                    goto cleanup_processes;

                case CLD_EXITED:
                    if (exit_info.si_status != 0) {
                        errno = exit_info.si_status;
                        goto cleanup_processes;
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

 got_live: ;
        } else {
            assert(false);
        }
    }

    exit_status = 0;

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

 restore_sigmask:
    pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);

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
