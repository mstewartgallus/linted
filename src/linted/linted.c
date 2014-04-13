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

#include "binaries.h"

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

static errno_t run_game(char const *simulator_path, int simulator_binary,
                        char const *gui_path, int gui_binary,
                        char const *display);

int main(int argc, char **argv)
{
    /* First we check if we are run with proper security */
    uid_t const uid = getuid();
    uid_t const euid = geteuid();
    if (0 == euid || 0 == uid) {
        linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\
Bad administrator!\n\
It is insecure to run a game as root!\n"));
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

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\
  --simulator         the location of the simulator executable\n\
  --gui               the location of the gui executable\n"));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));

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
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR(PACKAGE_STRING));

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n\n"));

        linted_io_write_format(STDOUT_FILENO, NULL, "\
Copyright (C) %d Steven Stewart-Gallus\n\
License Apache License 2 <http://www.apache.org/licenses/LICENSE-2.0>\n\
This is free software, and you are welcome to redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n", COPYRIGHT_YEAR);

        return EXIT_SUCCESS;
    }

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing DISPLAY environment variable\n",
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

        errno_t errnum = linted_util_sanitize_environment(&essential_fds);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s\n", program_name,
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
    }

    /* Sandbox */
    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        assert(errno != EINVAL);

        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not drop ability to raise privileges: %s\n", program_name, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    /*
     * Don't bother closing these file handles. We are not writing and
     * do not have to confirm that writes have finished.
     */
    int simulator_binary = open(simulator_path, O_RDONLY | O_CLOEXEC);
    if (-1 == simulator_binary) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: %s: %s\n",
                               program_name,
                               simulator_path,
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    int gui_binary = open(gui_path, O_RDONLY | O_CLOEXEC);
    if (-1 == gui_binary) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: %s: %s\n",
                               program_name,
                               gui_path, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    openlog(PACKAGE_TARNAME, LOG_PERROR /* So the user can see this */
            | LOG_CONS          /* So we know there is an error */
            | LOG_PID           /* Because we fork several times */
            , LOG_USER          /* This is a game and is a user program */
        );

    int succesfully_executing = 0;

    errno_t game_status = run_game(simulator_path, simulator_binary,
                                   gui_path, gui_binary, display);
    if (game_status != 0) {
        succesfully_executing = -1;
        char const *error_string = linted_error_string_alloc(game_status);
        syslog(LOG_ERR, "could not run the game: %s", error_string);
        linted_error_string_free(error_string);
    }

    {
        errno_t errnum = linted_io_close(STDERR_FILENO);
        if (errnum != 0) {
            succesfully_executing = -1;
            char const *const error_string = linted_error_string_alloc(errnum);
            syslog(LOG_ERR, "could not close standard error: %s", error_string);
            linted_error_string_free(error_string);
        }
    }

    closelog();

    return (-1 == succesfully_executing) ? EXIT_FAILURE : EXIT_SUCCESS;
}

static errno_t run_game(char const *simulator_path, int simulator_binary,
                    char const *gui_path, int gui_binary, char const *display)
{
    errno_t error_status = 0;

    linted_updater updater_mqs[2];
    linted_controller controller_mqs[2];
    linted_shutdowner simulator_shutdowner_mqs[2];

    linted_updater updater_read;
    linted_updater updater_write;

    linted_controller controller_read;
    linted_controller controller_write;

    linted_shutdowner simulator_shutdowner_read;
    linted_shutdowner simulator_shutdowner_write;

    if ((error_status = linted_updater_pair(updater_mqs,
                                            O_NONBLOCK, O_NONBLOCK)) != 0) {
        return error_status;
    }

    updater_read = updater_mqs[0];
    updater_write = updater_mqs[1];

    if ((error_status = linted_controller_pair(controller_mqs,
                                               O_NONBLOCK, O_NONBLOCK)) != 0) {
        goto cleanup_updater_pair;
    }

    controller_read = controller_mqs[0];
    controller_write = controller_mqs[1];

    if ((error_status = linted_shutdowner_pair(simulator_shutdowner_mqs,
                                               O_NONBLOCK, 0)) != 0) {
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
        int updater_placeholder;
        int shutdowner_placeholder;
        int controller_placeholder;

        updater_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == updater_placeholder) {
            error_status = errno;
            goto restore_sigmask;
        }

        shutdowner_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == shutdowner_placeholder) {
            error_status = errno;
            goto close_updater_placeholder;
        }

        controller_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == controller_placeholder) {
            error_status = errno;
            goto close_shutdowner_placeholder;
        }

        {
            posix_spawn_file_actions_t file_actions;
            if (-1 == posix_spawn_file_actions_init(&file_actions)) {
                error_status = errno;
                goto cleanup_processes;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       updater_read,
                                                       updater_placeholder)) {
                error_status = errno;
                goto destroy_gui_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       simulator_shutdowner_write,
                                                       shutdowner_placeholder))
            {
                error_status = errno;
                goto destroy_gui_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       controller_write,
                                                       controller_placeholder))
            {
                error_status = errno;
                goto destroy_gui_file_actions;
            }

            {
                char updater_string[] = "--updater=" INT_STRING_PADDING;
                sprintf(updater_string, "--updater=%i", updater_placeholder);

                char shutdowner_string[] = "--shutdowner=" INT_STRING_PADDING;
                sprintf(shutdowner_string,
                        "--shutdowner=%i", shutdowner_placeholder);

                char controller_string[] = "--controller=" INT_STRING_PADDING;
                sprintf(controller_string,
                        "--controller=%i", controller_placeholder);

                char *args[] = {
                    (char *)gui_path,
                    updater_string,
                    shutdowner_string,
                    controller_string,
                    NULL
                };
                char *envp[] = { (char *)display, NULL };

                char fd_path[] = "/proc/self/fd/" INT_STRING_PADDING;
                sprintf(fd_path, "/proc/self/fd/%i", gui_binary);
                if (-1 == posix_spawn(&live_processes[0], fd_path,
                                      &file_actions, NULL, args, envp)) {
                    error_status = errno;
                }
            }

 destroy_gui_file_actions:
            if (-1 == posix_spawn_file_actions_destroy(&file_actions)
                && 0 == error_status) {
                error_status = errno;
            }

            if (error_status != 0) {
                goto close_controller_placeholder;
            }
        }

        {
            posix_spawn_file_actions_t file_actions;
            if (-1 == posix_spawn_file_actions_init(&file_actions)) {
                error_status = errno;
                goto cleanup_processes;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       updater_write,
                                                       updater_placeholder)) {
                error_status = errno;
                goto destroy_sim_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       simulator_shutdowner_read,
                                                       shutdowner_placeholder)) {
                error_status = errno;
                goto destroy_sim_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       controller_read,
                                                       controller_placeholder)) {
                error_status = errno;
                goto destroy_sim_file_actions;
            }

            {
                char updater_string[] = "--updater=" INT_STRING_PADDING;
                sprintf(updater_string, "--updater=%i", updater_placeholder);

                char shutdowner_string[] = "--shutdowner=" INT_STRING_PADDING;
                sprintf(shutdowner_string,
                        "--shutdowner=%i", shutdowner_placeholder);

                char controller_string[] = "--controller=" INT_STRING_PADDING;
                sprintf(controller_string,
                        "--controller=%i", controller_placeholder);

                char *args[] = {
                    (char *)simulator_path,
                    updater_string,
                    shutdowner_string,
                    controller_string,
                    NULL
                };
                char *envp[] = { NULL };

                char fd_path[] = "/proc/self/fd/" INT_STRING_PADDING;
                sprintf(fd_path, "/proc/self/fd/%i", simulator_binary);
                if (-1 == posix_spawn(&live_processes[1], fd_path,
                                      &file_actions, NULL, args, envp)) {
                    error_status = errno;
                }
            }

        destroy_sim_file_actions:
            if (-1 == posix_spawn_file_actions_destroy(&file_actions)
                && 0 == error_status) {
                error_status = errno;
            }
        }

    close_controller_placeholder:
        {
            errno_t errnum = linted_io_close(controller_placeholder);
            if (0 == error_status) {
                error_status = errnum;
            }
        }

    close_shutdowner_placeholder:
        {
            errno_t errnum = linted_io_close(shutdowner_placeholder);
            if (0 == error_status) {
                error_status = errnum;
            }
        }

    close_updater_placeholder:
        {
            errno_t errnum = linted_io_close(updater_placeholder);
            if (0 == error_status) {
                error_status = errnum;
            }
        }

        if (error_status != 0) {
            goto cleanup_processes;
        }
    }

    for (;;) {
        siginfo_t info;
        int signal_number;
        errno_t signal_status;
        do {
            signal_number = sigtimedwait(&sig_set, &info, NULL);
            signal_status = -1 == signal_number ? errno : 0;
        } while (EINTR == signal_status);
        if (signal_status != 0) {
            error_status = signal_status;
            goto cleanup_processes;
        }

        if (linted_manager_send_signal() == signal_number) {
            pid_t pid = info.si_pid;
            struct linted_manager_req *request = info.si_ptr;

            errno_t reply_status = 0;

            unsigned type;
            {
                errno_t errnum = linted_manager_req_type(pid, request, &type);
                if (errnum != 0) {
                    reply_status = errnum;
                    goto finish_reply;
                }
            }

            switch (type) {
            case LINTED_MANAGER_START:{
                    struct linted_manager_start_args arguments;
                    {
                        errno_t errnum = linted_manager_start_req_args(pid,
                                                                       request,
                                                                       &arguments);
                        if (errnum != 0) {
                            reply_status = errnum;
                            goto finish_reply;
                        }
                    }

                    switch (arguments.service) {
                    case LINTED_MANAGER_SERVICE_GUI:
                    case LINTED_MANAGER_SERVICE_SIMULATOR:{
                            /* Lie for now */
                            struct linted_manager_start_reply reply = {
                                .is_up = true
                            };
                            errno_t errnum = linted_manager_start_req_reply(pid,
                                                                            request,
                                                                            &reply);
                            if (errnum != 0) {
                                reply_status = errnum;
                                goto finish_reply;
                            }
                            break;
                        }

                    default:
                        reply_status = EINVAL;
                        goto finish_reply;
                    }
                    break;
                }

            default:
                reply_status = EINVAL;
                goto finish_reply;
            }

        finish_reply:
            {
                errno_t errnum = linted_manager_finish_reply(pid, reply_status);
                if (errnum != 0) {
                    error_status = errnum;
                    goto cleanup_processes;
                }
            }
        } else if (SIGCHLD == signal_number) {
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(live_processes); ++ii) {
                /* Poll for our processes */
                if (-1 == live_processes[ii]) {
                    continue;
                }

                siginfo_t exit_info;
                exit_info.si_pid = 0;
                if (-1 == waitid(P_PID, live_processes[ii], &exit_info,
                                 WEXITED | WNOHANG)) {
                    assert(errno != EINVAL);

                    error_status = errno;
                    goto cleanup_processes;
                }

                if (0 == exit_info.si_pid) {
                    continue;
                }

                live_processes[ii] = -1;

                switch (exit_info.si_code) {
                case CLD_DUMPED:
                case CLD_KILLED:
                    raise(exit_info.si_status);
                    error_status = errno;
                    goto cleanup_processes;

                case CLD_EXITED:
                    if (exit_info.si_status != 0) {
                        error_status = exit_info.si_status;
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

 cleanup_processes:
    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(live_processes); ++ii) {
        if (live_processes[ii] != -1) {
            int kill_status = kill(live_processes[ii], SIGQUIT);
            if (-1 == kill_status) {
                /* errno == ESRCH is fine */
                assert(errno != EINVAL);
                assert(errno != EPERM);
            }
        }
    }

 restore_sigmask:
    pthread_sigmask(SIG_SETMASK, &sigold_set, NULL);

    {
        int errnum = linted_shutdowner_close(simulator_shutdowner_read);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

    {
        int errnum = linted_shutdowner_close(simulator_shutdowner_write);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

 cleanup_controller_pair:
    {
        int errnum = linted_controller_close(controller_read);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

    {
        int errnum = linted_controller_close(controller_write);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

 cleanup_updater_pair:
    {
        int errnum = linted_updater_close(updater_read);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

    {
        int errnum = linted_updater_close(updater_write);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

    return error_status;
}
