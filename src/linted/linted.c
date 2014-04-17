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

#include "linted/controller.h"
#include "linted/io.h"
#include "linted/manager.h"
#include "linted/shutdowner.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#define BACKLOG 20

#define ACTIVE_MANAGEMENT_CONNECTIONS 10

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define SIMULATOR_OPTION "--simulator"
#define GUI_OPTION "--gui"

#define INT_STRING_PADDING "XXXXXXXXXXXXXX"

struct connection {
    union linted_manager_reply reply;
    int fd;
    bool has_reply_ready;
};

struct waiter_data {
    id_t process_group;
    int fd;
};

enum waiter_message_type {
    WAITER_FINISHED,
    WAITER_ERROR
};

struct waiter_message {
    enum waiter_message_type type;
    errno_t errnum;
};

static void * waiter_routine(void * data);

static errno_t on_new_connections_readable(int new_connections,
                                           size_t * connection_count,
                                           struct connection * connections);
static errno_t on_connection_readable(int new_socket,
                                      union linted_manager_reply *reply);
static errno_t on_connection_writeable(int fd,
                                       union linted_manager_reply *reply);

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
Copyright (C) %s Steven Stewart-Gallus\n\
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

    int cwd = open("./", O_CLOEXEC | O_DIRECTORY);
    if (-1 == cwd) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: can't open the current working directory: %s\n",
                               program_name, linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    {
        fd_set essential_fds;
        FD_ZERO(&essential_fds);

        FD_SET(STDERR_FILENO, &essential_fds);
        FD_SET(STDIN_FILENO, &essential_fds);
        FD_SET(STDOUT_FILENO, &essential_fds);

        FD_SET(cwd, &essential_fds);

        errno_t errnum = linted_util_sanitize_environment(&essential_fds);
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s\n", program_name, linted_error_string_alloc(errnum));
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

    {
        /* Set signals to a safe default */
        sigset_t sigblocked_set;
        sigemptyset(&sigblocked_set);

        /* Get EPIPEs */
        sigaddset(&sigblocked_set, SIGPIPE);

        pthread_sigmask(SIG_BLOCK, &sigblocked_set, NULL);
    }

    /*
     * Don't bother closing these file handles. We are not writing and
     * do not have to confirm that writes have finished.
     */
    int simulator_binary = openat(cwd, simulator_path, O_RDONLY | O_CLOEXEC);
    if (-1 == simulator_binary) {
        linted_io_write_format(STDERR_FILENO, NULL, "%s: %s: %s\n",
                               program_name,
                               simulator_path,
                               linted_error_string_alloc(errno));
        return EXIT_FAILURE;
    }

    int gui_binary = openat(cwd, gui_path, O_RDONLY | O_CLOEXEC);
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
                        char const *gui_path, int gui_binary,
                        char const *display)
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

    int process_group;

    /* Create placeholder file descriptors to be overwritten later on */
    {
        int updater_placeholder;
        int shutdowner_placeholder;
        int controller_placeholder;

        updater_placeholder = open("/dev/null", O_RDONLY | O_CLOEXEC);
        if (-1 == updater_placeholder) {
            error_status = errno;
            goto close_shutdowner;
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

            posix_spawnattr_t attr;
            if (-1 == posix_spawnattr_init(&attr)) {
                error_status = errno;
                goto destroy_gui_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       updater_read,
                                                       updater_placeholder)) {
                error_status = errno;
                goto destroy_spawnattr;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       simulator_shutdowner_write,
                                                       shutdowner_placeholder))
            {
                error_status = errno;
                goto destroy_spawnattr;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       controller_write,
                                                       controller_placeholder))
            {
                error_status = errno;
                goto destroy_spawnattr;
            }

            if (-1 == posix_spawnattr_setpgroup(&attr, 0)) {
                error_status = errno;
                goto destroy_spawnattr;
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
                if (-1 == posix_spawn(&process_group, fd_path,
                                      &file_actions, NULL,
                                      args, envp)) {
                    error_status = errno;
                }

                if (0 == error_status) {
                    errno_t errnum = -1 == setpgid(process_group, process_group) ? errno : 0;
                    if (errnum != 0 && errnum != EACCES) {
                        error_status = errnum;
                    }
                }
            }

 destroy_spawnattr:
            if (-1 == posix_spawnattr_destroy(&attr)
                && 0 == error_status) {
                error_status = errno;
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

            posix_spawnattr_t attr;
            if (-1 == posix_spawnattr_init(&attr)) {
                error_status = errno;
                goto destroy_sim_file_actions;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       updater_write,
                                                       updater_placeholder)) {
                error_status = errno;
                goto destroy_sim_spawnattr;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       simulator_shutdowner_read,
                                                       shutdowner_placeholder))
            {
                error_status = errno;
                goto destroy_sim_spawnattr;
            }

            if (-1 == posix_spawn_file_actions_adddup2(&file_actions,
                                                       controller_read,
                                                       controller_placeholder))
            {
                error_status = errno;
                goto destroy_sim_spawnattr;
            }

            if (-1 == posix_spawnattr_setpgroup(&attr, process_group)) {
                error_status = errno;
                goto destroy_sim_spawnattr;
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


                pid_t process;
                if (-1 == posix_spawn(&process, fd_path,
                                      &file_actions, NULL, args, envp)) {
                    error_status = errno;
                }

                if (0 == error_status) {
                    errno_t errnum = -1 == setpgid(process, process_group) ? errno : 0;
                    if (errnum != 0 && errnum != EACCES) {
                        error_status = errnum;
                    }
                }
            }

 destroy_sim_spawnattr:
            if (-1 == posix_spawnattr_destroy(&attr)
                && 0 == error_status) {
                error_status = errno;
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

    int new_connections = socket(AF_UNIX,
                                 SOCK_SEQPACKET | SOCK_NONBLOCK | SOCK_CLOEXEC,
                                 0);
    if (-1 == new_connections) {
        error_status = errno;
        goto cleanup_processes;
    }

    {
        sa_family_t address = AF_UNIX;
        if (-1 == bind(new_connections, (void *)&address, sizeof address)) {
            error_status = errno;
            goto close_new_connections;
        }
    }

    {
        struct sockaddr_un address;
        memset(&address, 0, sizeof address);

        socklen_t addr_len = sizeof address;
        if (-1 == getsockname(new_connections, (void *)&address, &addr_len)) {
            error_status = errno;
            goto close_new_connections;
        }
        linted_io_write_format(STDOUT_FILENO, NULL,
                               "management socket: %s\n", address.sun_path + 1);
    }

    if (-1 == listen(new_connections, BACKLOG)) {
        error_status = errno;
        goto close_new_connections;
    }

    int waiter_fds[2];
    if (-1 == pipe2(waiter_fds, O_CLOEXEC)) {
        error_status = errno;
        goto close_new_connections;
    }

    {
        struct waiter_data waiter_data = {
            .fd = waiter_fds[1],
            .process_group = process_group
        };

        pthread_t waiter_thread;
        if (-1 == pthread_create(&waiter_thread, NULL,
                                 waiter_routine, &waiter_data)) {
            goto close_waiter_fds;
        }

        size_t connection_count = 0;
        struct connection connections[ACTIVE_MANAGEMENT_CONNECTIONS];

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
            connections[ii].fd = -1;
        }

        for (;;) {
            enum {
                WAITER,
                NEW_CONNECTIONS,
                CONNECTION
            };
            struct pollfd fds[] = {
                [WAITER] = {.fd = waiter_fds[0], .events = POLLIN},
                [NEW_CONNECTIONS] = {.fd = new_connections, .events = POLLIN},

                [CONNECTION + ACTIVE_MANAGEMENT_CONNECTIONS - 1] = {.fd = 0}
            };
            size_t connection_ids[ACTIVE_MANAGEMENT_CONNECTIONS];

            size_t active_connections = 0;
            for (size_t ii = 0; ii < ACTIVE_MANAGEMENT_CONNECTIONS; ++ii) {
                struct connection * connection = &connections[ii];
                if (connection->fd != -1) {
                    fds[CONNECTION + active_connections].fd = connection->fd;
                    fds[CONNECTION + active_connections].events = connection->has_reply_ready ? POLLOUT : POLLIN;
                    connection_ids[active_connections] = ii;
                    ++active_connections;
                }
            }

            errno_t poll_status;
            do {
                poll_status = -1 == poll(fds,
                                         LINTED_ARRAY_SIZE(fds) - ACTIVE_MANAGEMENT_CONNECTIONS + active_connections,
                                         -1)
                    ? errno
                    : 0;
            } while (EINTR == poll_status);
            if (poll_status != 0) {
                error_status = poll_status;
                goto close_connections;
            }

            if ((fds[NEW_CONNECTIONS].revents & POLLIN) != 0) {
                errno_t errnum = on_new_connections_readable(new_connections,
                                                             &connection_count,
                                                             connections);
                if (errnum != 0) {
                    error_status = errnum;
                    goto close_connections;
                }
            }

            if ((fds[WAITER].revents & POLLIN) != 0) {
                struct waiter_message message;
                errno_t errnum = linted_io_read_all(waiter_fds[0], NULL,
                                                    &message, sizeof message);
                if (errnum != 0) {
                    error_status = errnum;
                    goto close_connections;
                }

                switch (message.type) {
                case WAITER_FINISHED:
                    goto close_connections;

                case WAITER_ERROR:
                    error_status = message.errnum;
                    goto close_connections;

                default:
                    assert(false);
                }
            }

            for (size_t ii = 0; ii < active_connections; ++ii) {
                size_t connection_id = connection_ids[ii];
                struct connection * connection = &connections[connection_id];

                int fd = connection->fd;

                if (connection->has_reply_ready) {
                    if ((fds[CONNECTION + ii].revents & POLLOUT) != 0) {
                        goto try_writing;
                    }
                } else {
                    if ((fds[CONNECTION + ii].revents & POLLIN) != 0) {
                        goto try_reading;
                    }
                }

                continue;

            try_reading:
                {
                    union linted_manager_reply reply;
                    errno_t errnum = on_connection_readable(fd, &reply);
                    switch (errnum) {
                    case 0:
                        break;

                    case EAGAIN:
                        /* Maybe the socket was only available for
                         * writing but not reading */
                        continue;

                    case EPIPE:
                        /* Ignore the misbehaving other end */
                        goto remove_connection;

                    default:
                        error_status = errnum;
                        goto close_connections;
                    }

                    connection->has_reply_ready = true;
                    connection->reply = reply;
                }

            try_writing:
                {
                    errno_t errnum = on_connection_writeable(fd,
                                                             &connection->reply);
                    switch (errnum) {
                    case 0:
                        break;

                    case EAGAIN:
                        /* Maybe the socket was only available for
                         * reading but not writing */
                        continue;

                    case EPIPE:
                        /* Ignore the misbehaving other end */
                        goto remove_connection;

                    default:
                        error_status = errnum;
                        goto close_connections;
                    }
                }

            remove_connection:
                connection->fd = -1;
                --connection_count;

                {
                    errno_t errnum = linted_io_close(fd);
                    if (errnum != 0) {
                        error_status = errnum;
                        goto close_connections;
                    }
                }
            }
        }

 close_connections:
    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
        struct connection * const connection = &connections[ii];
        int const fd = connection->fd;
        if (fd != -1) {
            errno_t errnum = linted_io_close(fd);
            if (0 == error_status) {
                error_status = errnum;
            }
        }
    }

 cancel_waiter_thread:
        pthread_cancel(waiter_thread);
        pthread_join(waiter_thread, NULL);
    }

 close_waiter_fds:
    {
        errno_t errnum = linted_io_close(waiter_fds[0]);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

    {
        errno_t errnum = linted_io_close(waiter_fds[1]);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

 close_new_connections:
    {
        errno_t errnum = linted_io_close(new_connections);
        if (0 == error_status) {
            error_status = errnum;
        }
    }

 cleanup_processes:
    if (error_status != 0) {
        errno_t errnum = -1 == kill(-process_group, SIGQUIT) ? errno : 0;
        /* errnum == ESRCH is fine */
        assert(errnum != EINVAL);
        assert(errnum != EPERM);
    }

 close_shutdowner:
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

static void * waiter_routine(void * data)
{
    struct waiter_data * waiter_data = data;

    errno_t error_status;
    for (;;) {
        siginfo_t exit_info;
        errno_t wait_status;
        do {
            wait_status = -1 == waitid(P_PGID, waiter_data->process_group,
                                       &exit_info, WEXITED) ? errno : 0;
            assert(wait_status != EINVAL);
        } while (EINTR == wait_status);

        if (ECHILD == wait_status) {
            struct waiter_message message = {
                .type = WAITER_FINISHED
            };
            errno_t errnum = linted_io_write_all(waiter_data->fd, NULL,
                                                 &message, sizeof message);
            if (errnum != 0) {
                error_status = errnum;
                goto handle_error;
            }
            break;
        }

        if (wait_status != 0) {
            error_status = wait_status;
            goto handle_error;
        }

        switch (exit_info.si_code) {
        case CLD_DUMPED:
        case CLD_KILLED:
            raise(exit_info.si_status);
            error_status = errno;
            goto handle_error;

        case CLD_EXITED:
            if (exit_info.si_status != 0) {
                error_status = exit_info.si_status;
                goto handle_error;
            }
            break;
        }
    }

    return NULL;

handle_error:;
    struct waiter_message message = {
        .type = WAITER_ERROR,
        .errnum = error_status
    };
    linted_io_write_all(waiter_data->fd, NULL,
                        &message, sizeof message);
    return NULL;
}

static errno_t on_new_connections_readable(int new_connections,
                                           size_t * connection_count,
                                           struct connection * connections)
{
    for (;;) {
        int new_socket = accept4(new_connections, NULL, NULL,
                                 SOCK_NONBLOCK | SOCK_CLOEXEC);
        if (-1 == new_socket) {
            errno_t errnum = errno;

            if (EAGAIN == errnum || EWOULDBLOCK == errnum){
                break;
            }

            return errnum;
        }

        errno_t error_status = 0;

        union linted_manager_reply reply;
        {
            errno_t errnum = on_connection_readable(new_socket, &reply);
            switch (errnum) {
            case 0:
                break;

            case EAGAIN:
                goto queue_socket;

            case EPIPE:
                /* Ignore the misbehaving other end */
                continue;

            default:
                error_status = errnum;
                goto close_new_socket;
            }
        }

        {
            errno_t errnum = on_connection_writeable(new_socket, &reply);
            switch (errnum) {
            case 0:
                break;

            case EAGAIN:
                goto queue_socket;

            case EPIPE:
                /* Ignore the misbehaving other end */
                continue;

            default:
                error_status = errnum;
                goto close_new_socket;
            }
        }

        /* Connection completed, do the next connection */
        {
            errno_t errnum = linted_io_close(new_socket);
            if (errnum != 0) {
                return errnum;
            }
        }
        continue;

    queue_socket:
        if (*connection_count >= ACTIVE_MANAGEMENT_CONNECTIONS) {
            /* I'm sorry sir but we are full today. */
            goto close_new_socket;
        }

        struct connection * connection;

        for (size_t ii = 0; ii < ACTIVE_MANAGEMENT_CONNECTIONS; ++ii) {
            connection = &connections[ii];
            if (-1 == connection->fd) {
                goto got_space;
            }
        }
        /* Impossible, listen has limited this */
        assert(false);
    got_space:;
        connection->fd = new_socket;
        connection->has_reply_ready = false;

        ++*connection_count;
        continue;

    close_new_socket:;
        {
            errno_t errnum = linted_io_close(new_socket);
            if (0 == error_status) {
                error_status = errnum;
            }
            return error_status;
        }
    }

    return 0;
}

static errno_t on_connection_readable(int fd,
                                      union linted_manager_reply *reply)
{
    union linted_manager_request request;

    {
        size_t bytes_read;
        errno_t errnum = linted_io_read_all(fd, &bytes_read,
                                            &request, sizeof request);
        if (errnum != 0) {
            return errnum;
        }

        if (0 == bytes_read) {
            /* Hangup */
            return EPIPE;
        }

        /* Sent malformed input */
        if (bytes_read != sizeof request) {
            return EPIPE;
        }
    }

    /* Sent malformed input */
    if (request.type != LINTED_MANAGER_START) {
        return EPIPE;
    }

    memset(reply, 0, sizeof *reply);
    reply->start.is_up = true;

    return 0;
}

static errno_t on_connection_writeable(int fd,
                                       union linted_manager_reply *reply)
{
    return linted_manager_send_reply(fd, reply);
}
