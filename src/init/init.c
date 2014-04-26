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
#include "linted/locale.h"
#include "linted/logger.h"
#include "linted/manager.h"
#include "linted/mq.h"
#include "linted/shutdowner.h"
#include "linted/spawn.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <poll.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/types.h>


#define BACKLOG 20

#define MAX_MANAGE_CONNECTIONS 10

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

enum waiter_message_type {
    WAITER_FINISHED,
    WAITER_ERROR
};

struct waiter_message {
    siginfo_t exit_info;
    errno_t errnum;
};

struct service {
    pid_t pid;
};

struct sim_config {
    char const *path;
    int working_directory;
};

struct gui_config {
    char const *const *environment;
    char const *path;
    int working_directory;
};

struct waiter_data;

struct waiter {
    pthread_t pthread;
    struct waiter_data * waiter_data;
    int init_wait_fd;
    int waiter_wait_fd;
};

static errno_t waiter_init(struct waiter * waiter, pid_t pid);
static int waiter_fd(struct waiter const * waiter);
static errno_t waiter_destroy(struct waiter const * waiter);

static void *waiter_routine(void *data);

static errno_t on_new_connections_readable(linted_manager new_connections,
                                           struct service const *services,
                                           size_t * connection_count,
                                           struct connection *connections);
static errno_t on_connection_readable(int fd,
                                      struct service const *services,
                                      bool * hungup,
                                      union linted_manager_reply *reply);
static errno_t on_connection_writeable(int fd,
                                       union linted_manager_reply *reply);

static errno_t run_game(char const *process_name,
                        struct sim_config const *sim_config,
                        struct gui_config const *gui_config);

static errno_t linted_help(int fildes, char const *program_name,
                           struct linted_str package_name,
                           struct linted_str package_url,
                           struct linted_str package_bugreport);

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
        linted_locale_missing_process_name(STDERR_FILENO,
                                           LINTED_STR(PACKAGE_TARNAME));
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
        linted_help(STDOUT_FILENO,
                    program_name,
                    LINTED_STR(PACKAGE_NAME),
                    LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(STDERR_FILENO, program_name, bad_option);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing DISPLAY environment variable\n",
                               program_name);
        linted_locale_try_for_more_help(STDERR_FILENO, program_name,
                                        LINTED_STR(HELP_OPTION));
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
        int kept_fds[] = {
            STDERR_FILENO,
            STDIN_FILENO,
            STDOUT_FILENO,
            cwd
        };

        errno_t errnum = linted_util_sanitize_environment(kept_fds,
                                                          LINTED_ARRAY_SIZE
                                                          (kept_fds));
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

    int succesfully_executing = 0;

    errno_t game_status;
    {
        struct sim_config sim_config = {
            .path = simulator_path,
            .working_directory = cwd
        };
        struct gui_config gui_config = {
            .environment = (char const *const[]){display, NULL},
            .path = gui_path,
            .working_directory = cwd
        };
        game_status = run_game(program_name, &sim_config, &gui_config);
    }
    if (game_status != 0) {
        succesfully_executing = -1;
        char const *error_string = linted_error_string_alloc(game_status);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "could not run the game: %s\n", error_string);
        linted_error_string_free(error_string);
    }

    {
        errno_t errnum = linted_io_close(STDERR_FILENO);
        if (errnum != 0) {
            succesfully_executing = -1;
            char const *const error_string = linted_error_string_alloc(errnum);
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "could not close standard error: %s\n",
                                   error_string);
            linted_error_string_free(error_string);
        }
    }

    return (-1 == succesfully_executing) ? EXIT_FAILURE : EXIT_SUCCESS;
}

static errno_t run_game(char const *process_name,
                        struct sim_config const *sim_config,
                        struct gui_config const *gui_config)
{
    errno_t errnum = 0;

    linted_logger logger_read;
    linted_logger logger_write;

    linted_updater updater_read;
    linted_updater updater_write;

    linted_controller controller_read;
    linted_controller controller_write;

    linted_shutdowner shutdowner_read;
    linted_shutdowner shutdowner_write;

    {
        linted_logger logger_logds[2];
        if ((errnum = linted_logger_pair(logger_logds)) != 0) {
            return errnum;
        }

        logger_read = logger_logds[0];
        logger_write = logger_logds[1];
    }

    {
        linted_updater updater_mqs[2];
        if ((errnum = linted_updater_pair(updater_mqs,
                                          O_NONBLOCK, O_NONBLOCK)) != 0) {
            goto close_logger_pair;
        }
        updater_read = updater_mqs[0];
        updater_write = updater_mqs[1];
    }

    {
        linted_controller controller_mqs[2];
        if ((errnum = linted_controller_pair(controller_mqs,
                                             O_NONBLOCK, O_NONBLOCK)) != 0) {
            goto close_updater_pair;
        }

        controller_read = controller_mqs[0];
        controller_write = controller_mqs[1];
    }

    {
        linted_shutdowner shutdowner_mqs[2];
        if ((errnum = linted_shutdowner_pair(shutdowner_mqs,
                                             O_NONBLOCK, 0)) != 0) {
            goto close_controller_pair;
        }

        shutdowner_read = shutdowner_mqs[0];
        shutdowner_write = shutdowner_mqs[1];
    }

    struct service services[] = {
        [LINTED_MANAGER_SERVICE_INIT] = {.pid = getpid()},
        [LINTED_MANAGER_SERVICE_GUI] = {.pid = -1},
        [LINTED_MANAGER_SERVICE_SIMULATOR] = {.pid = -1}
    };

    {
        int logger_dummy;
        int updater_dummy;
        int shutdowner_dummy;
        int controller_dummy;

        char logger_option[] = "--logger=" INT_STRING_PADDING;
        char updater_option[] = "--updater=" INT_STRING_PADDING;
        char shutdowner_option[] = "--shutdowner=" INT_STRING_PADDING;
        char controller_option[] = "--controller=" INT_STRING_PADDING;

        /* Create dummy file descriptors to be overwritten later on */
        if ((errnum = linted_io_dummy(&logger_dummy, O_CLOEXEC)) != 0) {
            goto close_shutdowner_pair;
        }

        if ((errnum = linted_io_dummy(&updater_dummy, O_CLOEXEC)) != 0) {
            goto close_logger_dummy;
        }

        if ((errnum = linted_io_dummy(&shutdowner_dummy, O_CLOEXEC)) != 0) {
            goto close_updater_dummy;
        }

        if ((errnum = linted_io_dummy(&controller_dummy, O_CLOEXEC)) != 0) {
            goto close_shutdowner_pair_dummy;
        }

        sprintf(logger_option, "--logger=%i", logger_dummy);
        sprintf(updater_option, "--updater=%i", updater_dummy);
        sprintf(shutdowner_option, "--shutdowner=%i", shutdowner_dummy);
        sprintf(controller_option, "--controller=%i", controller_dummy);

        {
            struct linted_spawn_file_actions *file_actions;
            if ((errnum = linted_spawn_file_actions_init(&file_actions)) != 0) {
                goto kill_processes;
            }

            struct linted_spawn_attr *attr;
            if ((errnum = linted_spawn_attr_init(&attr)) != 0) {
                goto destroy_gui_file_actions;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            logger_write,
                                                            logger_dummy))
                != 0) {
                goto destroy_spawnattr;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            updater_read,
                                                            updater_dummy))
                != 0) {
                goto destroy_spawnattr;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            shutdowner_write,
                                                            shutdowner_dummy))
                != 0) {
                goto destroy_spawnattr;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            controller_write,
                                                            controller_dummy))
                != 0) {
                goto destroy_spawnattr;
            }

            {
                char *args[] = {
                    (char *)gui_config->path,
                    logger_option,
                    updater_option,
                    shutdowner_option,
                    controller_option,
                    NULL
                };

                pid_t gui_process;
                if ((errnum = linted_spawn(&gui_process,
                                           gui_config->working_directory,
                                           gui_config->path,
                                           file_actions, attr, args,
                                           (char **)gui_config->environment)) !=
                    0) {
                    goto destroy_spawnattr;
                }

                services[LINTED_MANAGER_SERVICE_GUI].pid = gui_process;
            }

 destroy_spawnattr:
            linted_spawn_attr_destroy(attr);

 destroy_gui_file_actions:
            linted_spawn_file_actions_destroy(file_actions);

            if (errnum != 0) {
                goto close_controller_dummy;
            }
        }

        {
            struct linted_spawn_file_actions *file_actions;
            if ((errnum = linted_spawn_file_actions_init(&file_actions)) != 0) {
                goto kill_processes;
            }

            struct linted_spawn_attr *attr;
            if ((errnum = linted_spawn_attr_init(&attr)) != 0) {
                goto destroy_sim_file_actions;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            logger_write,
                                                            logger_dummy))
                != 0) {
                goto destroy_spawnattr;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            updater_write,
                                                            updater_dummy))
                != 0) {
                goto destroy_sim_spawnattr;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            shutdowner_read,
                                                            shutdowner_dummy))
                != 0) {
                goto destroy_sim_spawnattr;
            }

            if ((errnum = linted_spawn_file_actions_adddup2(&file_actions,
                                                            controller_read,
                                                            controller_dummy))
                != 0) {
                goto destroy_sim_spawnattr;
            }

            {
                char *args[] = {
                    (char *)sim_config->path,
                    logger_option,
                    updater_option,
                    shutdowner_option,
                    controller_option,
                    NULL
                };
                char *envp[] = { NULL };

                pid_t process;
                if ((errnum = linted_spawn(&process,
                                           sim_config->working_directory,
                                           sim_config->path,
                                           file_actions, attr, args,
                                           envp)) != 0) {
                    goto destroy_sim_spawnattr;
                }

                services[LINTED_MANAGER_SERVICE_SIMULATOR].pid = process;
            }

 destroy_sim_spawnattr:
            linted_spawn_attr_destroy(attr);

 destroy_sim_file_actions:
            linted_spawn_file_actions_destroy(file_actions);
        }

 close_controller_dummy:
        {
            errno_t close_errnum = linted_io_close(controller_dummy);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }

 close_shutdowner_pair_dummy:
        {
            errno_t close_errnum = linted_io_close(shutdowner_dummy);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }

 close_updater_dummy:
        {
            errno_t close_errnum = linted_io_close(updater_dummy);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }

 close_logger_dummy:
        {
            errno_t close_errnum = linted_io_close(logger_dummy);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }

        if (errnum != 0) {
            goto kill_processes;
        }
    }

    linted_manager new_connections;
    if ((errnum = linted_manager_bind(&new_connections, BACKLOG, NULL, 0)) != 0) {
        goto kill_processes;
    }

    {
        char buf[LINTED_MANAGER_PATH_MAX];
        size_t len;
        if ((errnum = linted_manager_path(new_connections, buf, &len)) != 0) {
            goto close_new_connections;
        }

        linted_io_write_str(STDOUT_FILENO, NULL,
                            LINTED_STR("management socket: "));
        linted_io_write_all(STDOUT_FILENO, NULL, buf + 1, len - 1);
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));
    }

    struct waiter gui_waiter;
    if ((errnum = waiter_init(&gui_waiter,
                              services[LINTED_MANAGER_SERVICE_GUI].pid)) != 0) {
        goto close_new_connections;
    }

    struct waiter simulator_waiter;
    if ((errnum = waiter_init(&simulator_waiter,
                              services[LINTED_MANAGER_SERVICE_SIMULATOR].pid)) != 0) {
        goto destroy_gui_waiter;
    }

    {
        size_t connection_count = 0;
        struct connection connections[MAX_MANAGE_CONNECTIONS];

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
            connections[ii].fd = -1;
        }

        for (;;) {
            enum {
                GUI_WAITER,
                SIMULATOR_WAITER,
                LOGGER,
                NEW_CONNECTIONS,
                CONNECTION
            };
            /* TODO: Allocate off the stack */
            struct pollfd pollfds[CONNECTION + MAX_MANAGE_CONNECTIONS] = {
                [GUI_WAITER] = {.fd = waiter_fd(&gui_waiter),.events = POLLIN},
                [SIMULATOR_WAITER] = {.fd = waiter_fd(&simulator_waiter),.events = POLLIN},
                [LOGGER] = {.fd = logger_read,.events = POLLIN},
                [NEW_CONNECTIONS] = {.fd = new_connections,.events = POLLIN}
            };
            /* TODO: Allocate off the stack */
            size_t connection_ids[MAX_MANAGE_CONNECTIONS];

            size_t active_connections = 0;
            for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
                struct connection *connection = &connections[ii];
                if (connection->fd != -1) {
                    struct pollfd *pollfd =
                        &pollfds[CONNECTION + active_connections];
                    pollfd->fd = connection->fd;
                    pollfd->events =
                        connection->has_reply_ready ? POLLOUT : POLLIN;
                    connection_ids[active_connections] = ii;
                    ++active_connections;
                }
            }

            errno_t poll_errnum;
            size_t constantfds =
                LINTED_ARRAY_SIZE(pollfds) - MAX_MANAGE_CONNECTIONS;
            size_t pollfd_count = constantfds + active_connections;
            do {
                poll_errnum = -1 == poll(pollfds, pollfd_count, -1) ? errno : 0;
            } while (EINTR == poll_errnum);
            if (poll_errnum != 0) {
                errnum = poll_errnum;
                goto close_connections;
            }

            if ((pollfds[NEW_CONNECTIONS].revents & POLLIN) != 0) {
                if ((errnum = on_new_connections_readable(new_connections,
                                                          services,
                                                          &connection_count,
                                                          connections)) != 0) {
                    goto close_connections;
                }
            }

            if ((pollfds[LOGGER].revents & POLLIN) != 0) {
                size_t log_size;
                /* TODO: Allocate buffer off the stack */
                char entry[LINTED_LOGGER_LOG_MAX];
                if ((errnum = linted_logger_recv_log(logger_read, entry,
                                                     &log_size)) != 0) {
                    goto close_connections;
                }

                linted_io_write_string(STDERR_FILENO, NULL, process_name);
                linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR(": "));
                linted_io_write_all(STDERR_FILENO, NULL, entry, log_size);
                linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\n"));
            }

            if ((pollfds[GUI_WAITER].revents & POLLIN) != 0) {
                struct waiter_message message;

                if ((errnum = linted_io_read_all(waiter_fd(&gui_waiter), NULL,
                                                 &message,
                                                 sizeof message)) != 0) {
                    goto close_connections;
                }

                if ((errnum = message.errnum) != 0) {
                    goto close_connections;
                }

                services[LINTED_MANAGER_SERVICE_GUI].pid = -1;

                siginfo_t *exit_info = &message.exit_info;

                switch (exit_info->si_code) {
                case CLD_DUMPED:
                case CLD_KILLED:
                    raise(exit_info->si_status);
                    errnum = errno;
                    goto close_connections;

                case CLD_EXITED:
                    if (exit_info->si_status != 0) {
                        errnum = exit_info->si_status;
                        goto close_connections;
                    }
                    break;

                default:
                    assert(false);
                }

                if (-1 == services[LINTED_MANAGER_SERVICE_SIMULATOR].pid) {
                    goto close_connections;
                }
            }

            if ((pollfds[SIMULATOR_WAITER].revents & POLLIN) != 0) {
                struct waiter_message message;

                if ((errnum = linted_io_read_all(waiter_fd(&simulator_waiter),
                                                 NULL,
                                                 &message, sizeof message)) != 0) {
                    goto close_connections;
                }

                if ((errnum = message.errnum) != 0) {
                    goto close_connections;
                }

                services[LINTED_MANAGER_SERVICE_SIMULATOR].pid = -1;

                siginfo_t *exit_info = &message.exit_info;

                switch (exit_info->si_code) {
                case CLD_DUMPED:
                case CLD_KILLED:
                    raise(exit_info->si_status);
                    errnum = errno;
                    goto close_connections;

                case CLD_EXITED:
                    if (exit_info->si_status != 0) {
                        errnum = exit_info->si_status;
                        goto close_connections;
                    }
                    break;

                default:
                    assert(false);
                }

                if (-1 == services[LINTED_MANAGER_SERVICE_GUI].pid) {
                    goto close_connections;
                }
            }

            for (size_t ii = 0; ii < active_connections; ++ii) {
                size_t connection_id = connection_ids[ii];
                struct connection *connection = &connections[connection_id];

                int fd = connection->fd;

                if (connection->has_reply_ready) {
                    if ((pollfds[CONNECTION + ii].revents & POLLOUT) != 0) {
                        goto try_writing;
                    }
                } else {
                    if ((pollfds[CONNECTION + ii].revents & POLLIN) != 0) {
                        goto try_reading;
                    }
                }

                continue;

 try_reading:
                {
                    union linted_manager_reply reply;
                    bool hungup;
                    errnum = on_connection_readable(fd, services,
                                                    &hungup, &reply);
                    switch (errnum) {
                    case 0:
                        if (hungup) {
                            /* Ignore the misbehaving other end */
                            goto remove_connection;
                        }
                        break;

                    case EAGAIN:
                        /* Maybe the socket was only available for
                         * writing but not reading */
                        errnum = 0;
                        continue;

                    case EPROTO:
                        /* Ignore the misbehaving other end */
                        errnum = 0;
                        goto remove_connection;

                    default:
                        goto close_connections;
                    }

                    connection->has_reply_ready = true;
                    connection->reply = reply;
                }

 try_writing:
                errnum = on_connection_writeable(fd, &connection->reply);
                switch (errnum) {
                case 0:
                    break;

                case EAGAIN:
                    /* Maybe the socket was only available for
                     * reading but not writing */
                    errnum = 0;
                    continue;

                case EPIPE:
                case EPROTO:
                    /* Ignore the misbehaving other end */
                    errnum = 0;
                    goto remove_connection;

                default:
                    goto close_connections;
                }

 remove_connection:
                connection->fd = -1;
                --connection_count;

                {
                    errno_t close_errnum = linted_io_close(fd);
                    if (close_errnum != 0) {
                        errnum = close_errnum;
                        goto close_connections;
                    }
                }
            }
        }

 close_connections:
        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
            struct connection *const connection = &connections[ii];
            int const fd = connection->fd;
            if (fd != -1) {
                errno_t close_errnum = linted_io_close(fd);
                if (0 == errnum) {
                    errnum = close_errnum;
                }
            }
        }
    }

    {
        errno_t destroy_errnum = waiter_destroy(&simulator_waiter);
        if (0 == errnum) {
            errnum = destroy_errnum;
        }
    }

 destroy_gui_waiter:
    {
        errno_t destroy_errnum = waiter_destroy(&gui_waiter);
        if (0 == errnum) {
            errnum = destroy_errnum;
        }
    }

 close_new_connections:
    {
        errno_t close_errnum = linted_manager_close(new_connections);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

 kill_processes:
    if (errnum != 0) {
        for (size_t ii = 1; ii < LINTED_ARRAY_SIZE(services); ++ii) {
            pid_t pid = services[ii].pid;

            if (-1 == pid) {
                continue;
            }

            errno_t kill_errnum = -1 == kill(pid, SIGKILL) ? errno : 0;
            /* kill_errnum == ESRCH is fine */
            assert(kill_errnum != EINVAL);
            assert(kill_errnum != EPERM);

            services[ii].pid = -1;
        }
    }

 close_shutdowner_pair:
    {
        errno_t close_errnum = linted_shutdowner_close(shutdowner_read);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    {
        errno_t close_errnum = linted_shutdowner_close(shutdowner_write);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

 close_controller_pair:
    {
        errno_t close_errnum = linted_controller_close(controller_read);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    {
        errno_t close_errnum = linted_controller_close(controller_write);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

close_updater_pair:
    {
        errno_t close_errnum = linted_updater_close(updater_read);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    {
        errno_t close_errnum = linted_updater_close(updater_write);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

 close_logger_pair:
    {
        errno_t close_errnum = linted_logger_close(logger_read);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    {
        errno_t close_errnum = linted_updater_close(logger_write);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    return errnum;
}

static errno_t on_new_connections_readable(linted_manager new_connections,
                                           struct service const *services,
                                           size_t * connection_count,
                                           struct connection *connections)
{
    for (;;) {
        int new_socket = accept4(new_connections, NULL, NULL,
                                 SOCK_NONBLOCK | SOCK_CLOEXEC);
        if (-1 == new_socket) {
            errno_t errnum = errno;

            if (EAGAIN == errnum || EWOULDBLOCK == errnum) {
                break;
            }

            return errnum;
        }

        errno_t error_status = 0;

        union linted_manager_reply reply;
        {
            bool hungup;
            errno_t errnum = on_connection_readable(new_socket, services,
                                                    &hungup,
                                                    &reply);
            switch (errnum) {
            case 0:
                if (hungup) {
                    /* Ignore the misbehaving other end */
                    continue;
                }
                break;

            case EAGAIN:
                goto queue_socket;

            case EPIPE:
            case EPROTO:
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

            case EPROTO:
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
        if (*connection_count >= MAX_MANAGE_CONNECTIONS) {
            /* I'm sorry sir but we are full today. */
            goto close_new_socket;
        }

        struct connection *connection;

        for (size_t ii = 0; ii < MAX_MANAGE_CONNECTIONS; ++ii) {
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
                                      struct service const *services,
                                      bool * hungup,
                                      union linted_manager_reply *reply)
{
    union linted_manager_request request;

    {
        size_t bytes_read;
        errno_t errnum = linted_manager_recv_request(fd, &request, &bytes_read);
        if (errnum != 0) {
            return errnum;
        }

        if (0 == bytes_read) {
            /* Hangup */
            *hungup = true;
            return 0;
        }
    }

    memset(reply, 0, sizeof *reply);

    switch (request.type) {
        {
    case LINTED_MANAGER_STATUS:;
            struct service const *service = &services[request.status.service];
            errno_t errnum = -1 == kill(service->pid, 0) ? errno : 0;
            assert(errnum != EINVAL);
            switch (errnum) {
            case 0:
                reply->status.is_up = true;
                break;

            case ESRCH:
                reply->status.is_up = false;
                break;

            default:
                return errnum;
            }
            break;
        }

        {
    case LINTED_MANAGER_STOP:;
            struct service const *service = &services[request.stop.service];
            errno_t errnum = -1 == kill(service->pid, SIGKILL) ? errno : 0;
            assert(errnum != EINVAL);
            switch (errnum) {
            case 0:
                reply->stop.was_up = true;
                break;

            case ESRCH:
                reply->stop.was_up = false;
                break;

            default:
                return errnum;
            }
            break;
        }

    default:
        /* Sent malformed input */
        return EPROTO;
    }

    *hungup = false;
    return 0;
}

static errno_t on_connection_writeable(int fd,
                                       union linted_manager_reply *reply)
{
    return linted_manager_send_reply(fd, reply);
}

struct waiter_data {
    pid_t process;
    int fd;
};


static void *waiter_routine(void *data)
{
    struct waiter_data *waiter_data = data;

    siginfo_t exit_info;
    memset(&exit_info, 0, sizeof exit_info);

    errno_t errnum;
    do {
        int wait_status = waitid(P_PID, waiter_data->process,
                                 &exit_info, WEXITED);
        errnum = -1 == wait_status ? errno : 0;
        assert(errnum != EINVAL);
    } while (EINTR == errnum);

    {
        struct waiter_message message;
        memset(&message, 0, sizeof message);

        message.exit_info = exit_info;
        message.errnum = errnum;

        linted_io_write_all(waiter_data->fd, NULL,
                            &message, sizeof message);
        /* TODO: Handle the error */
    }

    /* Loop forever until cancelled */
    char dummy;
    linted_io_read_all(waiter_data->fd, NULL, &dummy, sizeof dummy);

    /* TODO: Handle the error */

    return NULL;
}

static errno_t waiter_init(struct waiter * waiter, pid_t pid)
{
    errno_t errnum;

    int init_wait_fd;
    int waiter_wait_fd;
    {
        int waiter_fds[2];
        if (-1 == socketpair(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0,
                             waiter_fds)) {
            return errno;
        }
        init_wait_fd = waiter_fds[0];
        waiter_wait_fd = waiter_fds[1];
    }

    struct waiter_data * waiter_data = malloc(sizeof *waiter_data);
    if (NULL == waiter_data) {
        errnum = errno;
        goto close_fds;
    }

    waiter_data->fd = waiter_wait_fd;
    waiter_data->process = pid;

    if (-1 == pthread_create(&waiter->pthread, NULL,
                             waiter_routine, waiter_data)) {
        errnum = errno;
        goto free_waiter;
    }

    waiter->waiter_data = waiter_data;
    waiter->init_wait_fd = init_wait_fd;
    waiter->waiter_wait_fd = waiter_wait_fd;

    return 0;

free_waiter:;
    free(waiter_data);

close_fds:;
    linted_io_close(init_wait_fd);
    linted_io_close(waiter_wait_fd);

    return errnum;
}

static int waiter_fd(struct waiter const * waiter)
{
    return waiter->init_wait_fd;
}

static errno_t waiter_destroy(struct waiter const * waiter)
{
    errno_t errnum = 0;

    errnum = pthread_cancel(waiter->pthread);
    assert(errnum != ESRCH);

    errnum = pthread_join(waiter->pthread, NULL);
    assert(errnum != ESRCH);

    free(waiter->waiter_data);

    {
        errno_t close_errnum = linted_io_close(waiter->init_wait_fd);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    {
        errno_t close_errnum = linted_io_close(waiter->waiter_wait_fd);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }

    return errnum;
}

static errno_t linted_help(int fildes, char const *program_name,
                           struct linted_str package_name,
                           struct linted_str package_url,
                           struct linted_str package_bugreport)
{
    errno_t errnum;

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum =
         linted_io_write_str(fildes, NULL, LINTED_STR(" [OPTIONS]\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Play the game.\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
  --simulator         the location of the simulator executable\n\
  --gui               the location of the gui executable\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
Report bugs to <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_bugreport)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL, package_name)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("\
 home page: <"))) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, package_url)) != 0) {
        return errnum;
    }
    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR(">\n"))) != 0) {
        return errnum;
    }

    return 0;
}
