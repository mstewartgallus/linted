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

#include "linted/asynch.h"
#include "linted/controller.h"
#include "linted/db.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/logger.h"
#include "linted/manager.h"
#include "linted/mq.h"
#include "linted/shutdowner.h"
#include "linted/start.h"
#include "linted/spawn.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/types.h>

#define BACKLOG 20

#define MAX_MANAGE_CONNECTIONS 10

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define SIMULATOR_OPTION "--simulator"
#define GUI_OPTION "--gui"

#define STR(X) #X
#define XSTR(X) STR(X)

enum service_type {
    SERVICE_INIT,
    SERVICE_PROCESS,
    SERVICE_FILE_PAIR
};

enum service_end {
    READ,
    WRITE
};

struct dup_pair
{
    enum service_end service_end;
    enum linted_manager_service service;
    int newfildes;
};

struct dup_pairs
{
    size_t size;
    struct dup_pair const *dup_pairs;
};

#define DUP_PAIRS(...)                                                         \
    ((struct dup_pairs const) { .size = LINTED_ARRAY_SIZE(__VA_ARGS__),        \
                                .dup_pairs = __VA_ARGS__ })

struct service_config_process
{
    enum service_type type;
    char const *path;
    char const *const *arguments;
    char const *const *environment;
    struct dup_pairs dup_pairs;
    linted_ko working_directory;
};

struct service_config_file_pair
{
    enum service_type type;
    linted_error (*generator)(int fildes[2]);
};

union service_config
{
    enum service_type type;
    struct service_config_process process;
    struct service_config_file_pair file_pair;
};

struct service_init
{
    pid_t pid;
};

struct service_process
{
    pid_t pid;
};

struct service_file_pair
{
    linted_ko read_end;
    linted_ko write_end;
};

union service
{
    struct service_init init;
    struct service_process process;
    struct service_file_pair file_pair;
};

struct connection
{
    struct linted_manager_task_recv_request recv_request_task;
    struct linted_manager_task_send_reply send_reply_task;
    linted_ko fd;
    bool has_reply_ready;
};

static linted_error updater_pair(int fildes[2])
{
    return linted_updater_pair(fildes, O_NONBLOCK, O_NONBLOCK);
}

static linted_error controller_pair(int fildes[2])
{
    return linted_controller_pair(fildes, O_NONBLOCK, O_NONBLOCK);
}

static linted_error shutdowner_pair(int fildes[2])
{
    return linted_shutdowner_pair(fildes, O_NONBLOCK, 0);
}

static linted_error on_new_connection(linted_manager new_socket,
                                      struct linted_asynch_pool *pool,
                                      union service_config const *config,
                                      union service const *services,
                                      size_t *connection_count,
                                      struct connection *connections);

static linted_error on_connection_recv_request(
    struct linted_asynch_pool *pool, struct connection *connection,
    size_t *connection_count, struct connection *connections,
    union service_config const *config, union service const *services);
static linted_error remove_connection(struct connection *connection,
                                      size_t *connection_count);

static linted_error run_game(char const *process_name,
                             union service_config const *config,
                             int logger_dummy, int updater_dummy,
                             int shutdowner_dummy, int controller_dummy);

static linted_error linted_help(int fildes, char const *program_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);

uint_fast8_t linted_start(int cwd, char const *const program_name, size_t argc,
                          char const *const argv[const])
{
    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    char const *simulator_path = PKGLIBEXECDIR "/simulator" EXEEXT;
    char const *gui_path = PKGLIBEXECDIR "/gui" EXEEXT;

    for (size_t ii = 1; ii < argc; ++ii) {
        char const *argument = argv[ii];

        if (0 == strcmp(argument, HELP_OPTION)) {
            need_help = true;
        } else if (0 == strcmp(argument, VERSION_OPTION)) {
            need_version = true;

        } else if (0 == strncmp(argument, SIMULATOR_OPTION "=",
                                strlen(SIMULATOR_OPTION "="))) {

            simulator_path = argument + strlen(SIMULATOR_OPTION "=");

        } else if (0 ==
                   strncmp(argument, GUI_OPTION "=", strlen(GUI_OPTION "="))) {

            gui_path = argument + strlen(GUI_OPTION "=");

        } else {
            bad_option = argument;
        }
    }

    if (need_help) {
        linted_help(STDOUT_FILENO, program_name, LINTED_STR(PACKAGE_NAME),
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

    linted_error errnum;

    linted_db my_db;
    {
        /**
         * @todo Place the database according to the XDG base
         *       directory specification.
         */
        int xx;
        if ((errnum = linted_db_open(&xx, cwd, "linted-db", LINTED_DB_CREAT)) !=
            0) {
            errno = errnum;
            perror("linted_db_open");
            return EXIT_FAILURE;
        }
        my_db = xx;
    }

    {
        int tmp;
        {
            int xx;
            if ((errnum = linted_db_temp_file(&my_db, &xx)) != 0) {
                errno = errnum;
                perror("linted_db_temp_file");
                return EXIT_FAILURE;
            }
            tmp = xx;
        }

        {
            static char const hello[] = "Hello anybody!";
            char const *data = hello;
            size_t data_size = sizeof hello - 1;

            if ((errnum = linted_io_write_all(tmp, NULL, data, data_size)) !=
                0) {
                perror("linted_io_write");
                return EXIT_FAILURE;
            }
        }

        if ((errnum = linted_db_temp_send(&my_db, "hello", tmp)) != 0) {
            perror("linted_db_send");
            return EXIT_FAILURE;
        }

        if ((errnum = linted_ko_close(tmp)) != 0) {
            perror("linted_ko_close");
            return EXIT_FAILURE;
        }
    }

    if ((errnum = linted_db_close(&my_db)) != 0) {
        errno = errnum;
        perror("linted_db_close");
        return EXIT_FAILURE;
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

    {
        int kept_fds[] = { STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO, cwd };

        errnum = linted_util_sanitize_environment(kept_fds,
                                                  LINTED_ARRAY_SIZE(kept_fds));
        if (errnum != 0) {
            linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not sanitize the environment: %s\n",
                                   program_name,
                                   linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
    }

    /* Sandbox */
    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        assert(errno != EINVAL);

        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: can not drop ability to raise privileges: %s\n",
                               program_name, linted_error_string_alloc(errno));
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

    linted_error game_status;
    {
        int logger_dummy;
        int updater_dummy;
        int shutdowner_dummy;
        int controller_dummy;

        if ((game_status = linted_ko_dummy(&logger_dummy)) != 0) {
            goto done;
        }

        if ((game_status = linted_ko_dummy(&updater_dummy)) != 0) {
            goto done;
        }

        if ((game_status = linted_ko_dummy(&shutdowner_dummy)) != 0) {
            goto done;
        }

        if ((game_status = linted_ko_dummy(&controller_dummy)) != 0) {
            goto done;
        }

        char logger_option[] = "--logger=XXXXXXXXXXX";
        sprintf(logger_option, "--logger=%i", logger_dummy);

        char updater_option[] = "--updater=XXXXXXXXXXX";
        sprintf(updater_option, "--updater=%i", updater_dummy);

        char shutdowner_option[] = "--shutdowner=XXXXXXXXXXX";
        sprintf(shutdowner_option, "--shutdowner=%i", shutdowner_dummy);

        char controller_option[] = "--controller=XXXXXXXXXXX";
        sprintf(controller_option, "--controller=%i", controller_dummy);

        union service_config const configuration[] =
            {[LINTED_MANAGER_SERVICE_INIT] = { .type = SERVICE_INIT },
             [LINTED_MANAGER_SERVICE_SIMULATOR] = { .process = {
                                                        .type = SERVICE_PROCESS,
                                                        .working_directory =
                                                            cwd,
                                                        .path = simulator_path,
                                                        .arguments = (char const *
                                                                      const[]) {
                                                            simulator_path,
                                                            logger_option,
                                                            updater_option,
                                                            shutdowner_option,
                                                            controller_option,
                                                            NULL
                                                        },
                                                        .environment =
                                                            (char const *
                                                             const[]) { NULL },
                                                        .dup_pairs = DUP_PAIRS((
                                                            struct
                                                            dup_pair const[]) {
                                                            { WRITE,
                                                              LINTED_MANAGER_SERVICE_LOGGER,
                                                              logger_dummy },
                                                            { WRITE,
                                                              LINTED_MANAGER_SERVICE_UPDATER,
                                                              updater_dummy },
                                                            { READ,
                                                              LINTED_MANAGER_SERVICE_SHUTDOWNER,
                                                              shutdowner_dummy },
                                                            { READ,
                                                              LINTED_MANAGER_SERVICE_CONTROLLER,
                                                              controller_dummy }
                                                        })
                                                    } },
             [LINTED_MANAGER_SERVICE_GUI] = { .process = {
                                                  .type = SERVICE_PROCESS,
                                                  .working_directory = cwd,
                                                  .path = gui_path,
                                                  .arguments =
                                                      (char const * const[]) {
                                                          gui_path,
                                                          logger_option,
                                                          updater_option,
                                                          shutdowner_option,
                                                          controller_option,
                                                          NULL
                                                      },
                                                  .environment =
                                                      (char const * const[]) {
                                                          display, NULL
                                                      },
                                                  .dup_pairs = DUP_PAIRS((
                                                      struct dup_pair const[]) {
                                                      { WRITE,
                                                        LINTED_MANAGER_SERVICE_LOGGER,
                                                        logger_dummy },
                                                      { READ,
                                                        LINTED_MANAGER_SERVICE_UPDATER,
                                                        updater_dummy },
                                                      { WRITE,
                                                        LINTED_MANAGER_SERVICE_SHUTDOWNER,
                                                        shutdowner_dummy },
                                                      { WRITE,
                                                        LINTED_MANAGER_SERVICE_CONTROLLER,
                                                        controller_dummy }
                                                  })
                                              } },
             [LINTED_MANAGER_SERVICE_LOGGER] = { .file_pair = {
                                                     .type = SERVICE_FILE_PAIR,
                                                     .generator =
                                                         linted_logger_pair
                                                 } },
             [LINTED_MANAGER_SERVICE_UPDATER] = { .file_pair = {
                                                      .type = SERVICE_FILE_PAIR,
                                                      .generator = updater_pair
                                                  } },
             [LINTED_MANAGER_SERVICE_CONTROLLER] = { .file_pair = {
                                                         .type =
                                                             SERVICE_FILE_PAIR,
                                                         .generator =
                                                             controller_pair
                                                     } },
             [LINTED_MANAGER_SERVICE_SHUTDOWNER] = { .file_pair = {
                                                         .type =
                                                             SERVICE_FILE_PAIR,
                                                         .generator =
                                                             shutdowner_pair
                                                     } } };
        game_status =
            run_game(program_name, configuration, logger_dummy, updater_dummy,
                     shutdowner_dummy, controller_dummy);
    }
done:
    ;
    if (game_status != 0) {
        succesfully_executing = -1;
        char const *error_string = linted_error_string_alloc(game_status);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "could not run the game: %s\n", error_string);
        linted_error_string_free(error_string);
    }

    if ((errnum = linted_ko_close(STDERR_FILENO)) != 0) {
        succesfully_executing = -1;
        char const *const error_string = linted_error_string_alloc(errnum);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "could not close standard error: %s\n",
                               error_string);
        linted_error_string_free(error_string);
    }

    return (-1 == succesfully_executing) ? EXIT_FAILURE : EXIT_SUCCESS;
}

enum {
    GUI_WAITER,
    SIMULATOR_WAITER,
    LOGGER,
    NEW_CONNECTIONS,
    CONNECTION
};

static linted_error run_game(char const *process_name,
                             union service_config const *config,
                             int logger_dummy, int updater_dummy,
                             int shutdowner_dummy, int controller_dummy)
{
    linted_error errnum = 0;

    enum {
        MAX_TASKS = CONNECTION + MAX_MANAGE_CONNECTIONS
    };

    struct linted_asynch_pool *pool;
    {
        struct linted_asynch_pool *xx;
        if ((errnum = linted_asynch_pool_create(&xx, MAX_TASKS)) != 0) {
            return errnum;
        }
        pool = xx;
    }

    union service services[] =
        {[LINTED_MANAGER_SERVICE_INIT] = { .init = { .pid = getpid() } },
         [LINTED_MANAGER_SERVICE_GUI] = { .process = { .pid = -1 } },
         [LINTED_MANAGER_SERVICE_SIMULATOR] = { .process = { .pid = -1 } },
         [LINTED_MANAGER_SERVICE_LOGGER] = { .file_pair = { .read_end = -1,
                                                            .write_end = -1 } },
         [LINTED_MANAGER_SERVICE_UPDATER] = { .file_pair = { .read_end = -1,
                                                             .write_end =
                                                                 -1 } },
         [LINTED_MANAGER_SERVICE_CONTROLLER] = { .file_pair = { .read_end = -1,
                                                                .write_end =
                                                                    -1 } },
         [LINTED_MANAGER_SERVICE_SHUTDOWNER] = { .file_pair = { .read_end = -1,
                                                                .write_end =
                                                                    -1 } } };

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        union service_config const *service_config = &config[ii];
        if (service_config->type != SERVICE_FILE_PAIR) {
            continue;
        }

        union service *service = &services[ii];

        int fildes[2];
        if ((errnum = service_config->file_pair.generator(fildes)) != 0) {
            goto exit_services;
        }

        service->file_pair.read_end = fildes[0];
        service->file_pair.write_end = fildes[1];
    }

    linted_logger logger_read =
        services[LINTED_MANAGER_SERVICE_LOGGER].file_pair.read_end;

    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        if (config[ii].type != SERVICE_PROCESS) {
            continue;
        }

        struct service_process *service = &services[ii].process;
        struct service_config_process const *proc_config = &config[ii].process;
        struct linted_spawn_file_actions *file_actions;

        if ((errnum = linted_spawn_file_actions_init(&file_actions)) != 0) {
            goto exit_services;
        }

        size_t dup_pairs_size = proc_config->dup_pairs.size;
        for (size_t jj = 0; jj < dup_pairs_size; ++jj) {
            struct dup_pair const *dup_pair =
                &proc_config->dup_pairs.dup_pairs[jj];

            struct service_file_pair const *file_pair =
                &services[dup_pair->service].file_pair;

            int oldfildes;
            switch (dup_pair->service_end) {
            case READ:
                oldfildes = file_pair->read_end;
                break;

            case WRITE:
                oldfildes = file_pair->write_end;
                break;

            default:
                assert(false);
            }

            if ((errnum = linted_spawn_file_actions_adddup2(
                     &file_actions, oldfildes, dup_pair->newfildes)) != 0) {
                goto destroy_file_actions;
            }
        }

        {
            pid_t process;
            if ((errnum = linted_spawn(&process, proc_config->working_directory,
                                       proc_config->path, file_actions, NULL,
                                       (char **)proc_config->arguments,
                                       (char **)proc_config->environment)) !=
                0) {
                goto destroy_file_actions;
            }

            service->pid = process;
        }

    destroy_file_actions:
        linted_spawn_file_actions_destroy(file_actions);

        if (errnum != 0) {
            goto exit_services;
        }
    }

    linted_manager new_connections;
    if ((errnum = linted_manager_bind(&new_connections, BACKLOG, NULL, 0)) !=
        0) {
        goto exit_services;
    }

    {
        char buf[LINTED_MANAGER_PATH_MAX];
        size_t len;
        if ((errnum = linted_manager_path(new_connections, buf, &len)) != 0) {
            goto close_new_connections;
        }

        linted_io_write_str(STDOUT_FILENO, NULL,
                            LINTED_STR("management socket: "));
        linted_io_write_all(STDOUT_FILENO, NULL, buf, len);
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));
    }

    char *logger_buffer = malloc(LINTED_LOGGER_LOG_MAX);
    if (NULL == logger_buffer) {
        goto close_connections;
    }

    {
        size_t connection_count = 0;
        struct connection connections[MAX_MANAGE_CONNECTIONS];

        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
            connections[ii].fd = -1;
        }

        struct service_process *gui_service =
            &services[LINTED_MANAGER_SERVICE_GUI].process;
        struct service_process *sim_service =
            &services[LINTED_MANAGER_SERVICE_SIMULATOR].process;

        struct linted_asynch_task_waitid gui_waiter_task;
        struct linted_asynch_task_waitid sim_waiter_task;
        struct linted_logger_task logger_task;
        struct linted_manager_task_accept new_connections_accept_task;

        if ((errnum = linted_asynch_pool_bind_ko(pool, logger_read)) != 0) {
            goto close_connections;
        }

        if ((errnum = linted_asynch_pool_bind_ko(pool, new_connections)) != 0) {
            goto close_connections;
        }

        linted_asynch_waitid(&gui_waiter_task, GUI_WAITER, P_PID,
                             gui_service->pid, WEXITED);

        linted_asynch_waitid(&sim_waiter_task, SIMULATOR_WAITER, P_PID,
                             sim_service->pid, WEXITED);

        linted_logger_receive(&logger_task, LOGGER, logger_read, logger_buffer);

        linted_manager_accept(&new_connections_accept_task, NEW_CONNECTIONS,
                              new_connections);

        linted_asynch_pool_submit(pool, LINTED_UPCAST(&gui_waiter_task));
        linted_asynch_pool_submit(pool, LINTED_UPCAST(&sim_waiter_task));
        linted_asynch_pool_submit(pool,
                                  LINTED_UPCAST(LINTED_UPCAST(&logger_task)));
        linted_asynch_pool_submit(
            pool, LINTED_UPCAST(LINTED_UPCAST(&new_connections_accept_task)));

        for (;;) {
            struct linted_asynch_task *completed_tasks[20];
            size_t task_count;
            linted_asynch_pool_wait(pool, completed_tasks,
                                    LINTED_ARRAY_SIZE(completed_tasks),
                                    &task_count);

            for (size_t ii = 0; ii < task_count; ++ii) {
                struct linted_asynch_task *completed_task = completed_tasks[ii];
                switch (completed_task->task_action) {
                case NEW_CONNECTIONS: {
                    if ((errnum = completed_task->errnum) != 0) {
                        goto close_connections;
                    }

                    struct linted_asynch_task_accept *task_accept =
                        LINTED_DOWNCAST(struct linted_asynch_task_accept,
                                        completed_task);
                    linted_manager returned_ko = task_accept->returned_ko;
                    linted_asynch_pool_submit(pool, completed_task);

                    if ((errnum = on_new_connection(returned_ko, pool, config,
                                                    services, &connection_count,
                                                    connections)) != 0) {
                        goto close_connections;
                    }
                    break;
                }

                case LOGGER: {
                    if ((errnum = completed_task->errnum) != 0) {
                        goto close_connections;
                    }

                    size_t log_size = LINTED_UPCAST(&logger_task)->bytes_read;

                    linted_io_write_string(STDERR_FILENO, NULL, process_name);
                    linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR(": "));
                    linted_io_write_all(STDERR_FILENO, NULL, logger_buffer,
                                        log_size);
                    linted_io_write_str(STDERR_FILENO, NULL, LINTED_STR("\n"));

                    linted_asynch_pool_submit(pool, completed_task);
                    break;
                }

                case GUI_WAITER: {
                    if ((errnum = completed_task->errnum) != 0) {
                        goto close_connections;
                    }

                    gui_service->pid = -1;

                    siginfo_t *exit_info = &gui_waiter_task.info;

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

                    if (-1 == sim_service->pid) {
                        goto close_connections;
                    }
                    break;
                }

                case SIMULATOR_WAITER: {
                    if ((errnum = completed_task->errnum) != 0) {
                        goto close_connections;
                    }

                    sim_service->pid = -1;

                    siginfo_t *exit_info = &sim_waiter_task.info;

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

                    if (-1 == gui_service->pid) {
                        goto close_connections;
                    }
                    break;
                }

                default: {
                    assert(CONNECTION <= completed_task->task_action);

                    size_t jj = completed_task->task_action - CONNECTION;
                    struct connection *connection = &connections[jj];

                    if ((errnum = completed_task->errnum) != 0) {
                        /* The other end did something bad */
                        if ((errnum = remove_connection(
                                 connection, &connection_count)) != 0) {
                            goto close_connections;
                        }
                    }

                    if (!connection->has_reply_ready) {
                        if ((errnum = on_connection_recv_request(
                                 pool, connection, &connection_count,
                                 connections, config, services)) != 0) {
                            goto close_connections;
                        }
                    } else {
                        if ((errnum = remove_connection(
                                 connection, &connection_count)) != 0) {
                            goto close_connections;
                        }
                    }
                    break;
                }
                }
            }
        }

    close_connections:
        for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(connections); ++ii) {
            struct connection *const connection = &connections[ii];
            int const fd = connection->fd;
            if (fd != -1) {
                linted_error close_errnum = linted_ko_close(fd);
                if (0 == errnum) {
                    errnum = close_errnum;
                }
            }
        }
    }

    free(logger_buffer);

close_new_connections : {
    linted_error close_errnum = linted_manager_close(new_connections);
    if (0 == errnum) {
        errnum = close_errnum;
    }
}

exit_services:
    for (size_t ii = 0; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        union service_config const *service_config = &config[ii];

        switch (service_config->type) {
        case SERVICE_PROCESS: {
            struct service_process *service = &services[ii].process;
            pid_t pid = service->pid;

            if (pid != -1) {
                linted_error kill_errnum = -1 == kill(pid, SIGKILL) ? errno : 0;
                /* kill_errnum == ESRCH is fine */
                assert(kill_errnum != EINVAL);
                assert(kill_errnum != EPERM);

                service->pid = -1;
            }
            break;
        }

        case SERVICE_FILE_PAIR: {
            struct service_file_pair *file_pair = &services[ii].file_pair;
            int read_end = file_pair->read_end;
            int write_end = file_pair->write_end;
            if (read_end != -1) {
                linted_error close_errnum = linted_ko_close(read_end);
                if (0 == errnum) {
                    errnum = close_errnum;
                }
            }
            if (write_end != -1) {
                linted_error close_errnum = linted_ko_close(write_end);
                if (0 == errnum) {
                    errnum = close_errnum;
                }
            }
            break;
        }
        default:
            break;
        }
    }

    {
        linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
        if (0 == errnum) {
            errnum = destroy_errnum;
        }
    }

    return errnum;
}

static linted_error on_new_connection(linted_manager new_socket,
                                      struct linted_asynch_pool *pool,
                                      union service_config const *config,
                                      union service const *services,
                                      size_t *connection_count,
                                      struct connection *connections)
{
    linted_error errnum = 0;

    if (*connection_count >= MAX_MANAGE_CONNECTIONS) {
        /* I'm sorry sir but we are full today. */
        goto close_new_socket;
    }

    if ((errnum = linted_asynch_pool_bind_ko(pool, new_socket)) != 0) {
        goto close_new_socket;
    }

    struct connection *connection;

    size_t ii = 0;
    for (; ii < MAX_MANAGE_CONNECTIONS; ++ii) {
        connection = &connections[ii];
        if (-1 == connection->fd) {
            goto got_space;
        }
    }
    /* Impossible, listen has limited this */
    assert(false);
got_space:
    connection->fd = new_socket;
    connection->has_reply_ready = false;

    linted_manager_recv_request(&connection->recv_request_task, CONNECTION + ii,
                                new_socket);
    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(&connection->recv_request_task)));

    ++*connection_count;
    return 0;

close_new_socket : {
    linted_error close_errnum = linted_ko_close(new_socket);
    if (0 == errnum) {
        errnum = close_errnum;
    }
}
    return errnum;
}

static linted_error on_connection_recv_request(
    struct linted_asynch_pool *pool, struct connection *connection,
    size_t *connection_count, struct connection *connections,
    union service_config const *config, union service const *services)
{
    linted_error errnum;

    struct linted_manager_task_recv_request *task_recv =
        &connection->recv_request_task;

    int fd = connection->fd;

    union linted_manager_request *request = &task_recv->request;
    union linted_manager_reply reply;

    switch (request->type) {
    case LINTED_MANAGER_STATUS: {
        union service const *service = &services[request->status.service];

        switch (config[request->status.service].type) {
            {
                pid_t pid;

                if (false) {
                case SERVICE_INIT:
                    pid = service->init.pid;
                } else {
                case SERVICE_PROCESS:
                    pid = service->process.pid;
                }

                errnum = -1 == kill(pid, 0) ? errno : 0;
                assert(errnum != EINVAL);
                switch (errnum) {
                case 0:
                    reply.status.is_up = true;
                    break;

                case ESRCH:
                    reply.status.is_up = false;
                    break;

                default:
                    goto remove_connection;
                }
                break;
            }

        default:
            break;
        }
        break;
    }

    case LINTED_MANAGER_STOP: {
        union service const *service = &services[request->stop.service];

        switch (config[request->status.service].type) {
            {
                pid_t pid;

                if (false) {
                case SERVICE_INIT:
                    pid = service->init.pid;
                } else {
                case SERVICE_PROCESS:
                    pid = service->process.pid;
                }

                errnum = -1 == kill(pid, SIGKILL) ? errno : 0;
                assert(errnum != EINVAL);
                switch (errnum) {
                case 0:
                    reply.stop.was_up = true;
                    break;

                case ESRCH:
                    reply.stop.was_up = false;
                    break;

                default:
                    goto remove_connection;
                }
            }

        default:
            break;
        }
        break;
    }
    }
    connection->has_reply_ready = true;

    size_t ii = connections - connection;
    linted_manager_send_reply(&connection->send_reply_task, CONNECTION + ii, fd,
                              &reply);
    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(&connection->send_reply_task)));

    return 0;

remove_connection:
    remove_connection(connection, connection_count);
    return errnum;
}

static linted_error remove_connection(struct connection *connection,
                                      size_t *connection_count)
{
    int fd = connection->fd;

    connection->fd = -1;
    --*connection_count;

    return linted_ko_close(fd);
}

static linted_error linted_help(int fildes, char const *program_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport)
{
    linted_error errnum;

    if ((errnum = linted_io_write_str(fildes, NULL, LINTED_STR("Usage: "))) !=
        0) {
        return errnum;
    }

    if ((errnum = linted_io_write_string(fildes, NULL, program_name)) != 0) {
        return errnum;
    }

    if ((errnum = linted_io_write_str(fildes, NULL,
                                      LINTED_STR(" [OPTIONS]\n"))) != 0) {
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
