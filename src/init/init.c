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
#define _POSIX_C_SOURCE 200809L

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
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/spawn.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/types.h>

#ifndef PR_SET_CHILD_SUBREAPER
#define PR_SET_CHILD_SUBREAPER 36
#endif

#define BACKLOG 20u

#define MAX_MANAGE_CONNECTIONS 10u

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define SIMULATOR_OPTION "--simulator"
#define GUI_OPTION "--gui"

enum {
    WAITER,
    LOGGER,
    NEW_CONNECTIONS,
    CONNECTION
};

enum service_type {
    SERVICE_INIT,
    SERVICE_PROCESS,
    SERVICE_FILE
};

struct dup_pair
{
    int flags;
    enum linted_service service;
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
    linted_ko dirko;
};

struct service_config_file
{
    enum service_type type;
    linted_error (*generator)(linted_ko *kop);
};

union service_config
{
    enum service_type type;
    struct service_config_process process;
    struct service_config_file file;
};

struct service_init
{
    pid_t pid;
};

struct service_process
{
    pid_t pid;
};

struct service_file
{
    linted_ko ko;
    bool is_open : 1;
};

union service
{
    struct service_init init;
    struct service_process process;
    struct service_file file;
};

struct connection
{
    struct linted_manager_task_recv_request recv_request_task;
    struct linted_manager_task_send_reply send_reply_task;
    linted_ko ko;
    bool has_reply_ready;
};

struct connection_pool
{
    struct connection connections[MAX_MANAGE_CONNECTIONS];
    size_t count;
};

static linted_error find_stdin(linted_ko *kop);
static linted_error find_stdout(linted_ko *kop);
static linted_error find_stderr(linted_ko *kop);

static linted_error updater_create(linted_ko *kop);
static linted_error controller_create(linted_ko *kop);

static linted_error on_new_connection(linted_manager new_socket,
                                      struct linted_asynch_pool *pool,
                                      union service_config const *config,
                                      union service const *services,
                                      struct connection_pool *connection_pool);

static linted_error on_connection_recv_request(
    struct linted_asynch_pool *pool, struct connection *connection,
    struct connection_pool *connection_pool, union service_config const *config,
    union service const *services);

static linted_error check_db(linted_ko cwd);

static linted_error connection_pool_create(struct connection_pool **poolp);
static linted_error connection_pool_destroy(struct connection_pool *pool);
static linted_error connection_remove(struct connection *connection,
                                      struct connection_pool *connection_pool);

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);

linted_ko kos[3u];

struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-init",
    .open_current_working_directory = true,
    .kos_size = LINTED_ARRAY_SIZE(kos),
    .kos = kos
};

uint_fast8_t linted_start(int cwd, char const *const process_name, size_t argc,
                          char const *const argv[const])
{
    linted_ko stdout = kos[1u];
    linted_ko stderr = kos[2u];

    bool need_help = false;
    bool need_version = false;

    char const *bad_option = NULL;

    char const *simulator_path = PKGLIBEXECDIR "/simulator" EXEEXT;
    char const *gui_path = PKGLIBEXECDIR "/gui" EXEEXT;

    for (size_t ii = 1u; ii < argc; ++ii) {
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
        linted_help(stdout, process_name, LINTED_STR(PACKAGE_NAME),
                    LINTED_STR(PACKAGE_URL), LINTED_STR(PACKAGE_BUGREPORT));
        return EXIT_SUCCESS;
    }

    if (bad_option != NULL) {
        linted_locale_on_bad_option(stderr, process_name, bad_option);
        linted_locale_try_for_more_help(stderr, process_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    if (need_version) {
        linted_locale_version(stdout, LINTED_STR(PACKAGE_STRING),
                              LINTED_STR(COPYRIGHT_YEAR));
        return EXIT_SUCCESS;
    }

    linted_error errnum;

    if ((errnum = check_db(cwd)) != 0) {
        linted_io_write_format(stderr, NULL, "\
%s: database: %s\n",
                               process_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_format(stderr, NULL,
                               "%s: missing DISPLAY environment variable\n",
                               process_name);
        linted_locale_try_for_more_help(stderr, process_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    size_t display_value_length = strlen(original_display);
    size_t display_string_length =
        strlen("DISPLAY=") + display_value_length + 1u;
    char *display = linted_mem_alloc(&errnum, display_string_length);
    if (errnum != 0) {
        linted_io_write_format(stderr, NULL,
                               "%s: can't allocate DISPLAY string: %s\n",
                               process_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }
    memcpy(display, "DISPLAY=", strlen("DISPLAY="));
    memcpy(display + strlen("DISPLAY="), original_display,
           display_value_length);
    display[display_string_length - 1u] = '\0';

    errnum = linted_util_sanitize_environment();
    if (errnum != 0) {
        linted_io_write_format(stderr, NULL, "\
%s: can not sanitize the environment: %s\n",
                               process_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    if (-1 == prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0)) {
        errnum = errno;
        assert(errnum != 0);
        assert(errnum != EINVAL);

        linted_io_write_format(stderr, NULL, "\
%s: can not set child subreaper: %s\n",
                               process_name, linted_error_string_alloc(errnum));
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

    union service_config const config[] =
        {[LINTED_SERVICE_INIT] = { .type = SERVICE_INIT },
         [LINTED_SERVICE_SIMULATOR] = { .process = {
                                            .type = SERVICE_PROCESS,
                                            .dirko = cwd,
                                            .path = simulator_path,
                                            .arguments =
                                                (char const * const[]) {
                                                    simulator_path, NULL
                                                },
                                            .environment =
                                                (char const * const[]) { NULL },
                                            .dup_pairs = DUP_PAIRS((
                                                struct dup_pair const[]) {
                                                { LINTED_KO_RDONLY,
                                                  LINTED_SERVICE_STDIN },
                                                { LINTED_KO_WRONLY,
                                                  LINTED_SERVICE_STDOUT },
                                                { LINTED_KO_WRONLY,
                                                  LINTED_SERVICE_STDERR },
                                                { LINTED_KO_WRONLY,
                                                  LINTED_SERVICE_LOGGER },
                                                { LINTED_KO_RDONLY,
                                                  LINTED_SERVICE_CONTROLLER },
                                                { LINTED_KO_WRONLY,
                                                  LINTED_SERVICE_UPDATER }
                                            })
                                        } },
         [LINTED_SERVICE_GUI] = { .process = {
                                      .type = SERVICE_PROCESS,
                                      .dirko = cwd,
                                      .path = gui_path,
                                      .arguments =
                                          (char const * const[]) { gui_path,
                                                                   NULL },
                                      .environment =
                                          (char const * const[]) { display,
                                                                   NULL },
                                      .dup_pairs =
                                          DUP_PAIRS((struct dup_pair const[]) {
                                              { LINTED_KO_RDONLY,
                                                LINTED_SERVICE_STDIN },
                                              { LINTED_KO_WRONLY,
                                                LINTED_SERVICE_STDOUT },
                                              { LINTED_KO_WRONLY,
                                                LINTED_SERVICE_STDERR },
                                              { LINTED_KO_WRONLY,
                                                LINTED_SERVICE_LOGGER },
                                              { LINTED_KO_WRONLY,
                                                LINTED_SERVICE_CONTROLLER },
                                              { LINTED_KO_RDONLY,
                                                LINTED_SERVICE_UPDATER }
                                          })
                                  } },
         [LINTED_SERVICE_STDIN] = { .file = { .type = SERVICE_FILE,
                                              .generator = find_stdin } },
         [LINTED_SERVICE_STDOUT] = { .file = { .type = SERVICE_FILE,
                                               .generator = find_stdout } },
         [LINTED_SERVICE_STDERR] = { .file = { .type = SERVICE_FILE,
                                               .generator = find_stderr } },
         [LINTED_SERVICE_LOGGER] = { .file = { .type = SERVICE_FILE,
                                               .generator =
                                                   linted_logger_create } },
         [LINTED_SERVICE_UPDATER] = { .file = { .type = SERVICE_FILE,
                                                .generator = updater_create } },
         [LINTED_SERVICE_CONTROLLER] = { .file = { .type = SERVICE_FILE,
                                                   .generator =
                                                       controller_create } } };

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
        {[LINTED_SERVICE_INIT] = { .init = { .pid = getpid() } },
         [LINTED_SERVICE_GUI] = { .process = { .pid = -1 } },
         [LINTED_SERVICE_STDIN] = { .file = { .is_open = false } },
         [LINTED_SERVICE_STDOUT] = { .file = { .is_open = false } },
         [LINTED_SERVICE_STDERR] = { .file = { .is_open = false } },
         [LINTED_SERVICE_SIMULATOR] = { .process = { .pid = -1 } },
         [LINTED_SERVICE_LOGGER] = { .file = { .is_open = false } },
         [LINTED_SERVICE_UPDATER] = { .file = { .is_open = false } },
         [LINTED_SERVICE_CONTROLLER] = { .file = { .is_open = false } } };

    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        union service_config const *service_config = &config[ii];
        if (service_config->type != SERVICE_FILE) {
            continue;
        }

        union service *service = &services[ii];

        linted_ko ko;
        if ((errnum = service_config->file.generator(&ko)) != 0) {
            goto exit_services;
        }

        service->file.ko = ko;
        service->file.is_open = true;
    }

    linted_logger logger_read = services[LINTED_SERVICE_LOGGER].file.ko;

    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        if (config[ii].type != SERVICE_PROCESS) {
            continue;
        }

        struct service_process *service = &services[ii].process;
        struct service_config_process const *proc_config = &config[ii].process;
        struct linted_spawn_file_actions *file_actions;
        struct linted_spawn_attr *attr;

        if ((errnum = linted_spawn_file_actions_init(&file_actions)) != 0) {
            goto exit_services;
        }

        if ((errnum = linted_spawn_attr_init(&attr)) != 0) {
            goto destroy_file_actions;
        }

        size_t dup_pairs_size = proc_config->dup_pairs.size;
        linted_ko *proc_kos = linted_mem_alloc_array(
            &errnum, sizeof proc_kos[0u], dup_pairs_size);
        if (errnum != 0) {
            goto destroy_attr;
        }
        size_t kos_opened = 0u;
        for (; kos_opened < dup_pairs_size;) {
            struct dup_pair const *dup_pair =
                &proc_config->dup_pairs.dup_pairs[kos_opened];

            struct service_file const *file = &services[dup_pair->service].file;

            linted_ko ko = file->ko;
            if ((errnum = linted_ko_reopen(&ko, dup_pair->flags)) != 0) {
                goto destroy_proc_kos;
            }

            proc_kos[kos_opened] = ko;
            ++kos_opened;

            if ((errnum = linted_spawn_file_actions_adddup2(
                     &file_actions, ko, kos_opened - 1)) != 0) {
                goto destroy_proc_kos;
            }
        }

        linted_spawn_attr_setpgroup(attr, 0);

        {
            pid_t process;
            if ((errnum = linted_spawn(
                     &process, proc_config->dirko, proc_config->path,
                     file_actions, attr, (char **)proc_config->arguments,
                     (char **)proc_config->environment)) != 0) {
                goto destroy_attr;
            }

            service->pid = process;
        }

    destroy_proc_kos:
        for (size_t jj = 0; jj < kos_opened; ++jj) {
            linted_ko_close(proc_kos[jj]);
        }
        free(proc_kos);

    destroy_attr:
        linted_spawn_attr_destroy(attr);

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

        linted_io_write_str(stdout, NULL, LINTED_STR("management socket: "));
        linted_io_write_all(stdout, NULL, buf, len);
        linted_io_write_str(stdout, NULL, LINTED_STR("\n"));
    }

    char *logger_buffer = linted_mem_alloc(&errnum, LINTED_LOGGER_LOG_MAX);
    if (errnum != 0) {
        goto close_new_connections;
    }

    {
        struct connection_pool *connection_pool;

        if ((errnum = connection_pool_create(&connection_pool)) != 0) {
            goto free_logger_buffer;
        }

        struct service_process *gui_service =
            &services[LINTED_SERVICE_GUI].process;
        struct service_process *sim_service =
            &services[LINTED_SERVICE_SIMULATOR].process;

        struct linted_asynch_task_waitid waiter_task;
        struct linted_logger_task logger_task;
        struct linted_manager_task_accept new_connections_accept_task;

        linted_asynch_waitid(&waiter_task, WAITER, P_ALL, -1, WEXITED);

        linted_logger_receive(&logger_task, LOGGER, logger_read, logger_buffer);

        linted_manager_accept(&new_connections_accept_task, NEW_CONNECTIONS,
                              new_connections);

        linted_asynch_pool_submit(pool, LINTED_UPCAST(&waiter_task));
        linted_asynch_pool_submit(pool,
                                  LINTED_UPCAST(LINTED_UPCAST(&logger_task)));
        linted_asynch_pool_submit(
            pool, LINTED_UPCAST(LINTED_UPCAST(&new_connections_accept_task)));

        for (;;) {
            struct linted_asynch_task *completed_tasks[20u];
            size_t task_count;
            linted_asynch_pool_wait(pool, completed_tasks,
                                    LINTED_ARRAY_SIZE(completed_tasks),
                                    &task_count);

            for (size_t ii = 0u; ii < task_count; ++ii) {
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
                                                    services,
                                                    connection_pool)) != 0) {
                        goto close_connections;
                    }
                    break;
                }

                case LOGGER: {
                    if ((errnum = completed_task->errnum) != 0) {
                        goto close_connections;
                    }

                    size_t log_size = LINTED_UPCAST(&logger_task)->bytes_read;

                    linted_io_write_string(stderr, NULL, process_name);
                    linted_io_write_str(stderr, NULL, LINTED_STR(": "));
                    linted_io_write_all(stderr, NULL, logger_buffer, log_size);
                    linted_io_write_str(stderr, NULL, LINTED_STR("\n"));

                    linted_asynch_pool_submit(pool, completed_task);
                    break;
                }

                case WAITER: {
                    if ((errnum = completed_task->errnum) != 0) {
                        goto close_connections;
                    }

                    siginfo_t exit_info = waiter_task.info;
                    linted_asynch_pool_submit(pool, completed_task);

                    pid_t pid = exit_info.si_pid;

                    if (pid == gui_service->pid) {
                        gui_service->pid = -1;
                    }

                    if (pid == sim_service->pid) {
                        sim_service->pid = -1;
                    }

                    switch (exit_info.si_code) {
                    case CLD_DUMPED:
                    case CLD_KILLED:
                        raise(exit_info.si_status);
                        errnum = errno;
                        assert(errnum != 0);
                        goto close_connections;

                    case CLD_EXITED:
                        if (exit_info.si_status != 0) {
                            errnum = exit_info.si_status;
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
                    assert(completed_task->task_action <
                           CONNECTION + MAX_MANAGE_CONNECTIONS);

                    size_t jj = completed_task->task_action - CONNECTION;
                    struct connection *connection =
                        &connection_pool->connections[jj];

                    if ((errnum = completed_task->errnum) != 0) {
                        /* The other end did something bad */
                        if ((errnum = connection_remove(
                                 connection, connection_pool)) != 0) {
                            goto close_connections;
                        }
                        break;
                    }

                    if (!connection->has_reply_ready) {
                        if ((errnum = on_connection_recv_request(
                                 pool, connection, connection_pool, config,
                                 services)) != 0) {
                            goto close_connections;
                        }
                    } else {
                        if ((errnum = connection_remove(
                                 connection, connection_pool)) != 0) {
                            goto close_connections;
                        }
                    }
                    break;
                }
                }
            }
        }

    close_connections : {
        linted_error close_errnum = connection_pool_destroy(connection_pool);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }
    }

free_logger_buffer:
    linted_mem_free(logger_buffer);

close_new_connections : {
    linted_error close_errnum = linted_ko_close(new_connections);
    if (0 == errnum) {
        errnum = close_errnum;
    }
}

exit_services:
    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        union service_config const *service_config = &config[ii];

        if (service_config->type != SERVICE_PROCESS) {
            continue;
        }

        struct service_process *service = &services[ii].process;
        pid_t pid = service->pid;

        if (pid != -1) {
            linted_error kill_errnum = -1 == kill(pid, SIGTERM) ? errno : 0;
            /* kill_errnum == ESRCH is fine */
            assert(kill_errnum != EINVAL);
            assert(kill_errnum != EPERM);

            service->pid = -1;
        }
    }

    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        union service_config const *service_config = &config[ii];

        if (LINTED_SERVICE_STDERR == ii) {
            continue;
        }

        if (service_config->type != SERVICE_FILE) {
            continue;
        }

        struct service_file *file = &services[ii].file;
        if (file->is_open) {
            linted_error close_errnum = linted_ko_close(file->ko);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }
        file->is_open = false;
    }

    {
        linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
        if (0 == errnum) {
            errnum = destroy_errnum;
        }
    }

    if (errnum != 0) {
        char const *error_string = linted_error_string_alloc(errnum);
        linted_io_write_format(stderr, NULL, "could not run the game: %s\n",
                               error_string);
        linted_error_string_free(error_string);

        return EXIT_FAILURE;
    }

    if (linted_ko_close(stderr) != 0) {
        /* Sadly, this is all we can do */
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static linted_error find_stdin(linted_ko *kop)
{
    *kop = kos[0u];
    return 0;
}

static linted_error find_stdout(linted_ko *kop)
{
    *kop = kos[1u];
    return 0;
}

static linted_error find_stderr(linted_ko *kop)
{
    *kop = kos[2u];
    return 0;
}

static linted_error updater_create(linted_ko *kop)
{
    return linted_updater_create(kop, 0);
}

static linted_error controller_create(linted_ko *kop)
{
    return linted_controller_create(kop, 0);
}

static linted_error on_new_connection(linted_manager new_socket,
                                      struct linted_asynch_pool *pool,
                                      union service_config const *config,
                                      union service const *services,
                                      struct connection_pool *connection_pool)
{
    linted_error errnum = 0;

    if (connection_pool->count >= MAX_MANAGE_CONNECTIONS) {
        /* I'm sorry sir but we are full today. */
        goto close_new_socket;
    }

    struct connection *connection;

    size_t ii = 0u;
    for (; ii < MAX_MANAGE_CONNECTIONS; ++ii) {
        connection = &connection_pool->connections[ii];
        if (-1 == connection->ko) {
            goto got_space;
        }
    }
    /* Impossible, listen has limited this */
    assert(false);
got_space:
    connection->ko = new_socket;
    connection->has_reply_ready = false;

    linted_manager_recv_request(&connection->recv_request_task, CONNECTION + ii,
                                new_socket);
    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(&connection->recv_request_task)));

    ++connection_pool->count;
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
    struct connection_pool *connection_pool, union service_config const *config,
    union service const *services)
{
    linted_error errnum;

    struct linted_manager_task_recv_request *task_recv =
        &connection->recv_request_task;

    linted_ko ko = connection->ko;

    int task_action = LINTED_UPCAST(LINTED_UPCAST(task_recv))->task_action;
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
                    goto connection_remove;
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
                    goto connection_remove;
                }
            }

        default:
            break;
        }
        break;
    }
    }
    connection->has_reply_ready = true;

    linted_manager_send_reply(&connection->send_reply_task, task_action, ko,
                              &reply);
    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(&connection->send_reply_task)));

    return 0;

connection_remove:
    connection_remove(connection, connection_pool);
    return errnum;
}

static linted_error check_db(linted_ko cwd)
{
    enum {
        TMP_WRITE_FINISHED
    };

    linted_error errnum;

    struct linted_asynch_pool *pool;
    {
        struct linted_asynch_pool *xx;
        if ((errnum = linted_asynch_pool_create(&xx, 1)) != 0) {
            return errnum;
        }
        pool = xx;
    }

    linted_db my_db;
    {
        /**
         * @todo Place the database according to the XDG base
         *       directory specification.
         */
        linted_db xx;
        if ((errnum = linted_db_open(&xx, cwd, "linted-db", LINTED_DB_CREAT)) !=
            0) {
            goto destroy_pool;
        }
        my_db = xx;
    }

    {
        linted_ko tmp;
        {
            linted_ko xx;
            if ((errnum = linted_db_temp_file(&my_db, &xx)) != 0) {
                goto close_db;
            }
            tmp = xx;
        }

        static char const hello[] = "Hello anybody!";
        char const *data = hello;
        size_t data_size = sizeof hello - 1u;

        struct linted_asynch_task_write write_task;

        linted_asynch_write(&write_task, TMP_WRITE_FINISHED, tmp, data,
                            data_size);
        linted_asynch_pool_submit(pool, LINTED_UPCAST(&write_task));

        struct linted_asynch_task *completed_tasks[20u];
        size_t task_count;
        linted_asynch_pool_wait(pool, completed_tasks,
                                LINTED_ARRAY_SIZE(completed_tasks),
                                &task_count);

        for (size_t ii = 0u; ii < task_count; ++ii) {
            struct linted_asynch_task *completed_task = completed_tasks[ii];
            if ((errnum = completed_task->errnum) != 0) {
                goto close_tmp;
            }

            switch (completed_task->task_action) {
            case TMP_WRITE_FINISHED:
                goto done_writing;

            default:
                assert(false);
            }
        }

    done_writing:
        if ((errnum = linted_db_temp_send(&my_db, "hello", tmp)) != 0) {
            goto close_tmp;
        }

    close_tmp : {
        linted_error close_errnum = linted_ko_close(tmp);
        if (0 == errnum) {
            errnum = close_errnum;
        }
    }
    }

close_db : {
    linted_error close_errnum = linted_db_close(&my_db);
    if (0 == errnum) {
        errnum = close_errnum;
    }
}

destroy_pool : {
    linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
    if (0 == errnum) {
        errnum = destroy_errnum;
    }
}

    return errnum;
}

static linted_error connection_pool_create(struct connection_pool **poolp)
{
    linted_error errnum;

    struct connection_pool *pool = linted_mem_alloc(&errnum, sizeof *pool);
    if (errnum != 0) {
        return errnum;
    }

    pool->count = 0u;

    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(pool->connections); ++ii) {
        pool->connections[ii].ko = -1;
    }

    *poolp = pool;
    return 0;
}

static linted_error connection_pool_destroy(struct connection_pool *pool)
{
    linted_error errnum = 0;
    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(pool->connections); ++ii) {
        struct connection *const connection = &pool->connections[ii];
        linted_ko const ko = connection->ko;
        if (ko != -1) {
            linted_error close_errnum = linted_ko_close(ko);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }
    }
    linted_mem_free(pool);
    return errnum;
}

static linted_error connection_remove(struct connection *connection,
                                      struct connection_pool *pool)
{
    linted_ko ko = connection->ko;

    connection->ko = -1;
    --pool->count;

    return linted_ko_close(ko);
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

    if ((errnum = linted_io_write_str(ko, NULL, LINTED_STR(" [OPTIONS]\n"))) !=
        0) {
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
