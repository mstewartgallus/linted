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

#include "init.h"
#include "monitor.h"

#include "linted/asynch.h"
#include "linted/controller.h"
#include "linted/db.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/logger.h"
#include "linted/manager.h"
#include "linted/mem.h"
#include "linted/spawn.h"
#include "linted/updater.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <mntent.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/prctl.h>
#include <unistd.h>

#define BACKLOG 20U

#define MAX_MANAGE_CONNECTIONS 10U

#ifndef PR_SET_CHILD_SUBREAPER
#define PR_SET_CHILD_SUBREAPER 36UL
#endif

enum {
    WAITER,
    LOGGER,
    NEW_CONNECTIONS,
    READ_CONNECTION,
    WRITE_CONNECTION
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
    char const *fstab;
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

struct init_logger_task
{
    struct linted_logger_task parent;
    struct linted_asynch_pool *pool;
    char const *process_name;
    linted_ko log_ko;
};

struct wait_service_task
{
    struct linted_asynch_task_waitid parent;
    struct linted_asynch_pool *pool;
    struct service_process *gui_service;
    struct service_process *sim_service;
};

struct connection;
struct connection_pool;

struct new_connection_task
{
    struct linted_manager_task_accept parent;
    struct linted_asynch_pool *pool;
    struct connection_pool *connection_pool;
    union service const *services;
    union service_config const *config;
};

struct read_conn_task
{
    struct linted_manager_task_recv_request parent;
    struct linted_asynch_pool *pool;
    struct connection_pool *connection_pool;
    struct connection *connection;
    union service const *services;
    union service_config const *config;
};

struct write_conn_task
{
    struct linted_manager_task_send_reply parent;
    struct linted_asynch_pool *pool;
    struct connection_pool *connection_pool;
    struct connection *connection;
};

struct connection
{
    struct read_conn_task read_task;
    struct write_conn_task write_task;
    linted_ko ko;
};

struct connection_pool
{
    struct connection connections[MAX_MANAGE_CONNECTIONS];
    size_t count;
};

static char const *const gui_envvars_to_keep[]
    = { "DISPLAY", "LD_PRELOAD", "LD_LIBRARY_PATH", "EGL_DRIVERS_PATH",
        "EGL_DRIVER", "EGL_PLATFORM", "EGL_LOG_LEVEL", "EGL_SOFTWARE",
        "LIBGL_DEBUG", "LIBGL_DRIVERS_PATH", "LIBGL_ALWAYS_INDIRECT",
        "LIBGL_ALWAYS_SOFTWARE", "LIBGL_NO_DRAWARRAYS", "LIBGL_SHOW_FPS",
        "MESA_NO_ASM", "MESA_NO_MMX", "MESA_NO_3DNOW", "MESA_NO_SSE",
        "MESA_DEBUG", "MESA_LOG_FILE", "MESA_TEX_PROG", "MESA_TNL_PROG",
        "MESA_EXTENSION_OVERRIDE"
        "MESA_EXTENSION_MAX_YEAR",
        "MESA_GL_VERSION_OVERRIDE", "MESA_GLSL_VERSION_OVERRIDE", "MESA_GLSL",
        "MESA_RGB_VISUAL", "MESA_CI_VISUAL", "MESA_BACK_BUFFER", "MESA_GAMMA",
        "MESA_XSYNC", "MESA_GLX_FORCE_CI", "MESA_GLX_FORCE_ALPHA",
        "MESA_GLX_DEPTH_BITS", "MESA_GLX_ALPHA_BITS", "INTEL_NO_HW",
        "INTEL_DEBUG", "RADEON_NO_TCL", "GALLIUM_HUD", "GALLIUM_LOG_FILE",
        "GALLIUM_PRINT_OPTIONS"
        "GALLIUM_DUMP_CPU"
        "TGSI_PRINT_SANITY"
        "DRAW_FSE",
        "DRAW_NO_FSE", "DRAW_USE_LLVM", "ST_DEBUG", "SOFTPIPE_DUMP_FS",
        "SOFTPIPE_DUMP_GS", "SOFTPIPE_NO_RAST", "SOFTPIPE_USE_LLVM",
        "LP_NO_RAST", "LP_DEBUG", "LP_PERF", "LP_NUM_THREADS",
        "SVGA_FORCE_SWTNL", "SVGA_NO_SWTNL", "SVGA_DEBUG" };

static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path);

static linted_error get_flags_and_data(char const *opts,
                                       unsigned long *mountflagsp,
                                       char const **leftoversp);

static linted_error find_stdin(linted_ko *kop);
static linted_error find_stdout(linted_ko *kop);
static linted_error find_stderr(linted_ko *kop);

static linted_error logger_create(linted_ko *kop);
static linted_error updater_create(linted_ko *kop);
static linted_error controller_create(linted_ko *kop);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_receive_log(struct linted_asynch_task *task);
static linted_error on_new_connection(struct linted_asynch_task *task);
static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_read_connection(struct linted_asynch_task *task);
static linted_error on_write_connection(struct linted_asynch_task *task);

static linted_error dispatch_drainers(struct linted_asynch_task *completed_task);

static linted_error drain_on_receive_log(struct linted_asynch_task *task);
static linted_error drain_on_new_connection(struct linted_asynch_task *task);
static linted_error drain_on_process_wait(struct linted_asynch_task *task);
static linted_error drain_on_read_connection(struct linted_asynch_task *task);
static linted_error drain_on_write_connection(struct linted_asynch_task *task);

static linted_error check_db(linted_ko cwd);

static linted_error connection_pool_create(struct connection_pool **poolp);
static linted_error connection_pool_destroy(struct connection_pool *pool);
static linted_error connection_remove(struct connection *connection,
                                      struct connection_pool *connection_pool);

uint_fast8_t linted_init_monitor(linted_ko cwd, char const *chrootdir_path,
                                 char const *simulator_fstab_path,
                                 char const *gui_fstab_path,
                                 char const *simulator_path,
                                 char const *gui_path)
{
    linted_error errnum;

    static char const process_name[] = "monitor";

    /* The process monitor and manager */
    if (-1 == prctl(PR_SET_NAME, (unsigned long)process_name, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    if (-1 == prctl(PR_SET_PDEATHSIG, (unsigned long)SIGKILL, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    if (-1 == prctl(PR_SET_CHILD_SUBREAPER, 1UL, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    /* Acquire socket before unsharing namespaces */
    linted_manager new_connections;
    {
        linted_manager xx;
        if ((errnum = linted_manager_bind(&xx, BACKLOG, NULL, 0)) != 0) {
            errno = errnum;
            perror("linted_manager_bind");
            return EXIT_FAILURE;
        }
        new_connections = xx;
    }

    if ((errnum = check_db(cwd)) != 0) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: database: %s\n",
                               process_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    char const **gui_envvars = NULL;
    size_t gui_envvars_count = 0U;
    for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(gui_envvars_to_keep); ++ii) {
        char const *envvar_name = gui_envvars_to_keep[ii];

        char const *envvar = getenv(envvar_name);
        if (NULL == envvar) {
            continue;
        }

        ++gui_envvars_count;
        {
            void *xx;
            if ((errnum
                 = linted_mem_realloc_array(&xx, gui_envvars, gui_envvars_count,
                                            sizeof gui_envvars[0U])) != 0) {
                errno = errnum;
                perror("linted_mem_realloc_array");
                return EXIT_FAILURE;
            }
            gui_envvars = xx;
        }

        size_t envvar_name_length = strlen(envvar_name);
        size_t envvar_length = strlen(envvar);

        size_t assign_string_length = envvar_name_length + 1U + envvar_length;

        char *assign_string;
        {
            void *xx;
            if ((errnum = linted_mem_alloc(&xx, assign_string_length + 1U))
                != 0) {
                errno = errnum;
                perror("linted_mem_alloc");
                return EXIT_FAILURE;
            }
            assign_string = xx;
        }
        memcpy(assign_string, envvar_name, envvar_name_length);
        assign_string[envvar_name_length] = '=';
        memcpy(assign_string + envvar_name_length + 1U, envvar, envvar_length);
        assign_string[assign_string_length] = '\0';

        gui_envvars[gui_envvars_count - 1U] = assign_string;
    }
    ++gui_envvars_count;
    {
        void *xx;
        if ((errnum = linted_mem_realloc_array(
                 &xx, gui_envvars, gui_envvars_count, sizeof gui_envvars[0U]))
            != 0) {
            errno = errnum;
            perror("linted_mem_realloc_array");
            return EXIT_FAILURE;
        }
        gui_envvars = xx;
    }
    gui_envvars[gui_envvars_count - 1U] = NULL;

    union service_config const config[]
        = {[LINTED_SERVICE_INIT] = { .type = SERVICE_INIT },
           [LINTED_SERVICE_SIMULATOR]
               = { .process
                   = { .type = SERVICE_PROCESS,
                       .dirko = cwd,
                       .fstab = simulator_fstab_path,
                       .path = simulator_path,
                       .arguments
                       = (char const * const[]) { simulator_path, NULL },
                       .environment = (char const * const[]) { NULL },
                       .dup_pairs = DUP_PAIRS((struct dup_pair const[]) {
                           { LINTED_KO_RDONLY, LINTED_SERVICE_STDIN },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_STDOUT },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_STDERR },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_LOGGER },
                           { LINTED_KO_RDONLY, LINTED_SERVICE_CONTROLLER },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_UPDATER }
                       }) } },
           [LINTED_SERVICE_GUI]
               = { .process
                   = { .type = SERVICE_PROCESS,
                       .dirko = cwd,
                       .fstab = gui_fstab_path,
                       .path = gui_path,
                       .arguments = (char const * const[]) { gui_path, NULL },
                       .environment = gui_envvars,
                       .dup_pairs = DUP_PAIRS((struct dup_pair const[]) {
                           { LINTED_KO_RDONLY, LINTED_SERVICE_STDIN },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_STDOUT },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_STDERR },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_LOGGER },
                           { LINTED_KO_WRONLY, LINTED_SERVICE_CONTROLLER },
                           { LINTED_KO_RDONLY, LINTED_SERVICE_UPDATER }
                       }) } },
           [LINTED_SERVICE_STDIN]
               = { .file = { .type = SERVICE_FILE, .generator = find_stdin } },
           [LINTED_SERVICE_STDOUT]
               = { .file = { .type = SERVICE_FILE, .generator = find_stdout } },
           [LINTED_SERVICE_STDERR]
               = { .file = { .type = SERVICE_FILE, .generator = find_stderr } },
           [LINTED_SERVICE_LOGGER] = { .file = { .type = SERVICE_FILE,
                                                 .generator = logger_create } },
           [LINTED_SERVICE_UPDATER]
               = { .file
                   = { .type = SERVICE_FILE, .generator = updater_create } },
           [LINTED_SERVICE_CONTROLLER]
               = { .file = { .type = SERVICE_FILE,
                             .generator = controller_create } } };

    enum {
        MAX_TASKS = READ_CONNECTION + MAX_MANAGE_CONNECTIONS
    };

    struct linted_asynch_pool *pool;
    {
        struct linted_asynch_pool *xx;
        if ((errnum = linted_asynch_pool_create(&xx, MAX_TASKS)) != 0) {
            return errnum;
        }
        pool = xx;
    }

    union service services[]
        = {[LINTED_SERVICE_INIT] = { .init = { .pid = getpid() } },
           [LINTED_SERVICE_GUI] = { .process = { .pid = -1 } },
           [LINTED_SERVICE_STDIN] = { .file = { .is_open = false } },
           [LINTED_SERVICE_STDOUT] = { .file = { .is_open = false } },
           [LINTED_SERVICE_STDERR] = { .file = { .is_open = false } },
           [LINTED_SERVICE_SIMULATOR] = { .process = { .pid = -1 } },
           [LINTED_SERVICE_LOGGER] = { .file = { .is_open = false } },
           [LINTED_SERVICE_UPDATER] = { .file = { .is_open = false } },
           [LINTED_SERVICE_CONTROLLER] = { .file = { .is_open = false } } };

    for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(services); ++ii) {
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

    for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(services); ++ii) {
        if (config[ii].type != SERVICE_PROCESS) {
            continue;
        }

        struct service_process *service = &services[ii].process;
        struct service_config_process const *proc_config = &config[ii].process;
        struct linted_spawn_file_actions *file_actions;
        struct linted_spawn_attr *attr;

        {
            struct linted_spawn_file_actions *xx;
            if ((errnum = linted_spawn_file_actions_init(&xx)) != 0) {
                goto exit_services;
            }
            file_actions = xx;
        }

        {
            struct linted_spawn_attr *xx;
            if ((errnum = linted_spawn_attr_init(&xx)) != 0) {
                goto destroy_file_actions;
            }
            attr = xx;
        }

        linted_spawn_attr_drop_caps(attr);

        linted_spawn_attr_setchrootdir(attr, chrootdir_path);

        /* TODO: Close files leading outside of the sandbox  */
        if ((errnum = parse_fstab(attr, cwd, proc_config->fstab)) != 0) {
            errno = errnum;
            perror("parse_fstab");
            return EXIT_FAILURE;
        }

        size_t dup_pairs_size = proc_config->dup_pairs.size;
        linted_ko *proc_kos;
        {
            void *xx;
            if ((errnum = linted_mem_alloc_array(&xx, sizeof proc_kos[0U],
                                                 dup_pairs_size)) != 0) {
                goto destroy_attr;
            }
            proc_kos = xx;
        }
        size_t kos_opened = 0U;
        for (; kos_opened < dup_pairs_size;) {
            struct dup_pair const *dup_pair =
                &proc_config->dup_pairs.dup_pairs[kos_opened];

            struct service_file const *file = &services[dup_pair->service].file;

            linted_ko ko;
            {
                linted_ko xx;
                if ((errnum = linted_ko_reopen(&xx, file->ko, dup_pair->flags))
                    != 0) {
                    goto destroy_proc_kos;
                }
                ko = xx;
            }

            proc_kos[kos_opened] = ko;
            ++kos_opened;

            if ((errnum = linted_spawn_file_actions_adddup2(
                     &file_actions, ko, kos_opened - 1U)) != 0) {
                goto destroy_proc_kos;
            }
        }

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

    {
        char buf[LINTED_MANAGER_PATH_MAX];
        size_t len;
        if ((errnum = linted_manager_path(new_connections, buf, &len)) != 0) {
            goto close_new_connections;
        }

        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("LINTED_SOCKET="));
        linted_io_write_all(STDOUT_FILENO, NULL, buf, len);
        linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));
    }

    char *logger_buffer;
    {
        void *xx;
        if ((errnum = linted_mem_alloc(&xx, LINTED_LOGGER_LOG_MAX)) != 0) {
            goto close_new_connections;
        }
        logger_buffer = xx;
    }

    struct connection_pool *connection_pool;

    {
        struct connection_pool *xx;
        if ((errnum = connection_pool_create(&xx)) != 0) {
            goto free_logger_buffer;
        }
        connection_pool = xx;
    }

    struct service_process *gui_service = &services[LINTED_SERVICE_GUI].process;
    struct service_process *sim_service =
        &services[LINTED_SERVICE_SIMULATOR].process;

    struct wait_service_task waiter_task;
    struct init_logger_task logger_task;
    struct new_connection_task new_connection_task;

    linted_asynch_task_waitid(LINTED_UPCAST(&waiter_task), WAITER, P_ALL, -1,
                              0);
    waiter_task.pool = pool;
    waiter_task.gui_service = gui_service;
    waiter_task.sim_service = sim_service;

    linted_logger_receive(LINTED_UPCAST(&logger_task), LOGGER, logger_read,
                          logger_buffer);
    logger_task.log_ko = STDERR_FILENO;
    logger_task.process_name = process_name;
    logger_task.pool = pool;

    linted_manager_accept(LINTED_UPCAST(&new_connection_task), NEW_CONNECTIONS,
                          new_connections);
    new_connection_task.pool = pool;
    new_connection_task.connection_pool = connection_pool;
    new_connection_task.services = services;
    new_connection_task.config = config;

    linted_asynch_pool_submit(pool, LINTED_UPCAST(LINTED_UPCAST(&waiter_task)));
    linted_asynch_pool_submit(
        pool, LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&logger_task))));
    linted_asynch_pool_submit(pool, LINTED_UPCAST(LINTED_UPCAST(
                                        LINTED_UPCAST(&new_connection_task))));
    {
        struct linted_asynch_task *completed_tasks[20U];
        size_t task_count;
        size_t ii;

        for (;;) {
            {
                size_t xx;
                linted_asynch_pool_wait(pool, completed_tasks,
                                        LINTED_ARRAY_SIZE(completed_tasks), &xx);
                task_count = xx;
            }

            for (ii = 0U; ii < task_count; ++ii) {
                if ((errnum = dispatch(completed_tasks[ii])) != 0) {
                    goto drain_dispatches;
                }
            }

            if (-1 == gui_service->pid) {
                goto drain_dispatches;
            }
        }

    drain_dispatches:
        linted_asynch_pool_stop(pool);

        for (; ii < task_count; ++ii) {
            linted_error dispatch_error = dispatch_drainers(completed_tasks[ii]);
            if (0 == errnum) {
                errnum = dispatch_error;
            }
        }

        linted_error poll_errnum;
        do {
            {
                size_t xx;
                poll_errnum = linted_asynch_pool_poll(pool, completed_tasks,
                                                      LINTED_ARRAY_SIZE(completed_tasks), &xx);
                task_count = xx;
            }

            for (ii = 0U; ii < task_count; ++ii) {
                linted_error dispatch_error = dispatch_drainers(completed_tasks[ii]);
                if (0 == errnum) {
                    errnum = dispatch_error;
                }
            }
        } while (poll_errnum != EAGAIN);
    }

    {
        linted_error close_errnum = connection_pool_destroy(connection_pool);
        if (0 == errnum) {
            errnum = close_errnum;
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

exit_services : {
    if (-1 == kill(-1, SIGKILL)) {
        linted_error kill_errnum = errno;
        LINTED_ASSUME(kill_errnum != 0);
        if (kill_errnum != ESRCH) {
            assert(kill_errnum != EINVAL);
            assert(kill_errnum != EPERM);
            LINTED_ASSUME_UNREACHABLE();
        }
    }
}

    for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(services); ++ii) {
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

        /* Insure that the tasks are in proper scope until they are
         * terminated */
        (void)waiter_task;
        (void)logger_task;
        (void)new_connection_task;
    }

    if (errnum != 0) {
        char const *error_string = linted_error_string_alloc(errnum);
        linted_io_write_format(STDERR_FILENO, NULL,
                               "could not run the game: %s\n", error_string);
        linted_error_string_free(error_string);

        return EXIT_FAILURE;
    }

    if (linted_ko_close(STDERR_FILENO) != 0) {
        /* Sadly, this is all we can do */
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path)
{
    linted_error errnum = 0;

    char const *abspath;
    if (fstab_path[0U] != '/') {
        char *xx;
        if (-1 == asprintf(&xx, "/proc/self/fd/%i/%s", cwd, fstab_path)) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            return errnum;
        }
        abspath = xx;
    } else {
        abspath = fstab_path;
    }

    FILE *fstab = setmntent(abspath, "re");
    errnum = errno;

    if (abspath != fstab_path) {
        linted_mem_free((char *)abspath);
    }

    if (NULL == fstab) {
        LINTED_ASSUME(errnum != 0);
        return errnum;
    }

    for (;;) {
        errno = 0;
        struct mntent *entry = getmntent(fstab);
        if (NULL == entry) {
            errnum = errno;
            if (errnum != 0) {
                goto close_file;
            }

            break;
        }

        char const *fsname = entry->mnt_fsname;
        char const *dir = entry->mnt_dir;
        char const *type = entry->mnt_type;
        char const *opts = entry->mnt_opts;

        if (0 == strcmp("none", fsname)) {
            fsname = NULL;
        }

        if (0 == strcmp("none", type)) {
            type = NULL;
        }

        if (0 == strcmp("none", opts)) {
            opts = NULL;
        }

        unsigned long mountflags;
        char const *data;
        if (NULL == opts) {
            mountflags = 0U;
            data = NULL;
        } else {
            unsigned long xx;
            char const *yy;
            if ((errnum = get_flags_and_data(opts, &xx, &yy)) != 0) {
                goto close_file;
            }
            mountflags = xx;
            data = yy;
        }

        if ((errnum = linted_spawn_attr_setmount(attr, fsname, dir, type,
                                                 mountflags, data)) != 0) {
            goto close_file;
        }
    }

close_file:
    if (endmntent(fstab) != 1) {
        if (0 == errnum) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
        }
    }

    return errnum;
}

static linted_error get_flags_and_data(char const *opts,
                                       unsigned long *mountflagsp,
                                       char const **leftoversp)
{
    enum {
        BIND,
        RBIND,
        RO,
        RW,
        SUID,
        NOSUID,
        NODEV,
        NOEXEC
    };

    linted_error errnum;

    static char const *const tokens[]
        = {[BIND] = "bind",   [RBIND] = "rbind",    [RO] = MNTOPT_RO,
           [RW] = MNTOPT_RW,  [SUID] = MNTOPT_SUID, [NOSUID] = MNTOPT_NOSUID,
           [NODEV] = "nodev", [NOEXEC] = "noexec",  NULL };
    bool bind = false;
    bool rec = false;
    bool readonly = false;
    bool readwrite = false;
    bool suid = true;
    bool dev = true;
    bool exec = true;
    char *leftovers = NULL;

    char *subopts_str = strdup(opts);
    if (NULL == subopts_str) {
        errnum = errno;
        LINTED_ASSUME(errnum != 0);
        return errnum;
    }

    char *subopts = subopts_str;
    char *value = NULL;

    while (*subopts != '\0') {
        int token;
        {
            char *xx = subopts;
            char *yy = value;
            token = getsubopt(&xx, (char * const *)tokens, &yy);
            subopts = xx;
            value = yy;
        }
        switch (token) {
        case BIND:
            bind = true;
            break;

        case RBIND:
            bind = true;
            rec = true;
            break;

        case RO:
            readonly = true;
            break;

        case RW:
            readwrite = true;
            break;

        case SUID:
            suid = true;
            break;

        case NOSUID:
            suid = false;
            break;

        case NODEV:
            dev = false;
            break;

        case NOEXEC:
            exec = false;
            break;

        default:
            leftovers = strstr(opts, value);
            goto free_subopts_str;
        }
    }

free_subopts_str:
    linted_mem_free(subopts_str);

    if (readwrite && readonly) {
        return EINVAL;
    }

    if (bind && rec && readonly) {
        /*
         * Due to a completely idiotic kernel bug (see
         * https://bugzilla.kernel.org/show_bug.cgi?id=24912) using a
         * recursive bind mount as readonly would fail completely
         * silently and there is no way to workaround this.
         *
         * Even after working around by remounting it will fail for
         * the recursive case. For example, /home directory that is
         * recursively bind mounted as readonly and that has encrypted
         * user directories as an example. The /home directory will be
         * readonly but the user directory /home/user will not be.
         */
        return EINVAL;
    }

    unsigned long mountflags = 0;

    if (bind) {
        mountflags |= MS_BIND;
    }

    if (rec) {
        mountflags |= MS_REC;
    }

    if (readonly) {
        mountflags |= MS_RDONLY;
    }

    if (!suid) {
        mountflags |= MS_NOSUID;
    }

    if (!dev) {
        mountflags |= MS_NODEV;
    }

    if (!exec) {
        mountflags |= MS_NOEXEC;
    }

    *leftoversp = leftovers;
    *mountflagsp = mountflags;
    return 0;
}

static linted_error find_stdin(linted_ko *kop)
{
    *kop = STDIN_FILENO;
    return 0;
}

static linted_error find_stdout(linted_ko *kop)
{
    *kop = STDOUT_FILENO;
    return 0;
}

static linted_error find_stderr(linted_ko *kop)
{
    *kop = STDERR_FILENO;
    return 0;
}

static linted_error logger_create(linted_ko *kop)
{
    return linted_logger_create(kop, 0);
}

static linted_error updater_create(linted_ko *kop)
{
    return linted_updater_create(kop, 0);
}

static linted_error controller_create(linted_ko *kop)
{
    return linted_controller_create(kop, 0);
}

static linted_error dispatch(struct linted_asynch_task *completed_task)
{
    switch (completed_task->task_action) {
    case NEW_CONNECTIONS:
        return on_new_connection(completed_task);

    case LOGGER:
        return on_receive_log(completed_task);

    case WAITER:
        return on_process_wait(completed_task);

    case READ_CONNECTION:
        return on_read_connection(completed_task);

    case WRITE_CONNECTION:
        return on_write_connection(completed_task);

    default:
        LINTED_ASSUME_UNREACHABLE();
    }
}

static linted_error on_receive_log(struct linted_asynch_task *completed_task)
{
    linted_error errnum;

    if ((errnum = completed_task->errnum) != 0) {
        return errnum;
    }

    struct init_logger_task *logger_task
        = LINTED_DOWNCAST(struct init_logger_task, completed_task);

    struct linted_asynch_pool *pool = logger_task->pool;
    linted_ko log_ko = logger_task->log_ko;
    char const *process_name = logger_task->process_name;
    size_t log_size = LINTED_UPCAST(LINTED_UPCAST(logger_task))->bytes_read;
    char const *logger_buffer = LINTED_UPCAST(LINTED_UPCAST(logger_task))->buf;

    linted_io_write_string(log_ko, NULL, process_name);
    linted_io_write_str(log_ko, NULL, LINTED_STR(": "));
    linted_io_write_all(log_ko, NULL, logger_buffer, log_size);
    linted_io_write_str(log_ko, NULL, LINTED_STR("\n"));

    linted_asynch_pool_submit(pool, completed_task);

    return 0;
}

static linted_error on_process_wait(struct linted_asynch_task *completed_task)
{
    linted_error errnum;

    if ((errnum = completed_task->errnum) != 0) {
        return errnum;
    }

    struct wait_service_task *wait_service_task
        = LINTED_DOWNCAST(struct wait_service_task, completed_task);

    struct linted_asynch_pool *pool = wait_service_task->pool;
    struct service_process *gui_service = wait_service_task->gui_service;
    struct service_process *sim_service = wait_service_task->sim_service;

    int exit_status;
    int exit_code;
    pid_t pid;
    {
        siginfo_t *exit_info = &LINTED_UPCAST(wait_service_task)->info;
        exit_status = exit_info->si_status;
        exit_code = exit_info->si_code;
        pid = exit_info->si_pid;
    }

    linted_asynch_pool_submit(pool, completed_task);

    switch (exit_code) {
    case CLD_DUMPED:
    case CLD_KILLED:
        if (pid == gui_service->pid || pid == sim_service->pid) {
            raise(exit_status);
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            return errnum;
        }
        break;

    case CLD_EXITED:
        if (pid == gui_service->pid || pid == sim_service->pid) {
            if (exit_status != 0) {
                errnum = exit_status;
                LINTED_ASSUME(errnum != 0);
                return errnum;
            }
            goto process_exited;
        }
        break;

    case CLD_STOPPED:
        /* Presumably the process was stopped for a reason */
        break;

    default:
        LINTED_ASSUME_UNREACHABLE();
    }

    return 0;

process_exited:
    if (pid == gui_service->pid) {
        gui_service->pid = -1;
    }

    if (pid == sim_service->pid) {
        sim_service->pid = -1;
    }

    return 0;
}

static linted_error on_new_connection(struct linted_asynch_task *completed_task)
{
    linted_error errnum;

    if ((errnum = completed_task->errnum) != 0) {
        return errnum;
    }

    struct new_connection_task *new_connection_task
        = LINTED_DOWNCAST(struct new_connection_task, completed_task);

    struct linted_ko_task_accept *accept_task
        = LINTED_UPCAST(LINTED_UPCAST(new_connection_task));

    struct linted_asynch_pool *pool = new_connection_task->pool;
    struct connection_pool *connection_pool
        = new_connection_task->connection_pool;

    union service const *services = new_connection_task->services;
    union service_config const *config = new_connection_task->config;

    linted_manager new_socket = accept_task->returned_ko;
    linted_asynch_pool_submit(pool, completed_task);

    if (connection_pool->count >= MAX_MANAGE_CONNECTIONS) {
        /* I'm sorry sir but we are full today. */
        goto close_new_socket;
    }

    struct connection *connection;

    size_t ii = 0U;
    for (; ii < MAX_MANAGE_CONNECTIONS; ++ii) {
        connection = &connection_pool->connections[ii];
        if (-1 == connection->ko) {
            goto got_space;
        }
    }
    /* Impossible, listen has limited this */
    LINTED_ASSUME_UNREACHABLE();
got_space:
    connection->ko = new_socket;

    linted_manager_recv_request(LINTED_UPCAST(&connection->read_task),
                                READ_CONNECTION, new_socket);
    connection->read_task.pool = pool;
    connection->read_task.connection_pool = connection_pool;
    connection->read_task.connection = connection;
    connection->read_task.services = services;
    connection->read_task.config = config;

    linted_asynch_pool_submit(
        pool,
        LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&connection->read_task))));

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

static linted_error on_read_connection(struct linted_asynch_task
                                       *completed_task)
{
    linted_error errnum;

    struct read_conn_task *read_conn_task
        = LINTED_DOWNCAST(struct read_conn_task, completed_task);

    struct linted_asynch_pool *pool = read_conn_task->pool;
    struct connection_pool *connection_pool = read_conn_task->connection_pool;
    struct connection *connection = read_conn_task->connection;
    union service const *services = read_conn_task->services;
    union service_config const *config = read_conn_task->config;

    if ((errnum = completed_task->errnum) != 0) {
        /* The other end did something bad */
        if ((errnum = connection_remove(connection, connection_pool)) != 0) {
            return errnum;
        }
        return 0;
    }

    struct linted_manager_task_recv_request *task_recv
        = LINTED_UPCAST(read_conn_task);
    struct linted_ko_task_read *task_read = LINTED_UPCAST(task_recv);

    linted_ko ko = task_read->ko;

    union linted_manager_request *request = &task_recv->request;
    union linted_manager_reply reply;

    switch (request->type) {
    case LINTED_MANAGER_REBOOT: {
        if (-1 == reboot(RB_POWER_OFF)) {
            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            goto connection_remove;
        }
        break;
    }

    case LINTED_MANAGER_STATUS: {
        union service const *service = &services[request->status.service];
        pid_t pid;
        switch (config[request->status.service].type) {
        case SERVICE_INIT:
            pid = service->init.pid;
            goto status_service_process;

        case SERVICE_PROCESS:
            pid = service->process.pid;
            goto status_service_process;

        status_service_process:
            if (0 == kill(pid, 0)) {
                reply.status.is_up = true;
                break;
            }

            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            assert(errnum != EINVAL);
            switch (errnum) {
            case ESRCH:
                reply.status.is_up = false;
                break;

            default:
                goto connection_remove;
            }
            break;

        default:
            break;
        }
        break;
    }

    case LINTED_MANAGER_STOP: {
        union service const *service = &services[request->stop.service];
        pid_t pid;
        switch (config[request->status.service].type) {
        case SERVICE_INIT:
            pid = service->init.pid;
            goto stop_service_process;

        case SERVICE_PROCESS:
            pid = service->process.pid;
            goto stop_service_process;

        stop_service_process:
            if (0 == kill(pid, SIGKILL)) {
                reply.stop.was_up = true;
                break;
            }

            errnum = errno;
            LINTED_ASSUME(errnum != 0);
            assert(errnum != EINVAL);
            switch (errnum) {
            case ESRCH:
                reply.stop.was_up = false;
                break;

            default:
                goto connection_remove;
            }
            break;

        default:
            break;
        }
    }
    }

    linted_manager_send_reply(LINTED_UPCAST(&connection->write_task),
                              WRITE_CONNECTION, ko, &reply);
    connection->write_task.pool = pool;
    connection->write_task.connection_pool = connection_pool;
    connection->write_task.connection = connection;

    linted_asynch_pool_submit(
        pool,
        LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&connection->write_task))));

    return 0;

connection_remove:
    connection_remove(connection, connection_pool);
    return errnum;
}

static linted_error on_write_connection(struct linted_asynch_task *task)
{
    linted_error errnum;

    struct write_conn_task *write_conn_task
        = LINTED_DOWNCAST(struct write_conn_task, task);
    struct connection_pool *connection_pool = write_conn_task->connection_pool;
    struct connection *connection = write_conn_task->connection;

    errnum = task->errnum;

    {
        linted_error remove_errnum
            = connection_remove(connection, connection_pool);
        if (0 == errnum) {
            errnum = remove_errnum;
        }
    }

    return errnum;
}

static linted_error dispatch_drainers(struct linted_asynch_task *completed_task)
{
    switch (completed_task->task_action) {
    case NEW_CONNECTIONS:
        return drain_on_new_connection(completed_task);

    case LOGGER:
        return drain_on_receive_log(completed_task);

    case WAITER:
        return drain_on_process_wait(completed_task);

    case READ_CONNECTION:
        return drain_on_read_connection(completed_task);

    case WRITE_CONNECTION:
        return drain_on_write_connection(completed_task);

    default:
        LINTED_ASSUME_UNREACHABLE();
    }
}

static linted_error drain_on_receive_log(struct linted_asynch_task *task)
{
    return task->errnum;
}

static linted_error drain_on_new_connection(struct linted_asynch_task *task)
{
    linted_error errnum;

    if ((errnum = task->errnum) != 0) {
        return errnum;
    }

    struct new_connection_task *new_connection_task
        = LINTED_DOWNCAST(struct new_connection_task, task);

    struct linted_ko_task_accept *accept_task
        = LINTED_UPCAST(LINTED_UPCAST(new_connection_task));

    linted_manager new_socket = accept_task->returned_ko;

    return linted_ko_close(new_socket);
}

static linted_error drain_on_process_wait(struct linted_asynch_task *task)
{
    return task->errnum;
}

static linted_error drain_on_read_connection(struct linted_asynch_task *task)
{
    return task->errnum;
}

static linted_error drain_on_write_connection(struct linted_asynch_task *task)
{
    return task->errnum;
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
        if ((errnum = linted_asynch_pool_create(&xx, 1U)) != 0) {
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
        if ((errnum = linted_db_open(&xx, cwd, "linted-db", LINTED_DB_CREAT))
            != 0) {
            goto destroy_pool;
        }
        my_db = xx;
    }

    {
        linted_ko tmp;
        char *path;
        {
            linted_ko xx;
            char *yy;
            if ((errnum = linted_db_temp_file(my_db, &xx, &yy)) != 0) {
                goto close_db;
            }
            tmp = xx;
            path = yy;
        }

        static char const hello[] = "Hello anybody!";
        char const *data = hello;
        size_t data_size = sizeof hello - 1U;

        struct linted_ko_task_write write_task;

        linted_ko_task_write(&write_task, TMP_WRITE_FINISHED, tmp, data,
                             data_size);
        linted_asynch_pool_submit(pool, LINTED_UPCAST(&write_task));

        struct linted_asynch_task *completed_tasks[20U];
        size_t task_count;
        {
            size_t xx;
            linted_asynch_pool_wait(pool, completed_tasks,
                                    LINTED_ARRAY_SIZE(completed_tasks), &xx);
            task_count = xx;
        }

        for (size_t ii = 0U; ii < task_count; ++ii) {
            struct linted_asynch_task *completed_task = completed_tasks[ii];
            if ((errnum = completed_task->errnum) != 0) {
                goto close_tmp;
            }

            switch (completed_task->task_action) {
            case TMP_WRITE_FINISHED:
                goto done_writing;

            default:
                LINTED_ASSUME_UNREACHABLE();
            }
        }

    done_writing:
        if ((errnum = linted_db_temp_send(my_db, path, "hello")) != 0) {
            goto close_tmp;
        }

    close_tmp:
        linted_mem_free(path);

        {
            linted_error close_errnum = linted_ko_close(tmp);
            if (0 == errnum) {
                errnum = close_errnum;
            }
        }
    }

close_db : {
    linted_error close_errnum = linted_db_close(my_db);
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

    struct connection_pool *pool;
    {
        void *xx;
        if ((errnum = linted_mem_alloc(&xx, sizeof *pool)) != 0) {
            return errnum;
        }
        pool = xx;
    }

    pool->count = 0U;

    for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(pool->connections); ++ii) {
        pool->connections[ii].ko = -1;
    }

    *poolp = pool;
    return 0;
}

static linted_error connection_pool_destroy(struct connection_pool *pool)
{
    linted_error errnum = 0;
    for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(pool->connections); ++ii) {
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
