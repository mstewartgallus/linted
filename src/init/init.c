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
#include <mntent.h>
#include <fcntl.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <linux/capability.h>
#include <linux/sched.h>

#define BACKLOG 20u

#define MAX_MANAGE_CONNECTIONS 10u

#define HELP_OPTION "--help"
#define VERSION_OPTION "--version"

#define FSTAB_OPTION "--fstab"
#define SIMULATOR_OPTION "--simulator"
#define GUI_OPTION "--gui"

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

static uint_fast8_t do_init(linted_ko cwd, char const *fstab_path,
                            char const *simulator_path, char const *gui_path);
static uint_fast8_t do_monitor(linted_ko cwd, char const *fstab_path,
                               char const *simulator_path,
                               char const *gui_path);

static linted_error find_stdin(linted_ko *kop);
static linted_error find_stdout(linted_ko *kop);
static linted_error find_stderr(linted_ko *kop);

static linted_error logger_create(linted_ko *kop);
static linted_error updater_create(linted_ko *kop);
static linted_error controller_create(linted_ko *kop);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_receive_log(struct linted_asynch_task *completed_task);
static linted_error on_new_connection(struct linted_asynch_task *task);
static linted_error on_process_wait(struct linted_asynch_task *completed_task);
static linted_error on_read_connection(struct linted_asynch_task *task);
static linted_error on_write_connection(struct linted_asynch_task *task);

static linted_error check_db(linted_ko cwd);

static linted_error connection_pool_create(struct connection_pool **poolp);
static linted_error connection_pool_destroy(struct connection_pool *pool);
static linted_error connection_remove(struct connection *connection,
                                      struct connection_pool *connection_pool);

static linted_error linted_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);

struct linted_start_config const linted_start_config
    = { .canonical_process_name = PACKAGE_NAME "-init",
        .open_current_working_directory = true,
        .kos_size = 0u,
        .kos = NULL };

static unsigned long const capabilities[]
    = { CAP_CHOWN,           CAP_DAC_OVERRIDE,     CAP_DAC_READ_SEARCH,
        CAP_FOWNER,          CAP_FSETID,           CAP_KILL,
        CAP_SETGID,          CAP_SETUID,           CAP_SETPCAP,
        CAP_LINUX_IMMUTABLE, CAP_NET_BIND_SERVICE, CAP_NET_BROADCAST,
        CAP_NET_ADMIN,       CAP_NET_RAW,          CAP_IPC_LOCK,
        CAP_IPC_OWNER,       CAP_SYS_MODULE,       CAP_SYS_RAWIO,
        CAP_SYS_CHROOT,      CAP_SYS_PTRACE,       CAP_SYS_PACCT,
        CAP_SYS_ADMIN,       CAP_SYS_BOOT,         CAP_SYS_NICE,
        CAP_SYS_RESOURCE,    CAP_SYS_TIME,         CAP_SYS_TTY_CONFIG,
        CAP_MKNOD,           CAP_LEASE,            CAP_AUDIT_WRITE,
        CAP_AUDIT_CONTROL,   CAP_SETFCAP,          CAP_MAC_OVERRIDE,
        CAP_MAC_ADMIN,       CAP_SYSLOG,           CAP_WAKE_ALARM };

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

    for (size_t ii = 1u; ii < argc; ++ii) {
        char const *argument = argv[ii];

        if (0 == strcmp(argument, HELP_OPTION)) {
            need_help = true;
        } else if (0 == strcmp(argument, VERSION_OPTION)) {
            need_version = true;
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
            return do_init(cwd, fstab_path, simulator_path, gui_path);
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

static uint_fast8_t do_init(linted_ko cwd, char const *fstab_path,
                            char const *simulator_path, char const *gui_path)
{
    linted_error errnum;

    static char const process_name[] = "init";

    /* The init. In the future it should reap all processes and
     * monitor the process monitor to restart it if it dies
     */
    if (-1 == prctl(PR_SET_NAME, (unsigned long)process_name, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    if (-1 == prctl(PR_SET_PDEATHSIG, (unsigned long)SIGKILL, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    pid_t child;
    {
        child = fork();
        if (-1 == child) {
            linted_io_write_format(STDERR_FILENO, NULL,
                                   "%s: can't clone unprivileged process: %s\n",
                                   process_name,
                                   linted_error_string_alloc(errno));
            return EXIT_FAILURE;
        }

        if (0 == child) {
            return do_monitor(cwd, fstab_path, simulator_path, gui_path);
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

static uint_fast8_t do_monitor(linted_ko cwd, char const *fstab_path,
                               char const *simulator_path, char const *gui_path)
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

    if (-1 == prctl(PR_SET_CHILD_SUBREAPER, 1L, 0UL, 0UL, 0UL)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    /* Acquire socket before unsharing namespaces */
    linted_manager new_connections;
    {
        linted_manager xx;
        if ((errnum = linted_manager_bind(&xx, BACKLOG, NULL, 0)) != 0) {
            goto exit_services;
        }
        new_connections = xx;
    }

    if ((errnum = check_db(cwd)) != 0) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
%s: database: %s\n",
                               process_name, linted_error_string_alloc(errnum));
        return EXIT_FAILURE;
    }

    if (-1
        == unshare(CLONE_NEWIPC | CLONE_NEWNET | CLONE_NEWUTS | CLONE_NEWNS)) {
        perror("unshare");
        return EXIT_FAILURE;
    }

    /* Favor other processes over this process hierarchy. Only
     * superuser may lower priorities so this is not stoppable. This
     * also makes the process hierarchy nicer for the OOM killer.
     */
    errno = 0;
    int priority = getpriority(PRIO_PROCESS, 0);
    if (-1 == priority) {
        errnum = errno;
        if (errnum != 0) {
            perror("getpriority");
            return EXIT_FAILURE;
        }
    }

    if (-1 == setpriority(PRIO_PROCESS, 0, priority + 1)) {
        perror("setpriority");
        return EXIT_FAILURE;
    }

    /* TODO: Close files leading outside of the sandbox  */
    FILE *fstab = setmntent(fstab_path, "re");
    if (NULL == fstab) {
        perror("setmtent");
        return EXIT_FAILURE;
    }

    {
        char const *runtime_dir_string = getenv("XDG_RUNTIME_DIR");
        if (NULL == runtime_dir_string) {
            /* TODO: Fallback somewhere smart */
            fputs("missing XDG_RUNTIME_DIR\n", stderr);
            return EXIT_FAILURE;
        }

        if (-1 == chdir(runtime_dir_string)) {
            perror("chdir");
            return EXIT_FAILURE;
        }
    }

    if (-1 == mkdir("linted", S_IRWXU)) {
        errnum = errno;
        if (errnum != EEXIST) {
            perror("mkdir");
            return EXIT_FAILURE;
        }
    }

    if (-1 == mount(NULL, "linted", "tmpfs", 0, NULL)) {
        perror("mount");
        return EXIT_FAILURE;
    }

    if (-1 == chdir("linted")) {
        perror("chdir");
        return EXIT_FAILURE;
    }

    for (;;) {
        errno = 0;
        struct mntent *entry = getmntent(fstab);
        if (NULL == entry) {
            errnum = errno;
            if (errnum != 0) {
                perror("getmntent");
                return EXIT_FAILURE;
            }

            break;
        }

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
        static char const *const tokens[]
            = {[BIND] = "bind",      [RBIND] = "rbind",
               [RO] = MNTOPT_RO,     [RW] = MNTOPT_RW,
               [SUID] = MNTOPT_SUID, [NOSUID] = MNTOPT_NOSUID,
               [NODEV] = "nodev",    [NOEXEC] = "noexec",
               NULL };
        bool bind = false;
        bool rec = false;
        bool readonly = false;
        bool readwrite = false;
        bool suid = true;
        bool dev = true;
        bool exec = true;
        char *leftovers = NULL;
        {
            char *mnt_opts = entry->mnt_opts;

            if (0 == strcmp("none", mnt_opts)) {
                goto mount;
            }

            char *subopts_str = strdup(mnt_opts);
            if (NULL == subopts_str) {
                perror("strdup");
                return EXIT_FAILURE;
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
                    leftovers = strstr(mnt_opts, value);
                    goto free_subopts_str;
                }
            }
        free_subopts_str:
            free(subopts_str);
        }

    mount:
        if (readwrite && readonly) {
            fprintf(stderr, "Only one of '%s' and '%s' can be specified\n",
                    tokens[RO], tokens[RW]);
            return EXIT_FAILURE;
        }

        if (bind && readonly) {
            fputs("\
Due to a completely idiotic kernel bug (see \
https://bugzilla.kernel.org/show_bug.cgi?id=24912) using a bind mount\
as readonly would fail completely silently and there is no way to \
workaround this\n",
                  stderr);
            return EXIT_FAILURE;
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

        if (-1 == mkdir(entry->mnt_dir, S_IRWXU)) {
            errnum = errno;
            if (errnum != EEXIST) {
                perror("mkdir");
                return EXIT_FAILURE;
            }
        }

        if (-1
            == mount(0 == strcmp("none", entry->mnt_fsname) ? NULL
                                                            : entry->mnt_fsname,
                     entry->mnt_dir, entry->mnt_type, mountflags, leftovers)) {
            perror("mount");
            return EXIT_FAILURE;
        }
    }

    if (endmntent(fstab) != 1) {
        perror("endmntent");
        return EXIT_FAILURE;
    }

    if (-1 == mount(".", "/", NULL, MS_MOVE, NULL)) {
        perror("mount");
        return EXIT_FAILURE;
    }

    if (-1 == chroot(".")) {
        perror("chroot");
        return EXIT_FAILURE;
    }

    if (-1 == chdir("/")) {
        perror("chdir");
        return EXIT_FAILURE;
    }

    if (-1 == setgroups(0u, NULL)) {
        perror("setgroups");
        return EXIT_FAILURE;
    }

    if (-1 == sethostname(PACKAGE_TARNAME, sizeof PACKAGE_TARNAME - 1u)) {
        perror("sethostname");
        return EXIT_FAILURE;
    }

    /* Prevent future privilege gains */
    if (-1 == prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        perror("prctl");
        return EXIT_FAILURE;
    }

    /* Drop all privileges I might possibly have. I'm not sure I need
     * to do this and I probably can do this in a better way. */
    for (size_t ii = 0u; ii < LINTED_ARRAY_SIZE(capabilities); ++ii) {
        if (-1 == prctl(PR_CAPBSET_DROP, capabilities[ii], 0, 0, 0)) {
            perror("prctl");
            return EXIT_FAILURE;
        }
    }

    char const *original_display = getenv("DISPLAY");
    if (NULL == original_display) {
        linted_io_write_format(STDERR_FILENO, NULL,
                               "%s: missing DISPLAY environment variable\n",
                               process_name);
        linted_locale_try_for_more_help(STDERR_FILENO, process_name,
                                        LINTED_STR(HELP_OPTION));
        return EXIT_FAILURE;
    }

    size_t display_value_length = strlen(original_display);
    size_t display_string_length = strlen("DISPLAY=") + display_value_length
                                   + 1u;
    char *display;
    {
        void *xx;
        if ((errnum = linted_mem_alloc(&xx, display_string_length)) != 0) {
            linted_io_write_format(
                STDERR_FILENO, NULL, "%s: can't allocate DISPLAY string: %s\n",
                process_name, linted_error_string_alloc(errnum));
            return EXIT_FAILURE;
        }
        display = xx;
    }
    memcpy(display, "DISPLAY=", strlen("DISPLAY="));
    memcpy(display + strlen("DISPLAY="), original_display,
           display_value_length);
    display[display_string_length - 1u] = '\0';

    errnum = linted_util_sanitize_environment();
    if (errnum != 0) {
        linted_io_write_format(STDERR_FILENO, NULL, "\
    %s: can not sanitize the environment: %s\n",
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

    union service_config const config[]
        = {[LINTED_SERVICE_INIT] = { .type = SERVICE_INIT },
           [LINTED_SERVICE_SIMULATOR]
               = { .process
                   = { .type = SERVICE_PROCESS,
                       .dirko = cwd,
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
                       .path = gui_path,
                       .arguments = (char const * const[]) { gui_path, NULL },
                       .environment = (char const * const[]) { display, NULL },
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

        size_t dup_pairs_size = proc_config->dup_pairs.size;
        linted_ko *proc_kos;
        {
            void *xx;
            if ((errnum = linted_mem_alloc_array(&xx, sizeof proc_kos[0u],
                                                 dup_pairs_size)) != 0) {
                goto destroy_attr;
            }
            proc_kos = xx;
        }
        size_t kos_opened = 0u;
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
                     &file_actions, ko, kos_opened - 1u)) != 0) {
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

        linted_io_write_str(STDOUT_FILENO, NULL,
                            LINTED_STR("management socket: "));
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

    for (;;) {
        struct linted_asynch_task *completed_tasks[20u];
        size_t task_count;
        {
            size_t xx;
            linted_asynch_pool_wait(pool, completed_tasks,
                                    LINTED_ARRAY_SIZE(completed_tasks), &xx);
            task_count = xx;
        }

        for (size_t ii = 0u; ii < task_count; ++ii) {
            if ((errnum = dispatch(completed_tasks[ii])) != 0) {
                goto close_connections;
            }

            if (-1 == gui_service->pid) {
                goto close_connections;
            }
        }
    }

close_connections : {
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
    linted_error kill_errnum = -1 == kill(-1, SIGKILL) ? errno : 0;
    /* kill_errnum == ESRCH is fine */
    assert(kill_errnum != EINVAL);
    assert(kill_errnum != EPERM);
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
        assert(false);
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
            assert(errnum != 0);
            return errnum;
        }
        break;

    case CLD_EXITED:
        if (pid == gui_service->pid || pid == sim_service->pid) {
            if (exit_status != 0) {
                errnum = exit_status;
                assert(errnum != 0);
                return errnum;
            }
            goto process_exited;
        }
        break;

    case CLD_STOPPED:
        /* Presumably the process was stopped for a reason */
        break;

    default:
        assert(false);
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

static linted_error on_write_connection(struct linted_asynch_task
                                        *completed_task)
{
    linted_error errnum;

    struct write_conn_task *write_conn_task
        = LINTED_DOWNCAST(struct write_conn_task, completed_task);
    struct connection_pool *connection_pool = write_conn_task->connection_pool;
    struct connection *connection = write_conn_task->connection;

    errnum = completed_task->errnum;

    {
        linted_error remove_errnum
            = connection_remove(connection, connection_pool);
        if (0 == errnum) {
            errnum = remove_errnum;
        }
    }

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
        size_t data_size = sizeof hello - 1u;

        struct linted_ko_task_write write_task;

        linted_ko_task_write(&write_task, TMP_WRITE_FINISHED, tmp, data,
                             data_size);
        linted_asynch_pool_submit(pool, LINTED_UPCAST(&write_task));

        struct linted_asynch_task *completed_tasks[20u];
        size_t task_count;
        {
            size_t xx;
            linted_asynch_pool_wait(pool, completed_tasks,
                                    LINTED_ARRAY_SIZE(completed_tasks), &xx);
            task_count = xx;
        }

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
