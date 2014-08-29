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
#include "linted/log.h"
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
	NEW_CONNECTIONS,
	READ_CONNECTION,
	WRITE_CONNECTION
};

enum service_type {
	SERVICE_TYPE_INIT,
	SERVICE_TYPE_PROCESS,
	SERVICE_TYPE_FILE
};

enum {
	MAX_TASKS = READ_CONNECTION + MAX_MANAGE_CONNECTIONS
};

struct dup_pair
{
	unsigned long flags;
	size_t service;
};

struct dup_pairs
{
	size_t size;
	struct dup_pair const *dup_pairs;
};

#define DUP_PAIRS(...)                                                         \
	((struct dup_pairs const) { .size = LINTED_ARRAY_SIZE(__VA_ARGS__),    \
		                    .dup_pairs = __VA_ARGS__ })
struct service_config_init
{
	enum service_type type;
	char const *name;
};

struct service_config_process
{
	enum service_type type;
	char const *name;
	char const *path;
	char const *fstab;
	char const *const *arguments;
	char const *const *environment;
	struct dup_pairs dup_pairs;
	linted_ko dirko;
	bool halt_after_exit : 1U;
};

struct service_config_file
{
	enum service_type type;
	char const *name;
	linted_error (*generator)(linted_ko *kop);
};

union service_config
{
	enum service_type type;
	struct service_config_init init;
	struct service_config_process process;
	struct service_config_file file;
};

struct service_init
{
	enum service_type type;
	char const *name;
	pid_t pid;
};

struct service_process
{
	enum service_type type;
	char const *name;
	pid_t pid;
};

struct service_file
{
	enum service_type type;
	char const *name;
	linted_ko ko;
	bool is_open : 1U;
};

union service
{
	enum service_type type;
	struct service_init init;
	struct service_process process;
	struct service_file file;
};

struct wait_service_task
{
	struct linted_asynch_task_waitid parent;
	struct linted_asynch_pool *pool;
	pid_t halt_pid;
	bool time_to_exit : 1U;
};

struct connection;
struct connection_pool;

struct new_connection_task
{
	struct linted_manager_task_accept parent;
	struct linted_asynch_pool *pool;
	struct connection_pool *connection_pool;
	union service const *services;
	size_t services_size;
	union service_config const *config;
};

struct read_conn_task
{
	struct linted_manager_task_recv_request parent;
	struct linted_asynch_pool *pool;
	struct connection_pool *connection_pool;
	struct connection *connection;
	union service const *services;
	size_t services_size;
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

static char const *const gui_envvars_to_keep[] = {
	"DISPLAY",                  "LD_PRELOAD",
	"LD_LIBRARY_PATH",          "EGL_DRIVERS_PATH",
	"EGL_DRIVER",               "EGL_PLATFORM",
	"EGL_LOG_LEVEL",            "EGL_SOFTWARE",
	"LIBGL_DEBUG",              "LIBGL_DRIVERS_PATH",
	"LIBGL_ALWAYS_INDIRECT",    "LIBGL_ALWAYS_SOFTWARE",
	"LIBGL_NO_DRAWARRAYS",      "LIBGL_SHOW_FPS",
	"MESA_NO_ASM",              "MESA_NO_MMX",
	"MESA_NO_3DNOW",            "MESA_NO_SSE",
	"MESA_DEBUG",               "MESA_LOG_FILE",
	"MESA_TEX_PROG",            "MESA_TNL_PROG",
	"MESA_EXTENSION_OVERRIDE",  "MESA_EXTENSION_MAX_YEAR",
	"MESA_GL_VERSION_OVERRIDE", "MESA_GLSL_VERSION_OVERRIDE",
	"MESA_GLSL",                "MESA_RGB_VISUAL",
	"MESA_CI_VISUAL",           "MESA_BACK_BUFFER",
	"MESA_GAMMA",               "MESA_XSYNC",
	"MESA_GLX_FORCE_CI",        "MESA_GLX_FORCE_ALPHA",
	"MESA_GLX_DEPTH_BITS",      "MESA_GLX_ALPHA_BITS",
	"INTEL_NO_HW",              "INTEL_DEBUG",
	"RADEON_NO_TCL",            "GALLIUM_HUD",
	"GALLIUM_LOG_FILE",         "GALLIUM_PRINT_OPTIONS",
	"GALLIUM_DUMP_CPU",         "TGSI_PRINT_SANITY",
	"DRAW_FSE",                 "DRAW_NO_FSE",
	"DRAW_USE_LLVM",            "ST_DEBUG",
	"SOFTPIPE_DUMP_FS",         "SOFTPIPE_DUMP_GS",
	"SOFTPIPE_NO_RAST",         "SOFTPIPE_USE_LLVM",
	"LP_NO_RAST",               "LP_DEBUG",
	"LP_PERF",                  "LP_NUM_THREADS",
	"SVGA_FORCE_SWTNL",         "SVGA_NO_SWTNL",
	"SVGA_DEBUG",               NULL
};

static linted_error parse_fstab(struct linted_spawn_attr *attr, linted_ko cwd,
                                char const *fstab_path);

static linted_error get_flags_and_data(char const *opts, bool *mkdir_flagp,
                                       bool *touch_flagp,
                                       unsigned long *mountflagsp,
                                       char const **leftoversp);

static linted_error find_stdin(linted_ko *kop);
static linted_error find_stdout(linted_ko *kop);
static linted_error find_stderr(linted_ko *kop);

static linted_error log_create(linted_ko *kop);
static linted_error updater_create(linted_ko *kop);
static linted_error controller_create(linted_ko *kop);

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_new_connection(struct linted_asynch_task *task);
static linted_error on_process_wait(struct linted_asynch_task *task);
static linted_error on_read_connection(struct linted_asynch_task *task);
static linted_error on_write_connection(struct linted_asynch_task *task);

static linted_error dispatch_drainers(struct linted_asynch_task *task);

static linted_error drain_on_new_connection(struct linted_asynch_task *task);
static linted_error drain_on_process_wait(struct linted_asynch_task *task);
static linted_error drain_on_read_connection(struct linted_asynch_task *task);
static linted_error drain_on_write_connection(struct linted_asynch_task *task);

static linted_error check_db(linted_ko cwd);

static linted_error connection_pool_create(struct connection_pool **poolp);
static linted_error connection_pool_destroy(struct connection_pool *pool);
static linted_error connection_remove(struct connection *connection,
                                      struct connection_pool *connection_pool);

static union service const *service_for_name(union service const *services,
                                             size_t size, const char *name);

static linted_error filter_envvars(char ***resultsp,
                                   char const *const *allowed_envvars);

unsigned char linted_init_monitor(
    linted_ko cwd, char const *chrootdir_path, char const *logger_fstab_path,
    char const *simulator_fstab_path, char const *gui_fstab_path,
    char const *logger_path, char const *simulator_path, char const *gui_path)
{
	linted_error errnum;

	static char const process_name[] = "monitor";

	/* The process monitor and manager */
	if (-1 ==
	    prctl(PR_SET_NAME, (unsigned long)process_name, 0UL, 0UL, 0UL)) {
		perror("prctl");
		return EXIT_FAILURE;
	}

	if (-1 ==
	    prctl(PR_SET_PDEATHSIG, (unsigned long)SIGKILL, 0UL, 0UL, 0UL)) {
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
		errnum = linted_manager_bind(&xx, BACKLOG, NULL, 0);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_manager_bind");
			return EXIT_FAILURE;
		}
		new_connections = xx;
	}

	{
		char buf[LINTED_MANAGER_PATH_MAX];
		size_t len;
		errnum = linted_manager_path(new_connections, buf, &len);
		if (errnum != 0) {
			return errnum;
		}

		linted_io_write_str(STDOUT_FILENO, NULL,
		                    LINTED_STR("LINTED_SOCKET="));
		linted_io_write_all(STDOUT_FILENO, NULL, buf, len);
		linted_io_write_str(STDOUT_FILENO, NULL, LINTED_STR("\n"));
	}

	if ((errnum = check_db(cwd)) != 0) {
		linted_io_write_format(STDERR_FILENO, NULL, "\
%s: database: %s\n",
		                       process_name,
		                       linted_error_string(errnum));
		return EXIT_FAILURE;
	}

	char **gui_envvars;
	{
		char **xx;
		errnum = filter_envvars(&xx, gui_envvars_to_keep);
		if (errnum != 0) {
			errno = errnum;
			perror("filter_envvars");
			return EXIT_FAILURE;
		}
		gui_envvars = xx;
	}

	enum {
		SERVICE_INIT,
		SERVICE_LOGGER,
		SERVICE_SIMULATOR,
		SERVICE_GUI,
		SERVICE_STDIN,
		SERVICE_STDOUT,
		SERVICE_STDERR,
		SERVICE_LOG,
		SERVICE_UPDATER,
		SERVICE_CONTROLLER
	};

	union service_config const config[] =
	    {[SERVICE_INIT] = { .init = { .name = "init",
				          .type = SERVICE_TYPE_INIT } },
	     [SERVICE_LOGGER] = { .process = {
				          .name = "logger",
				          .type = SERVICE_TYPE_PROCESS,
				          .dirko = cwd,
				          .fstab = logger_fstab_path,
				          .path = logger_path,
				          .arguments =
				              (char const *
				               const[]) { logger_path, NULL },
				          .environment =
				              (char const * const[]) { NULL },
				          .dup_pairs = DUP_PAIRS(
				              (struct dup_pair const[]) {
					              { LINTED_KO_RDONLY,
						        SERVICE_STDIN },
					              { LINTED_KO_WRONLY,
						        SERVICE_STDOUT },
					              { LINTED_KO_WRONLY,
						        SERVICE_STDERR },
					              { LINTED_KO_RDONLY,
						        SERVICE_LOG }
					      })
				  } },
	     [SERVICE_SIMULATOR] = { .process =
			                 { .name = "simulator",
				           .type = SERVICE_TYPE_PROCESS,
				           .dirko = cwd,
				           .fstab = simulator_fstab_path,
				           .path = simulator_path,
				           .arguments = (char const * const[]) {
					           simulator_path, NULL
					   },
				           .environment =
				               (char const * const[]) { NULL },
				           .dup_pairs = DUP_PAIRS(
				               (struct dup_pair const[]) {
					               { LINTED_KO_RDONLY,
						         SERVICE_STDIN },
					               { LINTED_KO_WRONLY,
						         SERVICE_STDOUT },
					               { LINTED_KO_WRONLY,
						         SERVICE_STDERR },
					               { LINTED_KO_WRONLY,
						         SERVICE_LOG },
					               { LINTED_KO_RDONLY,
						         SERVICE_CONTROLLER },
					               { LINTED_KO_WRONLY,
						         SERVICE_UPDATER }
					       }) } },
	     [SERVICE_GUI] = { .process =
			           { .name = "gui",
				     .type = SERVICE_TYPE_PROCESS,
				     .halt_after_exit = true,
				     .dirko = cwd,
				     .fstab = gui_fstab_path,
				     .path = gui_path,
				     .arguments = (char const *
				                   const[]) { gui_path, NULL },
				     .environment =
				         (char const * const *)gui_envvars,
				     .dup_pairs = DUP_PAIRS((
				         struct dup_pair const[]) {
					     { LINTED_KO_RDONLY,
					       SERVICE_STDIN },
					     { LINTED_KO_WRONLY,
					       SERVICE_STDOUT },
					     { LINTED_KO_WRONLY,
					       SERVICE_STDERR },
					     { LINTED_KO_WRONLY, SERVICE_LOG },
					     { LINTED_KO_WRONLY,
					       SERVICE_CONTROLLER },
					     { LINTED_KO_RDONLY,
					       SERVICE_UPDATER }
				     }) } },
	     [SERVICE_STDIN] = { .file = { .name = "stdin",
				           .type = SERVICE_TYPE_FILE,
				           .generator = find_stdin } },
	     [SERVICE_STDOUT] = { .file = { .name = "stdout",
				            .type = SERVICE_TYPE_FILE,
				            .generator = find_stdout } },
	     [SERVICE_STDERR] = { .file = { .name = "stderr",
				            .type = SERVICE_TYPE_FILE,
				            .generator = find_stderr } },
	     [SERVICE_LOG] = { .file = { .name = "log",
				         .type = SERVICE_TYPE_FILE,
				         .generator = log_create } },
	     [SERVICE_UPDATER] = { .file = { .name = "updater",
				             .type = SERVICE_TYPE_FILE,
				             .generator = updater_create } },
	     [SERVICE_CONTROLLER] = { .file = { .name = "controller",
				                .type = SERVICE_TYPE_FILE,
				                .generator =
				                    controller_create } } };

	size_t services_size = 0U;
	union service *services = NULL;

	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(config); ++ii) {
		size_t new_services_size = services_size + 1U;
		{
			void *xx;
			errnum = linted_mem_realloc_array(
			         &xx, services, new_services_size,
			         sizeof services[0U]);
			if (errnum != 0) {
				errno = errnum;
				perror("linted_mem_alloc_array");
				return EXIT_FAILURE;
			}
			services = xx;
		}
		services_size = new_services_size;

		union service_config const *service_config = &config[ii];
		union service *service = &services[ii];
		switch (service_config->type) {
		case SERVICE_TYPE_INIT:
			service->type = SERVICE_TYPE_INIT;
			service->init.name = service_config->init.name;
			service->init.pid = getpid();
			break;

		case SERVICE_TYPE_PROCESS:
			service->type = SERVICE_TYPE_PROCESS;
			service->process.name = service_config->process.name;
			service->process.pid = -1;
			break;

		case SERVICE_TYPE_FILE:
			service->type = SERVICE_TYPE_FILE;
			service->file.name = service_config->file.name;
			service->file.is_open = false;
			break;
		}
	}

	struct connection_pool *connection_pool;

	{
		struct connection_pool *xx;
		errnum = connection_pool_create(&xx);
		if (errnum != 0) {
			return errnum;
		}
		connection_pool = xx;
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		errnum = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (errnum != 0) {
			return errnum;
		}
		pool = xx;
	}

	struct wait_service_task waiter_task;
	struct new_connection_task new_connection_task;

	linted_manager_accept(LINTED_UPCAST(&new_connection_task),
	                      NEW_CONNECTIONS, new_connections);
	new_connection_task.pool = pool;
	new_connection_task.connection_pool = connection_pool;
	new_connection_task.services = services;
	new_connection_task.services_size = services_size;
	new_connection_task.config = config;

	linted_asynch_pool_submit(
	    pool,
	    LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&new_connection_task))));

	for (size_t ii = 0U; ii < services_size; ++ii) {
		union service_config const *service_config = &config[ii];
		if (service_config->type != SERVICE_TYPE_FILE) {
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

	pid_t halt_pid = -1;

	for (size_t ii = 0U; ii < services_size; ++ii) {
		if (config[ii].type != SERVICE_TYPE_PROCESS) {
			continue;
		}

		struct service_process *service = &services[ii].process;
		struct service_config_process const *proc_config =
		    &config[ii].process;

		size_t dup_pairs_size = proc_config->dup_pairs.size;
		struct dup_pair const *dup_pairs =
		    proc_config->dup_pairs.dup_pairs;
		char const *fstab = proc_config->fstab;
		linted_ko dirko = proc_config->dirko;
		char const *const *environment = proc_config->environment;
		char const *const *arguments = proc_config->arguments;
		char const *path = proc_config->path;

		struct linted_spawn_file_actions *file_actions;
		struct linted_spawn_attr *attr;

		{
			struct linted_spawn_file_actions *xx;
			errnum = linted_spawn_file_actions_init(&xx);
			if (errnum != 0) {
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

		/* TODO: Close files leading outside of the sandbox  */
		linted_spawn_attr_drop_caps(attr);

		if (fstab != NULL) {
			linted_spawn_attr_setchrootdir(attr, chrootdir_path);

			if ((errnum = parse_fstab(attr, cwd, fstab)) != 0) {
				errno = errnum;
				perror("parse_fstab");
				return EXIT_FAILURE;
			}
		}

		linted_ko *proc_kos;
		{
			void *xx;
			errnum = linted_mem_alloc_array(&xx,
			                                sizeof proc_kos[0U],
			                                dup_pairs_size);
			if (errnum != 0) {
				goto destroy_attr;
			}
			proc_kos = xx;
		}
		size_t kos_opened = 0U;
		for (; kos_opened < dup_pairs_size;) {
			struct dup_pair const *dup_pair =
			    &dup_pairs[kos_opened];

			struct service_file const *file =
			    &services[dup_pair->service].file;

			linted_ko ko;
			{
				linted_ko xx;
				errnum = linted_ko_reopen(&xx, file->ko,
				                          dup_pair->flags);
				if (errnum !=  0) {
					goto destroy_proc_kos;
				}
				ko = xx;
			}

			proc_kos[kos_opened] = ko;
			++kos_opened;

			errnum = linted_spawn_file_actions_adddup2(&file_actions,
			                                           ko,
			                                           kos_opened - 1U);
			if (errnum != 0) {
				goto destroy_proc_kos;
			}
		}

		{
			pid_t process;
			errnum = linted_spawn(
			         &process, dirko, path, file_actions, attr,
			         (char **)arguments, (char **)environment);
			if (errnum != 0) {
				goto destroy_attr;
			}

			if (proc_config->halt_after_exit) {
				halt_pid = process;
			}

			service->pid = process;
		}

	destroy_proc_kos:
		for (size_t jj = 0; jj < kos_opened; ++jj) {
			linted_ko_close(proc_kos[jj]);
		}
		linted_mem_free(proc_kos);

	destroy_attr:
		linted_spawn_attr_destroy(attr);

	destroy_file_actions:
		linted_spawn_file_actions_destroy(file_actions);

		if (errnum != 0) {
			goto exit_services;
		}
	}

	linted_asynch_task_waitid(LINTED_UPCAST(&waiter_task), WAITER, P_ALL,
	                          -1, WEXITED);
	waiter_task.pool = pool;
	waiter_task.halt_pid = halt_pid;
	waiter_task.time_to_exit = false;

	linted_asynch_pool_submit(pool,
	                          LINTED_UPCAST(LINTED_UPCAST(&waiter_task)));

	while (!waiter_task.time_to_exit) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		if ((errnum = dispatch(completed_task)) != 0) {
			goto drain_dispatches;
		}
	}

drain_dispatches:
	linted_asynch_pool_stop(pool);

	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			if (EAGAIN == linted_asynch_pool_poll(pool, &xx)) {
				break;
			}
			completed_task = xx;
		}

		linted_error dispatch_error = dispatch_drainers(completed_task);
		if (0 == errnum) {
			errnum = dispatch_error;
		}
	}

	{
		linted_error close_errnum =
		    connection_pool_destroy(connection_pool);
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

	for (size_t ii = 0U; ii < services_size; ++ii) {
		union service_config const *service_config = &config[ii];

		if (SERVICE_STDERR == ii) {
			continue;
		}

		if (service_config->type != SERVICE_TYPE_FILE) {
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
		(void)new_connection_task;
	}

	{
		linted_error close_errnum = linted_ko_close(new_connections);
		if (0 == errnum) {
			errnum = close_errnum;
		}
	}

	if (errnum != 0) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "could not run the game: %s\n",
		                       linted_error_string(errnum));

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
		if (-1 ==
		    asprintf(&xx, "/proc/self/fd/%i/%s", cwd, fstab_path)) {
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

		if (0 == strcmp("none", opts)) {
			opts = NULL;
		}

		bool mkdir_flag;
		bool touch_flag;
		unsigned long mountflags;
		char const *data;
		if (NULL == opts) {
			mkdir_flag = false;
			touch_flag = false;
			mountflags = 0U;
			data = NULL;
		} else {
			bool xx;
			bool yy;
			unsigned long zz;
			char const *ww;
			errnum = get_flags_and_data(opts, &xx, &yy, &zz, &ww);
			if (errnum != 0) {
				goto close_file;
			}
			mkdir_flag = xx;
			touch_flag = yy;
			mountflags = zz;
			data = ww;
		}

		errnum = linted_spawn_attr_setmount(
		         attr, fsname, dir, type, mkdir_flag, touch_flag,
		         mountflags, data);
		if (errnum != 0) {
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

static linted_error get_flags_and_data(char const *opts, bool *mkdir_flagp,
                                       bool *touch_flagp,
                                       unsigned long *mountflagsp,
                                       char const **leftoversp)
{
	enum {
		MKDIR,
		TOUCH,
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

	static char const *const tokens[] =
	    {[MKDIR] = "mkdir",    [TOUCH] = "touch",        [BIND] = "bind",
	     [RBIND] = "rbind",    [RO] = MNTOPT_RO,         [RW] = MNTOPT_RW,
	     [SUID] = MNTOPT_SUID, [NOSUID] = MNTOPT_NOSUID, [NODEV] = "nodev",
	     [NOEXEC] = "noexec",  NULL };
	bool touch_flag = false;
	bool mkdir_flag = false;
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
		case MKDIR:
			mkdir_flag = true;
			break;

		case TOUCH:
			touch_flag = true;
			break;

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

	if (mkdir_flag && touch_flag) {
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
	*mkdir_flagp = mkdir_flag;
	*touch_flagp = touch_flag;
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

static linted_error log_create(linted_ko *kop)
{
	return linted_log_create(kop, 0U);
}

static linted_error updater_create(linted_ko *kop)
{
	return linted_updater_create(kop, 0U);
}

static linted_error controller_create(linted_ko *kop)
{
	return linted_controller_create(kop, 0U);
}

static linted_error dispatch(struct linted_asynch_task *task)
{
	switch (task->task_action) {
	case NEW_CONNECTIONS:
		return on_new_connection(task);

	case WAITER:
		return on_process_wait(task);

	case READ_CONNECTION:
		return on_read_connection(task);

	case WRITE_CONNECTION:
		return on_write_connection(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_process_wait(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0) {
		return errnum;
	}

	struct wait_service_task *wait_service_task =
	    LINTED_DOWNCAST(struct wait_service_task, task);

	struct linted_asynch_pool *pool = wait_service_task->pool;
	pid_t halt_pid = wait_service_task->halt_pid;

	int exit_status;
	int exit_code;
	pid_t pid;
	{
		siginfo_t *exit_info = &LINTED_UPCAST(wait_service_task)->info;
		exit_status = exit_info->si_status;
		exit_code = exit_info->si_code;
		pid = exit_info->si_pid;
	}

	linted_asynch_pool_submit(pool, task);

	switch (exit_code) {
	case CLD_DUMPED:
	case CLD_KILLED:
		raise(exit_status);
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;

	case CLD_EXITED:
		if (exit_status != 0) {
			errnum = exit_status;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		goto process_exited;

	case CLD_STOPPED:
		/* Presumably the process was stopped for a reason */
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}

	return 0;

process_exited:
	if (pid == halt_pid) {
		wait_service_task->time_to_exit = true;
	}

	return 0;
}

static linted_error on_new_connection(struct linted_asynch_task *completed_task)
{
	linted_error errnum;

	if ((errnum = completed_task->errnum) != 0) {
		return errnum;
	}

	struct new_connection_task *new_connection_task =
	    LINTED_DOWNCAST(struct new_connection_task, completed_task);

	struct linted_ko_task_accept *accept_task =
	    LINTED_UPCAST(LINTED_UPCAST(new_connection_task));

	struct linted_asynch_pool *pool = new_connection_task->pool;
	struct connection_pool *connection_pool =
	    new_connection_task->connection_pool;

	union service const *services = new_connection_task->services;
	size_t services_size = new_connection_task->services_size;
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
	LINTED_ASSUME_UNREACHABLE();
got_space:
	connection->ko = new_socket;

	linted_manager_recv_request(LINTED_UPCAST(&connection->read_task),
	                            READ_CONNECTION, new_socket);
	connection->read_task.pool = pool;
	connection->read_task.connection_pool = connection_pool;
	connection->read_task.connection = connection;
	connection->read_task.services = services;
	connection->read_task.services_size = services_size;
	connection->read_task.config = config;

	linted_asynch_pool_submit(
	    pool, LINTED_UPCAST(
	              LINTED_UPCAST(LINTED_UPCAST(&connection->read_task))));

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

static linted_error on_read_connection(struct linted_asynch_task *task)
{
	linted_error errnum;

	struct read_conn_task *read_conn_task =
	    LINTED_DOWNCAST(struct read_conn_task, task);

	struct linted_asynch_pool *pool = read_conn_task->pool;
	struct connection_pool *connection_pool =
	    read_conn_task->connection_pool;
	struct connection *connection = read_conn_task->connection;
	union service const *services = read_conn_task->services;
	size_t services_size = read_conn_task->services_size;

	if ((errnum = task->errnum) != 0) {
		/* The other end did something bad */
		errnum = connection_remove(connection, connection_pool);
		if (errnum != 0) {
			return errnum;
		}
		return 0;
	}

	struct linted_manager_task_recv_request *task_recv =
	    LINTED_UPCAST(read_conn_task);
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
		union service const *service = service_for_name(
		    services, services_size, request->status.service_name);
		if (NULL == service) {
			reply.status.is_up = false;
			break;
		}

		pid_t pid;
		switch (service->type) {
		case SERVICE_TYPE_INIT:
			pid = service->init.pid;
			goto status_service_process;

		case SERVICE_TYPE_PROCESS:
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
		union service const *service = service_for_name(
		    services, services_size, request->status.service_name);
		if (NULL == service) {
			reply.status.is_up = false;
			break;
		}

		pid_t pid;
		switch (service->type) {
		case SERVICE_TYPE_INIT:
			pid = service->init.pid;
			goto stop_service_process;

		case SERVICE_TYPE_PROCESS:
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
	    pool, LINTED_UPCAST(
	              LINTED_UPCAST(LINTED_UPCAST(&connection->write_task))));

	return 0;

connection_remove:
	connection_remove(connection, connection_pool);
	return errnum;
}

static linted_error on_write_connection(struct linted_asynch_task *task)
{
	linted_error errnum;

	struct write_conn_task *write_conn_task =
	    LINTED_DOWNCAST(struct write_conn_task, task);
	struct connection_pool *connection_pool =
	    write_conn_task->connection_pool;
	struct connection *connection = write_conn_task->connection;

	errnum = task->errnum;

	linted_error remove_errnum =
	    connection_remove(connection, connection_pool);
	if (0 == errnum) {
		errnum = remove_errnum;
	}

	return errnum;
}

static linted_error dispatch_drainers(struct linted_asynch_task *task)
{
	switch (task->task_action) {
	case NEW_CONNECTIONS:
		return drain_on_new_connection(task);

	case WAITER:
		return drain_on_process_wait(task);

	case READ_CONNECTION:
		return drain_on_read_connection(task);

	case WRITE_CONNECTION:
		return drain_on_write_connection(task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error drain_on_new_connection(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0) {
		return errnum;
	}

	struct new_connection_task *new_connection_task =
	    LINTED_DOWNCAST(struct new_connection_task, task);

	struct linted_ko_task_accept *accept_task =
	    LINTED_UPCAST(LINTED_UPCAST(new_connection_task));

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
		if ((errnum = linted_db_open(&xx, cwd, "linted-db",
		                             LINTED_DB_CREAT)) != 0) {
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
			if ((errnum = linted_db_temp_file(my_db, &xx, &yy)) !=
			    0) {
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

		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		if ((errnum = completed_task->errnum) != 0) {
			goto close_tmp;
		}

		switch (completed_task->task_action) {
		case TMP_WRITE_FINISHED:
			goto done_writing;

		default:
			LINTED_ASSUME_UNREACHABLE();
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

destroy_pool:
	linted_asynch_pool_stop(pool);

	{
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

static union service const *service_for_name(union service const *services,
                                             size_t size, const char *name)
{
	for (size_t ii = 0U; ii < size; ++ii) {
		switch (services[ii].type) {
		case SERVICE_TYPE_INIT:
			if (0 == strcmp(services[ii].init.name, name)) {
				return &services[ii];
			}
			break;

		case SERVICE_TYPE_PROCESS:
			if (0 == strcmp(services[ii].process.name, name)) {
				return &services[ii];
			}
			break;

		case SERVICE_TYPE_FILE:
			if (0 == strcmp(services[ii].file.name, name)) {
				return &services[ii];
			}
			break;
		}
	}

	return NULL;
}

static linted_error filter_envvars(char ***result_envvarsp,
                                   char const *const *allowed_envvars)
{
	size_t allowed_envvars_size;
	char **result_envvars;
	linted_error errnum;

	for (size_t ii = 0U;; ++ii) {
		if (NULL == allowed_envvars[ii]) {
			allowed_envvars_size = ii;
			break;
		}
	}

	{
		void *xx;
		if ((errnum = linted_mem_alloc_array(
		         &xx, allowed_envvars_size,
		         sizeof result_envvars[0U])) != 0) {
			return errnum;
		}
		result_envvars = xx;
	}

	size_t result_envvars_size = 0U;
	for (size_t ii = 0U; ii < allowed_envvars_size; ++ii) {
		char const *envvar_name = allowed_envvars[ii];

		char const *envvar_value = getenv(envvar_name);
		if (NULL == envvar_value) {
			continue;
		}

		++result_envvars_size;

		size_t envvar_name_length = strlen(envvar_name);
		size_t envvar_value_length = strlen(envvar_value);

		size_t assign_string_length =
		    envvar_name_length + 1U + envvar_value_length;

		char *assign_string;
		{
			void *xx;
			if ((errnum = linted_mem_alloc(
			         &xx, assign_string_length + 1U)) != 0) {
				goto free_result_envvars;
			}
			assign_string = xx;
		}
		memcpy(assign_string, envvar_name, envvar_name_length);
		assign_string[envvar_name_length] = '=';
		memcpy(assign_string + envvar_name_length + 1U, envvar_value,
		       envvar_value_length);
		assign_string[assign_string_length] = '\0';

		result_envvars[result_envvars_size - 1U] = assign_string;
	}

	{
		void *xx;
		if ((errnum = linted_mem_realloc_array(
		         &xx, result_envvars, result_envvars_size + 1U,
		         sizeof result_envvars[0U])) != 0) {
			goto free_result_envvars;
		}
		result_envvars = xx;
	}
	result_envvars[result_envvars_size] = NULL;

	*result_envvarsp = result_envvars;

	return 0;

free_result_envvars:
	for (size_t ii = 0U; ii < result_envvars_size; ++ii) {
		linted_mem_free(result_envvars[ii]);
	}
	linted_mem_free(result_envvars);
	return errnum;
}
