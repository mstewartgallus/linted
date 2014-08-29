/*
 * Copyright 2014 Steven Stewart-Gallus
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

#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/locale.h"
#include "linted/manager.h"
#include "linted/mem.h"
#include "linted/start.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <sys/syscall.h>

#include <linux/filter.h>
#include <linux/seccomp.h>

static struct sock_fprog const seccomp_filter;
struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-control",
	.open_current_working_directory = false,
	.kos_size = 0U,
	.kos = NULL,
	.seccomp_bpf = &seccomp_filter
};

static uint_fast8_t run_reboot(char const *process_name, size_t argc,
                               char const *const argv[const]);
static uint_fast8_t run_status(char const *process_name, size_t argc,
                               char const *const argv[const]);
static uint_fast8_t run_stop(char const *process_name, size_t argc,
                             char const *const argv[const]);

static linted_error reboot_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);
static linted_error ctl_help(linted_ko ko, char const *process_name,
                             struct linted_str package_name,
                             struct linted_str package_url,
                             struct linted_str package_bugreport);
static linted_error status_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport);
static linted_error stop_help(linted_ko ko, char const *process_name,
                              struct linted_str package_name,
                              struct linted_str package_url,
                              struct linted_str package_bugreport);
static linted_error failure(linted_ko ko, char const *process_name,
                            struct linted_str message, linted_error errnum);

uint_fast8_t linted_start(int cwd, char const *const process_name, size_t argc,
                          char const *const argv[const])
{
	bool need_help = false;
	bool need_version = false;

	char const *bad_option = NULL;
	char const *command = NULL;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			command = argument;
			break;
		}
	}
	++last_index;

	if (need_help) {
		ctl_help(STDOUT_FILENO, process_name, LINTED_STR(PACKAGE_NAME),
		         LINTED_STR(PACKAGE_URL),
		         LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != NULL) {
		linted_locale_on_bad_option(STDERR_FILENO, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	if (NULL == command) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: missing COMMAND\n", process_name);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	enum {
		REBOOT,
		STATUS,
		STOP
	};

	static char const *const commands[] = {[REBOOT] = "reboot",
		                               [STATUS] = "status",
		                               [STOP] = "stop" };

	int arg = -1;
	for (size_t ii = 0U; ii < LINTED_ARRAY_SIZE(commands); ++ii) {
		if (0 == strcmp(command, commands[ii])) {
			arg = ii;
			break;
		}
	}

	size_t new_argc = argc - last_index + 1U;
	char const *const *new_argv = argv + last_index - 1U;
	switch (arg) {
	case REBOOT:
		return run_reboot(process_name, new_argc, new_argv);

	case STATUS:
		return run_status(process_name, new_argc, new_argv);

	case STOP:
		return run_stop(process_name, new_argc, new_argv);
	}

	linted_io_write_format(STDERR_FILENO, NULL,
	                       "%s: unrecognized command '%s'\n", process_name,
	                       command);
	linted_locale_try_for_more_help(STDERR_FILENO, process_name,
	                                LINTED_STR("--help"));
	return EXIT_FAILURE;
}

#define ALLOW(XX)                                                              \
	BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, __NR_##XX, 0U, 1U),                \
	    BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW)

static struct sock_filter const real_filter[] = {
	/*  */ BPF_STMT(BPF_LD | BPF_W | BPF_ABS,
	                offsetof(struct seccomp_data, nr)),
	/*  */ ALLOW(access),
	/*  */ ALLOW(arch_prctl),
	/*  */ ALLOW(brk),
	/*  */ ALLOW(chdir),
	/*  */ ALLOW(close),
	/*  */ ALLOW(connect),
	/*  */ ALLOW(dup2),
	/*  */ ALLOW(execve),
	/*  */ ALLOW(exit_group),
	/*  */ ALLOW(fcntl),
	/*  */ ALLOW(fstat),
	/*  */ ALLOW(futex),
	/*  */ ALLOW(getdents),
	/*  */ ALLOW(geteuid),
	/*  */ ALLOW(getrlimit),
	/*  */ ALLOW(getuid),
	/*  */ ALLOW(lseek),
	/*  */ ALLOW(mmap),
	/*  */ ALLOW(mprotect),
	/*  */ ALLOW(munmap),
	/*  */ ALLOW(open),
	/*  */ ALLOW(openat),
	/*  */ ALLOW(prctl),
	/*  */ ALLOW(read),
	/*  */ ALLOW(restart_syscall),
	/*  */ ALLOW(rt_sigaction),
	/*  */ ALLOW(rt_sigprocmask),
	/*  */ ALLOW(rt_sigtimedwait),
	/*  */ ALLOW(set_robust_list),
	/*  */ ALLOW(set_tid_address),
	/*  */ ALLOW(socket),
	/*  */ ALLOW(stat),
	/*  */ ALLOW(write),
	/*  */ BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL)
};

static struct sock_fprog const seccomp_filter = {
	.len = LINTED_ARRAY_SIZE(real_filter),
	.filter = (struct sock_filter *)real_filter
};

static uint_fast8_t run_reboot(char const *process_name, size_t argc,
                               char const *const argv[const])
{
	bool need_version = false;
	bool need_add_help = false;
	char const *bad_option = NULL;
	char const *bad_argument = NULL;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_add_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			bad_argument = argument;
			break;
		}
	}

	if (need_add_help) {
		reboot_help(STDOUT_FILENO, process_name,
		            LINTED_STR(PACKAGE_NAME), LINTED_STR(PACKAGE_URL),
		            LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != NULL) {
		linted_locale_on_bad_option(STDERR_FILENO, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (bad_argument != NULL) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	char const *path = getenv("LINTED_SOCKET");
	if (NULL == path) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: missing LINTED_SOCKET\n",
		                       process_name);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t path_len = strlen(path);
	if (path_len > LINTED_MANAGER_PATH_MAX - 1U) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: LINTED_SOCKET is too long\n",
		                       process_name);
		return EXIT_FAILURE;
	}

	linted_error errnum;
	linted_manager linted;

	{
		linted_manager manager;
		errnum = linted_manager_connect(&manager, path, path_len);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not create socket"), errnum);
			return EXIT_FAILURE;
		}
		linted = manager;
	}

	{
		union linted_manager_request request = { 0 };

		request.type = LINTED_MANAGER_REBOOT;

		linted_io_write_format(STDOUT_FILENO, NULL,
		                       "%s: sending the reboot request\n",
		                       process_name);

		errnum = linted_manager_send_request(linted, &request);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not send request"), errnum);
			return EXIT_FAILURE;
		}
	}

	{
		union linted_manager_reply reply;
		size_t bytes_read;

		errnum = linted_manager_recv_reply(linted, &reply, &bytes_read);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not read reply"), errnum);
			return EXIT_FAILURE;
		}

		if (0U == bytes_read) {
			linted_io_write_format(STDERR_FILENO, NULL,
			                       "%s: socket hung up\n",
			                       process_name);
			return EXIT_FAILURE;
		}

		/* Sent malformed input */
		if (bytes_read != sizeof reply) {
			linted_io_write_format(STDERR_FILENO, NULL,
			                       "%s: reply was too small: %i\n",
			                       process_name, bytes_read);
			return EXIT_FAILURE;
		}
	}

	return EXIT_SUCCESS;
}

static uint_fast8_t run_status(char const *process_name, size_t argc,
                               char const *const argv[const])
{
	bool need_version = false;
	bool need_add_help = false;
	char const *service_name = NULL;
	char const *bad_option = NULL;
	char const *bad_argument = NULL;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_add_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			if (service_name != NULL) {
				bad_argument = argument;
				break;
			} else {
				service_name = argument;
			}
		}
	}

	if (need_add_help) {
		status_help(STDOUT_FILENO, process_name,
		            LINTED_STR(PACKAGE_NAME), LINTED_STR(PACKAGE_URL),
		            LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != NULL) {
		linted_locale_on_bad_option(STDERR_FILENO, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (bad_argument != NULL) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	char const *path = getenv("LINTED_SOCKET");
	if (NULL == path) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: missing LINTED_SOCKET\n",
		                       process_name);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t path_len = strlen(path);
	if (path_len > LINTED_MANAGER_PATH_MAX - 1U) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: LINTED_SOCKET is too long\n",
		                       process_name);
		return EXIT_FAILURE;
	}

	linted_error errnum;

	if (NULL == service_name) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: missing SERVICE\n", process_name);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t service_name_len = strlen(service_name);
	if (service_name_len > LINTED_SERVICE_NAME_MAX) {
		failure(STDERR_FILENO, process_name, LINTED_STR("SERVICE"),
		        EINVAL);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	linted_manager linted;

	{
		linted_manager manager;
		errnum = linted_manager_connect(&manager, path, path_len);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not create socket"), errnum);
			return EXIT_FAILURE;
		}
		linted = manager;
	}

	{
		union linted_manager_request request = { 0 };

		request.type = LINTED_MANAGER_STATUS;
		request.status.size = service_name_len;
		memcpy(request.status.service_name, service_name,
		       service_name_len);

		linted_io_write_format(
		    STDOUT_FILENO, NULL,
		    "%s: sending the status request for %s\n", process_name,
		    service_name);

		errnum = linted_manager_send_request(linted, &request);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not send request"), errnum);
			return EXIT_FAILURE;
		}
	}

	{
		union linted_manager_reply reply;
		size_t bytes_read;
		errnum = linted_manager_recv_reply(linted, &reply, &bytes_read);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not read reply"), errnum);
			return EXIT_FAILURE;
		}

		if (0U == bytes_read) {
			linted_io_write_format(STDERR_FILENO, NULL,
			                       "%s: socket hung up\n",
			                       process_name);
			return EXIT_FAILURE;
		}

		/* Sent malformed input */
		if (bytes_read != sizeof reply) {
			linted_io_write_format(STDERR_FILENO, NULL,
			                       "%s: reply was too small: %i\n",
			                       process_name, bytes_read);
			return EXIT_FAILURE;
		}

		if (reply.status.is_up) {
			linted_io_write_format(STDOUT_FILENO, NULL,
			                       "%s: %s is up\n", process_name,
			                       service_name);
		} else {
			linted_io_write_format(STDOUT_FILENO, NULL,
			                       "%s: %s is down\n", process_name,
			                       service_name);
		}
	}

	return EXIT_SUCCESS;
}

static uint_fast8_t run_stop(char const *process_name, size_t argc,
                             char const *const argv[const])
{
	bool need_version = false;
	bool need_add_help = false;
	char const *bad_option = NULL;
	char const *bad_argument = NULL;
	size_t last_index = 1U;
	for (; last_index < argc; ++last_index) {
		char const *argument = argv[last_index];

		if (0 == strncmp(argument, "--", strlen("--"))) {
			if (0 == strcmp(argument, "--help")) {
				need_add_help = true;
			} else if (0 == strcmp(argument, "--version")) {
				need_version = true;
			} else {
				bad_option = argument;
			}
		} else {
			bad_argument = argument;
			break;
		}
	}

	if (need_add_help) {
		stop_help(STDOUT_FILENO, process_name, LINTED_STR(PACKAGE_NAME),
		          LINTED_STR(PACKAGE_URL),
		          LINTED_STR(PACKAGE_BUGREPORT));
		return EXIT_SUCCESS;
	}

	if (bad_option != NULL) {
		linted_locale_on_bad_option(STDERR_FILENO, process_name,
		                            bad_option);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (bad_argument != NULL) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: too many arguments: '%s'\n",
		                       process_name, bad_argument);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	if (need_version) {
		linted_locale_version(STDOUT_FILENO, LINTED_STR(PACKAGE_STRING),
		                      LINTED_STR(COPYRIGHT_YEAR));
		return EXIT_SUCCESS;
	}

	char const *path = getenv("LINTED_SOCKET");
	if (NULL == path) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: missing LINTED_SOCKET\n",
		                       process_name);
		linted_locale_try_for_more_help(STDERR_FILENO, process_name,
		                                LINTED_STR("--help"));
		return EXIT_FAILURE;
	}

	size_t path_len = strlen(path);
	if (path_len > LINTED_MANAGER_PATH_MAX - 1U) {
		linted_io_write_format(STDERR_FILENO, NULL,
		                       "%s: LINTED_SOCKET is too long\n",
		                       process_name);
		return EXIT_FAILURE;
	}

	linted_manager linted;

	{
		linted_manager manager;
		linted_error errnum =
		    linted_manager_connect(&manager, path, path_len);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not create socket"), errnum);
			return EXIT_FAILURE;
		}
		linted = manager;
	}

	{
		union linted_manager_request request = { 0 };

		request.type = LINTED_MANAGER_STOP;
		request.stop.size = sizeof "gui" - 1U;
		memcpy(request.stop.service_name, "gui", sizeof "gui" - 1U);

		linted_io_write_format(
		    STDOUT_FILENO, NULL,
		    "%s: sending the stop request for the gui\n", process_name);

		linted_error errnum =
		    linted_manager_send_request(linted, &request);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can send request"), errnum);
			return EXIT_FAILURE;
		}
	}

	{
		union linted_manager_reply reply;
		size_t bytes_read;
		linted_error errnum =
		    linted_manager_recv_reply(linted, &reply, &bytes_read);
		if (errnum != 0) {
			failure(STDERR_FILENO, process_name,
			        LINTED_STR("can not read reply"), errnum);
			return EXIT_FAILURE;
		}

		if (0U == bytes_read) {
			linted_io_write_format(STDERR_FILENO, NULL,
			                       "%s: socket hung up\n",
			                       process_name);
			return EXIT_FAILURE;
		}

		if (reply.stop.was_up) {
			linted_io_write_format(STDOUT_FILENO, NULL,
			                       "%s: gui was killed\n",
			                       process_name);
		} else {
			linted_io_write_format(STDOUT_FILENO, NULL,
			                       "%s: the gui was not killed\n",
			                       process_name);
		}
	}

	return EXIT_SUCCESS;
}

static linted_error ctl_help(linted_ko ko, char const *process_name,
                             struct linted_str package_name,
                             struct linted_str package_url,
                             struct linted_str package_bugreport)
{
	linted_error errnum;

	size_t size = 0U;
	size_t capacity = 0U;
	char *buffer = NULL;

	errnum = linted_str_append_str(&buffer, &capacity, &size,
	                               LINTED_STR("Usage: "));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_cstring(&buffer, &capacity, &size,
                                   process_name);
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
 [OPTIONS]\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
Run the manager program.\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                LINTED_STR("\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                LINTED_STR("\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
  status              request the status of the gui service\n\
  stop                stop the gui service\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
                                LINTED_STR("\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
Report bugs to <"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
	                               package_bugreport);
	if (errnum) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
                               LINTED_STR(">\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
	                               package_name);
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size, LINTED_STR("\
 home page: <"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
	                                package_url);
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_str_append_str(&buffer, &capacity, &size,
                               LINTED_STR(">\n"));
	if (errnum != 0) {
		goto free_buffer;
	}

	errnum = linted_io_write_all(ko, NULL, buffer, size);
	if (errnum != 0) {
		goto free_buffer;
	}

free_buffer:
	linted_mem_free(buffer);
	return errnum;
}

static linted_error reboot_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(
		ko, NULL, LINTED_STR("Usage: LINTED_SOCKET=SOCKET "));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_string(ko, NULL, process_name);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(" reboot [OPTIONS]\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Reboot the container.\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  SOCKET              the socket to connect to\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Report bugs to <"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, package_bugreport);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, package_name);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
 home page: <"));
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, package_url);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0) {
		return errnum;
	}

	return 0;
}

static linted_error status_help(linted_ko ko, char const *process_name,
                                struct linted_str package_name,
                                struct linted_str package_url,
                                struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(ko, NULL,
	                             LINTED_STR("Usage: LINTED_SOCKET=SOCKET "));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_string(ko, NULL, process_name);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(
		ko, NULL, LINTED_STR(" status [OPTIONS] SERVICE\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
Report the status.\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  SOCKET              the socket to connect to\n\
  SERVICE             the service to get the status of\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("Report bugs to <"));
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, package_bugreport);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, package_name);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(" home page: <"));
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, package_url);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0) {
		return errnum;
	}

	return 0;
}

static linted_error stop_help(linted_ko ko, char const *process_name,
                              struct linted_str package_name,
                              struct linted_str package_url,
                              struct linted_str package_bugreport)
{
	linted_error errnum;

	errnum = linted_io_write_str(ko, NULL,
	                             LINTED_STR("Usage: LINTED_SOCKET=SOCKET "));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_string(ko, NULL, process_name);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(" stop [OPTIONS]\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("Stop a service.\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  --help              display this help and exit\n\
  --version           display version information and exit\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\
  SOCKET              the socket to connect to\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("Report bugs to <"));
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, package_bugreport);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, package_name);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(" home page: <"));
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, package_url);
	if (errnum != 0) {
		return errnum;
	}
	errnum = linted_io_write_str(ko, NULL, LINTED_STR(">\n"));
	if (errnum != 0) {
		return errnum;
	}

	return 0;
}

static linted_error failure(linted_ko ko, char const *process_name,
                            struct linted_str message, linted_error error)
{
	linted_error errnum;

	errnum = linted_io_write_string(ko, NULL, process_name);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(": "));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, message);
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR(": "));
	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_string(ko, NULL, linted_error_string(error));

	if (errnum != 0) {
		return errnum;
	}

	errnum = linted_io_write_str(ko, NULL, LINTED_STR("\n"));
	if (errnum != 0) {
		return errnum;
	}

	return 0;
}
