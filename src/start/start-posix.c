/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define _POSIX_C_SOURCE 200809L

#include "config.h"

#define LNTD_START__NO_MAIN 1
#include "lntd/start.h"

#include "lntd/async.h"
#include "lntd/env.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/log.h"
#include "lntd/mem.h"
#include "lntd/path.h"
#include "lntd/signal.h"
#include "lntd/util.h"

#include <dirent.h>
#include <errno.h>
#include <locale.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#if defined HAVE_SYS_AUXV_H
#include <sys/auxv.h>
#endif

struct start_args {
	pthread_t parent;
	unsigned char (*start)(char const *process_name, size_t argc,
	                       char const *const argv[]);
	struct lntd_start_config const *config;
	size_t argc;
	char const *const *argv;
};

static void *start_routine(void *arg);

static int do_init(struct lntd_start_config const *config,
                   unsigned char (*start)(char const *process_name,
                                          size_t argc,
                                          char const *const argv[]),
                   size_t argc, char const *const *argv);
static void do_nothing(int signo);

static lntd_error open_standard_handles(void);
static lntd_error privilege_check(void);
static lntd_error sanitize_fds(void);

int lntd_start__main(struct lntd_start_config const *config,
                     unsigned char (*start)(char const *process_name,
                                            size_t argc,
                                            char const *const argv[]),
                     int argc, char **argv)
{
	if (config->dont_fork_thread)
		return do_init(config, start, argc,
		               (char const *const *)argv);

	lntd_error err = 0;

	static struct start_args start_args = {0};
	start_args.parent = pthread_self();
	start_args.start = start;
	start_args.config = config;
	start_args.argc = argc;
	start_args.argv = (char const *const *)argv;

	static pthread_t child;

	err = pthread_create(&child, 0, start_routine, &start_args);
	if (err != 0)
		return EXIT_FAILURE;

	pthread_exit(0);
}

static void *start_routine(void *foo)
{
	struct start_args *args = foo;

	pthread_t parent = args->parent;
	unsigned char (*start)(char const *process_name, size_t argc,
	                       char const *const argv[]) = args->start;
	struct lntd_start_config const *config = args->config;
	size_t argc = args->argc;
	char const *const *argv = args->argv;

	lntd_error err = pthread_join(parent, 0);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "pthread_join: %s",
		         lntd_error_string(err));
		exit(EXIT_FAILURE);
	}

	err = pthread_detach(pthread_self());
	if (err != 0)
		exit(EXIT_FAILURE);

	exit(do_init(config, start, argc, argv));
}

static int do_init(struct lntd_start_config const *config,
                   unsigned char (*start)(char const *process_name,
                                          size_t argc,
                                          char const *const argv[]),
                   size_t argc, char const *const *argv)
{
	lntd_error err = 0;

	err = open_standard_handles();
	if (err != 0) {
		return EXIT_FAILURE;
	}

	char const *process_name = 0;

	bool missing_name = false;

	char const *service;
	{
		char *xx;
		err = lntd_env_get("LINTED_SERVICE", &xx);
		if (err != 0)
			return EXIT_FAILURE;
		service = xx;
	}
	if (service != 0) {
		process_name = service;
	} else if (argc > 0) {
		process_name = argv[0U];
	} else {
		process_name = config->canonical_process_name;
		missing_name = true;
	}

	char *process_basename;
	{
		char *xx;
		err = lntd_path_base(&xx, process_name);
		if (err != 0)
			return EXIT_FAILURE;
		process_basename = xx;
	}

	if (config->sanitize_fds) {
		err = sanitize_fds();
		if (err != 0)
			return err;
	}

	lntd_log_open(process_basename);

	if (missing_name) {
		lntd_log(LNTD_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	if (config->check_privilege) {
		err = privilege_check();
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "privilege_check: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	{
		struct sigaction act = {0};
		sigemptyset(&act.sa_mask);
		act.sa_handler = do_nothing;
		act.sa_flags = 0;
		if (-1 == sigaction(LNTD_ASYNCH_SIGNO, &act, 0)) {
			lntd_log(LNTD_LOG_ERROR, "sigaction: %s",
			         lntd_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (!config->dont_init_signals) {
		err = lntd_signal_init();
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR, "lntd_signal_init: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (0 == setlocale(LC_ALL, "")) {
		lntd_log(LNTD_LOG_ERROR, "setlocale: %s",
		         lntd_error_string(errno));
		return EXIT_FAILURE;
	}

	tzset();

	return start(process_basename, argc, argv);
}

static void do_nothing(int signo)
{
	/* Do nothing */
}

static lntd_error open_standard_handles(void)
{
	lntd_error err = 0;

	for (;;) {
		lntd_ko ko;
		{
			lntd_ko xx;
			err = lntd_ko_open(&xx, LNTD_KO_CWD,
			                   "/dev/null", LNTD_KO_RDWR);
			if (err != 0)
				break;
			ko = xx;
		}

		if (ko > 2U) {
			err = lntd_ko_close(ko);
			break;
		}
	}

	return err;
}

static lntd_error privilege_check(void)
{
	uid_t uid = getuid();
	if (0 == uid)
		return EPERM;

	gid_t gid = getgid();
	if (0 == gid)
		return EPERM;

#if defined HAVE_SYS_AUXV_H && defined HAVE_GETAUXVAL &&               \
    defined AT_SECURE
	if (getauxval(AT_SECURE))
		return EPERM;
#endif

	return 0;
}

static lntd_error sanitize_fds(void)
{
	lntd_error err = 0;

	lntd_ko fds_dir_ko;
	{
		lntd_ko xx;
		err = lntd_ko_open(&xx, LNTD_KO_CWD,
		                   "/proc/thread-self/fd",
		                   LNTD_KO_DIRECTORY);
		if (err != 0)
			return err;
		fds_dir_ko = xx;
	}

	DIR *fds_dir = fdopendir(fds_dir_ko);
	if (0 == fds_dir) {
		err = errno;
		LNTD_ASSUME(err != 0);

		lntd_ko_close(fds_dir_ko);

		return err;
	}

	lntd_ko *kos_to_close = 0;
	size_t num_kos_to_close = 0U;
	/* Read all the open fds first and then close the fds
	 * after because otherwise there is a race condition */
	for (;;) {
		errno = 0;
		struct dirent *direntry = readdir(fds_dir);
		if (0 == direntry) {
			err = errno;
			break;
		}

		char const *fdname = direntry->d_name;

		if (0 == strcmp(".", fdname))
			continue;
		if (0 == strcmp("..", fdname))
			continue;

		lntd_ko open_fd = (lntd_ko)atoi(fdname);
		if (0U == open_fd)
			continue;
		if (1U == open_fd)
			continue;
		if (2U == open_fd)
			continue;

		if (fds_dir_ko == open_fd)
			continue;

		{
			void *xx;
			err = lntd_mem_realloc_array(
			    &xx, kos_to_close, num_kos_to_close + 1U,
			    sizeof kos_to_close[0U]);
			if (err != 0)
				break;
			kos_to_close = xx;
		}
		kos_to_close[num_kos_to_close] = open_fd;
		++num_kos_to_close;
	}

	if (-1 == closedir(fds_dir)) {
		err = errno;
		LNTD_ASSUME(err != 0);
	}

	/* Deliberately don't check the closed fds */
	for (size_t ii = 0U; ii < num_kos_to_close; ++ii)
		lntd_ko_close(kos_to_close[ii]);
	lntd_mem_free(kos_to_close);

	return err;
}
