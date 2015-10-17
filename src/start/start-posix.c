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
#define LINTED_START__NO_MAIN 1

#include "config.h"

#include "linted/start.h"

#include "linted/async.h"
#include "linted/environment.h"
#include "linted/error.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/signal.h"
#include "linted/str.h"
#include "linted/util.h"

#include <errno.h>
#include <libgen.h>
#include <locale.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct start_args {
	pthread_t parent;
	unsigned char (*start)(char const *process_name, size_t argc,
	                       char const *const argv[]);
	char const *process_basename;
	size_t argc;
	char const *const *argv;
};

static void *start_routine(void *arg);
static void do_nothing(int signo);

int linted_start__main(struct linted_start_config const *config,
                       unsigned char (*start)(char const *process_name,
                                              size_t argc,
                                              char const *const argv[]),
                       int argc, char **argv)
{
	linted_error err = 0;

	for (;;) {
		linted_ko ko;
		{
			linted_ko xx;
			err =
			    linted_ko_open(&xx, LINTED_KO_CWD,
			                   "/dev/null", LINTED_KO_RDWR);
			if (err != 0)
				return EXIT_FAILURE;
			ko = xx;
		}

		if (ko > 2U) {
			linted_ko_close(ko);
			break;
		}
	}

	char const *process_name = 0;

	bool missing_name = false;

	char const *service;
	{
		char *xx;
		err = linted_environment_get("LINTED_SERVICE", &xx);
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
		err = linted_str_dup(&xx, process_name);
		if (err != 0)
			return EXIT_FAILURE;
		process_basename = xx;
	}
	process_basename = basename(process_basename);

	if (!config->dont_init_logging)
		linted_log_open(process_basename);

	if (missing_name) {
		linted_log(LINTED_LOG_ERROR, "missing process name");
		return EXIT_FAILURE;
	}

	{
		struct sigaction act = {0};
		sigemptyset(&act.sa_mask);
		act.sa_handler = do_nothing;
		act.sa_flags = 0;
		if (-1 == sigaction(LINTED_ASYNCH_SIGNO, &act, 0)) {
			linted_log(LINTED_LOG_ERROR, "sigaction: %s",
			           linted_error_string(errno));
			return EXIT_FAILURE;
		}
	}

	if (!config->dont_init_signals) {
		err = linted_signal_init();
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_signal_init: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	if (0 == setlocale(LC_ALL, "")) {
		linted_log(LINTED_LOG_ERROR, "setlocale: %s",
		           linted_error_string(errno));
		return EXIT_FAILURE;
	}

	tzset();

	struct start_args *start_args;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *start_args);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_mem_alloc: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		start_args = xx;
	}
	start_args->parent = pthread_self();
	start_args->start = start;
	start_args->process_basename = process_basename;
	start_args->argc = argc;
	start_args->argv = (char const *const *)argv;

	if (config->dont_fork_thread)
		return start(process_basename, argc,
		             (char const *const *)argv);

	pthread_t child;
	{
		pthread_t xx;
		err = pthread_create(&xx, 0, start_routine, start_args);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "pthread_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		child = xx;
	}

	err = pthread_detach(child);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "pthread_detach: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	pthread_exit(0);
}

static void *start_routine(void *foo)
{
	struct start_args *args = foo;

	pthread_t parent = args->parent;
	unsigned char (*start)(char const *process_name, size_t argc,
	                       char const *const argv[]) = args->start;
	char const *process_basename = args->process_basename;
	size_t argc = args->argc;
	char const *const *argv = args->argv;

	linted_mem_free(args);

	linted_error err = pthread_join(parent, 0);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "pthread_join: %s",
		           linted_error_string(err));
		exit(EXIT_FAILURE);
	}

	exit(start(process_basename, argc, argv));
}

static void do_nothing(int signo)
{
	/* Do nothing */
}
