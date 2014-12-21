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

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/start.h"
#include "linted/util.h"

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/inotify.h>
#include <unistd.h>

enum { ON_RECEIVE_LOG, MAX_TASKS };

struct logger_data
{
	struct linted_asynch_pool *pool;
	char const *process_name;
	linted_ko log_ko;
};

struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-logger",
	.kos_size = 0U,
	.kos = NULL
};

static char logger_buffer[LINTED_LOG_MAX];

unsigned char linted_start(char const *const process_name, size_t argc,
                           char const *const argv[const])
{
	linted_log log = open("log/log", O_RDWR | O_CLOEXEC | O_CREAT, S_IRWXU);
	if (-1 == log) {
		perror("socket");
		return EXIT_FAILURE;
	}

	int inotify = inotify_init1(IN_CLOEXEC);
	if (-1 == inotify) {
		perror("inotify_init1");
		return EXIT_FAILURE;
	}

	int wd = inotify_add_watch(inotify, "log/log", IN_MODIFY);
	if (-1 == wd) {
		perror("inotify_add_watch");
		return EXIT_FAILURE;
	}

	off_t file_offset = lseek(log, 0U, SEEK_HOLE);
	for (;;) {
		struct inotify_event event;
		if (-1 == read(inotify, &event, sizeof event)) {
			perror("read");
			return EXIT_FAILURE;
		}

		ssize_t bytes_read =
		    read(log, logger_buffer, sizeof logger_buffer);
		if (-1 == bytes_read) {
			perror("read");
			return EXIT_FAILURE;
		}
		if (0 == bytes_read)
			continue;

		fprintf(stderr, "%s: ", process_name);
		fwrite(logger_buffer, 1U, bytes_read, stderr);
		fprintf(stderr, "\n");
		fflush(stderr);

		file_offset += (size_t)bytes_read;
		if (-1 == fallocate(log,
		                    FALLOC_FL_KEEP_SIZE | FALLOC_FL_PUNCH_HOLE,
		                    0U, file_offset + 1U)) {
			perror("fallocate");
			return EXIT_FAILURE;
		}
	}

	return EXIT_SUCCESS;
}
