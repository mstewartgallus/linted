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

#include <arpa/inet.h>
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
	linted_error errnum = 0;

	linted_log log;
	{
		linted_ko xx;
		errnum = linted_ko_open(&xx, LINTED_KO_CWD, "/run/log",
		                        LINTED_KO_RDWR);
		if (errnum != 0) {
			errno = errnum;
			perror("linted_ko_open");
			return EXIT_FAILURE;
		}
		log = xx;
	}

	linted_ko inotify = inotify_init1(IN_CLOEXEC);
	if (-1 == inotify) {
		perror("inotify_init1");
		return EXIT_FAILURE;
	}

	{
		char path[] = "/proc/self/fd/XXXXXXXXXXXXX";
		sprintf(path, "/proc/self/fd/%i", log);
		int wd = inotify_add_watch(inotify, path, IN_MODIFY);
		if (-1 == wd) {
			perror("inotify_add_watch");
			return EXIT_FAILURE;
		}
	}

	off_t file_offset = lseek(log, 0U, SEEK_HOLE);
	for (;;) {
		{
			struct inotify_event event;
			if (-1 == read(inotify, &event, sizeof event)) {
				perror("read");
				return EXIT_FAILURE;
			}
		}

		uint32_t log_size;
		{
			uint32_t xx;
			ssize_t bytes_read = read(log, &xx, sizeof xx);
			if (bytes_read < 0) {
				perror("read");
				return EXIT_FAILURE;
			}
			if (0 == bytes_read)
				continue;
			if (bytes_read != sizeof xx) {
				fprintf(stderr, "%s: malformed log\n",
				        process_name);
				return EXIT_FAILURE;
			}
			log_size = ntohl(xx);
		}

		if (log_size > LINTED_LOG_MAX) {
			fprintf(stderr, "%s: malformed log\n", process_name);
			return EXIT_FAILURE;
		}

		ssize_t bytes_read = read(log, logger_buffer, log_size);
		if (-1 == bytes_read) {
			perror("read");
			return EXIT_FAILURE;
		}
		if (log_size != bytes_read) {
			fprintf(stderr, "%s: malformed log\n", process_name);
			return EXIT_FAILURE;
		}

		fprintf(stderr, "%s: %u ", process_name, log_size);
		fwrite(logger_buffer, 1U, log_size, stderr);
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
