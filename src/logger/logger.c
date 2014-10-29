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

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/io.h"
#include "linted/ko.h"
#include "linted/log.h"
#include "linted/start.h"
#include "linted/util.h"

#include <errno.h>
#include <stdbool.h>
#include <unistd.h>

enum {
	ON_RECEIVE_LOG,
	MAX_TASKS
};

struct logger_task
{
	struct linted_log_task_receive parent;
	struct linted_asynch_pool *pool;
	char const *process_name;
	linted_ko log_ko;
};

static linted_ko kos[1U];
struct linted_start_config const linted_start_config = {
	.canonical_process_name = PACKAGE_NAME "-logger",
	.kos_size = LINTED_ARRAY_SIZE(kos),
	.kos = kos,
	.seccomp_bpf = NULL
};

static char logger_buffer[LINTED_LOG_MAX];

static linted_error dispatch(struct linted_asynch_task *completed_task);

static linted_error on_receive_log(struct linted_asynch_task *completed_task);

unsigned char linted_start(char const *const process_name, size_t argc,
                           char const *const argv[const])
{
	linted_log log = kos[0U];

	linted_error errnum;

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		if ((errnum = linted_asynch_pool_create(&xx, MAX_TASKS)) != 0) {
			goto exit;
		}
		pool = xx;
	}

	struct logger_task logger_task;

	linted_log_receive(LINTED_UPCAST(&logger_task), ON_RECEIVE_LOG, log,
	                   logger_buffer);
	logger_task.log_ko = STDERR_FILENO;
	logger_task.process_name = process_name;
	logger_task.pool = pool;

	linted_asynch_pool_submit(
	    pool, LINTED_UPCAST(LINTED_UPCAST(LINTED_UPCAST(&logger_task))));

	/* TODO: Detect SIGTERM and exit normally */
	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			linted_asynch_pool_wait(pool, &xx);
			completed_task = xx;
		}

		if ((errnum = dispatch(completed_task)) != 0)
			goto destroy_pool;
	}

destroy_pool : {
	linted_asynch_pool_stop(pool);

	/* TODO: Print left over messages */
	for (;;) {
		struct linted_asynch_task *completed_task;
		linted_error poll_errnum;
		{
			struct linted_asynch_task *xx;
			poll_errnum = linted_asynch_pool_poll(pool, &xx);
			if (EAGAIN == poll_errnum) {
				break;
			}
			completed_task = xx;
		}

		linted_error dispatch_errnum = completed_task->errnum;
		if (0 == errnum) {
			errnum = dispatch_errnum;
		}
	}

	linted_error destroy_errnum = linted_asynch_pool_destroy(pool);
	if (0 == errnum)
		errnum = destroy_errnum;
	/* Insure that the tasks are in proper scope until they are
	 * terminated */
	(void)logger_task;
}

exit:
	return errnum;
}

static linted_error dispatch(struct linted_asynch_task *completed_task)
{
	switch (completed_task->task_action) {
	case ON_RECEIVE_LOG:
		return on_receive_log(completed_task);

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static linted_error on_receive_log(struct linted_asynch_task *task)
{
	linted_error errnum;

	if ((errnum = task->errnum) != 0)
		return errnum;

	struct logger_task *logger_task =
	    LINTED_DOWNCAST(struct logger_task, task);

	struct linted_asynch_pool *pool = logger_task->pool;
	linted_ko log_ko = logger_task->log_ko;
	char const *process_name = logger_task->process_name;
	size_t log_size = LINTED_UPCAST(LINTED_UPCAST(logger_task))->bytes_read;
	char const *buf = LINTED_UPCAST(LINTED_UPCAST(logger_task))->buf;

	linted_io_write_string(log_ko, NULL, process_name);
	linted_io_write_str(log_ko, NULL, LINTED_STR(": "));
	linted_io_write_all(log_ko, NULL, buf, log_size);
	linted_io_write_str(log_ko, NULL, LINTED_STR("\n"));

	linted_asynch_pool_submit(pool, task);

	return 0;
}
