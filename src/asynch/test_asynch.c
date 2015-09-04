/*
 * Copyright 2015 Steven Stewart-Gallus
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
#define _GNU_SOURCE

#include "config.h"

#include "linted/asynch.h"
#include "linted/error.h"
#include "linted/log.h"
#include "linted/sched.h"
#include "linted/start.h"
#include "linted/util.h"

#include <stdbool.h>
#include <stdlib.h>

#define MAX_TASKS 20U

enum { ON_IDLE };

struct idle_data {
	struct linted_asynch_pool *pool;
	unsigned long idle_count;
};

static unsigned char test_start(char const *process_name, size_t argc,
                                char const *const argv[]);

static void dispatch(struct linted_asynch_task *task);
static void on_idle(struct linted_asynch_task *task);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-asynch-test",
    .start = test_start};

static unsigned char test_start(char const *process_name, size_t argc,
                                char const *const argv[])
{
	linted_error err = 0;

	struct idle_data idle_data[MAX_TASKS] = {0};
	struct linted_sched_task_idle *idle_task[MAX_TASKS];

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		err = linted_sched_task_idle_create(&idle_task[ii],
		                                    &idle_data[ii]);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_sched_task_idle: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
	}

	struct linted_asynch_pool *pool;
	{
		struct linted_asynch_pool *xx;
		err = linted_asynch_pool_create(&xx, MAX_TASKS);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_asynch_pool_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		idle_data[ii].pool = pool;
		idle_data[ii].idle_count = 100U;

		linted_sched_task_idle_prepare(idle_task[ii], ON_IDLE);
	}

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		linted_asynch_pool_submit(
		    pool,
		    linted_sched_task_idle_to_asynch(idle_task[ii]));
	}

	for (;;) {
		struct linted_asynch_task *completed_task;
		{
			struct linted_asynch_task *xx;
			err = linted_asynch_pool_wait(pool, &xx);
			if (err != 0) {
				linted_log(LINTED_LOG_ERROR,
				           "linted_asynch_pool_wait: "
				           "%s",
				           linted_error_string(err));
				return EXIT_FAILURE;
			}
			completed_task = xx;
		}

		dispatch(completed_task);
		for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
			if (idle_data[ii].idle_count > 0U)
				goto continue_loop;
		}

		goto exit_loop;
	continue_loop:
		continue;
	}
exit_loop:
	err = linted_asynch_pool_destroy(pool);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_asynch_pool_destroy: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static void dispatch(struct linted_asynch_task *task)
{
	switch (linted_asynch_task_action(task)) {
	case ON_IDLE:
		on_idle(task);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static void on_idle(struct linted_asynch_task *task)
{
	linted_error err;

	err = linted_asynch_task_err(task);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_sched_idle: %s",
		           linted_error_string(err));
		exit(EXIT_FAILURE);
	}

	struct linted_sched_task_idle *idle_task =
	    linted_sched_task_idle_from_asynch(task);
	struct idle_data *idle_data =
	    linted_sched_task_idle_data(idle_task);

	unsigned long count = idle_data->idle_count;
	if (0U == count)
		return;
	idle_data->idle_count = count - 1U;
	linted_sched_task_idle_prepare(idle_task, ON_IDLE);
	linted_asynch_pool_submit(idle_data->pool, task);
}
