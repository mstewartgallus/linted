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
#include "config.h"

#include "linted/async.h"
#include "linted/error.h"
#include "linted/log.h"
#include "linted/sched.h"
#include "linted/start.h"
#include "linted/util.h"

#include <stddef.h>
#include <stdlib.h>

#define MAX_TASKS 20U

enum { ON_IDLE };

struct idle_data {
	struct linted_async_pool *pool;
	unsigned long idle_count;
};

static void dispatch(union linted_async_ck task_ck,
                     struct linted_async_task *task, linted_error err);
static void on_idle(struct linted_async_task *task, linted_error err);

static struct linted_start_config const linted_start_config = {
    .canonical_process_name = PACKAGE_NAME "-async-test"};

static unsigned char linted_start_main(char const *process_name,
                                       size_t argc,
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

	struct linted_async_pool *pool;
	{
		struct linted_async_pool *xx;
		err = linted_async_pool_create(&xx, MAX_TASKS);
		if (err != 0) {
			linted_log(LINTED_LOG_ERROR,
			           "linted_async_pool_create: %s",
			           linted_error_string(err));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		idle_data[ii].pool = pool;
		idle_data[ii].idle_count = 100U;

		linted_sched_task_idle_prepare(
		    idle_task[ii],
		    (union linted_async_ck){.u64 = ON_IDLE});
	}

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		linted_async_pool_submit(
		    pool,
		    linted_sched_task_idle_to_async(idle_task[ii]));
	}

	for (;;) {
		struct linted_async_result result;
		{
			struct linted_async_result xx;
			err = linted_async_pool_wait(pool, &xx);
			if (err != 0) {
				linted_log(LINTED_LOG_ERROR,
				           "linted_async_pool_wait: "
				           "%s",
				           linted_error_string(err));
				return EXIT_FAILURE;
			}
			result = xx;
		}

		dispatch(result.task_ck, result.task, result.err);
		for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
			if (idle_data[ii].idle_count > 0U)
				goto continue_loop;
		}

		goto exit_loop;
	continue_loop:
		continue;
	}
exit_loop:
	err = linted_async_pool_destroy(pool);
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR,
		           "linted_async_pool_destroy: %s",
		           linted_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static void dispatch(union linted_async_ck task_ck,
                     struct linted_async_task *task, linted_error err)
{
	switch (task_ck.u64) {
	case ON_IDLE:
		on_idle(task, err);
		break;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static void on_idle(struct linted_async_task *task, linted_error err)
{
	if (err != 0) {
		linted_log(LINTED_LOG_ERROR, "linted_sched_idle: %s",
		           linted_error_string(err));
		exit(EXIT_FAILURE);
	}

	struct linted_sched_task_idle *idle_task =
	    linted_sched_task_idle_from_async(task);
	struct idle_data *idle_data =
	    linted_sched_task_idle_data(idle_task);

	unsigned long count = idle_data->idle_count;
	if (0U == count)
		return;
	idle_data->idle_count = count - 1U;

	linted_async_pool_submit(
	    idle_data->pool,
	    linted_sched_task_idle_prepare(
	        idle_task, (union linted_async_ck){.u64 = ON_IDLE}));
}
