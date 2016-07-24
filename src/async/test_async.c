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

#include "lntd/async.h"
#include "lntd/error.h"
#include "lntd/log.h"
#include "lntd/sched.h"
#include "lntd/start.h"
#include "lntd/util.h"

#include <stddef.h>
#include <stdlib.h>

#define MAX_TASKS 20U

enum { ON_IDLE };

struct idle_data {
	struct lntd_async_pool *pool;
	unsigned long idle_count;
};

static void dispatch(union lntd_async_ck task_ck, void *userstate,
                     lntd_error err);
static void on_idle(struct lntd_sched_task_idle *idle_task,
                    lntd_error err);

static struct lntd_start_config const lntd_start_config = {
    .canonical_process_name = PACKAGE_NAME "-async-test"};

static unsigned char lntd_start_main(char const *process_name,
                                     size_t argc,
                                     char const *const argv[])
{
	lntd_error err = 0;

	struct idle_data idle_data[MAX_TASKS] = {0};
	struct lntd_sched_task_idle *idle_task[MAX_TASKS];

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		err = lntd_sched_task_idle_create(&idle_task[ii],
		                                  &idle_data[ii]);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_sched_task_idle: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
	}

	struct lntd_async_pool *pool;
	{
		struct lntd_async_pool *xx;
		err = lntd_async_pool_create(&xx, MAX_TASKS);
		if (err != 0) {
			lntd_log(LNTD_LOG_ERROR,
			         "lntd_async_pool_create: %s",
			         lntd_error_string(err));
			return EXIT_FAILURE;
		}
		pool = xx;
	}

	for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
		idle_data[ii].pool = pool;
		idle_data[ii].idle_count = 100U;

		struct lntd_sched_task_idle *task = idle_task[ii];

		lntd_sched_task_idle_submit(
		    pool, task, (union lntd_async_ck){.u64 = ON_IDLE},
		    task);
	}

	for (;;) {
		struct lntd_async_result result;
		{
			struct lntd_async_result xx;
			err = lntd_async_pool_wait(pool, &xx);
			if (err != 0) {
				lntd_log(LNTD_LOG_ERROR,
				         "lntd_async_pool_wait: "
				         "%s",
				         lntd_error_string(err));
				return EXIT_FAILURE;
			}
			result = xx;
		}

		dispatch(result.task_ck, result.userstate, result.err);
		for (size_t ii = 0U; ii < MAX_TASKS; ++ii) {
			if (idle_data[ii].idle_count > 0U)
				goto continue_loop;
		}

		goto exit_loop;
	continue_loop:
		continue;
	}
exit_loop:
	err = lntd_async_pool_destroy(pool);
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_async_pool_destroy: %s",
		         lntd_error_string(err));
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

static void dispatch(union lntd_async_ck task_ck, void *userstate,
                     lntd_error err)
{
	switch (task_ck.u64) {
	case ON_IDLE:
		on_idle(userstate, err);
		break;

	default:
		LNTD_ASSUME_UNREACHABLE();
	}
}

static void on_idle(struct lntd_sched_task_idle *idle_task,
                    lntd_error err)
{
	if (err != 0) {
		lntd_log(LNTD_LOG_ERROR, "lntd_sched_idle: %s",
		         lntd_error_string(err));
		exit(EXIT_FAILURE);
	}

	struct idle_data *idle_data =
	    lntd_sched_task_idle_data(idle_task);

	unsigned long count = idle_data->idle_count;
	if (0U == count)
		return;
	idle_data->idle_count = count - 1U;

	lntd_sched_task_idle_submit(
	    idle_data->pool, idle_task,
	    (union lntd_async_ck){.u64 = ON_IDLE}, idle_task);
}
