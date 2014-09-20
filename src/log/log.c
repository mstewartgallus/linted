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

#include "linted/log.h"
#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>

linted_error linted_log_create(linted_log *logp, unsigned long flags)
{
	if (flags != 0U)
		return EINVAL;

	return linted_mq_create(logp, "/log", 10U, LINTED_LOG_MAX, 0);
}

/**
 * @todo Make asynchronous
 */
linted_error linted_log_write(linted_log log, char const *msg_ptr,
                              size_t msg_len)
{
	struct linted_mq_task_send send_task;

	linted_mq_task_send(&send_task, 0, log, msg_ptr, msg_len);
	linted_asynch_pool_submit(NULL, LINTED_UPCAST(&send_task));

	return LINTED_UPCAST(&send_task)->errnum;
}

void linted_log_receive(struct linted_log_task *task, unsigned task_id,
                        linted_log log, char msg_ptr[static LINTED_LOG_MAX])
{
	linted_mq_task_receive(LINTED_UPCAST(task), task_id, log, msg_ptr,
	                       LINTED_LOG_MAX);
}
