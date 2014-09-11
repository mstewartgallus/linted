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

#include "linted/window-notifier.h"

#include "linted/mq.h"
#include "linted/util.h"

#include <errno.h>
#include <string.h>

void linted_window_notifier_send(struct linted_window_notifier_task_send *task,
                                 unsigned task_id,
                                 linted_window_notifier notifier,
                                 uint_fast32_t window)
{
	linted_mq_task_send(LINTED_UPCAST(task), task_id, notifier,
	                    task->message, sizeof task->message);

	char *tip = task->message;

	linted_rpc_pack_uint32(window, tip);
}

void
linted_window_notifier_receive(struct linted_window_notifier_task_receive *task,
                               unsigned task_id,
                               linted_window_notifier notifier)
{
	linted_mq_task_receive(LINTED_UPCAST(task), task_id, notifier,
	                       task->message, sizeof task->message);
}

uint_fast32_t linted_window_notifier_decode(
    struct linted_window_notifier_task_receive const *task)
{
	char const *tip = task->message;

	return linted_rpc_unpack_uint32(tip);
}
