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

#include "async.h"
#include "lntd/error.h"
#include "lntd/ko.h"
#include "lntd/util.h"

#include <sys/timerfd.h>
#include <sys/types.h>

generic module LntdTimerLinuxP()
{
	uses interface LntdAsyncCommand;
	provides interface LntdTimer;
}
implementation
{
	struct lntd_async_cmd_timer cmd;
	lntd_error last_error;
	bool have_ko = false;
	bool in_progress = false;

	task void timer_error_handler(void);

	command void LntdTimer.execute(struct timespec const *req)
	{
		LNTD_ASSERT(!in_progress);

		in_progress = true;

		if (!have_ko) {
			int tfd =
			    timerfd_create(CLOCK_MONOTONIC,
			                   TFD_NONBLOCK | TFD_CLOEXEC);
			if (-1 == tfd) {
				last_error = errno;
				post timer_error_handler();
				return;
			}
			cmd.ko = tfd;
			have_ko = true;
		}

		cmd.request = *req;

		call LntdAsyncCommand.execute(LNTD_ASYNC_CMD_TYPE_TIMER,
		                              &cmd);
	}

	command void LntdTimer.cancel(void)
	{
		call LntdAsyncCommand.cancel();
	}

	event void LntdAsyncCommand.done(lntd_error err)
	{
		in_progress = false;
		signal LntdTimer.tick_done(err);
	}

	task void timer_error_handler(void)
	{
		in_progress = false;
		signal LntdTimer.tick_done(last_error);
	}
}
