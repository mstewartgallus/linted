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

#include <assert.h>
#include <limits.h>

generic module LntdPollerP()
{
	uses interface LntdAsyncCommand;
	provides interface LntdPoller;
}
implementation
{
	struct lntd_async_cmd_poll cmd;

	command void LntdPoller.execute(lntd_ko ko,
	                                uint_fast64_t events)
	{
		lntd_error err = 0;

		if ((events &
		     ~(LNTD_ASYNC_POLLER_IN | LNTD_ASYNC_POLLER_OUT)) !=
		    0) {
			err = EINVAL;
			goto signal_error;
		}

		if (ko > INT_MAX) {
			err = EINVAL;
			goto signal_error;
		}

		cmd.ko = ko;
		cmd.events = events;

		call LntdAsyncCommand.execute(LNTD_ASYNC_CMD_TYPE_POLL,
		                              &cmd);
		return;

	signal_error:
		signal LntdPoller.poll_done(err, 0);
	}

	command void LntdPoller.cancel(void)
	{
		call LntdAsyncCommand.cancel();
	}

	event void LntdAsyncCommand.done(lntd_error err)
	{
		signal LntdPoller.poll_done(err, cmd.revents);
	}
}
