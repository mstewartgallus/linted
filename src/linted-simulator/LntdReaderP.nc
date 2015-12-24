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

#include <limits.h>
#include <stddef.h>

generic module LntdReaderP()
{
	uses interface LntdAsyncCommand;
	provides interface LntdReader;
}
implementation
{
	struct lntd_async_cmd_read cmd;
	lntd_error last_error;
	bool in_progress;

	task void signal_error(void);

	command void LntdReader.execute(lntd_ko ko, char *bytes,
	                                size_t size)
	{
		lntd_error err = 0;
		LNTD_ASSERT(!in_progress);

		if (ko > INT_MAX) {
			err = EINVAL;
			goto signal_error;
		}

		cmd.ko = ko;
		cmd.bytes = bytes;
		cmd.size = size;

		cmd.bytes_left = size;

		call LntdAsyncCommand.execute(LNTD_ASYNC_CMD_TYPE_READ,
		                              &cmd);
		return;

	signal_error:
		last_error = err;
		post signal_error();
	}

	command void LntdReader.cancel(void)
	{
		call LntdAsyncCommand.cancel();
	}

	event void LntdAsyncCommand.done(lntd_error err)
	{
		in_progress = false;
		signal LntdReader.read_done(err,
		                            cmd.size - cmd.bytes_left);
	}

	task void signal_error(void)
	{
		in_progress = false;
		signal LntdReader.read_done(last_error,
		                            cmd.size - cmd.bytes_left);
	}
}
