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
#include "bool.h"
#include "lntd/error.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

enum { LNTD_ASYNC_POLLER_IN = 1, LNTD_ASYNC_POLLER_OUT = 1 << 1 };

interface LntdAsyncCommand
{
	command void execute(lntd_async_cmd_type type, void *data);
	command void cancel(void);

	command lntd_error
	execute_sync(lntd_async_cmd_type type, void *data);

	event void done(lntd_error err);
}
