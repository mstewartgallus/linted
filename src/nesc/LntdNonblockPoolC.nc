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

configuration LntdNonblockPoolC
{
	uses interface LntdLogger;

	provides interface LntdTask[lntd_task_id task_id];
	provides interface LntdAsyncCommand[lntd_async_command_id id];
	provides interface LntdMainLoop;
}
implementation
{
#if defined HAVE_WINDOWS_API
	components LntdNonblockPoolWindowsP as Pool;
#elif defined HAVE_POSIX_API
	components LntdNonblockPoolLinuxP as Pool;
#else
#error Unsupported platform
#endif

	LntdTask = Pool.LntdTask;
	LntdAsyncCommand = Pool.LntdAsyncCommand;
	LntdMainLoop = Pool.LntdMainLoop;

	Pool.LntdLogger = LntdLogger;
}
