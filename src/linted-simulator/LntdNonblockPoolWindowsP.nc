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
#ifndef UNICODE
#define UNICODE
#endif

#define _UNICODE

#define WIN32_LEAN_AND_MEAN

#include "config.h"

#include "async.h"
#include "lntd/util.h"

#define MAXCMDS uniqueCount(LNTD_ASYNC_COMMAND)
#define MAXTASKS uniqueCount(LNTD_TASK_UNIQUE)

module LntdNonblockPoolWindowsP
{
	uses interface LntdLogger;

	provides interface LntdTask[lntd_task_id task_id];
	provides interface LntdAsyncCommand[lntd_async_command_id id];
	provides interface LntdMainLoop;
}
implementation
{
	command bool LntdTask.post_task[lntd_task_id task_id](void)
	{
		LNTD_CRASH_FAST();
	}

	command void LntdAsyncCommand.execute[lntd_async_command_id id](
	    lntd_async_cmd_type type, void *data)
	{
		LNTD_CRASH_FAST();
	}

	command void LntdAsyncCommand.cancel[lntd_async_command_id id](
	    void)
	{
		LNTD_CRASH_FAST();
	}

	command lntd_error
	    LntdAsyncCommand.execute_sync[lntd_async_command_id id](
	        lntd_async_cmd_type type, void *data)
	{
		LNTD_CRASH_FAST();
	}

	command void LntdMainLoop.exit(lntd_error status)
	{
		LNTD_CRASH_FAST();
	}

	int wWinMain(void *program_instance, void *prev_instance_unused,
	             wchar_t *command_line_unused, int show_command_arg)
	    @C() @spontaneous()
	{
		LNTD_CRASH_FAST();
	}

default event
	void LntdAsyncCommand.done[lntd_async_command_id id](
	    lntd_error err)
	{
		LNTD_CRASH_FAST();
	}

default event
	void LntdTask.run_task[lntd_task_id id](void)
	{
		LNTD_CRASH_FAST();
	}
}
