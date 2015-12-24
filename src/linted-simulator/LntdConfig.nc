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

#define LNTD_ASYNC_COMMAND                                             \
	"LntdAsyncCommand-6d5734b0-1474-4a28-945e-8ca970fc2a58"

configuration LntdConfig
{
}
implementation
{
	components LntdStartC;
	components LntdSimulatorC;
	components LntdStdioC;

	components new LntdNonblockPool(uniqueCount(LNTD_ASYNC_COMMAND))
	    as Pool;

	components new LntdLoggerC() as Logger;

	components new LntdControllerReaderC() as ControllerReader;
	components new LntdWriterC() as Writer;
	components new LntdTimerC() as Timer;

	LntdSimulatorC.LntdStart->LntdStartC;

	LntdSimulatorC.LntdMainLoop->Pool;
	LntdSimulatorC.LntdStdio->LntdStdioC;
	LntdSimulatorC.LntdLogger->Logger;

	LntdSimulatorC.Timer->Timer;
	LntdSimulatorC.ControllerReader->ControllerReader;
	LntdSimulatorC.Writer->Writer;

	LntdSimulatorC.start->Pool.main;

	Pool.LntdLogger->Logger;

	Logger.LntdStdio->LntdStdioC;

	LntdStdioC.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];

	Timer.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];
	ControllerReader.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];
	Writer.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];
}
