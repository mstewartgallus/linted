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
#define LNTD_ASYNC_COMMAND "LntdAsyncCommand"

configuration LntdConfig
{
}
implementation
{
	components LntdStartM;
	components LntdSimulator;
	components LntdPoolStdio;

	components new LntdNonblockPool(uniqueCount(LNTD_ASYNC_COMMAND))
	    as Pool;

	components new LntdStdioLogger() as Logger;

	components new LntdPoolReader() as Reader;
	components new LntdPoolWriter() as Writer;
	components new LntdPoolTimer() as Timer;

	LntdSimulator.LntdStart->LntdStartM;

	LntdSimulator.LntdMainLoop->Pool;
	LntdSimulator.LntdStdio->LntdPoolStdio;
	LntdSimulator.LntdLogger->Logger;

	LntdSimulator.Timer->Timer;
	LntdSimulator.Reader->Reader;
	LntdSimulator.Writer->Writer;

	LntdSimulator.start->Pool.main;

	Pool.LntdLogger->Logger;

	Logger.LntdStdio->LntdPoolStdio;

	LntdPoolStdio.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];

	Timer.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];
	Reader.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];
	Writer.LntdAsyncCommand->Pool
	    .LntdAsyncCommand[unique(LNTD_ASYNC_COMMAND)];
}
