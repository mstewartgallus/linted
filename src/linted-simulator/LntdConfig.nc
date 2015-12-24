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

configuration LntdConfig
{
}
implementation
{
	components LntdSimulatorC;
	components LntdStdioC;
	components LntdNonblockPoolC;
	components LntdLoggerC;

	components new LntdControllerReaderC() as ControllerReader;
	components new LntdUpdateWriterC() as UpdateWriter;
	components new LntdTimerC() as Timer;

	LntdSimulatorC.LntdMainLoop->LntdNonblockPoolC;
	LntdSimulatorC.LntdStdio->LntdStdioC;
	LntdSimulatorC.LntdLogger->LntdLoggerC;

	LntdSimulatorC.Timer->Timer;
	LntdSimulatorC.ControllerReader->ControllerReader;
	LntdSimulatorC.UpdateWriter->UpdateWriter;

	LntdNonblockPoolC.LntdLogger->LntdLoggerC;

	LntdLoggerC.LntdStdio->LntdStdioC;
}
