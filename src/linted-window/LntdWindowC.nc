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

configuration LntdWindowC
{
}
implementation
{
	components LntdWindowP;
	components LntdStdioC;
	components LntdNonblockPoolC;
	components LntdLoggerC;

	components new LntdPollerC() as Poller;
	components new LntdWriterC() as GuiNotifier;
	components new LntdWriterC() as DrawerNotifier;

	LntdWindowP.LntdMainLoop->LntdNonblockPoolC;
	LntdWindowP.LntdStdio->LntdStdioC;
	LntdWindowP.LntdLogger->LntdLoggerC;

	LntdWindowP.Poller->Poller;
	LntdWindowP.GuiNotifier->GuiNotifier;
	LntdWindowP.DrawerNotifier->DrawerNotifier;

	LntdNonblockPoolC.LntdLogger->LntdLoggerC;

	LntdLoggerC.LntdStdio->LntdStdioC;
}
