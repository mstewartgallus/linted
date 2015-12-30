# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2015 Steven Stewart-Gallus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.  See the License for the specific language governing
# permissions and limitations under the License.

EXTRA_DIST+=\
  %D%/LntdAsyncCommandC.nc\
  %D%/LntdAsyncCommand.nc\
  %D%/LntdControllerReaderC.nc\
  %D%/LntdControllerReader.nc\
  %D%/LntdControllerReaderP.nc\
  %D%/LntdControllerWriterC.nc\
  %D%/LntdControllerWriter.nc\
  %D%/LntdControllerWriterP.nc\
  %D%/LntdIdlerC.nc\
  %D%/LntdIdler.nc\
  %D%/LntdLoggerC.nc\
  %D%/LntdLogger.nc\
  %D%/LntdLoggerPosixP.nc\
  %D%/LntdLoggerWindowsP.nc\
  %D%/LntdMainLoop.nc\
  %D%/LntdNonblockPoolC.nc\
  %D%/LntdNonblockPoolLinuxP.nc\
  %D%/LntdNonblockPoolWindowsP.nc\
  %D%/LntdPollerC.nc\
  %D%/LntdPoller.nc\
  %D%/LntdPollerP.nc\
  %D%/LntdReaderC.nc\
  %D%/LntdReader.nc\
  %D%/LntdReaderP.nc\
  %D%/LntdStdioC.nc\
  %D%/LntdStdio.nc\
  %D%/LntdStdioP.nc\
  %D%/LntdTimerC.nc\
  %D%/LntdTimer.nc\
  %D%/LntdTimerLinuxP.nc\
  %D%/LntdTimerWindowsP.nc\
  %D%/LntdUpdateReaderC.nc\
  %D%/LntdUpdateReader.nc\
  %D%/LntdUpdateReaderP.nc\
  %D%/LntdUpdateWriterC.nc\
  %D%/LntdUpdateWriter.nc\
  %D%/LntdUpdateWriterP.nc\
  %D%/LntdWriterC.nc\
  %D%/LntdWriter.nc\
  %D%/LntdWriterP.nc\
  \
  %D%/async.h\
  %D%/bool.h\
  %D%/controller.h\
  %D%/update.h\
  %D%/logger.h

NESC_FILES=\
  %D%/LntdAsyncCommandC.nc\
  %D%/LntdAsyncCommand.nc\
  %D%/LntdControllerReaderC.nc\
  %D%/LntdControllerReader.nc\
  %D%/LntdControllerReaderP.nc\
  %D%/LntdControllerWriterC.nc\
  %D%/LntdControllerWriter.nc\
  %D%/LntdControllerWriterP.nc\
  %D%/LntdIdlerC.nc\
  %D%/LntdIdler.nc\
  %D%/LntdLoggerC.nc\
  %D%/LntdLogger.nc\
  %D%/LntdLoggerPosixP.nc\
  %D%/LntdLoggerWindowsP.nc\
  %D%/LntdMainLoop.nc\
  %D%/LntdNonblockPoolC.nc\
  %D%/LntdNonblockPoolLinuxP.nc\
  %D%/LntdNonblockPoolWindowsP.nc\
  %D%/LntdPollerC.nc\
  %D%/LntdPoller.nc\
  %D%/LntdPollerP.nc\
  %D%/LntdReaderC.nc\
  %D%/LntdReader.nc\
  %D%/LntdReaderP.nc\
  %D%/LntdStdioC.nc\
  %D%/LntdStdio.nc\
  %D%/LntdStdioP.nc\
  %D%/LntdTask.nc\
  %D%/LntdTimerC.nc\
  %D%/LntdTimer.nc\
  %D%/LntdTimerLinuxP.nc\
  %D%/LntdTimerWindowsP.nc\
  %D%/LntdUpdateReaderC.nc\
  %D%/LntdUpdateReader.nc\
  %D%/LntdUpdateReaderP.nc\
  %D%/LntdUpdateWriterC.nc\
  %D%/LntdUpdateWriter.nc\
  %D%/LntdUpdateWriterP.nc\
  %D%/LntdWriterC.nc\
  %D%/LntdWriter.nc\
  %D%/LntdWriterP.nc\
  \
  %D%/async.h\
  %D%/bool.h\
  %D%/controller.h\
  %D%/update.h\
  %D%/logger.h

%C%_task_unique=LntdTask-5167a5c7-2eb2-43cc-8e58-b0c6be0012c8

AM_NESCFLAGS=\
  -I$(srcdir)/src/nesc\
  -I$(srcdir)/include -I$(builddir)\
  $(AM_CPPFLAGS) $(CPPFLAGS)\
  -D__restrict__=\
  \
  -DLNTD_TASK_UNIQUE='"$(%C%_task_unique)"'\
  -fnesc-scheduler=LntdNonblockPoolC,$(%C%_task_unique),LntdTask,LntdTask,run_task,post_task\
  -D_GNU_SOURCE=1\
  -P\
  -c\
  -s -E\
  -Wnesc-all -Wnesc-error
