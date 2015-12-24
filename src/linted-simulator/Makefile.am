# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
pkglibexec_PROGRAMS+=%D%/linted-simulator

EXTRA_DIST+=\
  %D%/LntdConfig.nc\
  %D%/LntdAsyncCommandC.nc\
  %D%/LntdAsyncCommand.nc\
  %D%/LntdControllerReaderC.nc\
  %D%/LntdControllerReader.nc\
  %D%/LntdControllerReaderP.nc\
  %D%/LntdIdlerC.nc\
  %D%/LntdIdler.nc\
  %D%/LntdLoggerC.nc\
  %D%/LntdLogger.nc\
  %D%/LntdMainLoop.nc\
  %D%/LntdNonblockPoolC.nc\
  %D%/LntdPollerC.nc\
  %D%/LntdPoller.nc\
  %D%/LntdPollerP.nc\
  %D%/LntdReaderC.nc\
  %D%/LntdReader.nc\
  %D%/LntdReaderP.nc\
  %D%/LntdSimulatorC.nc\
  %D%/LntdStdioC.nc\
  %D%/LntdStdio.nc\
  %D%/LntdStdioP.nc\
  %D%/LntdTimerC.nc\
  %D%/LntdTimer.nc\
  %D%/LntdTimerP.nc\
  %D%/LntdUpdateWriterC.nc\
  %D%/LntdUpdateWriter.nc\
  %D%/LntdUpdateWriterP.nc\
  %D%/LntdWriterC.nc\
  %D%/LntdWriter.nc\
  %D%/LntdWriterP.nc

%C%_linted_simulator_SOURCES=\
  %D%/simulator.c\
  %D%/async.h\
  %D%/bool.h\
  %D%/logger.h

%C%_linted_simulator_LDADD=\
  -lm\
   src/ko/libko.la

dist_pkgdefaultconf_DATA+=%D%/simulator.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-simulator.service

if WINDOWS_API
%C%_linted_simulator_SOURCES+=%D%/resources.rc %D%/simulator.manifest
%C%_linted_simulator_LDADD+=%D%/resources.$(OBJEXT)
%D%/resources.$(OBJEXT): %D%/simulator.manifest
endif

%C%_linted_simulator_CFLAGS=$(AM_CFLAGS) $(linted_CFLAGS_NO_WARN)

MOSTLYCLEANFILES+=%D%/simulator.c
%D%/simulator.c: \
  %D%/LntdConfig.nc\
  %D%/LntdAsyncCommandC.nc\
  %D%/LntdAsyncCommand.nc\
  %D%/LntdControllerReaderC.nc\
  %D%/LntdControllerReader.nc\
  %D%/LntdControllerReaderP.nc\
  %D%/LntdIdlerC.nc\
  %D%/LntdIdler.nc\
  %D%/LntdLoggerC.nc\
  %D%/LntdLogger.nc\
  %D%/LntdMainLoop.nc\
  %D%/LntdNonblockPoolC.nc\
  %D%/LntdPollerC.nc\
  %D%/LntdPoller.nc\
  %D%/LntdPollerP.nc\
  %D%/LntdReaderC.nc\
  %D%/LntdReader.nc\
  %D%/LntdReaderP.nc\
  %D%/LntdSimulatorC.nc\
  %D%/LntdStdioC.nc\
  %D%/LntdStdio.nc\
  %D%/LntdStdioP.nc\
  %D%/LntdTimerC.nc\
  %D%/LntdTimer.nc\
  %D%/LntdTimerP.nc\
  %D%/LntdUpdateWriterC.nc\
  %D%/LntdUpdateWriter.nc\
  %D%/LntdUpdateWriterP.nc\
  %D%/LntdWriterC.nc\
  %D%/LntdWriter.nc\
  %D%/LntdWriterP.nc\
  \
  %D%/async.h\
  %D%/bool.h\
  %D%/logger.h
	$(linted_V_NESCC) env NESCPATH=$(abs_srcdir)/%D% $(NESCC)\
	  -I$(srcdir)/include -I$(builddir) $(%C%_linted_simulator_CPPFLAGS) $(CPPFLAGS)\
	  -fnesc-separator='__' -D_GNU_SOURCE=1 -conly -S -fnesc-cfile=$@ -Wnesc-all -Wnesc-error $<
