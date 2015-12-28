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
  %D%/LntdSimulatorC.nc\
  %D%/LntdSimulatorP.nc

%C%_linted_simulator_SOURCES=

%C%_linted_simulator_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_DETERMINISTIC)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_DEBUG_INFO)\
  $(linted_CFLAGS_META)\
  $(linted_CFLAGS_WARNINGS)

%C%_linted_simulator_LDADD=\
  -lm\
  src/ko/libko.la\
  src/ko-stack/libkostack.la\
  src/stack/libstack.la\
  %D%/simulator.o

dist_pkgdefaultconf_DATA+=%D%/simulator.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-simulator.service

if WINDOWS_API
%C%_linted_simulator_SOURCES+=%D%/resources.rc %D%/simulator.manifest
%C%_linted_simulator_LDADD+=%D%/resources.$(OBJEXT)
%D%/resources.$(OBJEXT): %D%/simulator.manifest
endif

%C%_linted_simulator_task_unique=LntdTask-5167a5c7-2eb2-43cc-8e58-b0c6be0012c8

MOSTLYCLEANFILES+=%D%/simulator.c
BUILT_SOURCES+=%D%/simulator.c
%D%/simulator.c: %D%/LntdSimulatorC.nc %D%/LntdSimulatorP.nc $(NESC_FILES)
	$(linted_V_NESCC) $(NESCC)\
	  -fnesc-path=$(abs_srcdir)/%D%:$(abs_srcdir)/src/nesc\
	  -I$(srcdir)/src/nesc\
	  -I$(srcdir)/include -I$(builddir)\
	  $(%C%_linted_simulator_CPPFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS)\
	  -D__restrict__=\
	  \
	  -DLNTD_TASK_UNIQUE='"$(%C%_linted_simulator_task_unique)"'\
	  -fnesc-scheduler=LntdNonblockPoolC,$(%C%_linted_simulator_task_unique),LntdTask,LntdTask,run_task,post_task\
	  -D_GNU_SOURCE=1\
	  -P\
	  -c\
	  -s -E -fnesc-cfile=$@\
	  -Wnesc-all -Wnesc-error\
	  $<
	test -f $@

%D%/simulator.o: %D%/simulator.c
	$(AM_V_CC) $(CC) $(%C%_linted_simulator_CFLAGS) $(CFLAGS) -c -o $@ $^
