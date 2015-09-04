# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2014, 2015 Steven Stewart-Gallus
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
noinst_LTLIBRARIES+=%D%/libasynch.la

%C%_libasynch_la_SOURCES=%D%/asynch.c include/linted/asynch.h
EXTRA_%C%_libasynch_la_SOURCES=\
  %D%/asynch-emscripten.c\
  %D%/asynch-posix.c\
  %D%/asynch-windows.c
%C%_libasynch_la_LIBADD=\
  src/ko/libko.la\
  src/channel/libchannel.la\
  src/queue/libqueue.la\
  $(WS2_32_LIBS)

TESTS+=%D%/test_asynch
check_PROGRAMS+=%D%/test_asynch
%C%_test_asynch_SOURCES=%D%/test_asynch.c
%C%_test_asynch_LDADD=\
 src/asynch/libasynch.la\
 src/log/liblog.la\
 src/sched/libsched.la\
 src/start/libstart.la
