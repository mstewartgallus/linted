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
noinst_LTLIBRARIES+=%D%/libasync.la

%C%_libasync_la_SOURCES=%D%/async.c include/linted/async.h
EXTRA_%C%_libasync_la_SOURCES=\
  %D%/async-emscripten.c\
  %D%/async-posix.c\
  %D%/async-windows.c
%C%_libasync_la_LIBADD=\
  src/ko/libko.la\
  src/channel/libchannel.la\
  src/queue/libqueue.la

if POSIX_API
%C%_libasync_la_LIBADD+=\
  src/ko-queue/libkoqueue.la
endif

if WINDOWS_API
%C%_libasync_la_LIBADD+=\
  $(WS2_32_LIBS)
endif

TESTS+=%D%/test_async
check_PROGRAMS+=%D%/test_async
%C%_test_async_SOURCES=%D%/test_async.c
%C%_test_async_LDADD=\
 src/async/libasync.la\
 src/log/liblog.la\
 src/sched/libsched.la\
 src/start/libstart.la
