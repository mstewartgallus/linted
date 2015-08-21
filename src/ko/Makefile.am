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

noinst_LTLIBRARIES+=%D%/libko.la
%C%_libko_la_SOURCES=%D%/ko.c include/linted/ko.h
EXTRA_%C%_libko_la_SOURCES=\
  %D%/ko-posix.c\
  %D%/ko-emscripten.c\
  %D%/ko-windows.c\
  include/linted/ko-posix.h\
  include/linted/ko-windows.h
%C%_libko_la_LIBADD=$(WS2_32_LIBS)

if WINDOWS_API
%C%_libko_la_LIBADD+=\
  src/str/libstr.la\
  src/utf/libutf.la
endif

TESTS+=%D%/test_ko
check_PROGRAMS+=%D%/test_ko

%C%_test_ko_SOURCES=%D%/ko-test.c
%C%_test_ko_LDADD=\
 src/ko/libko.la\
 src/io/libio.la
