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
noinst_LTLIBRARIES+=%D%/liberror.la

%C%_liberror_la_SOURCES=include/linted/error.h
EXTRA_%C%_liberror_la_SOURCES=\
  %D%/error-posix.c\
  include/linted/error-posix.h\
  include/linted/error-windows.h
%C%_liberror_la_LIBADD=

if POSIX_API
%C%_liberror_la_SOURCES+=%D%/error-posix.c
endif

if WINDOWS_API
%C%_liberror_la_SOURCES+=%D%/error-windows.c
%C%_liberror_la_LIBADD+=src/utf/libutf.la
endif

TESTS+=%D%/test_unknown_error

check_PROGRAMS+=%D%/test_unknown_error

%C%_test_unknown_error_SOURCES=%D%/test_unknown_error.c
%C%_test_unknown_error_LDADD=\
  %D%/liberror.la\
  src/start/libstart.la
