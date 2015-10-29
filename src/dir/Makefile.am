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

noinst_LTLIBRARIES+=%D%/libdir.la
%C%_libdir_la_SOURCES=include/linted/dir.h
%C%_linted_LDLIBS=\
  src/ko/libko.la\
  src/path/libpath.la\
  src/str/libstr.la

if POSIX_API
%C%_libdir_la_SOURCES+=%D%/dir-posix.c
endif

if WINDOWS_API
%C%_libdir_la_SOURCES+=%D%/dir-windows.c
endif
