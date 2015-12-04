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
pkglibexec_PROGRAMS+=%D%/linted-sandbox

%C%_linted_sandbox_SOURCES=
%C%_linted_sandbox_CFLAGS=
%C%_linted_sandbox_LDADD=\
  src/async/libasync.la\
  src/dir/libdir.la\
  src/error/liberror.la\
  src/fifo/libfifo.la\
  src/file/libfile.la\
  src/io/libio.la\
  src/ko/libko.la\
  src/locale/liblocale.la\
  src/log/liblog.la\
  src/path/libpath.la\
  src/start/libstart.la\
  src/str/libstr.la

if POSIX_API
%C%_linted_sandbox_SOURCES+=%D%/sandbox-posix.c
%C%_linted_sandbox_CFLAGS+=$(SECCOMP_CFLAGS) $(AM_CFLAGS)
%C%_linted_sandbox_LDADD+=$(CAP_LIBS) $(SECCOMP_LIBS)
endif

if WINDOWS_API
%C%_linted_sandbox_SOURCES+=%D%/sandbox-windows.c
%C%_linted_sandbox_LDADD+=src/utf/libutf.la
endif
