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
pkglibexec_PROGRAMS+=%D%/linted-init

%C%_linted_init_SOURCES=

%C%_linted_init_LDADD=\
  src/start/libstart.la\
  \
  src/env/libenv.la\
  src/error/liberror.la\
  src/io/libio.la\
  src/ko/libko.la\
  src/log/liblog.la\
  src/path/libpath.la\
  src/signal/libsignal.la

if POSIX_API
%C%_linted_init_SOURCES+=%D%/init-posix.c
%C%_linted_init_LDADD+=src/spawn/libspawn.la
endif

if WINDOWS_API
%C%_linted_init_SOURCES+=%D%/resources.rc %D%/init.manifest
%C%_linted_init_SOURCES+=%D%/init-windows.c
%C%_linted_init_LDADD+=\
  src/utf/libutf.la\
  \
  %D%/resources.$(OBJEXT)

%D%/resources.$(OBJEXT): %D%/init.manifest

endif
