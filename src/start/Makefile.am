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
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

noinst_LTLIBRARIES+=%D%/libstart.la
%C%_libstart_la_SOURCES=%D%/start.c include/linted/start.h
EXTRA_%C%_libstart_la_SOURCES=%D%/start-posix.c %D%/start-windows.c
%C%_libstart_la_LIBADD=\
  src/environment/libenvironment.la\
  src/error/liberror.la\
  src/ko/libko.la\
  src/log/liblog.la\
  src/signal/libsignal.la\
  \
  $(WS2_32_LIBS)

if WINDOWS_API
%C%_libstart_la_LIBADD+=src/utf/libutf.la
endif
