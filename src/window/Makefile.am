# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2013, 2014 Steven Stewart-Gallus
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
pkglibexec_PROGRAMS+=%D%/window

%C%_window_SOURCES=%D%/window.c
%C%_window_LDADD=\
  src/start/libstart.la\
  \
  src/asynch/libasynch.la\
  src/error/liberror.la\
  src/ko/libko.la\
  src/mem/libmem.la\
  \
  src/window-notifier/libwindow-notifier.la\
  \
  $(XCB_LIBS)

%C%_window_CPPFLAGS=\
  $(XCB_FLAGS)\
  $(AM_CPPFLAGS)

dist_pkgdefaultconf_DATA+=%D%/window-fstab
dist_pkgdefaultunits_DATA+=%D%/units/window.service
