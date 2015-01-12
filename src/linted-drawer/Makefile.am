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
pkglibexec_PROGRAMS+=%D%/linted-drawer

%C%_linted_drawer_SOURCES=%D%/drawer.c
%C%_linted_drawer_LDADD=\
  src/start/libstart.la\
  \
  src/asynch/libasynch.la\
  src/error/liberror.la\
  src/gpu/libgpu.la\
  src/ko/libko.la\
  src/xcb/libxcb.la\
  \
  src/updater/libupdater.la\
  src/window-notifier/libwindow-notifier.la\
  \
  $(XCB_LIBS)

%C%_linted_drawer_CPPFLAGS=\
  $(XCB_FLAGS)\
  $(AM_CPPFLAGS)

dist_pkgdefaultconf_DATA+=%D%/drawer.fstab
dist_pkgdefaultunits_DATA+=%D%/units/drawer.service
