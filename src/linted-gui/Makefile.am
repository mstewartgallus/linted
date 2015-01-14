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
pkglibexec_PROGRAMS+=%D%/linted-gui

%C%_linted_gui_SOURCES=%D%/gui.c
%C%_linted_gui_LDADD=\
  src/start/libstart.la\
  \
  src/asynch/libasynch.la\
  src/error/liberror.la\
  src/ko/libko.la\
  src/log/liblog.la\
  src/xcb/libxcb.la\
  \
  src/controller/libcontroller.la\
  src/window-notifier/libwindow-notifier.la\
  \
  $(XCB_LIBS)\
  $(XCB_XKB_LIBS)\
  $(XKB_LIBS)\
  $(XKB_X11_LIBS)

%C%_linted_gui_CPPFLAGS=\
  $(XCB_FLAGS) $(XCB_XKB_FLAGS) $(XKB_FLAGS) $(XKB_X11_FLAGS)\
  $(AM_CPPFLAGS)

dist_pkgdefaultconf_DATA+=%D%/gui.fstab
dist_pkgdefaultunits_DATA+=%D%/units/gui.service
