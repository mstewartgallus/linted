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
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
pkglibexec_PROGRAMS+=%D%/linted-window

%C%_linted_window_SOURCES=%D%/window.c
EXTRA_%C%_linted_window_SOURCES=%D%/window-x11.c %D%/window-windows.c
%C%_linted_window_LDADD=\
  src/start/libstart.la\
  \
  src/asynch/libasynch.la\
  src/error/liberror.la\
  src/ko/libko.la\
  src/log/liblog.la\
  \
  src/window/libwindow.la

if X11_API
%C%_linted_window_LDADD+=\
  src/xcb/libxcb.la\
  $(XCB_LIBS)
endif

if WINDOWS_API
%C%_linted_window_LDADD+=$(D3D11_LIBS)
endif

%C%_linted_window_CPPFLAGS=
if X11_API
%C%_linted_window_CPPFLAGS+=$(XCB_FLAGS)
endif
%C%_linted_window_CPPFLAGS+=$(AM_CPPFLAGS)

dist_pkgdefaultconf_DATA+=%D%/window.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-window.service
