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
pkglibexec_PROGRAMS+=%D%/linted-window


EXTRA_DIST+=%D%/LntdWindowC.nc %D%/LntdWindowP.nc

%C%_linted_window_SOURCES=
%C%_linted_window_CPPFLAGS=
%C%_linted_window_LDADD=\
  src/error/liberror.la\
  src/env/libenv.la\
  src/io/libio.la\
  src/ko/libko.la\
  src/log/liblog.la\
  \
  src/window/libwindow.la

if X11_API
%C%_linted_window_LDADD+=\
  %D%/window.o\
  src/xcb/libxcb.la\
  $(XCB_LIBS)
endif

if WINDOWS_API
%C%_linted_window_SOURCES+=%D%/linted-window-windows.c
%C%_linted_window_LDADD+=\
  $(USER_LIBS)\
  $(GDI_LIBS)
endif

if X11_API
%C%_linted_window_CPPFLAGS+=$(XCB_FLAGS)
endif
%C%_linted_window_CPPFLAGS+=$(AM_CPPFLAGS)

%C%_linted_window_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_DETERMINISTIC)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_DEBUG_INFO)\
  $(linted_CFLAGS_META)\
  $(linted_CFLAGS_WARNINGS)

MOSTLYCLEANFILES+=%D%/window.c
BUILT_SOURCES+=%D%/window.c
%D%/window.c: %D%/LntdWindowC.nc %D%/LntdWindowP.nc $(NESC_FILES)
	$(linted_V_NESCC) $(NESCC) $(AM_NESCFLAGS) -fnesc-path=$(abs_srcdir)/%D%:$(abs_srcdir)/src/nesc $(NESCFLAGS) -fnesc-cfile=$@ $<
	test -f $@

%D%/window.o: %D%/window.c
	$(AM_V_CC) $(CC) $(%C%_linted_window_CFLAGS) $(CFLAGS) -c -o $@ $^
