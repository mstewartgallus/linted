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
pkglibexec_PROGRAMS+=%D%/linted-gui

%C%_linted_gui_SOURCES=
%C%_linted_gui_LDADD=\
  %D%/gui.o\
  \
  src/error/liberror.la\
  src/ko/libko.la\
  src/xcb/libxcb.la\
  \
  src/window/libwindow.la\
  \
  $(XCB_LIBS)\
  $(XCB_XKB_LIBS)\
  $(XKB_LIBS)\
  $(XKB_X11_LIBS)

%C%_linted_gui_CPPFLAGS=\
  $(XCB_FLAGS) $(XCB_XKB_FLAGS) $(XKB_FLAGS) $(XKB_X11_FLAGS)\
  $(AM_CPPFLAGS)

dist_pkgdefaultconf_DATA+=%D%/gui.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-gui.service

%C%_linted_gui_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_DETERMINISTIC)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_DEBUG_INFO)\
  $(linted_CFLAGS_META)\
  $(linted_CFLAGS_WARNINGS)

MOSTLYCLEANFILES+=%D%/gui.c
BUILT_SOURCES+=%D%/gui.c
%D%/gui.c: %D%/LntdGuiC.nc %D%/LntdGuiP.nc $(NESC_FILES)
	$(MKDIR_P) %D%
	$(linted_V_NESCC) $(NESCC) $(AM_NESCFLAGS) -fnesc-path=$(abs_srcdir)/%D%:$(abs_srcdir)/src/nesc $(NESCFLAGS) -fnesc-cfile=$@ $<
	test -f $@

%D%/gui.o: %D%/gui.c
	$(AM_V_CC) $(CC) $(%C%_linted_gui_CFLAGS) $(CFLAGS) -c -o $@ $^
