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
pkglibexec_PROGRAMS+=%D%/linted-drawer

%C%_linted_drawer_SOURCES=
%C%_linted_drawer_LDADD=\
  %D%/drawer.o\
  \
  src/async/libasync.la\
  src/error/liberror.la\
  src/gpu/libgpu.la\
  src/ko/libko.la\
  src/xcb/libxcb.la\
  \
  src/window/libwindow.la\
  \
  $(XCB_LIBS)

%C%_linted_drawer_CPPFLAGS=\
  $(XCB_FLAGS)\
  $(AM_CPPFLAGS)

dist_pkgdefaultconf_DATA+=%D%/drawer.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-drawer.service

%C%_linted_drawer_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_DETERMINISTIC)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_DEBUG_INFO)\
  $(linted_CFLAGS_META)\
  $(linted_CFLAGS_WARNINGS)

EXTRA_DIST+=%D%/LntdDrawerC.nc %D%/LntdDrawerP.nc

MOSTLYCLEANFILES+=%D%/drawer.c
BUILT_SOURCES+=%D%/drawer.c
%D%/drawer.c: %D%/LntdDrawerC.nc %D%/LntdDrawerP.nc $(NESC_FILES)
	$(linted_V_NESCC) $(NESCC) $(AM_NESCFLAGS) -fnesc-path=$(abs_srcdir)/%D%:$(abs_srcdir)/src/nesc $(NESCFLAGS) -fnesc-cfile=$@ $<
	test -f $@

%D%/drawer.o: %D%/drawer.c
	$(AM_V_CC) $(CC) $(%C%_linted_drawer_CFLAGS) $(CFLAGS) -c -o $@ $^
