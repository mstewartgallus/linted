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
bin_PROGRAMS+=%D%/linted

%C%_linted_DEPS=$(builddir)/%D%/settings.h
%C%_linted_SOURCES=%D%/linted.c
%C%_linted_CPPFLAGS=-I$(builddir)/%D%/ $(AM_CPPFLAGS)
%C%_linted_LDADD=\
  src/dir/libdir.la\
  src/env/libenv.la\
  src/io/libio.la\
  src/locale/liblocale.la\
  src/log/liblog.la\
  src/path/libpath.la\
  src/pid/libpid.la\
  \
  src/start/libstart.la

if WINDOWS_API
%C%_linted_LDADD+=\
  src/utf/libutf.la\
  \
  %D%/resources.$(OBJEXT)

%D%/resources.$(OBJEXT): %D%/linted.manifest
endif

%D%/%C%_linted-linted.$(OBJEXT): %D%/settings.h

EXTRA_DIST+=%D%/settings.h.in
MOSTLYCLEANFILES+=%D%/settings.h
%D%/settings.h: %D%/settings.h.in Makefile
	$(linted_V_SED) sed \
	  -e 's|[@]LINTED_UNIT_PATH[@]|$(pkgunitsdir):$(pkgdefaultunitsdir)|g'\
	  -e 's|[@]LINTED_INIT[@]|$(pkglibexecdir)/linted-init$(EXEEXT)|g'\
	  -e 's|[@]LINTED_MONITOR[@]|$(pkglibexecdir)/linted-monitor$(EXEEXT)|g'\
	  -e 's|[@]LINTED_STARTUP[@]|$(pkglibexecdir)/linted-startup$(EXEEXT)|g'\
	  -e 's|[@]LINTED_SANDBOX[@]|$(pkglibexecdir)/linted-sandbox$(EXEEXT)|g'\
	  -e 's|[@]LINTED_WAITER[@]|$(pkglibexecdir)/linted-waiter$(EXEEXT)|g'\
	  -e 's|[@]LINTED_AUDIO[@]|$(pkglibexecdir)/linted-audio$(EXEEXT)|g'\
	  -e 's|[@]LINTED_AUDIO_FSTAB[@]|$(pkgdefaultconfdir)/audio.fstab|g'\
	  -e 's|[@]LINTED_GUI[@]|$(pkglibexecdir)/linted-gui$(EXEEXT)|g'\
	  -e 's|[@]LINTED_GUI_FSTAB[@]|$(pkgdefaultconfdir)/gui.fstab|g'\
	  -e 's|[@]LINTED_SIMULATOR[@]|$(pkglibexecdir)/linted-simulator$(EXEEXT)|g'\
	  -e 's|[@]LINTED_SIMULATOR_FSTAB[@]|$(pkgdefaultconfdir)/simulator.fstab|g'\
	  -e 's|[@]LINTED_DRAWER[@]|$(pkglibexecdir)/linted-drawer$(EXEEXT)|g'\
	  -e 's|[@]LINTED_DRAWER_FSTAB[@]|$(pkgdefaultconfdir)/drawer.fstab|g'\
	  -e 's|[@]LINTED_WINDOW[@]|$(pkglibexecdir)/linted-window$(EXEEXT)|g'\
	  -e 's|[@]LINTED_WINDOW_FSTAB[@]|$(pkgdefaultconfdir)/window.fstab|g'\
	  < $< > $@
