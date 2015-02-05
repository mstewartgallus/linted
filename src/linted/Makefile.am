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
bin_PROGRAMS+=%D%/linted

%C%_linted_SOURCES=%D%/linted.c
%C%_linted_CPPFLAGS=-I$(builddir)/%D%/ $(AM_CPPFLAGS)
%C%_linted_LDADD=\
  src/asynch/libasynch.la\
  src/dir/libdir.la\
  src/io/libio.la\
  src/locale/liblocale.la\
  src/log/liblog.la\
  src/start/libstart.la

BUILT_SOURCES+=%D%/settings.h
EXTRA_DIST+=%D%/settings.h.in
MOSTLYCLEANFILES+=%D%/settings.h
%D%/settings.h: Makefile %D%/settings.h.in
	sed \
	  -e 's|[@]LINTED_UNIT_PATH[@]|$(pkgunitsdir):$(pkgdefaultunitsdir)|g'\
	  -e 's|[@]LINTED_INIT[@]|$(pkglibexecdir)/linted-init$(exeext)|g'\
	  -e 's|[@]LINTED_MONITOR[@]|$(pkglibexecdir)/linted-monitor$(exeext)|g'\
	  -e 's|[@]LINTED_SANDBOX[@]|$(pkglibexecdir)/linted-sandbox$(exeext)|g'\
	  -e 's|[@]LINTED_WAITER[@]|$(pkglibexecdir)/linted-waiter$(exeext)|g'\
	  -e 's|[@]LINTED_GUI[@]|$(pkglibexecdir)/linted-gui$(exeext)|g'\
	  -e 's|[@]LINTED_GUI_FSTAB[@]|$(pkgdefaultconfdir)/gui.fstab|g'\
	  -e 's|[@]LINTED_SIMULATOR[@]|$(pkglibexecdir)/linted-simulator$(exeext)|g'\
	  -e 's|[@]LINTED_SIMULATOR_FSTAB[@]|$(pkgdefaultconfdir)/simulator.fstab|g'\
	  -e 's|[@]LINTED_DRAWER[@]|$(pkglibexecdir)/linted-drawer$(exeext)|g'\
	  -e 's|[@]LINTED_DRAWER_FSTAB[@]|$(pkgdefaultconfdir)/drawer.fstab|g'\
	  -e 's|[@]LINTED_WINDOW[@]|$(pkglibexecdir)/linted-window$(exeext)|g'\
	  $(srcdir)/%D%/settings.h.in > $@
