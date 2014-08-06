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
bin_PROGRAMS+=%D%/linted

%C%_linted_SOURCES=\
  %D%/boot.c\
  %D%/init.c %D%/init.h\
  %D%/monitor.c %D%/monitor.h
%C%_linted_CPPFLAGS=-I$(builddir)/%D%/ $(AM_CPPFLAGS)
%C%_linted_LDADD=\
  src/start/libstart.la\
  \
  src/error/liberror.la\
  src/ko/libko.la\
  src/io/libio.la\
  src/locale/liblocale.la\
  src/mem/libmem.la\
  src/spawn/libspawn.la\
  src/util/libutil.la\
  \
  src/controller/libcontroller.la\
  src/db/libdb.la\
  src/logger/liblogger.la\
  src/manager/libmanager.la\
  src/updater/libupdater.la

BUILT_SOURCES+=%D%/binaries.h
EXTRA_DIST+=%D%/binaries.h.in
MOSTLYCLEANFILES+=%D%/binaries.h
%D%/binaries.h: Makefile %D%/binaries.h.in
	sed \
	  -e 's|[@]pkglibexecdir[@]|$(pkglibexecdir)|g'\
	  -e 's|[@]chrootdir[@]|$(chrootdir)|g'\
	  -e 's|[@]pkgconfdir[@]|$(pkgconfdir)|g'\
	  -e 's|[@]EXEEXT[@]|$(EXEEXT)|g'\
	  $(srcdir)/%D%/binaries.h.in > $@
