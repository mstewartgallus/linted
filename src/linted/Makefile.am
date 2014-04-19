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
bin_PROGRAMS+=linted

linted_SOURCES=src/linted/linted.c
linted_CPPFLAGS=-I$(builddir)/src/linted/ $(AM_CPPFLAGS)
linted_LDADD=\
  libio.la\
  libutil.la\
  \
  libcontroller.la\
  liblogger.la\
  libmanager.la\
  libshutdowner.la\
  libupdater.la

BUILT_SOURCES+=src/linted/binaries.h

EXTRA_DIST+=src/linted/binaries.h.in
src/linted/binaries.h: $(MAKEFILE) src/linted/binaries.h.in
	sed \
	  -e 's|[@]pkglibexecdir[@]|$(pkglibexecdir)|g'\
	  -e 's|[@]EXEEXT[@]|$(EXEEXT)|g'\
	  $(srcdir)/src/linted/binaries.h.in > $@
