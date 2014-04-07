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
ACLOCAL_AMFLAGS=-I m4

bin_PROGRAMS=
pkglibexec_PROGRAMS=
noinst_LTLIBRARIES=
BUILT_SOURCES=
nodist_linted_SOURCES=
EXTRA_DIST=
MOSTLYCLEANFILES=

EXTRA_DIST+=scripts/ccwrapper

EXTRA_DIST+=\
  scripts/ccwrapper\
  \
  src/linted/binaries.h.in\
  \
  libs/glloadgen-reloaded\
  \
  $(headers)

AM_LDFLAGS=\
  -Xcompiler '$(linted_LDFLAGS_OPTIMIZE)\
  $(linted_LDFLAGS_HARDEN)\
  $(linted_LDFLAGS_LANGUAGE)'

AM_CPPFLAGS=\
  $(linted_CPPFLAGS_OPTIMIZE)\
  $(linted_CPPFLAGS_HARDEN)\
  $(linted_CPPFLAGS_LANGUAGE)\
  -I .\
  -I $(srcdir)/src\
  -I $(builddir)/src\
  -I $(builddir)/src/linted\
  -I src

AM_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_WARNINGS)

bin_SCRIPTS=test
EXTRA_DIST+=test.in
test: $(MAKEFILE) test.in
	cat $(srcdir)/test.in > $@
	chmod u+x $@

dist-hook:
	rm -rf $$(find $(distdir)/libs/glloadgen-reloaded -type d -name .git)

.PHONY: indent
indent: $(SOURCES) $(headers)
	$(INDENT) $(linted_INDENTFLAGS) $^

.PHONY: doxygen
doxygen:
	sed \
	  -e 's|[@]srcdir[@]|$(srcdir)|g'\
	  -e 's|[@]PACKAGE_NAME[@]|$(PACKAGE_NAME)|g'\
	  -e 's|[@]PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g'\
	  $(srcdir)/Doxyfile.in | doxygen -

linted_INDENTFLAGS=\
  --linux-style\
  --no-tabs\
  --indent-level4

include src/binaries/linted/Makefile.am
include src/binaries/gui/Makefile.am
include src/binaries/simulator/Makefile.am
include src/binaries/lintedctl/Makefile.am

include src/common/Makefile.am

include src/protocols/controller/Makefile.am
include src/protocols/manager/Makefile.am
include src/protocols/shutdowner/Makefile.am
include src/protocols/updater/Makefile.am
