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
pkgconfdir=$(sysconfdir)/$(PACKAGE)
pkgdefaultconfdir=$(datadir)/factory/$(PACKAGE)/etc
pkgdefaultunitsdir=$(pkgdefaultconfdir)/units
chrootdir=$(pkgdatadir)/chroot

TESTS=
XFAIL_TESTS=

bin_PROGRAMS=
pkglibexec_PROGRAMS=
check_PROGRAMS=

noinst_LTLIBRARIES=

BUILT_SOURCES=

EXTRA_DIST=
MOSTLYCLEANFILES=

dist_pkgdefaultconf_DATA=
dist_pkgdefaultunits_DATA=

install-data-hook:
	$(mkinstalldirs) '$(chrootdir)'

AM_LDFLAGS=\
  -Xcompiler '$(linted_LDFLAGS_LANGUAGE)\
  $(linted_LDFLAGS_OPTIMIZE)\
  $(linted_LDFLAGS_HARDEN)\
  $(linted_LDFLAGS_DEBUG)'

AM_CPPFLAGS=-I$(srcdir)/include

.PHONY: indent
indent: $(SOURCES)
	$(CLANG_FORMAT) -style=file $(linted_CLANG_FORMATFLAGS) $^

linted_CLANG_FORMATFLAGS=-i

.PHONY: doxygen
doxygen:
	sed \
	  -e 's|[@]srcdir[@]|$(srcdir)|g'\
	  -e 's|[@]PACKAGE_NAME[@]|$(PACKAGE_NAME)|g'\
	  -e 's|[@]PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g'\
	  $(srcdir)/Doxyfile.in | doxygen -

include src/Makefile.am
