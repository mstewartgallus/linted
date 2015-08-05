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
pkgconfdir=$(sysconfdir)/$(PACKAGE)
pkgdefaultconfdir=$(datadir)/factory/$(PACKAGE)/etc
pkgdefaultunitsdir=$(pkgdefaultconfdir)/units

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

AM_CFLAGS=\
 $(linted_CFLAGS_LANGUAGE)\
 $(linted_CFLAGS_OPTIMIZE)\
 $(linted_CFLAGS_HARDEN)\
 $(linted_CFLAGS_DEBUG)\
 $(linted_CFLAGS_DEBUG_INFO)\
 $(linted_CFLAGS_WARNINGS)

AM_CPPFLAGS=
AM_LDFLAGS=

linted_CLANG_FORMATFLAGS=

EXTRA_DIST+=%D%/configure-defaults

if WINDOWS_API
AM_CFLAGS+=$(linted_CFLAGS_UNICODE)
endif

AM_CPPFLAGS+=-I$(srcdir)/include

AM_LDFLAGS+=\
  $$(for II in \
  $(linted_LDFLAGS_LANGUAGE)\
  $(linted_LDFLAGS_OPTIMIZE)\
  $(linted_LDFLAGS_HARDEN)\
  $(linted_LDFLAGS_DEBUG)\
  $(linted_LDFLAGS_DEBUG_INFO)\
  $(linted_LDFLAGS_WARNINGS); do echo -Xcompiler "$$II"; done)

mostlyclean-local:
	- find "$(builddir)/src" -type f -name '*.dwo' -exec rm -f -- '{}' '+'

.PHONY: indent
indent:
	find $(srcdir)/src $(srcdir)/include -type f -name '*.[ch]' -exec \
	   $(CLANG_FORMAT) -style=file $(linted_CLANG_FORMATFLAGS) '{}' '+'

.rc.$(OBJEXT):
	$(WINDRES) -i $< -o $@ --output-format=coff

linted_CLANG_FORMATFLAGS+=-i

include docs/Makefile.am
include scripts/Makefile.am
include src/Makefile.am
