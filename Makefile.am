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

EXTRA_DIST+=scripts/ccwrapper.py libs/glloadgen-reloaded

AM_LDFLAGS=\
  -Xcompiler '$(linted_LDFLAGS_LANGUAGE)\
  $(linted_LDFLAGS_OPTIMIZE)\
  $(linted_LDFLAGS_HARDEN)\
  $(linted_LDFLAGS_DEBUG)'

AM_CPPFLAGS=\
  $(linted_CPPFLAGS_LANGUAGE)\
  $(linted_CPPFLAGS_OPTIMIZE)\
  $(linted_CPPFLAGS_HARDEN)\
  $(linted_CPPFLAGS_DEBUG)\
  -I$(srcdir)/include

AM_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_MINOR_DEBUG)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_WARNINGS)

EXTRA_DIST+=scripts/test.in
scripts/test: $(MAKEFILE) scripts/test.in
	$(MKDIR_P) scripts
	cat $(srcdir)/scripts/test.in > $@
	chmod u+x $@

EXTRA_DIST+=scripts/cbmcwrapper.in
scripts/cbmcwrapper: $(MAKEFILE) scripts/cbmcwrapper.in
	$(MKDIR_P) scripts
	sed \
	  -e 's|[@]CBMC[@]|$(CBMC)|g'\
	   $(srcdir)/scripts/cbmcwrapper.in > $@
	chmod u+x $@

dist-hook:
	rm -rf $$(find $(distdir)/libs/glloadgen-reloaded -type d -name .git)

.PHONY: indent
indent: $(SOURCES)
	$(CLANG_FORMAT) $(linted_CLANG_FORMATFLAGS) $^

.PHONY: doxygen
doxygen:
	sed \
	  -e 's|[@]srcdir[@]|$(srcdir)|g'\
	  -e 's|[@]PACKAGE_NAME[@]|$(PACKAGE_NAME)|g'\
	  -e 's|[@]PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g'\
	  $(srcdir)/Doxyfile.in | doxygen -

# We have to undefine certain flags because unfortunately we can't
# verify these compiler specific changes

.PHONY: json-c-models
json-c-models:
	$(MKDIR_P) json-c-models
	CONFIG=$$($(builddir)/config.status --config) &&\
	cd json-c-models &&\
	  (\
	     echo $${CONFIG} &&\
	     echo "CPPFLAGS=\"$(CPPFLAGS) -U_FORTIFY_SOURCE -U__OPTIMIZE__ -U_OPTIMIZE_SIZE__\"" &&\
	     echo "--disable-shared"\
	  ) | xargs\
	     $(abs_srcdir)/configure &&\
	  $(MAKE) $(AM_MAKEFLAGS) CC=$(abs_srcdir)/scripts/json-cc

LINTED_FRAMA_C_BINARY=simulator
.PHONY: frama-c-lint
frama-c-lint:
	BINARY=$(LINTED_FRAMA_C_BINARY) &&\
	(for II in $$($(srcdir)/scripts/flags json-c-models/$${BINARY}); do\
	  echo -cpp-extra-args="$${II}";\
	done && $(srcdir)/scripts/files json-c-models/$${BINARY}) | xargs\
	  $(FRAMA_C)\
	   -quiet\
	   -journal-disable\
	   -warn-signed-downcast\
	   -warn-unsigned-downcast\
	   -warn-unsigned-overflow\
	   -warn-decimal-float all\
	   -cpp-extra-args='-C'

.PHONY: cbmc-models
cbmc-models:
	$(MKDIR_P) cbmc-models
	CONFIG=$$($(builddir)/config.status --config) &&\
	cd cbmc-models &&\
	  (\
	     echo $${CONFIG} &&\
	     echo "CPPFLAGS=\"$(CPPFLAGS) -U_FORTIFY_SOURCE -U__OPTIMIZE__ -U_OPTIMIZE_SIZE__\"" &&\
	     echo "--disable-shared"\
	  ) | xargs\
	     $(abs_srcdir)/configure &&\
	  $(MAKE) $(AM_MAKEFLAGS) CC=$(GOTO_CC)

# This is not a real verification because our programs are unbounded
# and is only a basic double check.
.PHONY: cbmc-lint
cbmc-lint: scripts/cbmcwrapper
	./scripts/cbmcwrapper --unwind 30 --depth 300 cbmc-models/gui
	./scripts/cbmcwrapper --unwind 30 --depth 300 cbmc-models/simulator
	./scripts/cbmcwrapper --unwind 30 --depth 300 cbmc-models/linted
	./scripts/cbmcwrapper --unwind 30 --depth 300 cbmc-models/lintedctl

linted_CLANG_FORMATFLAGS=-i

include src/Makefile.am
