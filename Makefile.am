# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2013,2014,2015,2016 Steven Stewart-Gallus
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

pkgunitsdir=$(pkgconfdir)/units
pkgdefaultunitsdir=$(pkgdefaultconfdir)/units

AM_TESTS_ENVIRONMENT=\
  export MALLOC_CHECK_=3;\
  export MALLOC_PERTURB_=57005;

if HAVE_VALGRIND
LOG_COMPILER=$(VALGRIND) --error-exitcode=1 --quiet --
endif

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

SUFFIXES=

AM_LIBTOOLFLAGS=
AM_CPPFLAGS=
AM_CFLAGS=
AM_LDFLAGS=

linted_CLANG_FORMATFLAGS=

EXTRA_DIST+=%D%/configure-defaults

AM_LIBTOOLFLAGS+=--silent

AM_CFLAGS+=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_DETERMINISTIC)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_DEBUG_INFO)\
  $(linted_CFLAGS_META)\
  $(linted_CFLAGS_WARNINGS)\
  $(linted_CFLAGS_WARNINGS_PEDANTIC)

if LINTED_DETERMINISTIC
AM_CFLAGS+=-frandom-seed=$$(md5sum $< | sed -e 's/\(.*\) .*/\1/')
endif

if WINDOWS_API
AM_CFLAGS+=$(linted_CFLAGS_UNICODE)
endif

AM_CPPFLAGS+=-I$(srcdir)/include
if !HAVE_STDATOMIC_H
AM_CPPFLAGS+=-I$(srcdir)/include/portable-stdatomic
endif
if !HAVE_RPC_RPC_H
AM_CPPFLAGS+=-I$(srcdir)/include/portable-xdr
endif
AM_LDFLAGS+=\
  $$(for II in \
  $(linted_LDFLAGS_TARGET_TYPE)\
  $(linted_LDFLAGS_LANGUAGE)\
  $(linted_LDFLAGS_DETERMINISTIC)\
  $(linted_LDFLAGS_OPTIMIZE)\
  $(linted_LDFLAGS_HARDEN)\
  $(linted_LDFLAGS_DEBUG)\
  $(linted_LDFLAGS_DEBUG_INFO)\
  $(linted_LDFLAGS_META)\
  $(linted_LDFLAGS_WARNINGS); do echo -Xcompiler "$$II"; done)

GPRPROJECTPATH=$(builddir)/src/ada:$(srcdir)/src/ada-libc:$(srcdir)/src/ada-xcb:$(srcdir)/src/ada-xkb:$(srcdir)/src/ada-pulse:$(srcdir)/src/ada-core:$(srcdir)/src/spark_lemmas

ADAENV=SPARK_LEMMAS_INSTALLED=False SPARK_LEMMAS_OBJECT_DIR=$(abs_builddir)/src/ada GPR_PROJECT_PATH=$(GPRPROJECTPATH)

AM_GPRBUILDFLAGS=-q\
  -s\
  --create-missing-dirs\
  --autoconf=$(builddir)/auto.cgpr\
  --single-compile-per-obj-dir\
  --no-indirect-imports
AM_GNATPPFLAGS=-q -A0 -rnb
GPRCLEANFLAGS=-q --config=$(builddir)/auto.cgpr
GPRPROVEFLAGS=-j3 --level=0 --pedantic

LINTED_CLEAN_RULES=

mostlyclean-local: $(LINTED_CLEAN_RULES)

LINTED_CLEAN_RULES+=clean-%C%
.PHONY: clean-%C%
clean-%C%:
	- find "$(builddir)" -type f \(\
	  -name '*.dwo'\
	  -o -name '*.su'\
	  -o -name '*.bc'\
	  -o -name '*.s'\
	  -o -name '*.i'\
	  -o -name '*.res'\
	  -o -name '*.out'\
	  \) -exec rm -f -- '{}' '+'

.PHONY: prove
prove: src/ada/shared.gpr
	env $(ADAENV) gnatprove $(GPRPROVEFLAGS) -P$(srcdir)/src/linted-drawer/drawer.gpr

.PHONY: indent
indent:
	- find $(srcdir)/src $(srcdir)/include/lntd  $(srcdir)/include/fake-stdatomic $(srcdir)/docs -type f -\( -name '*.[ch]' -o -name '*.nc' -\) -exec \
	   $(CLANG_FORMAT) -style=file $(linted_CLANG_FORMATFLAGS) '{}' '+'
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(builddir)/src/linted/start.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/linted-audio/audio.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/linted-drawer/drawer.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/linted-simulator/simulator.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/ada-core/linted.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/ada-libc/libc.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/ada-pulse/pulse.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/ada-xcb/xcb.gpr
	- env $(ADAENV) gnatpp $(AM_GNATPPFLAGS) -P$(srcdir)/src/ada-xkb/xkb.gpr

CPROGRAMS=\
  src/linted/linted\
  src/linted-init/linted-init\
  src/linted-startup/linted-startup\
  src/linted-sandbox/linted-sandbox\
  src/linted-monitor/linted-monitor\
  src/linted-gui/linted-gui\
  src/linted-drawer/linted-drawer\
  src/linted-window/linted-window

.PHONY: list-programs
list-programs:
	@for II in $(CPROGRAMS) $(check_PROGRAMS); do printf -- '%s\n' "$${II}"; done

linted_CLANG_FORMATFLAGS+=-i

SUFFIXES+= .rc
.rc.$(OBJEXT):
	$(linted_V_WINDRES) $(WINDRES) -i $< -o $@ --output-format=coff

linted_V_SED = $(linted_v_SED_@AM_V@)
linted_v_SED_ = $(linted_v_SED_@AM_DEFAULT_V@)
linted_v_SED_0 = @echo "  SED     " $@;
linted_v_SED_1 =

linted_V_RPCGEN = $(linted_v_RPCGEN_@AM_V@)
linted_v_RPCGEN_ = $(linted_v_RPCGEN_@AM_DEFAULT_V@)
linted_v_RPCGEN_0 = @echo "  RPCGEN  " $@;
linted_v_RPCGEN_1 =

linted_V_WINDRES = $(linted_v_WINDRES_@AM_V@)
linted_v_WINDRES_ = $(linted_v_WINDRES_@AM_DEFAULT_V@)
linted_v_WINDRES_0 = @echo "  WINDRES " $@;
linted_v_WINDRES_1 =

linted_V_NESCC = $(linted_v_NESCC_@AM_V@)
linted_v_NESCC_ = $(linted_v_NESCC_@AM_DEFAULT_V@)
linted_v_NESCC_0 = @echo "  NESCC   " $@;
linted_v_NESCC_1 =

linted_V_GPRBUILD = $(linted_v_GPRBUILD_@AM_V@)
linted_v_GPRBUILD_ = $(linted_v_GPRBUILD_@AM_DEFAULT_V@)
linted_v_GPRBUILD_0 = @echo "  GPRBUILD" $@;
linted_v_GPRBUILD_1 =

include docs/Makefile.am
include scripts/Makefile.am
include src/Makefile.am
