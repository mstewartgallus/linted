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
pkglibexec_PROGRAMS+=%D%/linted-simulator

EXTRA_DIST+=\
  %D%/simulator.gpr.in\
  %D%/linted.gpr.in\
  %D%/shared.gpr.in\
  %D%/config.cgpr.in\
  %D%/pragmas.adc\
  \
  %D%/linted-simulator\
  %D%/linted

%D%/linted.gpr: %D%/linted.gpr.in Makefile
	$(linted_V_SED) sed \
	  -e 's|[@]srcdir[@]|$(abs_srcdir)/%D%|g'\
	  -e 's|[@]builddir[@]|$(abs_builddir)/%D%|g'\
	  $< > $@

%D%/shared.gpr: %D%/shared.gpr.in Makefile
	$(linted_V_SED) sed \
	  -e 's|[@]srcdir[@]|$(abs_srcdir)/%D%|g'\
	  -e 's|[@]builddir[@]|$(abs_builddir)/%D%|g'\
	  $< > $@

%D%/simulator.gpr: %D%/simulator.gpr.in Makefile
	$(linted_V_SED) sed \
	  -e 's|[@]srcdir[@]|$(abs_srcdir)/%D%|g'\
	  -e 's|[@]builddir[@]|$(abs_builddir)/%D%|g'\
	  $< > $@

%D%/config.cgpr: %D%/config.cgpr.in Makefile
	$(linted_V_SED) sed \
	  -e 's|[@]CCLD[@]|gcc|g'\
	  -e 's|[@]CC[@]|gcc|g'\
	  -e 's|[@]ADAC[@]|gcc|g'\
	  -e 's|[@]ADABIND[@]|/usr/lib/gprbind|g'\
	  $< > $@

.PHONY: %D%/linted-simulator$(EXEEXT)
%D%/linted-simulator$(EXEEXT): %D%/simulator.gpr %D%/linted.gpr %D%/shared.gpr %D%/config.cgpr
	$(linted_V_GPRBUILD) $(GPRBUILD) -q -P $(builddir)/%D%/simulator.gpr --config=$(builddir)/%D%/config.cgpr -o $(abs_builddir)/%D%/linted-simulator$(EXEEXT)

clean-local:
	- $(GPRCLEAN) -q -P $(builddir)/%D%/simulator.gpr --config=$(builddir)/%D%/config.cgpr

dist_pkgdefaultconf_DATA+=%D%/simulator.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-simulator.service
