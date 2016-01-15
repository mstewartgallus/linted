# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2015 Steven Stewart-Gallus
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
EXTRA_DIST+=\
  %D%/linted.gpr\
  %D%/shared.gpr.in\
  %D%/config.cgpr.in\
  %D%/pragmas.adc\
  \
  %D%/linted

%D%/shared.gpr: %D%/shared.gpr.in Makefile
	$(MKDIR_P) %D%
	$(linted_V_SED) sed \
	  -e 's|[@]srcdir[@]|$(abs_srcdir)|g'\
	  -e 's|[@]builddir[@]|$(abs_builddir)|g'\
	  $< > $@

%D%/config.cgpr: %D%/config.cgpr.in Makefile
	$(MKDIR_P) %D%
	$(linted_V_SED) sed \
	  -e 's|[@]CCLD[@]|gcc|g'\
	  -e 's|[@]CC[@]|gcc|g'\
	  -e 's|[@]ADAC[@]|gcc|g'\
	  -e 's|[@]ADABIND[@]|/usr/lib/gprbind|g'\
	  $< > $@

mostlyclean-local: clean-%C%
.PHONY: clean-%C%
clean-%C%:
	- $(GPRCLEAN) -q -aP$(builddir)/%D% -aP$(srcdir)/%D% -P $(builddir)/%D%/linted.gpr --config=$(builddir)/%D%/config.cgpr
