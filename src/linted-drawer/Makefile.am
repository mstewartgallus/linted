# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2016 Steven Stewart-Gallus
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
pkglibexec_PROGRAMS+=%D%/linted-drawer

EXTRA_DIST+=\
  %D%/drawer.gpr\
  \
  %D%/src

.PHONY: %D%/linted-drawer$(EXEEXT)
%D%/linted-drawer$(EXEEXT): src/ada/shared.gpr src/gpu/libgpu.la
	$(linted_V_GPRBUILD) env $(ADAENV) $(GPRBUILD) $(AM_GPRBUILDFLAGS) $(GPRBUILDFLAGS) -o $(abs_builddir)/$@ -P $(srcdir)/%D%/drawer.gpr

LINTED_CLEAN_RULES+=clean-%C%
.PHONY: clean-%C%
clean-%C%:
	- env $(ADAENV) $(GPRCLEAN) $(GPRCLEANFLAGS) -P $(srcdir)/%D%/drawer.gpr
