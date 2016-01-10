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
  %D%/simulator.gpr\
  %D%/gcc.cgpr\
  %D%/pragmas.adc\
  \
  %D%/src

.PHONY: %D%/linted-simulator$(EXEEXT)
%D%/linted-simulator$(EXEEXT):
	gprbuild -P $(srcdir)/%D%/simulator.gpr -XO_Dir=$(abs_builddir)/%D%/ -XPragmas=$(srcdir)/%D%/pragmas.adc --config=$(srcdir)/%D%/gcc.cgpr -o $(abs_builddir)/%D%/linted-simulator$(EXEEXT)

dist_pkgdefaultconf_DATA+=%D%/simulator.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-simulator.service
