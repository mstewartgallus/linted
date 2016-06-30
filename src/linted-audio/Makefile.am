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
pkglibexec_PROGRAMS+=%D%/linted-audio

EXTRA_DIST+=\
  %D%/audio.gpr\
  \
  %D%/src

.PHONY: %D%/linted-audio$(EXEEXT)
%D%/linted-audio$(EXEEXT): src/ada/shared.gpr src/ada/config.cgpr
	$(linted_V_GPRBUILD) $(GPRBUILD) $(AM_GPRBUILDFLAGS) $(GPRBUILDFLAGS) -o $(abs_builddir)/$@ -P $(srcdir)/%D%/audio.gpr

LINTED_CLEAN_RULES+=clean-%C%
.PHONY: clean-%C%
clean-%C%:
	- $(GPRCLEAN) $(GPRCLEANFLAGS) -P $(srcdir)/%D%/audio.gpr
