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
pkglibexec_PROGRAMS+=%D%/linted

EXTRA_DIST+=\
  %D%/src

%C%_linted_DEPS=\
   $(builddir)/%D%/src/linted-settings.ads\
   $(builddir)/%D%/start.gpr

.PHONY: %D%/linted$(EXEEXT)
%D%/linted$(EXEEXT): $(builddir)/%D%/start.gpr src/ada/shared.gpr $(builddir)/%D%/src/linted-settings.ads
	$(linted_V_GPRBUILD) env ADA_PROJECT_PATH=$(GPRPROJECTPATH) $(GPRBUILD) $(AM_GPRBUILDFLAGS) $(GPRBUILDFLAGS) -o $(abs_builddir)/$@ -P $(builddir)/%D%/start.gpr

LINTED_CLEAN_RULES+=clean-%C%
.PHONY: clean-%C%
clean-%C%:
	- env ADA_PROJECT_PATH=$(GPRPROJECTPATH) $(GPRCLEAN) $(GPRCLEANFLAGS) -P $(srcdir)/%D%/start.gpr

EXTRA_DIST+=%D%/src/linted-settings.ads.in
MOSTLYCLEANFILES+=%D%/src/linted-settings.ads
%D%/src/linted-settings.ads: %D%/src/linted-settings.ads.in Makefile
	$(MKDIR_P) %D%/src
	$(linted_V_SED) sed \
	  -e 's|[@]LNTD_SYSTEM_CONF_PATH[@]|$(pkgconfdir)/system.conf:$(pkgdefaultconfdir)/system.conf|g'\
	  -e 's|[@]LNTD_UNIT_PATH[@]|$(pkgunitsdir):$(pkgdefaultunitsdir)|g'\
	  -e 's|[@]LNTD_INIT[@]|$(pkglibexecdir)/linted-init$(EXEEXT)|g'\
	  -e 's|[@]LNTD_MONITOR[@]|$(pkglibexecdir)/linted-monitor$(EXEEXT)|g'\
	  -e 's|[@]LNTD_STARTUP[@]|$(pkglibexecdir)/linted-startup$(EXEEXT)|g'\
	  -e 's|[@]LNTD_SANDBOX[@]|$(pkglibexecdir)/linted-sandbox$(EXEEXT)|g'\
	  -e 's|[@]LNTD_WAITER[@]|$(pkglibexecdir)/linted-waiter$(EXEEXT)|g'\
	  -e 's|[@]LNTD_AUDIO[@]|$(pkglibexecdir)/linted-audio$(EXEEXT)|g'\
	  -e 's|[@]LNTD_AUDIO_FSTAB[@]|$(pkgdefaultconfdir)/audio.fstab|g'\
	  -e 's|[@]LNTD_GUI[@]|$(pkglibexecdir)/linted-gui$(EXEEXT)|g'\
	  -e 's|[@]LNTD_GUI_FSTAB[@]|$(pkgdefaultconfdir)/gui.fstab|g'\
	  -e 's|[@]LNTD_SIMULATOR[@]|$(pkglibexecdir)/linted-simulator$(EXEEXT)|g'\
	  -e 's|[@]LNTD_SIMULATOR_FSTAB[@]|$(pkgdefaultconfdir)/simulator.fstab|g'\
	  -e 's|[@]LNTD_DRAWER[@]|$(pkglibexecdir)/linted-drawer$(EXEEXT)|g'\
	  -e 's|[@]LNTD_DRAWER_FSTAB[@]|$(pkgdefaultconfdir)/drawer.fstab|g'\
	  -e 's|[@]LNTD_WINDOW[@]|$(pkglibexecdir)/linted-window$(EXEEXT)|g'\
	  -e 's|[@]LNTD_WINDOW_FSTAB[@]|$(pkgdefaultconfdir)/window.fstab|g'\
	  $< > $@

EXTRA_DIST+=%D%/start.gpr.in
MOSTLYCLEANFILES+=%D%/start.gpr
%D%/start.gpr: %D%/start.gpr.in Makefile
	$(MKDIR_P) %D%/src
	$(linted_V_SED) sed \
	  -e 's|[@]abs_builddir[@]|$(abs_builddir)|g'\
	  -e 's|[@]abs_srcdir[@]|$(abs_srcdir)|g'\
	  $< > $@
