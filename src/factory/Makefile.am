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
dist_pkgdefaultconf_DATA+=\
  %D%/system.conf\
  %D%/audio.fstab\
  %D%/drawer.fstab\
  %D%/gui.fstab\
  %D%/window.fstab\
  %D%/simulator.fstab

dist_pkgdefaultunits_DATA+=\
  %D%/units/controller.socket\
  %D%/units/updater.socket\
  %D%/units/window.socket\
  %D%/units/window-notifier-gui.socket\
  %D%/units/window-notifier-drawer.socket\
  \
  %D%/units/linted-drawer.service\
  %D%/units/linted-gui.service\
  %D%/units/linted-simulator.service\
  %D%/units/linted-window.service

dist_pkgdefaultunits_DATA+=%D%/units/linted-audio.service
