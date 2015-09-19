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

%C%_linted_simulator_SOURCES=%D%/simulator.c %D%/sim.h
%C%_linted_simulator_LDADD=\
  -lm\
  src/start/libstart.la\
  \
  src/async/libasync.la\
  src/error/liberror.la\
  src/io/libio.la\
  src/ko/libko.la\
  src/log/liblog.la\
  src/sched/libsched.la\
  \
  src/controller/libcontroller.la\
  src/updater/libupdater.la

dist_pkgdefaultconf_DATA+=%D%/simulator.fstab
dist_pkgdefaultunits_DATA+=%D%/units/linted-simulator.service

TESTS+=%D%/test_simulator_angle

check_PROGRAMS+=%D%/test_simulator_angle
%C%_test_simulator_angle_SOURCES=%D%/test_simulator_angle.c
%C%_test_simulator_angle_LDADD=\
  -lm\
  src/async/libasync.la\
  src/io/libio.la\
  src/start/libstart.la

if WINDOWS_API
%C%_linted_simulator_LDADD+=%D%/resources.$(OBJEXT)
%D%/resources.$(OBJEXT): %D%/simulator.manifest
endif
