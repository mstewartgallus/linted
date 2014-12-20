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
noinst_LTLIBRARIES+=%D%/libupdater.la

%C%_libupdater_la_SOURCES=\
  %D%/updater.c\
  include/linted/updater.h
%C%_libupdater_la_LIBADD=\
  src/asynch/libasynch.la\
  src/mem/libmem.la\
  src/ko/libko.la\
  \
  -lm
dist_pkgdefaultunits_DATA+=%D%/units/updater.socket
