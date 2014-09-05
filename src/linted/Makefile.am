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
bin_PROGRAMS+=%D%/linted

%C%_linted_SOURCES=%D%/linted.c
%C%_linted_CPPFLAGS=-I$(builddir)/%D%/ $(AM_CPPFLAGS)
%C%_linted_LDADD=\
  src/io/libio.la\
  src/locale/liblocale.la\
  src/start/libstart.la

BUILT_SOURCES+=%D%/settings.h
EXTRA_DIST+=%D%/settings.h.in
MOSTLYCLEANFILES+=%D%/settings.h
%D%/settings.h: Makefile %D%/settings.h.in
	sed \
	  -e 's|[@]pkglibexecdir[@]|$(pkglibexecdir)|g'\
	  -e 's|[@]chrootdir[@]|$(chrootdir)|g'\
	  -e 's|[@]pkgconfdir[@]|$(pkgconfdir)|g'\
	  -e 's|[@]pkgdefaultconfdir[@]|$(pkgdefaultconfdir)|g'\
	  -e 's|[@]pkgdefaultunitsdir[@]|$(pkgdefaultunitsdir)|g'\
	  -e 's|[@]EXEEXT[@]|$(EXEEXT)|g'\
	  $(srcdir)/%D%/settings.h.in > $@
