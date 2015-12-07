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
noinst_LTLIBRARIES+=%D%/libupdater.la

%C%_libupdater_la_SOURCES=\
  %D%/updater.c\
  include/lntd/updater.h

%C%_libupdater_la_CPPFLAGS=-I$(builddir)/%D% $(AM_CPPFLAGS)
%C%_libupdater_la_LIBADD=\
  %D%/librpc.la\
  src/io/libio.la\
  src/ko/libko.la\
  \
  $(MATH_LIBS)
dist_pkgdefaultunits_DATA+=%D%/units/updater.socket

noinst_LTLIBRARIES+=%D%/librpc.la
nodist_%C%_librpc_la_SOURCES=\
  %D%/updater_xdr.c\
  %D%/updater.h

%C%_librpc_la_CFLAGS=$(AM_CFLAGS) $(linted_CFLAGS_NO_WARN)

EXTRA_DIST+=%D%/updater.x

MOSTLYCLEANFILES+=%D%/updater_xdr.c
%D%/updater_xdr.c: %D%/updater.x
	$(linted_V_RPCGEN) $(RPCGEN) -N -c $< | sed -e 's|$(srcdir)/src/updater/updater.h|updater.h|g' > $@

BUILT_SOURCES+=%D%/updater.h
MOSTLYCLEANFILES+=%D%/updater.h
%D%/updater.h: %D%/updater.x
	$(linted_V_RPCGEN) $(RPCGEN) -N -h $< > $@

