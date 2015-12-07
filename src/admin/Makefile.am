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
noinst_LTLIBRARIES+=%D%/libadmin.la

%C%_libadmin_la_SOURCES=\
  %D%/admin.c\
  include/lntd/admin.h

%C%_libadmin_la_CPPFLAGS=-I$(builddir)/%D% $(AM_CPPFLAGS)
%C%_libadmin_la_LIBADD=\
  %D%/librpc.la\
  src/async/libasync.la\
  src/io/libio.la\
  src/ko/libko.la

noinst_LTLIBRARIES+=%D%/librpc.la
nodist_%C%_librpc_la_SOURCES=\
  %D%/admin_xdr.c\
  %D%/admin.h

%C%_librpc_la_CFLAGS=$(AM_CFLAGS) $(linted_CFLAGS_NO_WARN)

EXTRA_DIST+=%D%/admin.x

MOSTLYCLEANFILES+=%D%/admin_xdr.c
%D%/admin_xdr.c: %D%/admin.x
	$(linted_V_RPCGEN) $(RPCGEN) -N -c $< | sed -e 's|$(srcdir)/src/admin/admin.h|admin.h|g' > $@

BUILT_SOURCES+=%D%/admin.h
MOSTLYCLEANFILES+=%D%/admin.h
%D%/admin.h: %D%/admin.x
	$(linted_V_RPCGEN) $(RPCGEN) -N -h $< > $@

