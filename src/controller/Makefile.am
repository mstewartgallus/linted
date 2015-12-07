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
noinst_LTLIBRARIES+=%D%/libcontroller.la

%C%_libcontroller_la_SOURCES=\
  %D%/controller.c\
  include/lntd/controller.h
%C%_libcontroller_la_CPPFLAGS=-I$(builddir)/%D% $(AM_CPPFLAGS)
%C%_libcontroller_la_LIBADD=\
  %D%/librpc.la\
  src/io/libio.la\
  src/ko/libko.la
dist_pkgdefaultunits_DATA+=%D%/units/controller.socket

noinst_LTLIBRARIES+=%D%/librpc.la
nodist_%C%_librpc_la_SOURCES=\
  %D%/controller_xdr.c\
  %D%/controller.h
%C%_librpc_la_CFLAGS=$(AM_CFLAGS) $(linted_CFLAGS_NO_WARN)

EXTRA_DIST+=%D%/controller.x

MOSTLYCLEANFILES+=%D%/controller_xdr.c
%D%/controller_xdr.c: %D%/controller.x
	$(linted_V_RPCGEN) $(RPCGEN) -N -c $< | sed -e 's|$(srcdir)/src/controller/controller.h|controller.h|g' > $@

BUILT_SOURCES+=%D%/controller.h
MOSTLYCLEANFILES+=%D%/controller.h
%D%/controller.h: %D%/controller.x
	$(linted_V_RPCGEN) $(RPCGEN) -N -h $< > $@
