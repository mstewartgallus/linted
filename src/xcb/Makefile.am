# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2014, 2015 Steven Stewart-Gallus
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
noinst_LTLIBRARIES+=%D%/libxcb.la

%C%_libxcb_la_SOURCES=%D%/xcb.c include/lntd/xcb.h
%C%_libxcb_la_LIBADD=$(XCB_LIBS)
%C%_libxcb_la_CPPFLAGS=$(XCB_FLAGS) $(AM_CPPFLAGS)
