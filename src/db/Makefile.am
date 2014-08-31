# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2014 Steven Stewart-Gallus
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

noinst_LTLIBRARIES+=%D%/libdb.la
%C%_libdb_la_SOURCES=%D%/db.c include/linted/db.h
EXTRA_%C%_libdb_la_SOURCES=%D%/db-linux.c %D%/db-windows.c
%C%_libdb_la_LIBADD=\
  src/dir/libdir.la\
  src/error/liberror.la\
  src/file/libfile.la\
  src/io/libio.la\
  src/ko/libko.la\
  src/mem/libmem.la\
  src/random/librandom.la
