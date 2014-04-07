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

noinst_LTLIBRARIES+=libcommon.la

BUILT_SOURCES+=src/linted/binaries.h

libcommon_la_SOURCES=\
  src/common/io.c\
  src/common/mq.c\
  src/common/sandbox.c\
  src/common/util.c\
  \
  include/linted/check.h\
  include/linted/io.h\
  include/linted/mq.h\
  include/linted/sandbox.h\
  include/linted/str.h\
  include/linted/util.h
libcommon_la_LIBADD=-lrt

include/linted/binaries.h: $(MAKEFILE) include/linted/binaries.h.in
	sed \
	  -e 's|[@]pkglibexecdir[@]|$(pkglibexecdir)|g'\
	  -e 's|[@]EXEEXT[@]|$(EXEEXT)|g'\
	  $(srcdir)/include/linted/binaries.h.in > $@
