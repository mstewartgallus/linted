# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2015 Steven Stewart-Gallus
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
EXTRA_DIST+=\
  %D%/Doxyfile.in\
  %D%/builds.h\
  %D%/coding.h\
  %D%/guarding.h\
  %D%/platforms.h\
  %D%/process-management.h\
  %D%/todo.h\
  %D%/tools.h

%D%/Doxyfile.out: %D%/Doxyfile.in Makefile
	$(MKDIR_P) "$$(dirname $@)"
	$(linted_V_SED) sed \
	  -e 's|[@]srcdir[@]|$(srcdir)|g'\
	  -e 's|[@]PACKAGE_NAME[@]|$(PACKAGE_NAME)|g'\
	  -e 's|[@]PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g'\
	  $< > $@

.PHONY: doxygen
doxygen: %D%/Doxyfile.out
	unset POSIXLY_CORRECT; exec doxygen $<
