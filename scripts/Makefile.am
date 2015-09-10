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
  %D%/builddeps\
  %D%/configure-goto-cc\
  %D%/configure-json-cc\
  %D%/make-defaults\
  %D%/json-cc-files.py\
  %D%/json-cc-flags.py\
  %D%/json-cc.py\
  %D%/setsysroot

EXTRA_DIST+=\
  %D%/test.in\
  %D%/check-cbmc.in\
  %D%/check-cpa.in\
  %D%/check-clang-analysis.in\
  %D%/check-cppcheck.in\
  %D%/check-frama-c.in\
  %D%/check-iwyu.in\
  %D%/check-splint.in\
  %D%/cpa-config.in\
  %D%/check-clang-analysis.py
MOSTLYCLEANFILES+=\
  %D%/test.out\
  %D%/check-cbmc.out\
  %D%/check-cpa.out\
  %D%/check-clang-analysis.out\
  %D%/check-cppcheck.out\
  %D%/check-frama-c.out\
  %D%/check-iwyu.out\
  %D%/check-splint.out\
  %D%/cpa-config.out

all:\
  %D%/test.out\
  %D%/check-cbmc.out\
  %D%/check-cpa.out\
  %D%/check-clang-analysis.out\
  %D%/check-cppcheck.out\
  %D%/check-frama-c.out\
  %D%/check-iwyu.out\
  %D%/check-splint.out\
  %D%/cpa-config.out

%D%/test.out: Makefile
%D%/check-cbmc.out: Makefile
%D%/check-cpa.out: Makefile
%D%/check-clang-analysis.out: Makefile
%D%/check-cppcheck.out: Makefile
%D%/check-frama-c.out: Makefile
%D%/check-iwyu.out: Makefile
%D%/check-splint.out: Makefile
%D%/cpa-config.out: Makefile

SUFFIXES+= .out .in
.in.out:
	$(AM_V_at) $(MKDIR_P) "$$(dirname $@)"
	$(linted_V_SED)\
	  linted_OUT=$@\
	  linted_OUT="$$(cd "$$(dirname "$${linted_OUT}")" && pwd)/$$(basename "$${linted_OUT}")" &&\
	  linted_IN=$< &&\
	  cd "$$(dirname -- "$${linted_IN}")" && sed \
	  -e 's|[@]srcdir[@]|$(srcdir)|g'\
	  -e 's|[@]EXEEXT[@]|$(EXEEXT)|g'\
	  "$$(basename -- "$${linted_IN}")" > "$${linted_OUT}"
	$(AM_V_at) chmod u+x $@
