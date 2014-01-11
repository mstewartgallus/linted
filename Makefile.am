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

ACLOCAL_AMFLAGS=-I m4

bin_PROGRAMS=linted

linted_SOURCES=\
  src/bin/linted.c\
  src/bin/gui.c\
  src/bin/sandbox.c\
  src/bin/supervisor.c\
  src/bin/task.c\
  $(verified_sources)

verified_sources=\
  src/bin/util.c\
  src/bin/simulator.c\
  src/bin/sprintf.c

linted_LDADD=$(SDL_LIBS)
linted_CPPFLAGS=$(SDL_CFLAGS) $(AM_CPPFLAGS)

EXTRA_DIST=\
  src/linted/gui.h\
  src/linted/sandbox.h\
  src/linted/simulator.h\
  src/linted/sprintf.h\
  src/linted/supervisor.h\
  src/linted/task.h\
  src/linted/util.h

src/bin/linted.$(OBJEXT): config.h\
  src/linted/gui.h\
  src/linted/sandbox.h\
  src/linted/simulator.h\
  src/linted/sprintf.h\
  src/linted/task.h\
  src/linted/util.h

src/bin/io.$(OBJEXT): config.h\
  src/linted/io.h\
  src/linted/util.h

src/bin/simulator.$(OBJEXT): config.h\
  src/linted/io.h\
  src/linted/simulator.h\
  src/linted/task.h\
  src/linted/util.h

src/bin/gui.$(OBJEXT): config.h\
  src/linted/gui.h\
  src/linted/io.h\
  src/linted/sprintf.h\
  src/linted/base/util.h

src/bin/sandbox.$(OBJEXT): config.h src/linted/sandbox.h
src/bin/sprintf.$(OBJEXT): config.h src/linted/util.h src/linted/sprintf.h
src/bin/supervisor.$(OBJEXT): config.h src/linted/supervisor.h
src/bin/task.$(OBJEXT): config.h src/linted/task.h
src/bin/util.$(OBJEXT): config.h src/linted/util.h

.PHONY:\
  check-frama-c-pdg check-frama-c-gui check-frama-c\
  check-cppcheck\
  check-clang-analyzer\
  check-cbmc\
  indent

indent: $(linted_SOURCES) $(EXTRA_DIST)
	$(INDENT) $(linted_INDENTFLAGS) $^

check-frama-c-pdg: $(verified_sources)
	$(MKDIR_P) pdg
	$(FRAMA_C) $(linted_FRAMAFLAGS) -pdg -pdg-dot pdg/main $(FRAMAFLAGS) $^

check-frama-c-gui: $(verified_sources)
	$(FRAMA_C_GUI) $(linted_FRAMAFLAGS) $(FRAMAFLAGS) $^

check-frama-c: $(verified_sources)
	$(FRAMA_C) $(linted_FRAMAFLAGS) -val $(FRAMAFLAGS) $^

check-cbmc: $(linted_SOURCES)
	$(CBMC) $(linted_CBMCFLAGS) $(CBMCFLAGS) $(linted_CPPFLAGS) $(linted_CFLAGS) $(CFLAGS) $^

check-cppcheck:
	$(CPPCHECK) $(linted_CPPCHECKFLAGS) $(CPPCHECKFLAGS) $(srcdir)/src

check-clang-analyzer: $(linted_SOURCES)
	$(CLANG) --analyze $(linted_CPPFLAGS) $(linted_CFLAGS) $(CFLAGS) $^
	- rm *.plist

check: $(linted_CHECK_PHASES)

clean-local:
	- rm -rf scan-report


AM_CPPFLAGS=$(linted_CPPFLAGS_HARDEN) -I . -I $(srcdir)/src -I src

AM_CFLAGS=\
  $(linted_CFLAGS_C99_STD)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_WARNINGS)
if SAVE_TEMPS
AM_CFLAGS +=-save-temps=obj
endif

AM_LDFLAGS=$(linted_LDFLAGS_OPTIMIZE) $(linted_LDFLAGS_HARDEN)

linted_INDENTFLAGS=\
  --linux-style\
  --line-length90

linted_FRAMAFLAGS=\
  -quiet\
  -journal-disable\
  -warn-signed-downcast\
  -warn-unsigned-downcast\
  -warn-unsigned-overflow\
  -warn-decimal-float all\
  -cpp-extra-args="-I`$(FRAMA_C_GUI) -print-path`/libc"\
  -cpp-extra-args='-C'\
  -cpp-extra-args='$(linted_CPPFLAGS)'\
  -cpp-extra-args='$(CPPFLAGS)'\
  -main linted_simulator_run

linted_CBMCFLAGS=\
  --no-unwinding-assertions\
  --unwind 6\
  --bounds-check\
  --div-by-zero-check\
  --nan-check\
  --pointer-check\
  --overflow-check

linted_CHECK_PHASES=
if HAS_CPPCHECK
linted_CHECK_PHASES +=check-cppcheck
endif
if HAS_CLANG
linted_CHECK_PHASES +=check-clang-analyzer
endif
