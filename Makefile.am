# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2013 Steven Stewart-Gallus
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
pkglibexec_PROGRAMS=linted-gui linted-simulator

linted_SOURCES=src/linted.c
linted_LDADD=liblinted.la

linted_gui_SOURCES=src/gui.c
linted_gui_LDADD=liblinted.la $(SDL_LIBS)
linted_gui_CPPFLAGS=$(SDL_CFLAGS) $(AM_CPPFLAGS)

linted_simulator_SOURCES=src/simulator.c
linted_simulator_LDADD=liblinted.la

pkglib_LTLIBRARIES=liblinted.la
liblinted_la_SOURCES=\
	src/lib/gui.c\
	src/lib/actor.c\
	src/lib/util.c\
	src/lib/base/stdio.c

EXTRA_DIST=\
	src/linted/actor.h\
	src/linted/gui.h\
	src/linted/base/stdio.h\
	src/linted/util.h


src/linted/binaries.h: src/linted/binaries.h.in Makefile
	$(MKDIR_P) src/linted
	sed \
	  -e 's|@exeext[@]|$(EXEEXT)|g' \
	  -e 's|@pkglibexecdir[@]|$(pkglibexecdir)|g' \
	  > $@ < $<


src/linted.o: config.h src/linted/binaries.h

src/simulator.o: config.h\
	src/linted/gui.h src/linted/actor.h\
	src/linted/base/stdio.h src/linted/util.h

src/lib/gui_impl.o: config.h\
	src/linted/gui.h src/linted/actor.h\
	src/linted/base/stdio.h src/linted/base/util.h

src/lib/gui.o: config.h src/linted/gui.h src/linted/actor.h
src/lib/actor.o: config.h src/linted/actor.h
src/lib/util.o: config.h src/linted/util.h
src/lib/base/stdio.o: config.h src/linted/util.h src/linted/base/stdio.h

.PHONY:\
	frama-c-pdg frama-c-gui frama-c-check\
	cppcheck\
	clang-analyzer

frama-c-pdg: $(linted_simulator_SOURCES) $(liblinted_la_SOURCES)
	$(MKDIR_P) pdg
	$(FRAMA_C) $(linted_FRAMAFLAGS) -pdg -pdg-dot pdg/main $(FRAMAFLAGS) $^

frama-c-gui: $(linted_simulator_SOURCES) $(liblinted_la_SOURCES)
	$(FRAMA_C_GUI) $(linted_FRAMAFLAGS) $(FRAMAFLAGS) $^

frama-c-check: $(linted_simulator_SOURCES) $(liblinted_la_SOURCES)
	$(FRAMA_C) $(linted_FRAMAFLAGS) -val $(FRAMAFLAGS) $^

cppcheck:
	$(CPPCHECK) $(linted_CPPCHECKFLAGS) $(CPPCHECKFLAGS) $(srcdir)/src

clang-analyzer: $(linted_simulator_SOURCES) $(liblinted_la_SOURCES)
	$(CLANG) --analyze $(AM_CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) $^
	- rm *.plist

check: $(linted_CHECK_PHASES)

clean-local:
	- rm -rf scan-report


AM_CPPFLAGS=$(linted_CPPFLAGS_HARDEN) -I . -I $(srcdir)/src -I src

AM_CFLAGS=\
	$(linted_C99_STD)\
	$(linted_CFLAGS_OPTIMIZE)\
	$(linted_CFLAGS_HARDEN)\
	$(linted_CFLAGS_DEBUG)\
	$(linted_CFLAGS_WARNINGS)
if SAVE_TEMPS
AM_CFLAGS +=-save-temps=obj
endif

AM_LDFLAGS=$(linted_LDFLAGS_OPTIMIZE) $(linted_LDFLAGS_HARDEN)

linted_FRAMAFLAGS=\
	-quiet\
	-journal-disable\
	-warn-signed-downcast\
	-warn-unsigned-downcast\
	-warn-unsigned-overflow\
	-warn-decimal-float all\
	-cpp-extra-args="-I`$(FRAMA_C_GUI) -print-path`/libc"\
	-cpp-extra-args='-C'\
	-cpp-extra-args='$(AM_CPPFLAGS)'\
	-cpp-extra-args='$(CPPFLAGS)'\
	-main simulator_main

linted_CHECK_PHASES=
if HAS_CPPCHECK
linted_CHECK_PHASES +=cppcheck
endif
if HAS_CLANG
linted_CHECK_PHASES +=clang-analyzer
endif
if HAS_FRAMA_C
linted_CHECK_PHASES +=frama-c-check
endif
