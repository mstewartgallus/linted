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

pkglibexec_PROGRAMS=\
  gui\
  simulator

noinst_LTLIBRARIES=\
  libcommon.la\
  libcontroller.la\
  libupdater.la\
  libshutdowner.la

linted_SOURCES=src/binaries/linted.c
linted_LDADD=libcommon.la libcontroller.la libshutdowner.la libupdater.la

gui_SOURCES=src/binaries/gui/gui.c src/binaries/gui/assets.c src/binaries/gui/gl_core.c
gui_LDADD=libcommon.la libcontroller.la libshutdowner.la libupdater.la $(SDL_LIBS)

gui_CPPFLAGS=$(SDL_CFLAGS)
# Cancel out SDL's idiot defining of GNU source
gui_CPPFLAGS+=-U_GNU_SOURCE
gui_CPPFLAGS+=$(AM_CPPFLAGS)

simulator_SOURCES=src/binaries/simulator.c
simulator_LDADD=libcommon.la libcontroller.la libshutdowner.la libupdater.la

libcommon_la_SOURCES=\
  src/common/io.c\
  src/common/mq.c\
  src/common/sandbox.c\
  src/common/util.c
libcommon_la_LIBADD=-lrt

libcontroller_la_SOURCES=src/protocols/controller.c
libcontroller_la_LIBADD=libcommon.la -lrt

libshutdowner_la_SOURCES=src/protocols/shutdowner.c
libshutdowner_la_LIBADD=libcommon.la -lrt

libupdater_la_SOURCES=src/protocols/updater.c
libupdater_la_LIBADD=libcommon.la -lrt

nodist_linted_SOURCES=\
  src/binaries/gui/gl_core.c

headers=\
  src/linted/assets.h\
  src/linted/check.h\
  src/linted/controller.h\
  src/linted/gui.h\
  src/linted/io.h\
  src/linted/mq.h\
  src/linted/sandbox.h\
  src/linted/shutdowner.h\
  src/linted/simulator.h\
  src/linted/util.h\
  src/linted/updater.h

BUILT_SOURCES=src/binaries/gui/gl_core.c src/linted/gl_core.h

EXTRA_DIST=\
  libs/glloadgen\
  \
  $(headers)\
  \
  src/binaries/gui/assets/assets.py\
  src/binaries/gui/assets/shaders/fragment.glsl\
  src/binaries/gui/assets/shaders/varying.glsl\
  src/binaries/gui/assets/shaders/vertex.glsl\
  \
  src/binaries/gui/assets_generator/linted_assets_generator\
  src/binaries/gui/assets_generator/linted_assets_generator.py

MOSTLYCLEANFILES=src/binaries/gui/assets.c
src/binaries/gui/assets.c:\
  src/binaries/gui/assets/assets.py\
  src/binaries/gui/assets/shaders/fragment.glsl\
  src/binaries/gui/assets/shaders/varying.glsl\
  src/binaries/gui/assets/shaders/vertex.glsl\
  \
  src/binaries/gui/assets_generator/linted_assets_generator\
  src/binaries/gui/assets_generator/linted_assets_generator.py
	BLENDER=$(BLENDER) $(srcdir)/src/binaries/gui/assets_generator/linted_assets_generator\
	  $(srcdir)/src/binaries/gui/assets/assets.py\
	  --output=$@

MOSTLYCLEANFILES+=src/binaries/gui/gl_core.c src/linted/gl_core.h
src/binaries/gui/gl_core.c:
	cd $(builddir)/src/binaries/gui/ && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
	rm $(builddir)/src/binaries/gui/gl_core.h

src/linted/gl_core.h:
	$(MKDIR_P) $(builddir)/src/linted
	cd $(builddir)/src/linted && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
	rm $(builddir)/src/linted/gl_core.c

dist-hook:
	rm -rf `find $(distdir)/libs/glloadgen -type d -name .git`

.PHONY:\
  check-frama-c-pdg check-frama-c-gui check-frama-c\
  check-cppcheck\
  check-clang-analyzer\
  check-cbmc\
  indent\
  doxygen

indent: $(linted_SOURCES) $(headers)
	$(INDENT) $(linted_INDENTFLAGS) $^

doxygen:
	sed \
	  -e 's|[@]srcdir[@]|$(srcdir)|g'\
	  -e 's|[@]PACKAGE_NAME[@]|$(PACKAGE_NAME)|g'\
	  -e 's|[@]PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g'\
	  $(srcdir)/Doxyfile.in | doxygen -

check-frama-c-pdg: $(linted_SOURCES_verified)
	$(MKDIR_P) pdg
	$(FRAMA_C) $(linted_FRAMAFLAGS) -pdg -pdg-dot pdg/main $(FRAMAFLAGS) $^

check-frama-c-gui: $(linted_SOURCES_verified)
	$(FRAMA_C_GUI) $(linted_FRAMAFLAGS) $(FRAMAFLAGS) $^

check-frama-c: $(linted_SOURCES_verified)
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


AM_LDFLAGS=\
  $(linted_LDFLAGS_OPTIMIZE)\
  $(linted_LDFLAGS_HARDEN)

AM_CPPFLAGS=\
  $(linted_CPPFLAGS_HARDEN)\
  $(linted_CPPFLAGS_LANGUAGE)\
  -I .\
  -I $(srcdir)/src\
  -I $(builddir)/src\
  -I $(builddir)/src/linted\
  -I src

AM_CFLAGS=\
  $(linted_CFLAGS_LANGUAGE)\
  $(linted_CFLAGS_OPTIMIZE)\
  $(linted_CFLAGS_HARDEN)\
  $(linted_CFLAGS_DEBUG)\
  $(linted_CFLAGS_WARNINGS)

linted_INDENTFLAGS=\
  --linux-style\
  --no-tabs\
  --indent-level4

linted_FRAMAFLAGS=\
  -quiet\
  -journal-disable\
  -warn-signed-downcast\
  -warn-unsigned-downcast\
  -warn-unsigned-overflow\
  -warn-decimal-float all\
  -cpp-extra-args="-I$$($(FRAMA_C) -print-path)/libc"\
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
