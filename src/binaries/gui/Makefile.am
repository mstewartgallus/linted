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
pkglibexec_PROGRAMS+=gui

gui_SOURCES=src/binaries/gui/gui.c src/binaries/gui/assets.c src/binaries/gui/gl_core.c
gui_LDADD=libcommon.la libcontroller.la libshutdowner.la libupdater.la $(SDL_LIBS) $(GL_LIBS)

gui_CPPFLAGS=$(SDL_CFLAGS)
# Cancel out SDL's idiot defining of GNU source
gui_CPPFLAGS+=-U_GNU_SOURCE
gui_CPPFLAGS+=$(AM_CPPFLAGS)

nodist_linted_SOURCES+=src/binaries/gui/gl_core.c

BUILT_SOURCES+=\
  src/binaries/gui/gl_core.c src/linted/gl_core.h

EXTRA_DIST+=\
  src/linted/assets.h\
  \
  src/binaries/gui/assets/assets.py\
  src/binaries/gui/assets/scene.blend\
  src/binaries/gui/assets/shaders/fragment.glsl\
  src/binaries/gui/assets/shaders/varying.glsl\
  src/binaries/gui/assets/shaders/vertex.glsl\
  \
  src/binaries/gui/assets_generator/linted_assets_generator\
  src/binaries/gui/assets_generator/linted_assets_generator.py

MOSTLYCLEANFILES+=src/binaries/gui/assets.c
src/binaries/gui/assets.c:\
  src/binaries/gui/assets/assets.py\
  src/binaries/gui/assets/scene.blend\
  src/binaries/gui/assets/shaders/fragment.glsl\
  src/binaries/gui/assets/shaders/varying.glsl\
  src/binaries/gui/assets/shaders/vertex.glsl\
  \
  src/binaries/gui/assets_generator/linted_assets_generator\
  src/binaries/gui/assets_generator/linted_assets_generator.py
	BLENDER=$(BLENDER) $(srcdir)/src/binaries/gui/assets_generator/linted_assets_generator\
	  $(srcdir)/src/binaries/gui/assets/assets.py\
	  --output=$@

MOSTLYCLEANFILES+=\
  src/binaries/gui/gl_core.c src/linted/gl_core.h\
  src/linted/binaries.h

src/binaries/gui/gl_core.c:
	cd $(builddir)/src/binaries/gui/ && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen-reloaded/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
	rm $(builddir)/src/binaries/gui/gl_core.h

src/linted/gl_core.h:
	$(MKDIR_P) $(builddir)/src/linted
	cd $(builddir)/src/linted && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen-reloaded/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
	rm $(builddir)/src/linted/gl_core.c
