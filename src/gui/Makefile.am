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

gui_SOURCES=src/gui/gui.c src/gui/assets.c src/gui/gl_core.c
gui_LDADD=libcommon.la libcontroller.la libshutdowner.la libupdater.la $(SDL_LIBS) $(GL_LIBS)

gui_CPPFLAGS=$(SDL_CFLAGS)
# Cancel out SDL's idiot defining of GNU source
gui_CPPFLAGS+=-U_GNU_SOURCE
gui_CPPFLAGS+=$(AM_CPPFLAGS)

nodist_linted_SOURCES+=src/gui/gl_core.c

BUILT_SOURCES+=\
  src/gui/gl_core.c src/gui/gl_core.h

EXTRA_DIST+=\
  src/gui/assets.h\
  \
  src/gui/assets/assets.py\
  src/gui/assets/scene.blend\
  src/gui/assets/shaders/fragment.glsl\
  src/gui/assets/shaders/varying.glsl\
  src/gui/assets/shaders/vertex.glsl\
  \
  src/gui/assets_generator/linted_assets_generator\
  src/gui/assets_generator/linted_assets_generator.py

MOSTLYCLEANFILES+=src/gui/assets.c
src/gui/assets.c:\
  src/gui/assets/assets.py\
  src/gui/assets/scene.blend\
  src/gui/assets/shaders/fragment.glsl\
  src/gui/assets/shaders/varying.glsl\
  src/gui/assets/shaders/vertex.glsl\
  \
  src/gui/assets_generator/linted_assets_generator\
  src/gui/assets_generator/linted_assets_generator.py
	BLENDER=$(BLENDER) $(srcdir)/src/gui/assets_generator/linted_assets_generator\
	  $(srcdir)/src/gui/assets/assets.py\
	  --output=$@

MOSTLYCLEANFILES+=src/gui/gl_core.c
src/gui/gl_core.c:
	cd $(builddir)/src/gui/ && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen-reloaded/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
	rm $(builddir)/include/gui/gl_core.h

MOSTLYCLEANFILES+=include/gui/gl_core.h
include/gui/gl_core.h:
	$(MKDIR_P) $(builddir)/include/gui/
	cd $(builddir)/include/gui && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen-reloaded/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
	rm $(builddir)/src/gui/gl_core.c
