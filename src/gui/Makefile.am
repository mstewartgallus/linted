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

gui_SOURCES=%D%/gui.c %D%/assets.c
nodist_gui_SOURCES=%D%/gl_core.c
gui_LDADD=\
  src/error/liberror.la\
  src/ko/libko.la\
  src/locale/liblocale.la\
  src/util/libutil.la\
  \
  src/controller/libcontroller.la\
  src/logger/liblogger.la\
  src/shutdowner/libshutdowner.la\
  src/updater/libupdater.la\
  \
  -lm\
  $(X11_LIBS)\
  $(XCB_LIBS)\
  $(X11_XCB_LIBS)\
  $(GL_LIBS)
gui_LDFLAGS=$(AM_LDFLAGS)

gui_CPPFLAGS=\
  $(X11_FLAGS) $(XCB_FLAGS) $(X11_XCB_FLAGS) $(GL_FLAGS)\
  -I$(builddir)/%D%/ -I$(srcdir)/%D%/ $(AM_CPPFLAGS)

BUILT_SOURCES+=%D%/gl_core.c %D%/gl_core.h

EXTRA_DIST+=\
  %D%/assets.h\
  \
  %D%/assets/assets.py\
  %D%/assets/scene.blend\
  %D%/assets/shaders/fragment.glsl\
  %D%/assets/shaders/varying.glsl\
  %D%/assets/shaders/vertex.glsl\
  \
  %D%/assets_generator/linted_assets_generator\
  %D%/assets_generator/linted_assets_generator.py

MOSTLYCLEANFILES+=%D%/assets.c
%D%/assets.c:\
  %D%/assets/assets.py\
  %D%/assets/scene.blend\
  %D%/assets/shaders/fragment.glsl\
  %D%/assets/shaders/varying.glsl\
  %D%/assets/shaders/vertex.glsl\
  \
  %D%/assets_generator/linted_assets_generator\
  %D%/assets_generator/linted_assets_generator.py
	BLENDER=$(BLENDER) $(srcdir)/%D%/assets_generator/linted_assets_generator\
	  $(srcdir)/%D%/assets/assets.py\
	  --output=$@

MOSTLYCLEANFILES+=%D%/gl_core.c
%D%/gl_core.c:
	cd $(builddir)/%D%/ && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen-reloaded/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core

MOSTLYCLEANFILES+=include/gui/gl_core.h
%D%/gl_core.h:
	$(MKDIR_P) $(builddir)/%D%/
	cd $(builddir)/%D% && \
	  $(LUA) $(abs_srcdir)/libs/glloadgen-reloaded/LoadGen.lua\
	  -style=pointer_c -spec=gl -version=2.1 -profile=core -prefix=linted_gl_ core
