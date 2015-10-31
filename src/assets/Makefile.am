# Process this file with automake to produce Makefile.in (in this,
# and all subdirectories).
#
# Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
noinst_LTLIBRARIES+=%D%/libassets.la

%C%_libassets_la_SOURCES=\
  include/linted/assets.h\
  %D%/fragment_shader.c\
  %D%/vertex_shader.c\
  %D%/assets.c

EXTRA_DIST+=\
  %D%/assets/assets.py\
  %D%/assets/fragment_shader.py\
  %D%/assets/vertex_shader.py\
  %D%/assets/scene.blend\
  %D%/assets/shaders/fragment.glsl\
  %D%/assets/shaders/vertex.glsl\
  \
  %D%/assets_generator/linted_assets_generator\
  %D%/assets_generator/linted_assets_generator.py

linted_V_BLENDER= $(linted_v_BLENDER_@AM_V@)
linted_v_BLENDER_= $(linted_v_BLENDER_@AM_DEFAULT_V@)
linted_v_BLENDER_0= @echo "  BLENDER " $@;
linted_v_BLENDER_1 =

linted_V_PYTHON3= $(linted_v_PYTHON3_@AM_V@)
linted_v_PYTHON3= $(linted_v_PYTHON3_@AM_DEFAULT_V@)
linted_v_PYTHON3_0= @echo "  PYTHON3 " $@;
linted_v_PYTHON3_1 =

MOSTLYCLEANFILES+=%D%/assets.c
%D%/assets.c:\
  %D%/assets/assets.py\
  %D%/assets/scene.blend\
  \
  %D%/assets_generator/linted_assets_generator\
  %D%/assets_generator/linted_assets_generator.py
	$(linted_V_BLENDER) BLENDER=$(BLENDER) $(srcdir)/%D%/assets_generator/linted_assets_generator\
	 $<\
	  --output=$@

MOSTLYCLEANFILES+=%D%/fragment_shader.c
%D%/fragment_shader.c:\
  %D%/assets/fragment_shader.py\
  %D%/assets/shaders/fragment.glsl\
  \
  %D%/assets_generator/linted_assets_generator.py
	$(linted_V_PYTHON3) PYTHONDONTWRITEBYTECODE=1 $(PYTHON3) $(srcdir)/%D%/assets_generator/linted_assets_generator.py --\
	  $<\
	  --output=$@

MOSTLYCLEANFILES+=%D%/vertex_shader.c
%D%/vertex_shader.c:\
  %D%/assets/vertex_shader.py\
  %D%/assets/shaders/vertex.glsl\
  \
  %D%/assets_generator/linted_assets_generator.py
	$(linted_V_PYTHON3) PYTHONDONTWRITEBYTECODE=1 $(PYTHON3) $(srcdir)/%D%/assets_generator/linted_assets_generator.py --\
	 $<\
	  --output=$@
