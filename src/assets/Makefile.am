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

%C%_libassets_la_SOURCES=%D%/assets.c include/linted/assets.h

EXTRA_DIST+=\
  %D%/assets/assets.py\
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

MOSTLYCLEANFILES+=%D%/assets.c
%D%/assets.c:\
  %D%/assets/assets.py\
  %D%/assets/scene.blend\
  %D%/assets/shaders/fragment.glsl\
  %D%/assets/shaders/vertex.glsl\
  \
  %D%/assets_generator/linted_assets_generator\
  %D%/assets_generator/linted_assets_generator.py
	$(linted_V_BLENDER) BLENDER=$(BLENDER) $(srcdir)/%D%/assets_generator/linted_assets_generator\
	  $(srcdir)/%D%/assets/assets.py\
	  --output=$@
