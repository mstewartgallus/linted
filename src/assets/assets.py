# Copyright 2013, 2014 Steven Stewart-Gallus
#
# Licensed under the Apache License, Version 2.0 (the "License"); you
# may not use this file except in compliance with the License.  You may
# obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.  See the License for the specific language governing
# permissions and limitations under the License.
import bpy
import os
from string import Template
from linted_assets_generator import *

def cd(local_file):
    return os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        local_file)

def _process_mesh(mesh, last_index):
    indices = [Array(3, GLubyte)([_process_index(index, last_index)
                       for index in polygon.vertices])
               for polygon in _polygons(mesh)]

    vertices = [Array(3, GLfloat)([GLfloat(part) for part in vertex.co.to_tuple()])
                for vertex in mesh.vertices]

    return vertices, indices

def _process_index(index, last_index):
    new_index = last_index + index
    return GLubyte(new_index)


# Compatibility shim for older Blender versions
def _polygons(mesh):
    if hasattr(mesh, "polygons"):
        return mesh.polygons
    else:
        return mesh.faces

bpy.ops.wm.open_mainfile(filepath = cd("scene.blend"))

cube = bpy.data.meshes[0]

mesh_vertices, mesh_indices = _process_mesh(cube, 0)

_indices = StaticArray(Array(3, GLubyte))(mesh_indices).flatten(0)
_vertices = StaticArray(Array(3, GLfloat))(mesh_vertices).flatten(0)

# _vertices = Array(3, Array(2, GLfloat))([
#     Array(2, GLfloat)([GLfloat(-0.4), GLfloat(-0.4)]),
#     Array(2, GLfloat)([GLfloat(0.4), GLfloat(-0.4)]),
#     Array(2, GLfloat)([GLfloat(0.0), GLfloat(0.4)])]).flatten(0)

# _indices = Array(3, GLubyte)([GLubyte(0), GLubyte(1), GLubyte(2)]).flatten(0)

output = Template("""#include "linted/assets.h"
#include "linted/check.h"
#include "linted/util.h"

static linted_assets_point const raw_data[] = $vertices;
linted_assets_point const * const linted_assets_triangle_data = raw_data;

static GLubyte const indices_data[][3] = $indices;

GLubyte const * const linted_assets_triangle_indices = &indices_data[0][0];
size_t const linted_assets_triangle_indices_size = LINTED_ARRAY_SIZE(indices_data);
""").substitute(vertices=_vertices, indices=_indices)
