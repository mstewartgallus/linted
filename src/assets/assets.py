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

def output():
    def process_mesh(mesh, last_index):
        indices = [Array(3, GLubyte)([process_index(index, last_index)
                                      for index in polygon.vertices])
                   for polygon in polygons(mesh)]

        vertices = [Array(3, GLfloat)([GLfloat(part) for part in vertex.co.to_tuple()])
                    for vertex in mesh.vertices]

        return vertices, indices

    def process_index(index, last_index):
        new_index = last_index + index
        return GLubyte(new_index)

    # Compatibility shim for older Blender versions
    def polygons(mesh):
        if hasattr(mesh, "polygons"):
            return mesh.polygons
        else:
            return mesh.faces

    def load_shader(shadername):
        with open(shadername, 'r') as shaderfile:
            return shaderfile.read()

    def encode_shader(shader):
        return ("\""
                + shader.encode("unicode_escape")
                .decode("ascii")
                .replace("\"", "\\\"")
                + "\"")

    old_directory = os.getcwd()
    os.chdir(os.path.dirname(os.path.realpath(__file__)))
    try:
        bpy.ops.wm.open_mainfile(filepath = "scene.blend")

        cube = bpy.data.meshes[0]

        mesh_vertices, mesh_indices = process_mesh(cube, 0)

        indices = StaticArray(Array(3, GLubyte))(mesh_indices)
        vertices = StaticArray(Array(3, GLfloat))(mesh_vertices)

        varying_shader = load_shader("shaders/varying.glsl")

        fragment_shader = encode_shader(load_shader("shaders/fragment.glsl")
                                        .replace("#pragma linted include(\"varying.glsl\")",
                                                 varying_shader))

        vertex_shader = encode_shader(load_shader("shaders/vertex.glsl")
                                      .replace("#pragma linted include(\"varying.glsl\")",
                                               varying_shader))
    finally:
        os.chdir(old_directory)

    return Template("""#include "linted/assets.h"
#include "linted/check.h"
#include "linted/util.h"

static linted_assets_point const raw_data[] = $vertices;
linted_assets_point const * const linted_assets_triangle_data = raw_data;

static GLubyte const indices_data[][3] = $indices;

GLubyte const * const linted_assets_triangle_indices = &indices_data[0][0];
size_t const linted_assets_triangle_indices_size = LINTED_ARRAY_SIZE(indices_data);

GLchar const * const linted_assets_fragment_shader = $fragment_shader;
GLchar const * const linted_assets_vertex_shader = $vertex_shader;
""").substitute(
    vertices=vertices, indices=indices,
    fragment_shader=fragment_shader, vertex_shader=vertex_shader)
