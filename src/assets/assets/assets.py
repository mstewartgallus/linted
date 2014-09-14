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
import collections
import os
from string import Template
from linted_assets_generator import *
from io import StringIO

def output():
    def process_mesh(objct, indices, normals, vertices):
        mesh = objct.data
        matrix = objct.matrix_world

        old_vertices_length = len(vertices)

        for polygon in polygons(mesh):
            indices.append(Array(3, GLubyte)([GLubyte(index + old_vertices_length)
                                              for index in polygon.vertices]))

        for vertex in mesh.vertices:
            normals.append(Array(3, GLfloat)([GLfloat(part) for part in vertex.normal.to_tuple()]))
            vertices.append(Array(3, GLfloat)([GLfloat(part) for part in (matrix * vertex.co).to_tuple()]))

    # Compatibility shim for older Blender versions
    def polygons(mesh):
        if hasattr(mesh, "polygons"):
            return mesh.polygons
        else:
            return mesh.faces

    def load_shader(shadername):
        with StringIO() as lines:
            with open(shadername, 'r') as shaderfile:
                for line in shaderfile:
                    if line.startswith("#pragma linted include(\"") and line.endswith("\")\n"):
                        lines.write(load_shader(line[len("#pragma linted include(\""):-len("\")\n")]))
                    else:
                        lines.write(line)
                return lines.getvalue()

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

        mesh_objects = [objct for objct in bpy.data.objects if 'MESH' == objct.type]
        indices = []
        normals = []
        vertices = []
        for objct in mesh_objects:
            process_mesh(objct, indices, normals, vertices)

        fragment_shader = encode_shader(load_shader("shaders/fragment.glsl"))
        vertex_shader = encode_shader(load_shader("shaders/vertex.glsl"))
    finally:
        os.chdir(old_directory)

    return Template("""#include "config.h"

#include "linted/assets.h"

#include "linted/util.h"

static linted_assets_point const vertices[] = $vertices;
linted_assets_point const * const linted_assets_vertices = vertices;

static linted_assets_point const normals[] = $normals;
linted_assets_point const * const linted_assets_normals = normals;

static unsigned char const indices_data[][3u] = $indices;

unsigned char const * const linted_assets_indices = &indices_data[0U][0U];
size_t const linted_assets_indices_size = LINTED_ARRAY_SIZE(indices_data);

char const * const linted_assets_fragment_shader = $fragment_shader;
char const * const linted_assets_vertex_shader = $vertex_shader;
""").substitute(
    normals=StaticArray(Array(3, GLfloat))(normals),
    vertices=StaticArray(Array(3, GLfloat))(vertices),
    indices=StaticArray(Array(3, GLubyte))(indices),
    fragment_shader=fragment_shader, vertex_shader=vertex_shader)
