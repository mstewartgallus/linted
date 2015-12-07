# Copyright 2013, 2014, 2015 Steven Stewart-Gallus
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
import functools
import os
from string import Template

def output():
    def process_mesh(objct, indices, normals, vertices):
        mesh = objct.data
        matrix = objct.matrix_world

        old_vertices_length = len(vertices)

        for polygon in polygons(mesh):
            indices.append(Array(3, GLubyte)([GLubyte(index + old_vertices_length)
                                              for index in polygon.vertices]))

        for vertex in mesh.vertices:
            x_norm, y_norm, z_norm = vertex.normal.to_tuple()
            x, y, z = (matrix * vertex.co).to_tuple()
            normals.append(Array(3, GLfloat)((GLfloat(x_norm), GLfloat(y_norm), GLfloat(z_norm))))
            vertices.append(Array(3, GLfloat)((GLfloat(x), GLfloat(y), GLfloat(z))))

    # Compatibility shim for older Blender versions
    def polygons(mesh):
        if hasattr(mesh, "polygons"):
            return mesh.polygons
        else:
            return mesh.faces

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
    finally:
        os.chdir(old_directory)

    return Template("""#include "config.h"

#include "lntd/assets.h"

#include "lntd/util.h"

#include <stddef.h>
#include <stdint.h>

static lntd_assets_point const vertices[] = $vertices;
lntd_assets_point const * const lntd_assets_vertices = vertices;

size_t const lntd_assets_size = LNTD_ARRAY_SIZE(vertices);

static lntd_assets_point const normals[] = $normals;
lntd_assets_point const * const lntd_assets_normals = normals;

static uint16_t const indices_data[][3U] = $indices;

uint16_t const * const lntd_assets_indices = &indices_data[0U][0U];
size_t const lntd_assets_indices_size = LNTD_ARRAY_SIZE(indices_data);
""").substitute(
    normals=StaticArray(Array(3, GLfloat))(normals),
    vertices=StaticArray(Array(3, GLfloat))(vertices),
    indices=StaticArray(Array(3, GLubyte))(indices))

class C:
    def __str__(self):
        return self.flatten()

def structure(typename: str, fields: list):

    fieldnames = [name for (name, tp) in fields]
    members = [_spacing + tp.name + " " + prop + ";" for (prop, tp) in fields]

    class Structure(collections.namedtuple(typename, fieldnames), C):
        __name__ = typename

        name = typename

        definition = (
            "struct "
            + typename
            + " {\n"
            + "\n".join(members)
            + "\n};"
        )

        def flatten(self, indent: int = 0):
            cls = type(self)
            typename = cls.name

            if 0 == len(fields):
                return typename

            property_list = ["." + name + " = " + getattr(self, name).flatten(indent + 1)
                             for name in fieldnames]

            separator = "\n" + _spacing * indent
            property_list_string = separator + ("," + separator).join(property_list)

            return "{" +  property_list_string + "}"

    return Structure


@functools.lru_cache(maxsize = None)
def StaticArray(T: type):
    class StaticArrayType(C):
        __name__ = "StaticArray(" + str(T) + ")"

        name = T.name + " * const"

        def __init__(self, children: list):
            for child in children:
                assert type(child) == T
            self.children = children

        def flatten(self, indent: int = 0):
            member_list = [value.flatten(indent + 1) for value in self.children]

            # Heuristic: Static arrays are big, and so should be
            # spread out over multiple lines.
            separator = "\n" + _spacing * indent
            return (
                "{" + separator + ("," + separator).join(member_list)
                + "}")

    return StaticArrayType

@functools.lru_cache(maxsize = None)
def Array(size: int, T: type):
    assert size >= 0

    class ArrayType(C):
        __name__ = "Array(" + str(size) + ", " + str(T) + ")"

        name =  T.name + "[" + str(size) + "]"

        def __init__(self, children: list):
            assert len(children) == size
            for child in children:
                assert type(child) == T

            self.children = children

        def flatten(self, indent: int = 0):
            member_list = [value.flatten(indent + 1) for value in self.children]

            # Heuristic: Fixed sized arrays are small, and so should
            # not be spread out over multiple lines.
            return "{" + ", ".join(member_list) + "}"

    return ArrayType

class GLfloat(C):
    name = "GLfloat"

    def __init__(self, contents: float):
        self.contents = contents

    def flatten(self, indent: int = 0):
        return str(self.contents) + "F"


class Unsigned(C):
    def __init__(self, contents: int):
        assert contents >= 0
        self.contents = contents

    def flatten(self, indent: int = 0):
        return str(self.contents) + "U"

class GLubyte(Unsigned):
    name = "GLubyte"

class GLushort(Unsigned):
    name = "GLushort"

class GLuint(Unsigned):
    name = "GLuint"

_spacing = "    "
