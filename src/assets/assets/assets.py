# Copyright 2013, 2014, 2015, 2016, 2017 Steven Stewart-Gallus
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
    def process_mesh(mesh_objects):
        index_list = [None] * len(mesh_objects)

        total_vertex_length = 0

        for ii in range(0, len(mesh_objects)):
            objct = mesh_objects[ii]

            mesh = objct.data
            p = polygons(mesh)

            total_vertex_length += len(mesh.vertices)

            index_list[ii] = [None] * len(p)

        vertices = [None] * total_vertex_length
        normals = [None] * total_vertex_length

        old_vertices_length = 0
        for jj in range(0, len(mesh_objects)):
            objct = mesh_objects[jj]
            mesh = objct.data
            matrix = objct.matrix_world
            normal_matrix = matrix.inverted().transposed()

            p = polygons(mesh)

            for ii in range(0, len(p)):
                polygon = p[ii]
                index_list[jj][ii] = Array(3, GLubyte)([GLubyte(index + old_vertices_length)
                                                 for index in polygon.vertices])

            for ii in range(0, len(mesh.vertices)):
                vertex = mesh.vertices[ii]
                world_normal = normal_matrix * vertex.normal
                world_normal.normalize()
                x_norm, y_norm, z_norm = world_normal.to_tuple()
                x, y, z = (matrix * vertex.co).to_tuple()

                normals[ii + old_vertices_length] = Array(3, GLfloat)((GLfloat(x_norm), GLfloat(y_norm), GLfloat(z_norm)))
                vertices[ii + old_vertices_length] = Array(3, GLfloat)((GLfloat(x), GLfloat(y), GLfloat(z)))

            old_vertices_length += len(mesh.vertices)

        return index_list, normals, vertices

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

        index_list, normals, vertices = process_mesh(mesh_objects)

        indices = []
        ix = 0
        assets = []
        for item in index_list:
            indices.extend(item)
            assets.append(Asset(start=GLuint(ix), length=GLuint(len(item))))
            ix += len(item)
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

struct lntd_assets_asset const assets_data[] = $assets;

struct lntd_assets_asset const * const lntd_assets_assets = assets_data;
size_t const lntd_assets_assets_size = LNTD_ARRAY_SIZE(assets_data);
""").substitute(
    normals=StaticArray(Array(3, GLfloat))(normals),
    vertices=StaticArray(Array(3, GLfloat))(vertices),
    indices=StaticArray(Array(3, GLubyte))(indices),
    assets=StaticArray(Asset)(assets))

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

Asset = structure('asset', [('start', GLuint), ('length',GLuint)])
