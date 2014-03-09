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

import os
from string import Template
from linted_assets_generator import *

_vertices = Array(3, Array(2, GLfloat))([
    Array(2, GLfloat)([GLfloat(-0.4), GLfloat(-0.4)]),
    Array(2, GLfloat)([GLfloat(0.4), GLfloat(-0.4)]),
    Array(2, GLfloat)([GLfloat(0.0), GLfloat(0.4)])]).flatten(0)

_indices = Array(3, GLubyte)([GLubyte(0), GLubyte(1), GLubyte(2)]).flatten(0)

output = Template("""#include "linted/assets.h"
#include "linted/check.h"
#include "linted/util.h"

static linted_assets_point const raw_data[] = $vertices;
linted_assets_point const * const linted_assets_triangle_data = raw_data;

static GLubyte const indices_data[] = $indices;

GLubyte const * const linted_assets_triangle_indices = indices_data;
size_t const linted_assets_triangle_indices_size = LINTED_ARRAY_SIZE(indices_data);
""").substitute(vertices=_vertices, indices=_indices)

class _Shader:
    @classmethod
    def load(cls, filename):
        with open(filename, 'r') as shaderfile:
            result = Shader()
            result.contents = shaderfile.read()
            return result

    def __str__(self):
        quotation = self.contents.encode("unicode_escape").decode("ascii").replace("\"", "\\\"")
        return "\"" + quotation + "\""

_Faces = structure("_Faces", [
    ("indices", StaticArray(Array(3, GLushort))),
    ("vertices", StaticArray(Array(3, GLfloat)))])

_SimulatorData = structure("_SimulatorData", [
    ("dynamic_objects", StaticArray(Array(3, GLfloat))),
    ("static_objects", _Faces)])


_Vertex = structure("_Vertex", [
    ("position", Array(3, GLfloat)),
    ("normal", Array(3, GLfloat))])

_MeshCollection = structure("_MeshCollection", [
    ("indices", StaticArray(StaticArray(Array(3, GLushort)))),
    ("vertices", StaticArray(_Vertex))])

_RenderData = structure("_RenderData", [
    ("mesh_collection", _MeshCollection),
    ("objects_for_mesh", StaticArray(StaticArray(GLuint))),
    ("object_matrices", StaticArray(Array(4, Array(4, GLfloat))))])

_BlendFile = structure("_BlendFile", [
        ("simulator_data", _SimulatorData),
        ("render_data", _RenderData)])
_BlendFile.load = lambda filename: _load_blend_file(filename)


def _load_blend_file(filename):
    bpy.ops.wm.open_mainfile(filepath = filename)
    objects = bpy.data.objects
    mesh_objects = [objct for objct in objects if 'MESH' == objct.type]

    dynamic_objects = []
    static_objects = []
    for objct in mesh_objects:
        object_sets = {
            'DYNAMIC' : dynamic_objects,
            'STATIC' : static_objects
        }
        try:
            object_sets[objct.game.physics_type].append(objct)
        except KeyError:
            pass

    physics_objects = dynamic_objects + static_objects

    return _BlendFile(
        simulator_data = _SimulatorData(
            dynamic_objects = _object_set_locations(dynamic_objects),
            static_objects = _object_set_faces(static_objects)),
        render_data = _RenderData(
            mesh_collection = _mesh_collection(),
            objects_for_mesh = _objects_for_mesh(physics_objects),
            object_matrices = _object_matrices(physics_objects)))


def _object_set_locations(objects):
    return StaticArray(Array(3, GLfloat))([Array(3, GLfloat)(
        [GLfloat(value) for value in objct.location]) for objct in objects])

def _object_set_faces(objects):
    mesh_vertices = []
    mesh_indices = []
    for objct in objects:
        matrix = objct.matrix_world

        for mesh in bpy.data.meshes:
            if mesh == objct.data:
                indices = [Array(3, GLushort)([_process_index(index, len(mesh_vertices))
                                          for index in polygon.vertices])
                           for polygon in _polygons(mesh)]

                vertices = [Array(3, GLfloat)(
                    [GLfloat(part) for part in (matrix * vertex.co).to_tuple()])
                            for vertex in mesh.vertices]

                mesh_vertices += vertices
                mesh_indices.extend(indices)
    return _Faces(
        indices = StaticArray(Array(3, GLushort))(mesh_indices),
        vertices = StaticArray(Array(3, GLfloat))(mesh_vertices))


def _mesh_collection():
    mesh_vertices = []
    mesh_indices = []
    for mesh in bpy.data.meshes:
        vertices, indices = _process_mesh(mesh, len(mesh_vertices))
        mesh_vertices += vertices
        mesh_indices.append(StaticArray(Array(3, GLushort))(indices))

    return _MeshCollection(
        indices = StaticArray(StaticArray(Array(3, GLushort)))(mesh_indices),
        vertices = StaticArray(_Vertex)(mesh_vertices))


def _objects_for_mesh(objects):
    mesh_indices = [StaticArray(GLuint)([GLuint(ii)
        for ii in range(0, len(objects)) if mesh == objects[ii].data])
                    for mesh in bpy.data.meshes]
    return StaticArray(StaticArray(GLuint))(mesh_indices)


def _object_matrices(objects):
    matrices = [Array(4, Array(4, GLfloat))([Array(4, GLfloat)([GLfloat(value)
        for value in vertex])
        for vertex in objct.matrix_world])
        for objct in objects]

    return StaticArray(Array(4, Array(4, GLfloat)))(matrices)


def _process_mesh(mesh, last_index):
    indices = [Array(3, GLushort)([_process_index(index, last_index)
                       for index in polygon.vertices])
               for polygon in _polygons(mesh)]

    vertices = [_Vertex(
            position = Array(3, GLfloat)([GLfloat(part) for part in vertex.co.to_tuple()]),
            normal = Array(3, GLfloat)([GLfloat(part) for part in vertex.normal.to_tuple()]))
        for vertex in mesh.vertices]

    return vertices, indices

def _process_index(index, last_index):
    new_index = last_index + index
    return GLushort(new_index)


# Compatibility shim for older Blender versions
def _polygons(mesh):
    if hasattr(mesh, "polygons"):
        return mesh.polygons
    else:
        return mesh.faces
