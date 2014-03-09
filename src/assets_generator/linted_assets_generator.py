# -*- coding: utf-8 -*-
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
import argparse
import bpy
import collections
import functools
import imp
import os
import sys

def _main():
    parser = argparse.ArgumentParser(
        prog = "linted_assets_generator",
        description = "Generate Linted assets from their source format")
    parser.add_argument(
        "input_file",
        type = argparse.FileType('r'),
        help = "file to process as input")
    parser.add_argument(
        "--output",
        type = str,
        help = "file to output assets")
    arguments = parser.parse_args(sys.argv[sys.argv.index("--") + 1:])

    input_file = arguments.input_file

    sys.path.append(os.path.dirname(__file__))

    assets_file = imp.load_source(
        input_file.name,
        input_file.name,
        input_file)

    if None == arguments.output:
        print(assets_file.output)
    else:
        with open(arguments.output, 'w') as output_file:
            output_file.write(assets_file.output)


_spacing = "    "

def structure(typename: str, fields: list):

    fieldnames = [name for (name, tp) in fields]
    members = [_spacing + tp.name + " " + prop + ";" for (prop, tp) in fields]

    class Structure(collections.namedtuple(typename, fieldnames)):
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

        def __str__(self):
            return self.flatten()

    return Structure


@functools.lru_cache(maxsize = None)
def StaticArray(T: type):
    class StaticArrayType:
        __name__ = "StaticArray(" + str(T) + ")"

        name = T.name + " * const"

        def __init__(self, children: list):
            for child in children:
                assert type(child) == T
            self.children = children

        def flatten(self, indent):
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

    class ArrayType:
        __name__ = "Array(" + str(size) + ", " + str(T) + ")"

        name =  T.name + "[" + str(size) + "]"

        def __init__(self, children: list):
            assert len(children) == size
            for child in children:
                assert type(child) == T

            self.children = children

        def flatten(self, indent: int):
            member_list = [value.flatten(indent + 1) for value in self.children]

            # Heuristic: Fixed sized arrays are small, and so should
            # not be spread out over multiple lines.
            return "{" + ", ".join(member_list) + "}"

    return ArrayType


class GLfloat:
    name = "GLfloat"

    def __init__(self, contents: float):
        self.contents = contents

    def flatten(self, indent):
        return "LINTED_CHECK_FLOAT(" + self.name + ", " + str(self.contents) + "f)"


class Unsigned:
    def __init__(self, contents: int):
        assert contents >= 0
        self.contents = contents

    def flatten(self, indent):
        return "LINTED_CHECK_UNSIGNED(" + self.name + ", " + str(self.contents) + "u)"

class GLubyte(Unsigned):
    name = "GLubyte"

class GLushort(Unsigned):
    name = "GLushort"

class GLuint(Unsigned):
    name = "GLuint"


class Shader:
    @classmethod
    def load(cls, filename):
        with open(filename, 'r') as shaderfile:
            result = Shader()
            result.contents = shaderfile.read()
            return result

    def __str__(self):
        quotation = self.contents.encode("unicode_escape").decode("ascii").replace("\"", "\\\"")
        return "\"" + quotation + "\""


Faces = structure("Faces", [
    ("indices", StaticArray(Array(3, GLushort))),
    ("vertices", StaticArray(Array(3, GLfloat)))])

SimulatorData = structure("SimulatorData", [
    ("dynamic_objects", StaticArray(Array(3, GLfloat))),
    ("static_objects", Faces)])


Vertex = structure("Vertex", [
    ("position", Array(3, GLfloat)),
    ("normal", Array(3, GLfloat))])

MeshCollection = structure("MeshCollection", [
    ("indices", StaticArray(StaticArray(Array(3, GLushort)))),
    ("vertices", StaticArray(Vertex))])

RenderData = structure("RenderData", [
    ("mesh_collection", MeshCollection),
    ("objects_for_mesh", StaticArray(StaticArray(GLuint))),
    ("object_matrices", StaticArray(Array(4, Array(4, GLfloat))))])

BlendFile = structure("BlendFile", [
        ("simulator_data", SimulatorData),
        ("render_data", RenderData)])
BlendFile.load = lambda filename: load_blend_file(filename)


def load_blend_file(filename):
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

    return BlendFile(
        simulator_data = SimulatorData(
            dynamic_objects = object_set_locations(dynamic_objects),
            static_objects = object_set_faces(static_objects)),
        render_data = RenderData(
            mesh_collection = mesh_collection(),
            objects_for_mesh = objects_for_mesh(physics_objects),
            object_matrices = object_matrices(physics_objects)))


def object_set_locations(objects):
    return StaticArray(Array(3, GLfloat))([Array(3, GLfloat)(
        [GLfloat(value) for value in objct.location]) for objct in objects])

def object_set_faces(objects):
    mesh_vertices = []
    mesh_indices = []
    for objct in objects:
        matrix = objct.matrix_world

        for mesh in bpy.data.meshes:
            if mesh == objct.data:
                indices = [Array(3, GLushort)([process_index(index, len(mesh_vertices))
                                          for index in polygon.vertices])
                           for polygon in polygons(mesh)]

                vertices = [Array(3, GLfloat)(
                    [GLfloat(part) for part in (matrix * vertex.co).to_tuple()])
                            for vertex in mesh.vertices]

                mesh_vertices += vertices
                mesh_indices.extend(indices)
    return Faces(
        indices = StaticArray(Array(3, GLushort))(mesh_indices),
        vertices = StaticArray(Array(3, GLfloat))(mesh_vertices))


def mesh_collection():
    mesh_vertices = []
    mesh_indices = []
    for mesh in bpy.data.meshes:
        vertices, indices = process_mesh(mesh, len(mesh_vertices))
        mesh_vertices += vertices
        mesh_indices.append(StaticArray(Array(3, GLushort))(indices))

    return MeshCollection(
        indices = StaticArray(StaticArray(Array(3, GLushort)))(mesh_indices),
        vertices = StaticArray(Vertex)(mesh_vertices))


def objects_for_mesh(objects):
    mesh_indices = [StaticArray(GLuint)([GLuint(ii)
        for ii in range(0, len(objects)) if mesh == objects[ii].data])
                    for mesh in bpy.data.meshes]
    return StaticArray(StaticArray(GLuint))(mesh_indices)


def object_matrices(objects):
    matrices = [Array(4, Array(4, GLfloat))([Array(4, GLfloat)([GLfloat(value)
        for value in vertex])
        for vertex in objct.matrix_world])
        for objct in objects]

    return StaticArray(Array(4, Array(4, GLfloat)))(matrices)


def process_mesh(mesh, last_index):
    indices = [Array(3, GLushort)([process_index(index, last_index)
                       for index in polygon.vertices])
               for polygon in polygons(mesh)]

    vertices = [Vertex(
            position = Array(3, GLfloat)([GLfloat(part) for part in vertex.co.to_tuple()]),
            normal = Array(3, GLfloat)([GLfloat(part) for part in vertex.normal.to_tuple()]))
        for vertex in mesh.vertices]

    return vertices, indices

def process_index(index, last_index):
    new_index = last_index + index
    return GLushort(new_index)


# Compatibility shim for older Blender versions
def polygons(mesh):
    if hasattr(mesh, "polygons"):
        return mesh.polygons
    else:
        return mesh.faces

if __name__ == "__main__":
    try:
        _main()
    except SystemExit as e:
        # Prevent Blender spewing junk about unfreed memory on exit
        os._exit(e.code)
