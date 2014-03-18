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
        print(assets_file.output())
    else:
        with open(arguments.output, 'w') as output_file:
            output_file.write(assets_file.output())


_spacing = "    "

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
        return str(self.contents) + "f"


class Unsigned(C):
    def __init__(self, contents: int):
        assert contents >= 0
        self.contents = contents

    def flatten(self, indent: int = 0):
        return str(self.contents) + "u"

class GLubyte(Unsigned):
    name = "GLubyte"

class GLushort(Unsigned):
    name = "GLushort"

class GLuint(Unsigned):
    name = "GLuint"

if __name__ == "__main__":
    try:
        _main()
    except SystemExit as e:
        # Prevent Blender spewing junk about unfreed memory on exit
        os._exit(e.code)
