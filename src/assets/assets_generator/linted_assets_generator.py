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
import imp
import os
import sys
from io import StringIO

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

    output = assets_file.output()

    if None == arguments.output:
        print(output)
    else:
        with open(arguments.output, 'w') as output_file:
            output_file.write(output)

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

if __name__ == "__main__":
    try:
        _main()
    except SystemExit as e:
        # Prevent Blender spewing junk about unfreed memory on exit
        os._exit(e.code)
    os._exit(0)
