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
import collections
import os
from string import Template
from linted_assets_generator import *

def output():
    old_directory = os.getcwd()
    os.chdir(os.path.dirname(os.path.realpath(__file__)))
    try:
        fragment_shader = encode_shader(load_shader("shaders/fragment.glsl"))
    finally:
        os.chdir(old_directory)

    return Template("""#include "config.h"

#include "linted/assets.h"

char const * const linted_assets_fragment_shader = $fragment_shader;
""").substitute(fragment_shader=fragment_shader)
