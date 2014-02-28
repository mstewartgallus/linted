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
import linted_assets_generator


vertices = """{
    {-0.4f, -0.4f},
    {0.4f, -0.4f},
    {0.0f, 0.4f}
}"""

indices = """{ 0, 1, 2 }"""

output = Template("""#include "linted/assets.h"
#include "linted/util.h"

static linted_assets_point const raw_data[] = $vertices;
linted_assets_point const * const linted_assets_triangle_data = raw_data;

static GLuint const indices_data[] = $indices;

GLuint const * const linted_assets_triangle_indices = indices_data;
size_t const linted_assets_triangle_indices_size = LINTED_ARRAY_SIZE(indices_data);
""").substitute(vertices=vertices, indices=indices)
