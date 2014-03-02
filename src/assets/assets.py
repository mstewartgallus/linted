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


vertices = Array(3, Array(2, GLfloat))([
    Array(2, GLfloat)([GLfloat(-0.4), GLfloat(-0.4)]),
    Array(2, GLfloat)([GLfloat(0.4), GLfloat(-0.4)]),
    Array(2, GLfloat)([GLfloat(0.0), GLfloat(0.4)])]).flatten(0)

indices = Array(3, GLubyte)([GLubyte(0), GLubyte(1), GLubyte(2)]).flatten(0)

output = Template("""#include "linted/assets.h"
#include "linted/util.h"

#include <limits.h>
#include <float.h>


#define UNSIGNED_CHECK(T, X) (0u / ((X) <= ((T) -1)) + X)


#define SIGNED_MAX(T) (\\
    sizeof(T) == sizeof(signed char) ? SCHAR_MAX :\\
    sizeof(T) == sizeof(short)       ? SHRT_MAX  :\\
    sizeof(T) == sizeof(int)         ? INT_MAX   :\\
    sizeof(T) == sizeof(long)        ? LONG_MAX  :\\
    0u / 0u)

#define SIGNED_MIN(T) (\\
    sizeof(T) == sizeof(signed char) ? SCHAR_MIN :\\
    sizeof(T) == sizeof(short)       ? SHRT_MIN  :\\
    sizeof(T) == sizeof(int)         ? INT_MIN   :\\
    sizeof(T) == sizeof(long)        ? LONG_MIN  :\\
    0u / 0u)

#define SIGNED_CHECK(T, X) (0 / ((X) >= SIGNED_MIN(T)) + 0 / ((X) <= SIGNED_MAX(T)) + X)


#define FLOAT_MAX(T) (\\
    sizeof(T) == sizeof(float) ? FLT_MAX :\\
    sizeof(T) == sizeof(double) ? DBL_MAX  :\\
    0u / 0u)

#define FLOAT_MIN(T) (\\
    sizeof(T) == sizeof(float) ? -FLT_MAX :\\
    sizeof(T) == sizeof(double) ? -DBL_MAX  :\\
    0u / 0u)

#define FLOAT_CHECK(T, X) (0 / ((X) >= FLOAT_MIN(T)) + 0 / ((X) <= FLOAT_MAX(T)) + X)


static linted_assets_point const raw_data[] = $vertices;
linted_assets_point const * const linted_assets_triangle_data = raw_data;

static GLubyte const indices_data[] = $indices;

GLubyte const * const linted_assets_triangle_indices = indices_data;
size_t const linted_assets_triangle_indices_size = LINTED_ARRAY_SIZE(indices_data);
""").substitute(vertices=vertices, indices=indices)
