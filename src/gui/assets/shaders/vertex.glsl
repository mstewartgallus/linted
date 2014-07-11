/*
 * Copyright 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License.  You may
 * obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#version 100
#pragma linted include("shaders/varying.glsl")

precision highp float;

attribute vec3 vertex;
attribute vec3 normal;

uniform mat4 projection_matrix;
uniform mat4 x_rotation_matrix;
uniform mat4 y_rotation_matrix;
uniform mat4 camera_matrix;

void main()
{
    linted_varying_vertex = vertex;
    linted_varying_normal = normal;
    gl_Position = projection_matrix * ((y_rotation_matrix * x_rotation_matrix) * camera_matrix) * vec4(vertex, 1);
}
