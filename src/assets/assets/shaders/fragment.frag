#version 300 es
/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
precision highp float;

uniform vec3 eye_vertex;
uniform mat4 model_view_projection_matrix;

in vec3 linted_varying_vertex;
in vec3 linted_varying_normal;

out vec4 linted_fragment_color;

const vec3 light_location = vec3(0.0, 0.0, 8.0);
const vec3 light_colour = 0.5 * vec3(0.8, 0.9, 1.0);

const vec3 ambient_light = 0.01 * vec3(0.95, 1.0, 0.9);


const vec2 foggy_air = vec2(0.0001, 0.00001);

const vec3 dark_stone = vec3(1.0, 0.2, 0.1);
const float r = 0.4;
const float shininess = 9.0;

float compute_decay_factor(vec3 aa, vec3 bb);

void main()
{
    /* Note that linted_varying_normal is already normalized and norming it twice
     * reduces quality.
     */
    float impact_angle = max(0.0, dot(linted_varying_normal,
                                      normalize(light_location)));
    float specular = r * pow(max(0.0, dot(normalize(reflect(normalize(light_location), linted_varying_normal)), normalize(eye_vertex - linted_varying_vertex))), shininess);

    vec3 impact = ambient_light + (impact_angle + specular) * light_colour;

    vec3 value = impact / compute_decay_factor(eye_vertex, -linted_varying_vertex);

#if defined VIEW_NORMALS
    vec3 gamma_correct_value = linted_varying_normal;
#else
    vec3 gamma_correct_value = pow(value, vec3(1.0 / 2.2));
#endif

    linted_fragment_color = vec4(gamma_correct_value, 1.0);
}

float compute_decay_factor(vec3 aa, vec3 bb)
{
    float delta = distance(aa, bb);

    /*
     * At a distance of zero light can not decay from fog because there
     * is no fog between points at that distance.
     */

    /*
     * So far, we don't model fog that affects different colours differently
     */
    float decay = dot(foggy_air,
                      vec2(delta, delta * delta));

    return 1.0 + decay;
}
