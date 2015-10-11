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
#version 300 es

precision highp float;

uniform vec3 eye_vertex;
uniform mat4 model_view_projection_matrix;

const vec3 light_location = vec3(0.0, 0.0, 8.0);
const vec3 light_colour = 0.5 * vec3(0.94, 0.9, 1.0);

const vec3 ambient_light = 0.5 * vec3(0.95, 1.0, 0.9);

/* 1 + 1 * 1 + 1 * 1 * 1 = 3, These should never add up to more than one as
* that would be outputting more light energy than is received.
*/
const vec3 dark_stone_r = 0.9 * vec3(0.9, 0.1, 0.0);
const vec3 dark_stone_g = 0.7 * vec3(0.1, 0.02, 0.98);
const vec3 dark_stone_b = 0.8 * vec3(0.8, 0.1, 0.1);

const vec2 foggy_air = vec2(0.0001, 0.00001);

in vec3 linted_varying_vertex;
in vec3 linted_varying_normal;

out vec4 linted_fragment_color;

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

void main()
{
    vec3 light = light_colour;

    float impact_angle = max(0.0, dot(normalize(linted_varying_normal),
                                      normalize(light_location)));

    vec3 impact = impact_angle * light + ambient_light;

    /*
     * There can be no reflected back light when there is no input light
     * (unless the object glows or something.)  Also, if an object could take in light
     * and reflect it back shifted in hue is not modeled.
     */
    vec3 intensity = vec3(dot(dark_stone_r, vec3(impact.r, impact.r * impact.r, impact.r * impact.r * impact.r)),
                          dot(dark_stone_g, vec3(impact.g, impact.g * impact.g, impact.g * impact.g * impact.g)),
                          dot(dark_stone_b, vec3(impact.b, impact.b * impact.b, impact.b * impact.b * impact.b)));


    vec3 value = intensity / compute_decay_factor(eye_vertex, -linted_varying_vertex);

    vec3 gamma_correct_value = pow(value, vec3(2.2));

    linted_fragment_color = vec4(gamma_correct_value, 1.0);
}
