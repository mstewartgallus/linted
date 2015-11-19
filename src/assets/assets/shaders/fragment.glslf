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

in vec3 linted_varying_vertex;
in vec3 linted_varying_normal;

out vec4 linted_fragment_color;


const vec3 light_location = vec3(0.0, 0.0, 8.0);
const vec3 light_colour = 0.5 * vec3(0.94, 0.9, 1.0);

const vec3 ambient_light = 0.5 * vec3(0.95, 1.0, 0.9);


const vec2 foggy_air = vec2(0.0001, 0.00001);

const float e = 2.71828182846;

float compute_decay_factor(vec3 aa, vec3 bb);

void main()
{
    /*
     * The dark_stone matrices are factors in a taylor series that
     * approximiates the intensity of light after reflecting off a surface.
     *
     * So far, these matrices do not reflect light back shifted in hue.
     *
     * The total energy of the light should never add up to more light
     * energy than is received.
     */

    const mat3x3 dark_stone[3] = mat3x3[3](
            mat3x3(0.8, 0.0, 0.0,
                   0.0, 0.5, 0.0,
                   0.0, 0.0, 0.0),

            mat3x3(0.1, 0.0, 0.0,
                   0.0, 0.1, 0.0,
                   0.0, 0.0, 3.0),

            mat3x3(0.0, 0.0, 0.0,
                   0.0, 0.1, 0.0,
                   0.0, 0.0, -2.0)
    );

    vec3 light = light_colour;

    float impact_angle = max(0.0, dot(normalize(linted_varying_normal),
                                      normalize(light_location)));

    vec3 impact = impact_angle * light + ambient_light;

    /*
     * There can be no reflected back light when there is no input light
     * (unless the object glows or something.)  Also, if an object could take in light
     * and reflect it back shifted in hue is not modeled.
     */

    vec3 sqr = impact * impact;
    vec3 intensity = dark_stone[0] * impact + dark_stone[1] * sqr + dark_stone[2] * (impact * sqr);

    vec3 value = intensity / compute_decay_factor(eye_vertex, -linted_varying_vertex);

    vec3 gamma_correct_value = pow(value, vec3(2.2));

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
