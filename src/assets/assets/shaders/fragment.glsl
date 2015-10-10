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

const vec3 light_location = vec3(0.0, 0.0, 2.0);

const vec3 colour = vec3(0.9, 0.7, 1.0);
const vec3 dark_stone = vec3(0.9, 0.5, 0.4);

/* So that fogginess does not have an affect at a distance of zero,
 * the first value of foggy_air must be 1.
 */
const vec3 foggy_air = vec3(1.0, 0.1, 0.001);

in vec3 linted_varying_vertex;
in vec3 linted_varying_normal;

out vec4 linted_fragment_color;

void main()
{
    float impact = max(0.0, dot(normalize(linted_varying_normal),
                                normalize(light_location - linted_varying_vertex)));
    float intensity = dot(dark_stone,
                          vec3(1.0, impact, impact * impact));

    float delta = distance(light_location, linted_varying_vertex);

    float decay = dot(foggy_air,
                      vec3(1.0, delta, delta * delta));

    vec3 value = (intensity / decay) * colour;

    vec3 gamma_correct_value = pow(value, vec3(2.2));

    linted_fragment_color = vec4(gamma_correct_value, 1.0);
}
