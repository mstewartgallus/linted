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
#version 120

varying vec3 vertex;

void main() {
    vec3 light_location = vec3(-0.25, 1.25, 0.0);

    float delta = distance(light_location, vertex);
    float decay = 1.0 / (1.0 + delta + delta * delta);
    gl_FragColor = vec4(decay * vec3(1.0, 0.9, 1.0), 1.0);
}
