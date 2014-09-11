/*
 * Copyright 2014 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef LINTED_GUI_ASSETS_H
#define LINTED_GUI_ASSETS_H

#include <stddef.h>
#include <GLES2/gl2.h>

typedef GLfloat linted_gui_assets_point[3U];

extern linted_gui_assets_point const *const linted_gui_assets_vertices;
extern linted_gui_assets_point const *const linted_gui_assets_normals;

extern GLubyte const *const linted_gui_assets_indices;
extern size_t const linted_gui_assets_indices_size;

extern GLchar const *const linted_gui_assets_fragment_shader;
extern GLchar const *const linted_gui_assets_vertex_shader;

#endif                          /* LINTED_GUI_ASSETS_H */
