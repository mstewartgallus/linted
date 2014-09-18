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
#ifndef LINTED_DRAWER_H
#define LINTED_DRAWER_H

#include "linted/log.h"

#include <EGL/egl.h>

struct graphics_state;

struct sim_model
{
	float x_rotation;
	float y_rotation;

	float x_position;
	float y_position;
	float z_position;
};

linted_error linted_drawer_3d_create(EGLNativeDisplayType native_display,
                                     EGLNativeWindowType native_window,
                                     struct graphics_state **graphics_statep,
                                     linted_log log);
linted_error linted_drawer_3d_destroy(struct graphics_state *graphics_state);

void linted_drawer_3d_draw(struct graphics_state *graphics_state,
                           struct sim_model const *sim_model, unsigned width,
                           unsigned height, linted_log log);
void linted_drawer_3d_resize(struct graphics_state *graphics_state,
                             unsigned width, unsigned height, linted_log log);

#endif /* LINTED_DRAWER_H */
