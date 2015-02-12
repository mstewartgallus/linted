/*
 * Copyright 2014, 2015 Steven Stewart-Gallus
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
#ifndef LINTED_GPU_H
#define LINTED_GPU_H

#include "linted/error.h"

/**
 * @file
 *
 * Draws graphics using the GPU.
 */

#if defined HAVE_X11_API
#include "gpu-x11.h"
#else
#error no GPU acceleration for this platform has been implemented
#endif

struct linted_gpu_context;

struct linted_gpu_update
{
	float x_rotation;
	float y_rotation;

	float x_position;
	float y_position;
	float z_position;
};

linted_error
linted_gpu_context_create(struct linted_gpu_context **gpu_contextp);
linted_error linted_gpu_context_destroy(struct linted_gpu_context *gpu_context);

linted_error linted_gpu_setwindow(struct linted_gpu_context *gpu_context,
                                  linted_gpu_native_window native_window);
linted_error linted_gpu_unsetwindow(struct linted_gpu_context *gpu_context);

void linted_gpu_update_state(struct linted_gpu_context *gpu_context,
                             struct linted_gpu_update const *gpu_update);
void linted_gpu_draw(struct linted_gpu_context *gpu_context);
void linted_gpu_resize(struct linted_gpu_context *gpu_context, unsigned width,
                       unsigned height);

#endif /* LINTED_GPU_H */
