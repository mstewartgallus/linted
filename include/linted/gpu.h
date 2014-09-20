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
#ifndef LINTED_GPU_H
#define LINTED_GPU_H

#include "linted/log.h"

#ifdef __linux__
#include "gpu-x11.h"
#else
#error no 3d implementation for this platform has been implemented
#endif

struct linted_gpu_context;

struct linted_gpu_state
{
	float x_rotation;
	float y_rotation;

	float x_position;
	float y_position;
	float z_position;
};

linted_error linted_gpu_create(linted_gpu_native_display native_display,
                               linted_gpu_native_window native_window,
                               struct linted_gpu_context **cleanup_gpup,
                               linted_log log);
linted_error linted_gpu_destroy(struct linted_gpu_context *cleanup_gpu);

void linted_gpu_draw(struct linted_gpu_context *cleanup_gpu,
                     struct linted_gpu_state const *gpu_state, unsigned width,
                     unsigned height, linted_log log);
void linted_gpu_resize(struct linted_gpu_context *cleanup_gpu, unsigned width,
                       unsigned height, linted_log log);

#endif /* LINTED_GPU_H */
