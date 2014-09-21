/*
 * Copyright 2013, 2014 Steven Stewart-Gallus
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
#include "config.h"

#include "linted/assets.h"
#include "linted/error.h"
#include "linted/gpu.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/str.h"
#include "linted/util.h"

#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include <EGL/egl.h>
#include <GLES2/gl2.h>

enum renderer_state { BUFFER_COMMANDS, SWAP_BUFFERS };

struct linted_gpu_context
{
	EGLDisplay display;
	EGLSurface surface;
	EGLConfig config;

	unsigned width;
	unsigned height;

	struct linted_gpu_update update;

	EGLContext context;
	GLuint program;
	GLint model_view_projection_matrix;
	enum renderer_state state;

	bool has_gl_context : 1U;
	bool resize_pending : 1U;
	bool update_pending : 1U;
};

struct matrix
{
	GLfloat x[4U][4U];
};

static EGLint const attr_list[] = {EGL_RED_SIZE,     5,             /*  */
                                   EGL_GREEN_SIZE,   6,             /*  */
                                   EGL_BLUE_SIZE,    5,             /*  */
                                   EGL_ALPHA_SIZE,   EGL_DONT_CARE, /*  */
                                   EGL_DEPTH_SIZE,   16,            /*  */
                                   EGL_STENCIL_SIZE, EGL_DONT_CARE, /*  */
                                   EGL_NONE,         EGL_NONE};

static EGLint const context_attr[] = {EGL_CONTEXT_CLIENT_VERSION, 2, /*  */
                                      EGL_NONE, EGL_NONE};

static linted_error destroy_contexts(struct linted_gpu_context *gpu_context);
static linted_error assure_gl_context(struct linted_gpu_context *gpu_context,
                                      linted_log log);

static void real_draw(struct linted_gpu_context *gpu_context);

static void flush_gl_errors(void);
static struct matrix matrix_multiply(struct matrix a, struct matrix b);

/**
 * @todo get_gl_error's use of glGetError is incorrect. Multiple error
 *       flags may be set and returned by a single function.
 */
static linted_error get_gl_error(void);
static linted_error get_egl_error(void);

static double square(double x);

static linted_error log_str(linted_log log, struct linted_str start,
                            char const *str);

linted_error linted_gpu_create(linted_gpu_native_display native_display,
                               linted_gpu_native_window native_window,
                               struct linted_gpu_context **gpu_contextp,
                               linted_log log)
{
	linted_error errnum = 0;

	if (native_window > UINT32_MAX)
		return EINVAL;

	struct linted_gpu_context *gpu_context;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, sizeof *gpu_context);
		if (errnum != 0)
			return errnum;
		gpu_context = xx;
	}

	EGLDisplay display = eglGetDisplay(native_display);
	if (EGL_NO_DISPLAY == display) {
		errnum = get_egl_error();
		goto release_thread;
	}

	if (EGL_FALSE == eglInitialize(display, NULL, NULL)) {
		errnum = get_egl_error();
		goto destroy_display;
	}

	EGLint num_configs;
	{
		EGLint xx;
		if (EGL_FALSE == eglGetConfigs(display, NULL, 0, &xx)) {
			errnum = get_egl_error();
			goto destroy_display;
		}
		num_configs = xx;
	}

	EGLConfig config;
	{
		EGLConfig xx;
		EGLint yy = num_configs;
		if (EGL_FALSE ==
		    eglChooseConfig(display, attr_list, &xx, 1U, &yy)) {
			errnum = get_egl_error();
			goto destroy_display;
		}
		config = xx;
	}

	EGLSurface surface =
	    eglCreateWindowSurface(display, config, native_window, NULL);
	if (EGL_NO_SURFACE == surface) {
		errnum = get_egl_error();
		goto destroy_display;
	}

	gpu_context->update.x_rotation = 0;
	gpu_context->update.y_rotation = 0;

	gpu_context->update.x_position = 0;
	gpu_context->update.y_position = 0;
	gpu_context->update.z_position = 0;

	gpu_context->update_pending = false;

	gpu_context->width = 1U;
	gpu_context->height = 1U;
	gpu_context->resize_pending = true;

	gpu_context->display = display;
	gpu_context->config = config;
	gpu_context->surface = surface;
	gpu_context->has_gl_context = false;

	*gpu_contextp = gpu_context;

	return 0;

destroy_display:
	if (EGL_FALSE == eglTerminate(display)) {
		if (0 == errnum)
			errnum = get_egl_error();
	}

release_thread:
	if (EGL_FALSE == eglReleaseThread()) {
		if (0 == errnum)
			errnum = get_egl_error();
	}

	linted_mem_free(gpu_context);

	return errnum;
}

linted_error linted_gpu_destroy(struct linted_gpu_context *gpu_context)
{
	linted_error errnum = 0;

	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;

	if (gpu_context->has_gl_context)
		errnum = destroy_contexts(gpu_context);

	if (EGL_FALSE == eglDestroySurface(display, surface)) {
		if (0 == errnum)
			errnum = get_egl_error();
	}

	if (EGL_FALSE == eglTerminate(display)) {
		if (0 == errnum)
			errnum = get_egl_error();
	}

	if (EGL_FALSE == eglReleaseThread()) {
		if (0 == errnum)
			errnum = get_egl_error();
	}

	linted_mem_free(gpu_context);

	return errnum;
}

void linted_gpu_update_state(struct linted_gpu_context *gpu_context,
                             struct linted_gpu_update const *gpu_update)
{
	gpu_context->update = *gpu_update;
	gpu_context->update_pending = true;
}

void linted_gpu_resize(struct linted_gpu_context *gpu_context, unsigned width,
                       unsigned height)
{
	gpu_context->width = width;
	gpu_context->height = height;

	gpu_context->resize_pending = true;
}

void linted_gpu_draw(struct linted_gpu_context *gpu_context, linted_log log)
{
	linted_error errnum;

	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;

	errnum = assure_gl_context(gpu_context, log);
	if (errnum != 0)
		return;

	switch (gpu_context->state) {
	case BUFFER_COMMANDS:
		real_draw(gpu_context);
		glFlush();
		gpu_context->state = SWAP_BUFFERS;
		break;

	case SWAP_BUFFERS:
		if (EGL_FALSE == eglSwapBuffers(display, surface)) {
			errnum = get_egl_error();
			if (ENOSYS == errnum) {
				destroy_contexts(gpu_context);
				gpu_context->has_gl_context = false;
			}
		}
		gpu_context->state = BUFFER_COMMANDS;
		break;
	}
}

static linted_error destroy_contexts(struct linted_gpu_context *gpu_context)
{
	linted_error errnum = 0;

	EGLDisplay display = gpu_context->display;
	EGLContext context = gpu_context->context;

	glUseProgram(0);
	glDeleteProgram(gpu_context->program);

	if (EGL_FALSE == eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE,
	                                EGL_NO_CONTEXT))
		errnum = get_egl_error();

	if (EGL_FALSE == eglDestroyContext(display, context)) {
		if (0 == errnum)
			errnum = get_egl_error();
	}

	return errnum;
}

static linted_error assure_gl_context(struct linted_gpu_context *gpu_context,
                                      linted_log log)
{

	linted_error errnum;

	if (gpu_context->has_gl_context)
		return 0;

	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;
	EGLConfig config = gpu_context->config;

	EGLContext context =
	    eglCreateContext(display, config, EGL_NO_CONTEXT, context_attr);
	if (EGL_NO_CONTEXT == context)
		return get_egl_error();

	if (EGL_FALSE == eglMakeCurrent(display, surface, surface, context)) {
		errnum = get_egl_error();
		goto destroy_context;
	}

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	/* The brightest colour is (1, 1, 1) */
	/* The darkest colour is (0, 0, 0) */
	/* We want a neutral, middle brightness */

	/* It might be possible to use a physically based model based
	 * off the energy contained in a ray of light of a certain hue
	 * but we have chosen to model brightnes as:
	 */
	/* brightness = r red + g green + b blue */

	/* Note that this is a crappy approximation that only works
	 * for some monitors and eyes.
	 */

	/* brightest = r + g + b */
	/* darkest = 0 */

	/* We calculate a middling brightness taking into account
	 * gamma and nonlinearity.
	 */

	/* Note that how bright we want our background colour to be is
	 * really a matter of taste and not math. The halfway point is
	 * simply a good starting point.
	 */
	double r = 0.2126;
	double g = 0.7152;
	double b = 0.0722;

	double brightness = (r + g + b) * square(0.5);

	/* We can then carve off some red and green to make room for more
	 * blue but still keep the same amount of brightness.
	 */
	/* brightness = r red + g green + b blue */
	/* red = green = x */
	/* brightness = (r + g) x + b blue */
	/* brightness - b blue = (r + g) x */
	/* (brightness - b blue) / (r + g) = x */

	/* adjust blue to taste */
	double blue = square(0.75);
	double x = (brightness - b * blue) / (r + g);
	double red = x;
	double green = x;

	glClearColor(red, green, blue, 1);

	flush_gl_errors();
	GLuint program = glCreateProgram();
	if (0 == program) {
		errnum = get_gl_error();
		goto use_no_context;
	}

	{
		flush_gl_errors();
		GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
		if (0 == fragment_shader) {
			errnum = get_gl_error();
			goto cleanup_program;
		}
		glAttachShader(program, fragment_shader);
		glDeleteShader(fragment_shader);

		glShaderSource(fragment_shader, 1U,
		               (GLchar const **)&linted_assets_fragment_shader,
		               NULL);
		glCompileShader(fragment_shader);

		GLint is_valid;
		glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &is_valid);
		if (!is_valid) {
			errnum = EINVAL;

			GLint info_log_length;
			glGetShaderiv(fragment_shader, GL_INFO_LOG_LENGTH,
			              &info_log_length);

			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (0 == mem_errnum) {
				GLchar *info_log = xx;
				glGetShaderInfoLog(fragment_shader,
				                   info_log_length, NULL,
				                   info_log);
				log_str(log, LINTED_STR("Invalid shader: "),
				        info_log);
				linted_mem_free(info_log);
			}
			goto cleanup_program;
		}
	}

	{
		flush_gl_errors();
		GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
		if (0 == vertex_shader) {
			errnum = get_gl_error();
			goto cleanup_program;
		}
		glAttachShader(program, vertex_shader);
		glDeleteShader(vertex_shader);

		glShaderSource(vertex_shader, 1U,
		               (GLchar const **)&linted_assets_vertex_shader,
		               NULL);
		glCompileShader(vertex_shader);

		GLint is_valid;
		glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &is_valid);
		if (!is_valid) {
			errnum = EINVAL;

			GLint info_log_length;
			glGetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH,
			              &info_log_length);

			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (0 == mem_errnum) {
				GLchar *info_log = xx;
				glGetShaderInfoLog(vertex_shader,
				                   info_log_length, NULL,
				                   info_log);
				log_str(log, LINTED_STR("Invalid shader: "),
				        info_log);
				linted_mem_free(info_log);
			}
			goto cleanup_program;
		}
	}
	glLinkProgram(program);

	glValidateProgram(program);

	GLint is_valid;
	glGetProgramiv(program, GL_VALIDATE_STATUS, &is_valid);
	if (!is_valid) {
		errnum = EINVAL;

		GLint info_log_length;
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_log_length);

		void *xx;
		linted_error mem_errnum =
		    linted_mem_alloc(&xx, info_log_length);
		if (0 == mem_errnum) {
			GLchar *info_log = xx;
			glGetProgramInfoLog(program, info_log_length, NULL,
			                    info_log);
			log_str(log, LINTED_STR("Invalid program: "), info_log);
			linted_mem_free(info_log);
		}
		goto cleanup_program;
	}

	GLint mvp_matrix =
	    glGetUniformLocation(program, "model_view_projection_matrix");
	if (-1 == mvp_matrix) {
		errnum = EINVAL;
		goto cleanup_program;
	}

	GLint vertex = glGetAttribLocation(program, "vertex");
	if (-1 == vertex) {
		errnum = EINVAL;
		goto cleanup_program;
	}

	GLint normal = glGetAttribLocation(program, "normal");
	if (-1 == normal) {
		errnum = EINVAL;
		goto cleanup_program;
	}

	glEnableVertexAttribArray(vertex);
	glEnableVertexAttribArray(normal);

	glVertexAttribPointer(vertex,
	                      LINTED_ARRAY_SIZE(linted_assets_vertices[0U]),
	                      GL_FLOAT, false, 0, linted_assets_vertices);
	glVertexAttribPointer(normal,
	                      LINTED_ARRAY_SIZE(linted_assets_normals[0U]),
	                      GL_FLOAT, false, 0, linted_assets_normals);

	glUseProgram(program);

	gpu_context->context = context;
	gpu_context->program = program;
	gpu_context->model_view_projection_matrix = mvp_matrix;
	gpu_context->state = BUFFER_COMMANDS;
	gpu_context->has_gl_context = true;

	gpu_context->update_pending = true;
	gpu_context->resize_pending = true;

	return 0;

cleanup_program:
	glDeleteProgram(program);

use_no_context:
	eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

destroy_context:
	eglDestroyContext(display, context);

	return errnum;
}

static void real_draw(struct linted_gpu_context *gpu_context)
{
	struct linted_gpu_update const *update = &gpu_context->update;

	unsigned width = gpu_context->width;
	unsigned height = gpu_context->height;

	bool update_pending = gpu_context->update_pending;
	bool resize_pending = gpu_context->resize_pending;

	if (resize_pending) {
		glViewport(0, 0, width, height);
		gpu_context->resize_pending = false;
	}

	if (update_pending || resize_pending) {
		/* X, Y, Z, W coords of the resultant vector are the
		 * sums of the columns (row major order).
		 */

		GLfloat x_rotation = update->x_rotation;
		GLfloat y_rotation = update->y_rotation;

		GLfloat x_position = update->x_position;
		GLfloat y_position = update->y_position;
		GLfloat z_position = update->z_position;

		/* Rotate the camera */
		GLfloat cos_y = cosf(y_rotation);
		GLfloat sin_y = sinf(y_rotation);

		struct matrix const y_rotation_matrix = {{{1, 0, 0, 0},
		                                          {0, cos_y, -sin_y, 0},
		                                          {0, sin_y, cos_y, 0},
		                                          {0, 0, 0, 1}}};

		GLfloat cos_x = cosf(x_rotation);
		GLfloat sin_x = sinf(x_rotation);
		struct matrix const x_rotation_matrix = {{{cos_x, 0, sin_x, 0},
		                                          {0, 1, 0, 0},
		                                          {-sin_x, 0, cos_x, 0},
		                                          {0, 0, 0, 1}}};

		/* Translate the camera */
		struct matrix const camera = {
		    {{1, 0, 0, 0},
		     {0, 1, 0, 0},
		     {0, 0, 1, 0},
		     {x_position, y_position, z_position, 1}}};

		GLfloat aspect = width / (GLfloat)height;
		double fov = acos(-1.0) / 4;

		double d = 1 / tan(fov / 2);
		double far = 1000;
		double near = 1;

		struct matrix const projection = {
		    {{d / aspect, 0, 0, 0},
		     {0, d, 0, 0},
		     {0, 0, (far + near) / (near - far),
		      2 * far * near / (near - far)},
		     {0, 0, -1, 0}}};

		struct matrix rotations =
		    matrix_multiply(x_rotation_matrix, y_rotation_matrix);
		struct matrix model_view = matrix_multiply(camera, rotations);
		struct matrix model_view_projection =
		    matrix_multiply(model_view, projection);

		glUniformMatrix4fv(gpu_context->model_view_projection_matrix,
		                   1U, false, model_view_projection.x[0U]);

		gpu_context->update_pending = false;
	}

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glDrawElements(GL_TRIANGLES, 3U * linted_assets_indices_size,
	               GL_UNSIGNED_BYTE, linted_assets_indices);
}

static void flush_gl_errors(void)
{
	GLenum error;
	do {
		error = glGetError();
	} while (error != GL_NO_ERROR);
}

static linted_error get_gl_error(void)
{
	/* Get the first error. Note that a single OpenGL call may
	 * return multiple errors so this only gives the first of many
	 * possible errors.
	 */
	switch (glGetError()) {
	case GL_NO_ERROR:
		return 0;

	case GL_INVALID_ENUM:
	case GL_INVALID_VALUE:
	case GL_INVALID_OPERATION:
	case GL_INVALID_FRAMEBUFFER_OPERATION:
		return EINVAL;

	case GL_OUT_OF_MEMORY:
		return ENOMEM;

	default:
		return ENOSYS;
	}
}

static linted_error get_egl_error(void)
{
	switch (eglGetError()) {
	case EGL_SUCCESS:
		return 0;

	case EGL_NOT_INITIALIZED:
		return EINVAL;

	case EGL_BAD_ACCESS:
		return EAGAIN;

	case EGL_BAD_ALLOC:
		return ENOMEM;

	case EGL_BAD_ATTRIBUTE:
	case EGL_BAD_CONTEXT:
	case EGL_BAD_CONFIG:
	case EGL_BAD_CURRENT_SURFACE:
	case EGL_BAD_DISPLAY:
	case EGL_BAD_SURFACE:
	case EGL_BAD_MATCH:
	case EGL_BAD_PARAMETER:
	case EGL_BAD_NATIVE_PIXMAP:
	case EGL_BAD_NATIVE_WINDOW:
		return EINVAL;

	case EGL_CONTEXT_LOST:
		return ENOSYS;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static struct matrix matrix_multiply(struct matrix a, struct matrix b)
{
	GLfloat b_inverted[4U][4U];

	for (size_t ii = 0U; ii < 4U; ++ii) {
		GLfloat b_ii[4U];
		memcpy(b_ii, b.x[ii], sizeof b_ii);

		for (size_t jj = 0U; jj < 4U; ++jj)
			b_inverted[jj][ii] = b_ii[jj];
	}

	struct matrix result;

	for (size_t ii = 0U; ii < 4U; ++ii) {
		GLfloat a_ii[4U];
		memcpy(a_ii, a.x[ii], sizeof a_ii);

		GLfloat result_ii[4U];
		for (size_t jj = 0U; jj < 4U; ++jj) {
			GLfloat b_XX_jj[4U];
			memcpy(b_XX_jj, b_inverted[jj], sizeof b_XX_jj);

			result_ii[jj] =
			    (a_ii[0U] * b_XX_jj[0U] + a_ii[1U] * b_XX_jj[1U]) +
			    (a_ii[2U] * b_XX_jj[2U] + a_ii[3U] * b_XX_jj[3U]);
		}

		memcpy(result.x[ii], result_ii, sizeof result_ii);
	}

	return result;
}

static double square(double x)
{
	return x * x;
}

static linted_error log_str(linted_log log, struct linted_str start,
                            char const *error)
{
	linted_error errnum;
	size_t error_size = strlen(error);

	char *full_string;
	{
		void *xx;
		errnum = linted_mem_alloc(&xx, error_size + start.size);
		if (errnum != 0)
			/* Silently drop log */
			return errnum;
		full_string = xx;
	}

	memcpy(full_string, start.bytes, start.size);
	memcpy(full_string + start.size, error, error_size);

	errnum = linted_log_write(log, full_string, start.size + error_size);

	linted_mem_free(full_string);

	return errnum;
}
