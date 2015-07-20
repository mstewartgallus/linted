/*
 * Copyright 2013, 2014, 2015 Steven Stewart-Gallus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#define _GNU_SOURCE

#include "config.h"

#include "linted/assets.h"
#include "linted/error.h"
#include "linted/gpu.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/util.h"

#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GLES3/gl3.h>

enum { NO_EGL_SURFACE, HAS_EGL_SURFACE, HAS_SETUP_GL };
typedef unsigned char linted_gl_state;

struct linted_gpu_context {
	EGLDisplay display;
	EGLSurface surface;
	EGLConfig config;
	EGLContext context;

	linted_gpu_native_window window;

	struct linted_gpu_update update;

	unsigned width;
	unsigned height;

	GLuint program;

	GLuint vertex_buffer;
	GLuint normal_buffer;
	GLuint index_buffer;

	GLuint model_view_projection_matrix;

	linted_gl_state gl_state;

	bool buffer_commands : 1U;
	bool resize_pending : 1U;
	bool update_pending : 1U;
};

union chunk {
	GLfloat x[4U];
	long double __force_alignment;
};

struct matrix {
	union chunk x[4U];
};

static EGLint const attr_list[] = {
    EGL_CONFORMANT, EGL_OPENGL_ES3_BIT_KHR,      /**/
    EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT_KHR, /**/
    EGL_DEPTH_SIZE, 16,                          /**/
    EGL_COLOR_BUFFER_TYPE, EGL_RGB_BUFFER,       /**/
    EGL_RED_SIZE, 5,                             /**/
    EGL_GREEN_SIZE, 5,                           /**/
    EGL_BLUE_SIZE, 3,                            /**/
    EGL_NONE};

static EGLint const context_attr[] = {EGL_CONTEXT_CLIENT_VERSION,
                                      3, /**/
                                      EGL_NONE};

static linted_error destroy_gl(struct linted_gpu_context *gpu_context);

static linted_error
destroy_egl_surface(struct linted_gpu_context *gpu_context);

static linted_error
assure_current_egl_context(struct linted_gpu_context *gpu_context);

static linted_error setup_gl(struct linted_gpu_context *gpu_context);

static void real_draw(struct linted_gpu_context *gpu_context);

static void flush_gl_errors(void);
static struct matrix matrix_multiply(struct matrix a, struct matrix b);

/**
 * @todo get_gl_error's use of glGetError is incorrect. Multiple error
 *       flags may be set and returned by a single function.
 */
static linted_error get_gl_error(void);
static linted_error get_egl_error(void);

linted_error
linted_gpu_context_create(struct linted_gpu_context **gpu_contextp)
{
	linted_error err = 0;

	struct linted_gpu_context *gpu_context;
	{
		void *xx;
		err = linted_mem_alloc(&xx, sizeof *gpu_context);
		if (err != 0)
			return err;
		gpu_context = xx;
	}

	EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
	if (EGL_NO_DISPLAY == display) {
		err = get_egl_error();
		goto release_thread;
	}

	if (EGL_FALSE == eglInitialize(display, 0, 0)) {
		err = get_egl_error();
		goto destroy_display;
	}

	EGLConfig config;
	{
		EGLConfig xx;
		EGLint yy;
		if (EGL_FALSE ==
		    eglChooseConfig(display, attr_list, &xx, 1U, &yy))
			goto choose_config_failed;
		config = xx;
		goto choose_config_succeeded;
	}

choose_config_failed:
	err = get_egl_error();
	goto destroy_display;

choose_config_succeeded:
	;
	EGLContext context = eglCreateContext(
	    display, config, EGL_NO_CONTEXT, context_attr);
	if (EGL_NO_CONTEXT == context) {
		err = get_egl_error();
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

	gpu_context->window = 0;
	gpu_context->display = display;
	gpu_context->config = config;
	gpu_context->context = context;
	gpu_context->surface = EGL_NO_SURFACE;
	gpu_context->buffer_commands = true;

	gpu_context->gl_state = NO_EGL_SURFACE;

	*gpu_contextp = gpu_context;

	return 0;

destroy_display:
	if (EGL_FALSE == eglTerminate(display)) {
		if (0 == err)
			err = get_egl_error();
	}

release_thread:
	if (EGL_FALSE == eglReleaseThread()) {
		if (0 == err)
			err = get_egl_error();
	}

	linted_mem_free(gpu_context);

	return err;
}

linted_error
linted_gpu_context_destroy(struct linted_gpu_context *gpu_context)
{
	linted_error err = 0;

	EGLDisplay display = gpu_context->display;
	EGLContext context = gpu_context->context;
	EGLSurface surface = gpu_context->surface;

	switch (gpu_context->gl_state) {
	case HAS_SETUP_GL:
		err = destroy_gl(gpu_context);

	case HAS_EGL_SURFACE:
		if (EGL_FALSE == eglDestroySurface(display, surface)) {
			if (0 == err)
				err = get_egl_error();
		}
		break;

	case NO_EGL_SURFACE:
		break;
	}

	gpu_context->gl_state = NO_EGL_SURFACE;

	if (EGL_FALSE == eglMakeCurrent(display, EGL_NO_SURFACE,
	                                EGL_NO_SURFACE, EGL_NO_CONTEXT))
		err = get_egl_error();

	if (EGL_FALSE == eglDestroyContext(display, context)) {
		if (0 == err)
			err = get_egl_error();
	}

	if (EGL_FALSE == eglTerminate(display)) {
		if (0 == err)
			err = get_egl_error();
	}

	if (EGL_FALSE == eglReleaseThread()) {
		if (0 == err)
			err = get_egl_error();
	}

	linted_mem_free(gpu_context);

	return err;
}

linted_error
linted_gpu_setwindow(struct linted_gpu_context *gpu_context,
                     linted_gpu_native_window new_window)
{
	linted_error err = 0;

	if (new_window > UINT32_MAX)
		return EINVAL;

	EGLDisplay display = gpu_context->display;
	EGLConfig config = gpu_context->config;
	EGLSurface surface = gpu_context->surface;
	linted_gpu_native_window window = gpu_context->window;

	switch (gpu_context->gl_state) {
	case HAS_SETUP_GL:
		err = destroy_gl(gpu_context);
		gpu_context->gl_state = HAS_EGL_SURFACE;

	case HAS_EGL_SURFACE:
		if (window == new_window)
			return 0;

		if (EGL_FALSE == eglDestroySurface(display, surface))
			return get_egl_error();

		gpu_context->gl_state = NO_EGL_SURFACE;

	case NO_EGL_SURFACE:
		break;
	}

	EGLSurface new_surface =
	    eglCreateWindowSurface(display, config, new_window, 0);
	if (EGL_NO_SURFACE == new_surface)
		return get_egl_error();

	gpu_context->surface = new_surface;
	gpu_context->window = new_window;
	gpu_context->gl_state = HAS_EGL_SURFACE;

	return 0;
}

linted_error
linted_gpu_unsetwindow(struct linted_gpu_context *gpu_context)
{
	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;

	if (EGL_FALSE == eglDestroySurface(display, surface))
		return get_egl_error();

	gpu_context->gl_state = NO_EGL_SURFACE;

	return 0;
}

void linted_gpu_update_state(struct linted_gpu_context *gpu_context,
                             struct linted_gpu_update const *glupdate)
{
	gpu_context->update = *glupdate;
	gpu_context->update_pending = true;
}

void linted_gpu_resize(struct linted_gpu_context *gpu_context,
                       unsigned width, unsigned height)
{
	gpu_context->width = width;
	gpu_context->height = height;

	gpu_context->resize_pending = true;
}

void linted_gpu_draw(struct linted_gpu_context *gpu_context)
{
	linted_error err;

	err = assure_current_egl_context(gpu_context);
	if (err != 0)
		return;

	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;

	if (gpu_context->buffer_commands) {
		real_draw(gpu_context);
		gpu_context->buffer_commands = false;
	} else {
		if (EGL_FALSE == eglSwapBuffers(display, surface)) {
			err = get_egl_error();

			char const *error = linted_error_string(err);
			linted_log(LINTED_LOG_ERROR,
			           "eglSwapBuffers: %s", error);
			linted_error_string_free(error);
		} else {
			gpu_context->buffer_commands = true;
		}
	}

	if (err != 0) {
		destroy_egl_surface(gpu_context);
	}
}

static linted_error destroy_gl(struct linted_gpu_context *gpu_context)
{

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

	{
		GLuint xx[] = {gpu_context->vertex_buffer,
		               gpu_context->normal_buffer,
		               gpu_context->index_buffer};
		glDeleteBuffers(LINTED_ARRAY_SIZE(xx), xx);
	}

	glUseProgram(0);
	glDeleteProgram(gpu_context->program);

	return 0;
}

static linted_error
destroy_egl_surface(struct linted_gpu_context *gpu_context)
{
	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;

	switch (gpu_context->gl_state) {
	case NO_EGL_SURFACE:
		return EINVAL;

	case HAS_SETUP_GL:
		destroy_gl(gpu_context);

	case HAS_EGL_SURFACE:
		if (EGL_FALSE == eglDestroySurface(display, surface))
			return get_egl_error();
		break;
	}

	gpu_context->gl_state = NO_EGL_SURFACE;
	return 0;
}

static linted_error
assure_current_egl_context(struct linted_gpu_context *gpu_context)
{
	linted_error err;

	if (HAS_SETUP_GL == gpu_context->gl_state)
		return 0;

	EGLDisplay display = gpu_context->display;
	EGLSurface surface = gpu_context->surface;
	EGLSurface context = gpu_context->context;

	if (EGL_NO_SURFACE == surface)
		return 0;

	if (EGL_FALSE ==
	    eglMakeCurrent(display, surface, surface, context)) {
		err = get_egl_error();
		goto destroy_context;
	}

	err = setup_gl(gpu_context);
	if (err != 0)
		goto use_no_context;

	gpu_context->context = context;

	return 0;

use_no_context:
	eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE,
	               EGL_NO_CONTEXT);

destroy_context:
	eglDestroyContext(display, context);

	return err;
}

static linted_error setup_gl(struct linted_gpu_context *gpu_context)
{
	linted_error err = 0;

	glHint(GL_GENERATE_MIPMAP_HINT, GL_FASTEST);
	glHint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, GL_FASTEST);

	glDisable(GL_DITHER);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	/* Use the default clear color for performance */

	flush_gl_errors();
	GLuint program = glCreateProgram();
	if (0 == program) {
		return get_gl_error();
	}

	flush_gl_errors();
	GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
	if (0 == fragment_shader) {
		err = get_gl_error();
		goto cleanup_program;
	}
	glAttachShader(program, fragment_shader);
	glDeleteShader(fragment_shader);

	glShaderSource(fragment_shader, 1U,
	               (GLchar const **)&linted_assets_fragment_shader,
	               0);
	glCompileShader(fragment_shader);

	GLint fragment_is_valid;
	{
		GLint xx = false;
		glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &xx);
		fragment_is_valid = xx;
	}
	if (!fragment_is_valid) {
		err = EINVAL;

		size_t info_log_length;
		{
			GLint xx = 0;
			glGetShaderiv(fragment_shader,
			              GL_INFO_LOG_LENGTH, &xx);
			info_log_length = xx;
		}

		GLchar *info_log;
		{
			void *xx;
			linted_error mem_err =
			    linted_mem_alloc(&xx, info_log_length);
			if (mem_err != 0)
				goto cleanup_program;
			info_log = xx;
		}
		glGetShaderInfoLog(fragment_shader, info_log_length, 0,
		                   info_log);
		linted_log(LINTED_LOG_ERROR, "invalid shader: %s",
		           info_log);
		linted_mem_free(info_log);
	}

	flush_gl_errors();
	GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
	if (0 == vertex_shader) {
		err = get_gl_error();
		goto cleanup_program;
	}
	glAttachShader(program, vertex_shader);
	glDeleteShader(vertex_shader);

	glShaderSource(vertex_shader, 1U,
	               (GLchar const **)&linted_assets_vertex_shader,
	               0);
	glCompileShader(vertex_shader);

	GLint vertex_is_valid;
	{
		GLint xx = false;
		glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &xx);
		vertex_is_valid = xx;
	}
	if (!vertex_is_valid) {
		err = EINVAL;

		size_t info_log_length = 0;
		{
			GLint xx;
			glGetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH,
			              &xx);
			info_log_length = xx;
		}

		GLchar *info_log;
		{
			void *xx;
			linted_error mem_err =
			    linted_mem_alloc(&xx, info_log_length);
			if (mem_err != 0)
				goto cleanup_program;
			info_log = xx;
		}

		glGetShaderInfoLog(vertex_shader, info_log_length, 0,
		                   info_log);
		linted_log(LINTED_LOG_ERROR, "invalid shader: %s",
		           info_log);
		linted_mem_free(info_log);
		goto cleanup_program;
	}
	glLinkProgram(program);

	glValidateProgram(program);

	GLint program_is_valid;
	{
		GLint xx = false;
		glGetProgramiv(program, GL_VALIDATE_STATUS, &xx);
		program_is_valid = xx;
	}
	if (!program_is_valid) {
		err = EINVAL;

		size_t info_log_length;
		{
			GLint xx = 0;
			glGetProgramiv(program, GL_INFO_LOG_LENGTH,
			               &xx);
			info_log_length = xx;
		}

		GLchar *info_log;
		{
			void *xx;
			linted_error mem_err =
			    linted_mem_alloc(&xx, info_log_length);
			if (mem_err != 0)
				goto cleanup_program;
			info_log = xx;
		}

		glGetProgramInfoLog(program, info_log_length, 0,
		                    info_log);
		linted_log(LINTED_LOG_ERROR, "invalid program: %s",
		           info_log);
		linted_mem_free(info_log);
		goto cleanup_program;
	}

	GLuint vertex_buffer;
	GLuint normal_buffer;
	GLuint index_buffer;
	{
		GLuint xx[3U];
		glGenBuffers(LINTED_ARRAY_SIZE(xx), xx);
		vertex_buffer = xx[0U];
		normal_buffer = xx[1U];
		index_buffer = xx[2U];
	}

	GLint maybe_mvp_matrix = glGetUniformLocation(
	    program, "model_view_projection_matrix");
	if (-1 == maybe_mvp_matrix) {
		err = EINVAL;
		goto cleanup_buffers;
	}
	GLuint mvp_matrix = maybe_mvp_matrix;

	GLint maybe_vertex = glGetAttribLocation(program, "vertex");
	if (maybe_vertex < 0) {
		err = EINVAL;
		goto cleanup_buffers;
	}
	GLuint vertex = maybe_vertex;

	GLint maybe_normal = glGetAttribLocation(program, "normal");
	if (maybe_normal < 0) {
		err = EINVAL;
		goto cleanup_buffers;
	}
	GLuint normal = maybe_normal;

	glEnableVertexAttribArray(vertex);
	glEnableVertexAttribArray(normal);

	glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	glVertexAttribPointer(
	    vertex, LINTED_ARRAY_SIZE(linted_assets_vertices[0U]),
	    GL_FLOAT, false, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, normal_buffer);
	glVertexAttribPointer(
	    normal, LINTED_ARRAY_SIZE(linted_assets_normals[0U]),
	    GL_FLOAT, false, 0, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,
	             linted_assets_size *
	                 sizeof linted_assets_vertices[0U],
	             linted_assets_vertices, GL_STATIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, normal_buffer);
	glBufferData(GL_ARRAY_BUFFER,
	             linted_assets_size *
	                 sizeof linted_assets_normals[0U],
	             linted_assets_normals, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	glUseProgram(program);

	glReleaseShaderCompiler();

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,
	             3U * linted_assets_indices_size *
	                 sizeof linted_assets_indices[0U],
	             linted_assets_indices, GL_STATIC_DRAW);
	/* Leave bound for DrawElements */

	gpu_context->program = program;
	gpu_context->vertex_buffer = vertex_buffer;
	gpu_context->normal_buffer = normal_buffer;
	gpu_context->index_buffer = index_buffer;
	gpu_context->model_view_projection_matrix = mvp_matrix;
	gpu_context->buffer_commands = true;

	gpu_context->update_pending = true;
	gpu_context->resize_pending = true;

	gpu_context->gl_state = HAS_SETUP_GL;

	return 0;

cleanup_buffers : {
	GLuint xx[] = {vertex_buffer, normal_buffer, index_buffer};
	glDeleteBuffers(LINTED_ARRAY_SIZE(xx), xx);
}

cleanup_program:
	glDeleteProgram(program);

	return err;
}

static void real_draw(struct linted_gpu_context *gpu_context)
{
	struct linted_gpu_update const *update = &gpu_context->update;

	if (EGL_NO_SURFACE == gpu_context->surface)
		return;

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

		struct matrix const y_rotation_matrix = {
		    {{1, 0, 0, 0},
		     {0, cos_y, -sin_y, 0},
		     {0, sin_y, cos_y, 0},
		     {0, 0, 0, 1}}};

		GLfloat cos_x = cosf(x_rotation);
		GLfloat sin_x = sinf(x_rotation);
		struct matrix const x_rotation_matrix = {
		    {{cos_x, 0, sin_x, 0},
		     {0, 1, 0, 0},
		     {-sin_x, 0, cos_x, 0},
		     {0, 0, 0, 1}}};

		/* Translate the camera */
		struct matrix const camera = {
		    {{1, 0, 0, 0},
		     {0, 1, 0, 0},
		     {0, 0, 1, 0},
		     {x_position, y_position, z_position, 1}}};

		double aspect = width / (double)height;
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

		struct matrix rotations = matrix_multiply(
		    x_rotation_matrix, y_rotation_matrix);
		struct matrix model_view =
		    matrix_multiply(camera, rotations);
		struct matrix model_view_projection =
		    matrix_multiply(model_view, projection);

		glUniformMatrix4fv(
		    gpu_context->model_view_projection_matrix, 1U,
		    false, (void const *)&model_view_projection);

		gpu_context->update_pending = false;
	}

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glDrawElements(GL_TRIANGLES, 3U * linted_assets_indices_size,
	               GL_UNSIGNED_BYTE, 0);
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
	/* Note that a single OpenGL call may return multiple errors
	 * so we get them all and then return the most serious.
	 */
	bool invalid_parameter = false;
	bool out_of_memory = false;
	bool unimplemented_error = false;

	for (;;) {
		bool exit = false;

		switch (glGetError()) {
		case GL_NO_ERROR:
			exit = true;
			break;

		case GL_INVALID_ENUM:
		case GL_INVALID_VALUE:
		case GL_INVALID_OPERATION:
		case GL_INVALID_FRAMEBUFFER_OPERATION:
			invalid_parameter = true;
			break;

		case GL_OUT_OF_MEMORY:
			out_of_memory = true;
			break;

		default:
			unimplemented_error = true;
			break;
		}

		if (exit)
			break;
	}

	/* Prioritize the errors by how serious they are */

	/* An unknown type of error, could be anything */
	if (unimplemented_error)
		return LINTED_ERROR_UNIMPLEMENTED;

	/* Fundamental logic error, very serious */
	if (invalid_parameter)
		return LINTED_ERROR_INVALID_PARAMETER;

	/* Runtime error */
	if (out_of_memory)
		return ENOMEM;

	return 0;
}

static linted_error get_egl_error(void)
{
	switch (eglGetError()) {
	case EGL_SUCCESS:
		return 0;

	case EGL_NOT_INITIALIZED:
		return EINVAL;

	case EGL_BAD_ACCESS:
		return LINTED_ERROR_AGAIN;

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
		return LINTED_ERROR_INVALID_PARAMETER;

	case EGL_CONTEXT_LOST:
		return LINTED_ERROR_UNIMPLEMENTED;

	default:
		LINTED_ASSUME_UNREACHABLE();
	}
}

static struct matrix matrix_multiply(struct matrix a, struct matrix b)
{
	struct matrix b_inverted;

	{
		uint_fast8_t ii = 4U;

		do {
			--ii;

			GLfloat b_ii_0;
			GLfloat b_ii_1;
			GLfloat b_ii_2;
			GLfloat b_ii_3;

			{
				union chunk b_ii = b.x[ii];

				b_ii_0 = b_ii.x[0U];
				b_ii_1 = b_ii.x[1U];
				b_ii_2 = b_ii.x[2U];
				b_ii_3 = b_ii.x[3U];
			}

			char *b_inverted_XX_ii = &((
			    char *)&b_inverted)[ii * sizeof(GLfloat)];

			*(GLfloat *)(b_inverted_XX_ii +
			             0U * sizeof(union chunk)) = b_ii_0;
			*(GLfloat *)(b_inverted_XX_ii +
			             1U * sizeof(union chunk)) = b_ii_1;
			*(GLfloat *)(b_inverted_XX_ii +
			             2U * sizeof(union chunk)) = b_ii_2;
			*(GLfloat *)(b_inverted_XX_ii +
			             3U * sizeof(union chunk)) = b_ii_3;
		} while (ii != 0U);
	}

	struct matrix result;

	uint_fast8_t ii = 4U;
	do {
		--ii;

		GLfloat a_ii_0;
		GLfloat a_ii_1;
		GLfloat a_ii_2;
		GLfloat a_ii_3;

		{
			union chunk a_ii = a.x[ii];

			a_ii_0 = a_ii.x[0U];
			a_ii_1 = a_ii.x[1U];
			a_ii_2 = a_ii.x[2U];
			a_ii_3 = a_ii.x[3U];
		}

		union chunk result_ii;

		uint_fast8_t jj = 4U;
		do {
			--jj;

			GLfloat result_ii_jj;

			{
				GLfloat b_0_jj;
				GLfloat b_1_jj;
				GLfloat b_2_jj;
				GLfloat b_3_jj;

				{
					union chunk b_XX_jj =
					    b_inverted.x[jj];

					b_0_jj = b_XX_jj.x[0U];
					b_1_jj = b_XX_jj.x[1U];
					b_2_jj = b_XX_jj.x[2U];
					b_3_jj = b_XX_jj.x[3U];
				}

				result_ii_jj =
				    (a_ii_0 * b_0_jj +
				     a_ii_1 * b_1_jj) +
				    (a_ii_2 * b_2_jj + a_ii_3 * b_3_jj);
			}

			result_ii.x[jj] = result_ii_jj;
		} while (jj != 0U);

		result.x[ii] = result_ii;
	} while (ii != 0U);

	return result;
}
