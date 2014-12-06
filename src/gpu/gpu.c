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
#include <pthread.h>
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

	struct linted_gpu_update update;

	unsigned width;
	unsigned height;

	GLuint program;
	GLuint vertex_buffer;
	GLuint normal_buffer;
	GLuint index_buffer;

	EGLContext context;
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

static EGLint const attr_list[] = {
	EGL_CONFORMANT,        EGL_OPENGL_ES2_BIT, /**/
	EGL_RENDERABLE_TYPE,   EGL_OPENGL_ES2_BIT, /**/
	EGL_DEPTH_SIZE,        16,                 /**/
	EGL_COLOR_BUFFER_TYPE, EGL_RGB_BUFFER,     /**/
	EGL_RED_SIZE,          5,                  /**/
	EGL_GREEN_SIZE,        5,                  /**/
	EGL_BLUE_SIZE,         3,                  /**/
	EGL_NONE
};

static EGLint const context_attr[] = { EGL_CONTEXT_CLIENT_VERSION, 2, /**/
	                               EGL_NONE };

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

static linted_error gpu_init_procedures(void);

static void gpu_ClearColor(GLfloat red, GLfloat green, GLfloat blue,
                           GLfloat alpha);
static void gpu_Flush(void);
static void gpu_Enable(GLenum cap);
static GLuint gpu_CreateProgram(void);
static GLuint gpu_CreateShader(GLenum shader_type);
static void gpu_AttachShader(GLuint program, GLuint shader);
static void gpu_DeleteShader(GLuint shader);
static void gpu_ShaderSource(GLuint shader, GLsizei count,
                             const GLchar **string, const GLint *length);
static void gpu_CompileShader(GLuint shader);
static void gpu_GetShaderiv(GLuint shader, GLenum pname, GLint *params);
static void gpu_GetShaderInfoLog(GLuint shader, GLsizei maxLength,
                                 GLsizei *length, GLchar *infoLog);
static void gpu_UseProgram(GLuint program);
static void gpu_DeleteProgram(GLuint program);
static GLenum gpu_GetError(void);
static void gpu_LinkProgram(GLuint program);
static void gpu_ValidateProgram(GLuint program);
static void gpu_GetProgramiv(GLuint program, GLenum pname, GLint *params);
static GLint gpu_GetUniformLocation(GLuint program, GLchar const *name);
static GLint gpu_GetAttribLocation(GLuint program, GLchar const *name);
static void gpu_EnableVertexAttribArray(GLuint index);
static void gpu_GetProgramInfoLog(GLuint program, GLsizei maxLength,
                                  GLsizei *length, GLchar *infoLog);
static void gpu_Viewport(GLint x, GLint y, GLsizei width, GLsizei height);
static void gpu_UniformMatrix4fv(GLint location, GLsizei count,
                                 GLboolean transpose, GLfloat *value);
static void gpu_Clear(GLbitfield mask);
static void gpu_DrawElements(GLenum mode, GLsizei count, GLenum type,
                             GLvoid const *indices);
static void gpu_VertexAttribPointer(GLuint index, GLint size, GLenum type,
                                    GLboolean normalized, GLsizei stride,
                                    const GLvoid *pointer);
static void gpu_GenBuffers(GLsizei n, GLuint *buffers);
static void gpu_DeleteBuffers(GLsizei n, GLuint const *buffers);
static void gpu_BufferData(GLenum target, GLsizeiptr size, const GLvoid *data,
                           GLenum usage);
static void gpu_BindBuffer(GLenum target, GLuint buffer);

linted_error linted_gpu_context_create(linted_gpu_native_display native_display,
                                       linted_gpu_native_window native_window,
                                       struct linted_gpu_context **gpu_contextp)
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

	errnum = gpu_init_procedures();
	if (errnum != 0)
		goto release_thread;

	EGLDisplay display = eglGetDisplay(native_display);
	if (EGL_NO_DISPLAY == display) {
		errnum = get_egl_error();
		goto release_thread;
	}

	if (EGL_FALSE == eglInitialize(display, NULL, NULL)) {
		errnum = get_egl_error();
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
	errnum = get_egl_error();
	goto destroy_display;

choose_config_succeeded:
	;
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

linted_error linted_gpu_context_destroy(struct linted_gpu_context *gpu_context)
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

	/* Abort swapping buffers if the current processed or being
	 * processed buffer is stale */
	gpu_context->state = BUFFER_COMMANDS;
}

void linted_gpu_resize(struct linted_gpu_context *gpu_context, unsigned width,
                       unsigned height)
{
	gpu_context->width = width;
	gpu_context->height = height;

	gpu_context->resize_pending = true;

	/* Abort swapping buffers if the current processed or being
	 * processed buffer is stale */
	gpu_context->state = BUFFER_COMMANDS;
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
		gpu_Flush();
		gpu_context->state = SWAP_BUFFERS;
		break;

	case SWAP_BUFFERS:
		if (EGL_FALSE == eglSwapBuffers(display, surface)) {
			errnum = get_egl_error();
			break;
		}
		gpu_context->state = BUFFER_COMMANDS;
		break;
	}

	if (errnum != 0) {
		destroy_contexts(gpu_context);
		gpu_context->has_gl_context = false;
	}
}

static linted_error destroy_contexts(struct linted_gpu_context *gpu_context)
{
	linted_error errnum = 0;

	EGLDisplay display = gpu_context->display;
	EGLContext context = gpu_context->context;

	gpu_BindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

	{
		GLuint xx[] = { gpu_context->vertex_buffer,
			        gpu_context->normal_buffer,
			        gpu_context->index_buffer };
		gpu_DeleteBuffers(LINTED_ARRAY_SIZE(xx), xx);
	}

	gpu_UseProgram(0);
	gpu_DeleteProgram(gpu_context->program);

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

	gpu_Enable(GL_DEPTH_TEST);
	gpu_Enable(GL_CULL_FACE);

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

	gpu_ClearColor(red, green, blue, 1);

	flush_gl_errors();
	GLuint program = gpu_CreateProgram();
	if (0 == program) {
		errnum = get_gl_error();
		goto use_no_context;
	}

	flush_gl_errors();
	GLuint fragment_shader = gpu_CreateShader(GL_FRAGMENT_SHADER);
	if (0 == fragment_shader) {
		errnum = get_gl_error();
		goto cleanup_program;
	}
	gpu_AttachShader(program, fragment_shader);
	gpu_DeleteShader(fragment_shader);

	gpu_ShaderSource(fragment_shader, 1U,
	                 (GLchar const **)&linted_assets_fragment_shader, NULL);
	gpu_CompileShader(fragment_shader);

	GLint fragment_is_valid;
	{
		GLint xx = false;
		gpu_GetShaderiv(fragment_shader, GL_COMPILE_STATUS, &xx);
		fragment_is_valid = xx;
	}
	if (!fragment_is_valid) {
		errnum = EINVAL;

		GLint info_log_length;
		{
			GLint xx = 0;
			gpu_GetShaderiv(fragment_shader, GL_INFO_LOG_LENGTH,
			                &xx);
			info_log_length = xx;
		}

		GLchar *info_log;
		{
			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (mem_errnum != 0)
				goto cleanup_program;
			info_log = xx;
		}
		gpu_GetShaderInfoLog(fragment_shader, info_log_length, NULL,
		                     info_log);
		log_str(log, LINTED_STR("Invalid shader: "), info_log);
		linted_mem_free(info_log);
	}

	flush_gl_errors();
	GLuint vertex_shader = gpu_CreateShader(GL_VERTEX_SHADER);
	if (0 == vertex_shader) {
		errnum = get_gl_error();
		goto cleanup_program;
	}
	gpu_AttachShader(program, vertex_shader);
	gpu_DeleteShader(vertex_shader);

	gpu_ShaderSource(vertex_shader, 1U,
	                 (GLchar const **)&linted_assets_vertex_shader, NULL);
	gpu_CompileShader(vertex_shader);

	GLint vertex_is_valid;
	{
		GLint xx = false;
		gpu_GetShaderiv(vertex_shader, GL_COMPILE_STATUS, &xx);
		vertex_is_valid = xx;
	}
	if (!vertex_is_valid) {
		errnum = EINVAL;

		GLint info_log_length = 0;
		{
			GLint xx;
			gpu_GetShaderiv(vertex_shader, GL_INFO_LOG_LENGTH, &xx);
			info_log_length = xx;
		}

		GLchar *info_log;
		{
			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (mem_errnum != 0)
				goto cleanup_program;
			info_log = xx;
		}

		gpu_GetShaderInfoLog(vertex_shader, info_log_length, NULL,
		                     info_log);
		log_str(log, LINTED_STR("Invalid shader: "), info_log);
		linted_mem_free(info_log);
		goto cleanup_program;
	}
	gpu_LinkProgram(program);

	gpu_ValidateProgram(program);

	GLint program_is_valid;
	{
		GLint xx = false;
		gpu_GetProgramiv(program, GL_VALIDATE_STATUS, &xx);
		program_is_valid = xx;
	}
	if (!program_is_valid) {
		errnum = EINVAL;

		GLint info_log_length;
		{
			GLint xx = 0;
			gpu_GetProgramiv(program, GL_INFO_LOG_LENGTH, &xx);
			info_log_length = xx;
		}

		GLchar *info_log;
		{
			void *xx;
			linted_error mem_errnum =
			    linted_mem_alloc(&xx, info_log_length);
			if (mem_errnum != 0)
				goto cleanup_program;
			info_log = xx;
		}

		gpu_GetProgramInfoLog(program, info_log_length, NULL, info_log);
		log_str(log, LINTED_STR("Invalid program: "), info_log);
		linted_mem_free(info_log);
		goto cleanup_program;
	}

	GLuint vertex_buffer;
	GLuint normal_buffer;
	GLuint index_buffer;
	{
		GLuint xx[3U];
		gpu_GenBuffers(LINTED_ARRAY_SIZE(xx), xx);
		vertex_buffer = xx[0U];
		normal_buffer = xx[1U];
		index_buffer = xx[2U];
	}

	GLint mvp_matrix =
	    gpu_GetUniformLocation(program, "model_view_projection_matrix");
	if (-1 == mvp_matrix) {
		errnum = EINVAL;
		goto cleanup_buffers;
	}

	GLint vertex = gpu_GetAttribLocation(program, "vertex");
	if (-1 == vertex) {
		errnum = EINVAL;
		goto cleanup_buffers;
	}

	GLint normal = gpu_GetAttribLocation(program, "normal");
	if (-1 == normal) {
		errnum = EINVAL;
		goto cleanup_buffers;
	}

	gpu_EnableVertexAttribArray(vertex);
	gpu_EnableVertexAttribArray(normal);

	gpu_BindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	gpu_VertexAttribPointer(vertex,
	                        LINTED_ARRAY_SIZE(linted_assets_vertices[0U]),
	                        GL_FLOAT, false, 0, NULL);

	gpu_BindBuffer(GL_ARRAY_BUFFER, normal_buffer);
	gpu_VertexAttribPointer(normal,
	                        LINTED_ARRAY_SIZE(linted_assets_normals[0U]),
	                        GL_FLOAT, false, 0, NULL);
	gpu_BindBuffer(GL_ARRAY_BUFFER, 0);

	gpu_BindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
	gpu_BufferData(GL_ARRAY_BUFFER,
	               linted_assets_size * sizeof linted_assets_vertices[0U],
	               linted_assets_vertices, GL_STATIC_DRAW);

	gpu_BindBuffer(GL_ARRAY_BUFFER, normal_buffer);
	gpu_BufferData(GL_ARRAY_BUFFER,
	               linted_assets_size * sizeof linted_assets_normals[0U],
	               linted_assets_normals, GL_STATIC_DRAW);
	gpu_BindBuffer(GL_ARRAY_BUFFER, 0);

	gpu_UseProgram(program);

	gpu_BindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_buffer);
	gpu_BufferData(GL_ELEMENT_ARRAY_BUFFER,
	               3U * linted_assets_indices_size *
	                   sizeof linted_assets_indices[0U],
	               linted_assets_indices, GL_STATIC_DRAW);
	/* Leave bound for DrawElements */

	gpu_context->context = context;
	gpu_context->program = program;
	gpu_context->vertex_buffer = vertex_buffer;
	gpu_context->normal_buffer = normal_buffer;
	gpu_context->index_buffer = index_buffer;
	gpu_context->model_view_projection_matrix = mvp_matrix;
	gpu_context->state = BUFFER_COMMANDS;
	gpu_context->has_gl_context = true;

	gpu_context->update_pending = true;
	gpu_context->resize_pending = true;

	return 0;

cleanup_buffers : {
	GLuint xx[] = { vertex_buffer, normal_buffer, index_buffer };
	gpu_DeleteBuffers(LINTED_ARRAY_SIZE(xx), xx);
}

cleanup_program:
	gpu_DeleteProgram(program);

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
		gpu_Viewport(0, 0, width, height);
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
			{ { 1, 0, 0, 0 },
			  { 0, cos_y, -sin_y, 0 },
			  { 0, sin_y, cos_y, 0 },
			  { 0, 0, 0, 1 } }
		};

		GLfloat cos_x = cosf(x_rotation);
		GLfloat sin_x = sinf(x_rotation);
		struct matrix const x_rotation_matrix = {
			{ { cos_x, 0, sin_x, 0 },
			  { 0, 1, 0, 0 },
			  { -sin_x, 0, cos_x, 0 },
			  { 0, 0, 0, 1 } }
		};

		/* Translate the camera */
		struct matrix const camera = { { { 1, 0, 0, 0 },
				                 { 0, 1, 0, 0 },
				                 { 0, 0, 1, 0 },
				                 { x_position, y_position,
					           z_position, 1 } } };

		GLfloat aspect = width / (GLfloat)height;
		double fov = acos(-1.0) / 4;

		double d = 1 / tan(fov / 2);
		double far = 1000;
		double near = 1;

		struct matrix const projection = {
			{ { d / aspect, 0, 0, 0 },
			  { 0, d, 0, 0 },
			  { 0, 0, (far + near) / (near - far),
			    2 * far * near / (near - far) },
			  { 0, 0, -1, 0 } }
		};

		struct matrix rotations =
		    matrix_multiply(x_rotation_matrix, y_rotation_matrix);
		struct matrix model_view = matrix_multiply(camera, rotations);
		struct matrix model_view_projection =
		    matrix_multiply(model_view, projection);

		gpu_UniformMatrix4fv(gpu_context->model_view_projection_matrix,
		                     1U, false, model_view_projection.x[0U]);

		gpu_context->update_pending = false;
	}

	gpu_Clear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	gpu_DrawElements(GL_TRIANGLES, 3U * linted_assets_indices_size,
	                 GL_UNSIGNED_BYTE, NULL);
}

static void flush_gl_errors(void)
{
	GLenum error;
	do {
		error = gpu_GetError();
	} while (error != GL_NO_ERROR);
}

static linted_error get_gl_error(void)
{
	/* Get the first error. Note that a single OpenGL call may
	 * return multiple errors so this only gives the first of many
	 * possible errors.
	 */
	switch (gpu_GetError()) {
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

	{
		size_t ii = 4U;
		do {
			--ii;

			GLfloat b_ii_0;
			GLfloat b_ii_1;
			GLfloat b_ii_2;
			GLfloat b_ii_3;

			{
				GLfloat b_ii[4U];
				memcpy(b_ii, b.x[ii], sizeof b_ii);

				b_ii_0 = b_ii[0U];
				b_ii_1 = b_ii[1U];
				b_ii_2 = b_ii[2U];
				b_ii_3 = b_ii[3U];
			}

			b_inverted[0U][ii] = b_ii_0;
			b_inverted[1U][ii] = b_ii_1;
			b_inverted[2U][ii] = b_ii_2;
			b_inverted[3U][ii] = b_ii_3;
		} while (ii != 0U);
	}

	struct matrix result;

	size_t ii = 4U;
	do {
		--ii;

		GLfloat a_ii[4U];
		memcpy(a_ii, a.x[ii], sizeof a_ii);

		GLfloat result_ii[4U];

		size_t jj = 4U;
		do {
			--jj;

			GLfloat b_XX_jj[4U];
			memcpy(b_XX_jj, b_inverted[jj], sizeof b_XX_jj);

			result_ii[jj] =
			    (a_ii[0U] * b_XX_jj[0U] + a_ii[1U] * b_XX_jj[1U]) +
			    (a_ii[2U] * b_XX_jj[2U] + a_ii[3U] * b_XX_jj[3U]);
		} while (jj != 0U);

		memcpy(result.x[ii], result_ii, sizeof result_ii);
	} while (ii != 0U);

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

/* Don't need to query display for support because OpenGL ES 2.0
 * supports all of these. Also, function pointers aren't local to a
 * specific display. */

typedef void (*gpu_ClearColor_func)(GLfloat red, GLfloat green, GLfloat blue,
                                    GLfloat alpha);
typedef void (*gpu_Flush_func)(void);
typedef void (*gpu_Enable_func)(GLenum cap);
typedef GLuint (*gpu_CreateProgram_func)(void);
typedef GLuint (*gpu_CreateShader_func)(GLenum shader_type);
typedef void (*gpu_AttachShader_func)(GLuint program, GLuint shader);
typedef void (*gpu_DeleteShader_func)(GLuint shader);
typedef void (*gpu_ShaderSource_func)(GLuint shader, GLsizei count,
                                      const GLchar **string,
                                      const GLint *length);
typedef void (*gpu_CompileShader_func)(GLuint shader);
typedef void (*gpu_GetShaderiv_func)(GLuint shader, GLenum pname,
                                     GLint *params);
typedef void (*gpu_GetShaderInfoLog_func)(GLuint shader, GLsizei maxLength,
                                          GLsizei *length, GLchar *infoLog);
typedef void (*gpu_UseProgram_func)(GLuint program);
typedef void (*gpu_DeleteProgram_func)(GLuint program);
typedef GLenum (*gpu_GetError_func)(void);
typedef void (*gpu_LinkProgram_func)(GLuint program);
typedef void (*gpu_ValidateProgram_func)(GLuint program);
typedef void (*gpu_GetProgramiv_func)(GLuint program, GLenum pname,
                                      GLint *params);
typedef GLint (*gpu_GetUniformLocation_func)(GLuint program,
                                             GLchar const *name);
typedef GLint (*gpu_GetAttribLocation_func)(GLuint program, GLchar const *name);
typedef void (*gpu_EnableVertexAttribArray_func)(GLuint index);
typedef void (*gpu_GetProgramInfoLog_func)(GLuint program, GLsizei maxLength,
                                           GLsizei *length, GLchar *infoLog);
typedef void (*gpu_Viewport_func)(GLint x, GLint y, GLsizei width,
                                  GLsizei height);
typedef void (*gpu_UniformMatrix4fv_func)(GLint location, GLsizei count,
                                          GLboolean transpose, GLfloat *value);
typedef void (*gpu_Clear_func)(GLbitfield mask);
typedef void (*gpu_DrawElements_func)(GLenum mode, GLsizei count, GLenum type,
                                      GLvoid const *indices);
typedef void (*gpu_VertexAttribPointer_func)(GLuint index, GLint size,
                                             GLenum type, GLboolean normalized,
                                             GLsizei stride,
                                             const GLvoid *pointer);
typedef void (*gpu_GenBuffers_func)(GLsizei n, GLuint *buffers);
typedef void (*gpu_DeleteBuffers_func)(GLsizei n, GLuint const *buffers);
typedef void (*gpu_BufferData_func)(GLenum target, GLsizeiptr size,
                                    const GLvoid *data, GLenum usage);
typedef void (*gpu_BindBuffer_func)(GLenum target, GLuint buffer);

static gpu_ClearColor_func gpu_ClearColor_pointer;
static gpu_Flush_func gpu_Flush_pointer;
static gpu_Enable_func gpu_Enable_pointer;
static gpu_CreateProgram_func gpu_CreateProgram_pointer;
static gpu_CreateShader_func gpu_CreateShader_pointer;
static gpu_AttachShader_func gpu_AttachShader_pointer;
static gpu_DeleteShader_func gpu_DeleteShader_pointer;
static gpu_ShaderSource_func gpu_ShaderSource_pointer;
static gpu_CompileShader_func gpu_CompileShader_pointer;
static gpu_GetShaderiv_func gpu_GetShaderiv_pointer;
static gpu_GetShaderInfoLog_func gpu_GetShaderInfoLog_pointer;
static gpu_UseProgram_func gpu_UseProgram_pointer;
static gpu_DeleteProgram_func gpu_DeleteProgram_pointer;
static gpu_GetError_func gpu_GetError_pointer;
static gpu_LinkProgram_func gpu_LinkProgram_pointer;
static gpu_ValidateProgram_func gpu_ValidateProgram_pointer;
static gpu_GetProgramiv_func gpu_GetProgramiv_pointer;
static gpu_GetUniformLocation_func gpu_GetUniformLocation_pointer;
static gpu_GetAttribLocation_func gpu_GetAttribLocation_pointer;
static gpu_EnableVertexAttribArray_func gpu_EnableVertexAttribArray_pointer;
static gpu_GetProgramInfoLog_func gpu_GetProgramInfoLog_pointer;
static gpu_Viewport_func gpu_Viewport_pointer;
static gpu_UniformMatrix4fv_func gpu_UniformMatrix4fv_pointer;
static gpu_Clear_func gpu_Clear_pointer;
static gpu_DrawElements_func gpu_DrawElements_pointer;
static gpu_VertexAttribPointer_func gpu_VertexAttribPointer_pointer;
static gpu_GenBuffers_func gpu_GenBuffers_pointer;
static gpu_DeleteBuffers_func gpu_DeleteBuffers_pointer;
static gpu_BufferData_func gpu_BufferData_pointer;
static gpu_BindBuffer_func gpu_BindBuffer_pointer;

static void gpu_ClearColor(GLfloat red, GLfloat green, GLfloat blue,
                           GLfloat alpha)
{
	gpu_ClearColor_pointer(red, green, blue, alpha);
}

static void gpu_Flush(void)
{
	gpu_Flush_pointer();
}

static void gpu_Enable(GLenum cap)
{
	gpu_Enable_pointer(cap);
}

static GLuint gpu_CreateProgram(void)
{
	return gpu_CreateProgram_pointer();
}

static GLuint gpu_CreateShader(GLenum shader_type)
{
	return gpu_CreateShader_pointer(shader_type);
}

static void gpu_AttachShader(GLuint program, GLuint shader)
{
	gpu_AttachShader_pointer(program, shader);
}

static void gpu_DeleteShader(GLuint shader)
{
	gpu_DeleteShader_pointer(shader);
}

static void gpu_ShaderSource(GLuint shader, GLsizei count,
                             const GLchar **string, const GLint *length)
{
	gpu_ShaderSource_pointer(shader, count, string, length);
}

static void gpu_CompileShader(GLuint shader)
{
	gpu_CompileShader_pointer(shader);
}

static void gpu_GetShaderiv(GLuint shader, GLenum pname, GLint *params)
{
	gpu_GetShaderiv_pointer(shader, pname, params);
}

static void gpu_GetShaderInfoLog(GLuint shader, GLsizei maxLength,
                                 GLsizei *length, GLchar *infoLog)
{
	gpu_GetShaderInfoLog_pointer(shader, maxLength, length, infoLog);
}

static void gpu_UseProgram(GLuint program)
{
	gpu_UseProgram_pointer(program);
}

static void gpu_DeleteProgram(GLuint program)
{
	gpu_DeleteProgram_pointer(program);
}

static GLenum gpu_GetError(void)
{
	return gpu_GetError_pointer();
}

static void gpu_LinkProgram(GLuint program)
{
	gpu_LinkProgram_pointer(program);
}

static void gpu_ValidateProgram(GLuint program)
{
	gpu_ValidateProgram_pointer(program);
}

static void gpu_GetProgramiv(GLuint program, GLenum pname, GLint *params)
{
	gpu_GetProgramiv_pointer(program, pname, params);
}

static GLint gpu_GetUniformLocation(GLuint program, GLchar const *name)
{
	return gpu_GetUniformLocation_pointer(program, name);
}

static GLint gpu_GetAttribLocation(GLuint program, GLchar const *name)
{
	return gpu_GetAttribLocation_pointer(program, name);
}

static void gpu_EnableVertexAttribArray(GLuint index)
{
	gpu_EnableVertexAttribArray_pointer(index);
}

static void gpu_GetProgramInfoLog(GLuint program, GLsizei maxLength,
                                  GLsizei *length, GLchar *infoLog)
{
	gpu_GetProgramInfoLog_pointer(program, maxLength, length, infoLog);
}

static void gpu_Viewport(GLint x, GLint y, GLsizei width, GLsizei height)
{
	gpu_Viewport_pointer(x, y, width, height);
}

static void gpu_UniformMatrix4fv(GLint location, GLsizei count,
                                 GLboolean transpose, GLfloat *value)
{
	gpu_UniformMatrix4fv_pointer(location, count, transpose, value);
}

static void gpu_Clear(GLbitfield mask)
{
	gpu_Clear_pointer(mask);
}

static void gpu_DrawElements(GLenum mode, GLsizei count, GLenum type,
                             GLvoid const *indices)
{
	gpu_DrawElements_pointer(mode, count, type, indices);
}

static void gpu_VertexAttribPointer(GLuint index, GLint size, GLenum type,
                                    GLboolean normalized, GLsizei stride,
                                    const GLvoid *pointer)
{
	gpu_VertexAttribPointer_pointer(index, size, type, normalized, stride,
	                                pointer);
}

static void gpu_GenBuffers(GLsizei n, GLuint *buffers)
{
	gpu_GenBuffers_pointer(n, buffers);
}

static void gpu_DeleteBuffers(GLsizei n, GLuint const *buffers)
{
	fprintf(stderr, "delete\n");
	gpu_DeleteBuffers_pointer(n, buffers);
}

static void gpu_BufferData(GLenum target, GLsizeiptr size, const GLvoid *data,
                           GLenum usage)
{
	gpu_BufferData_pointer(target, size, data, usage);
}

static void gpu_BindBuffer(GLenum target, GLuint buffer)
{
	gpu_BindBuffer_pointer(target, buffer);
}

static pthread_mutex_t init_procedures_lock = PTHREAD_MUTEX_INITIALIZER;
static bool init_successful = false;

static linted_error gpu_init_procedures(void)
{
	linted_error errnum = 0;

	pthread_mutex_lock(&init_procedures_lock);
	if (init_successful)
		goto unlock_mutex;

	if (NULL ==
	    (gpu_ClearColor_pointer =
	         (gpu_ClearColor_func)eglGetProcAddress("glClearColor")))
		goto fail_init;

	if (NULL ==
	    (gpu_Flush_pointer = (gpu_Flush_func)eglGetProcAddress("glFlush")))
		goto fail_init;

	if (NULL == (gpu_Enable_pointer =
	                 (gpu_Enable_func)eglGetProcAddress("glEnable")))
		goto fail_init;

	if (NULL ==
	    (gpu_CreateProgram_pointer =
	         (gpu_CreateProgram_func)eglGetProcAddress("glCreateProgram")))
		goto fail_init;

	if (NULL ==
	    (gpu_CreateShader_pointer =
	         (gpu_CreateShader_func)eglGetProcAddress("glCreateShader")))
		goto fail_init;

	if (NULL ==
	    (gpu_AttachShader_pointer =
	         (gpu_AttachShader_func)eglGetProcAddress("glAttachShader")))
		goto fail_init;

	if (NULL ==
	    (gpu_DeleteShader_pointer =
	         (gpu_DeleteShader_func)eglGetProcAddress("glDeleteShader")))
		goto fail_init;

	if (NULL ==
	    (gpu_ShaderSource_pointer =
	         (gpu_ShaderSource_func)eglGetProcAddress("glShaderSource")))
		goto fail_init;

	if (NULL ==
	    (gpu_CompileShader_pointer =
	         (gpu_CompileShader_func)eglGetProcAddress("glCompileShader")))
		goto fail_init;

	if (NULL ==
	    (gpu_GetShaderiv_pointer =
	         (gpu_GetShaderiv_func)eglGetProcAddress("glGetShaderiv")))
		goto fail_init;

	if (NULL == (gpu_GetShaderInfoLog_pointer = (gpu_GetShaderInfoLog_func)
	             eglGetProcAddress("glGetShaderInfoLog")))
		goto fail_init;

	if (NULL ==
	    (gpu_UseProgram_pointer =
	         (gpu_UseProgram_func)eglGetProcAddress("glUseProgram")))
		goto fail_init;

	if (NULL ==
	    (gpu_DeleteProgram_pointer =
	         (gpu_DeleteProgram_func)eglGetProcAddress("glDeleteProgram")))
		goto fail_init;

	if (NULL == (gpu_GetError_pointer =
	                 (gpu_GetError_func)eglGetProcAddress("glGetError")))
		goto fail_init;

	if (NULL ==
	    (gpu_LinkProgram_pointer =
	         (gpu_LinkProgram_func)eglGetProcAddress("glLinkProgram")))
		goto fail_init;

	if (NULL == (gpu_ValidateProgram_pointer = (gpu_ValidateProgram_func)
	             eglGetProcAddress("glValidateProgram")))
		goto fail_init;

	if (NULL ==
	    (gpu_GetProgramiv_pointer =
	         (gpu_GetProgramiv_func)eglGetProcAddress("glGetProgramiv")))
		goto fail_init;

	if (NULL == (gpu_GetUniformLocation_pointer =
	                 (gpu_GetUniformLocation_func)eglGetProcAddress(
	                     "glGetUniformLocation")))
		goto fail_init;

	if (NULL == (gpu_GetAttribLocation_pointer =
	                 (gpu_GetAttribLocation_func)eglGetProcAddress(
	                     "glGetAttribLocation")))
		goto fail_init;

	if (NULL == (gpu_EnableVertexAttribArray_pointer =
	                 (gpu_EnableVertexAttribArray_func)eglGetProcAddress(
	                     "glEnableVertexAttribArray")))
		goto fail_init;

	if (NULL == (gpu_GetProgramInfoLog_pointer =
	                 (gpu_GetProgramInfoLog_func)eglGetProcAddress(
	                     "glGetProgramInfoLog")))
		goto fail_init;

	if (NULL == (gpu_Viewport_pointer =
	                 (gpu_Viewport_func)eglGetProcAddress("glViewport")))
		goto fail_init;

	if (NULL == (gpu_UniformMatrix4fv_pointer = (gpu_UniformMatrix4fv_func)
	             eglGetProcAddress("glUniformMatrix4fv")))
		goto fail_init;

	if (NULL ==
	    (gpu_Clear_pointer = (gpu_Clear_func)eglGetProcAddress("glClear")))
		goto fail_init;

	if (NULL ==
	    (gpu_DrawElements_pointer =
	         (gpu_DrawElements_func)eglGetProcAddress("glDrawElements")))
		goto fail_init;

	if (NULL == (gpu_VertexAttribPointer_pointer =
	                 (gpu_VertexAttribPointer_func)eglGetProcAddress(
	                     "glVertexAttribPointer")))
		goto fail_init;

	if (NULL ==
	    (gpu_GenBuffers_pointer =
	         (gpu_GenBuffers_func)eglGetProcAddress("glGenBuffers")))
		goto fail_init;

	if (NULL ==
	    (gpu_DeleteBuffers_pointer =
	         (gpu_DeleteBuffers_func)eglGetProcAddress("glDeleteBuffers")))
		goto fail_init;

	if (NULL ==
	    (gpu_BufferData_pointer =
	         (gpu_BufferData_func)eglGetProcAddress("glBufferData")))
		goto fail_init;

	if (NULL ==
	    (gpu_BindBuffer_pointer =
	         (gpu_BindBuffer_func)eglGetProcAddress("glBindBuffer")))
		goto fail_init;

	init_successful = true;

	if (0) {
	fail_init:
		errnum = get_egl_error();
	}

unlock_mutex:
	pthread_mutex_unlock(&init_procedures_lock);

	return errnum;
}
