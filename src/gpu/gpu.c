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
#define _POSIX_C_SOURCE 200112L

#include "config.h"

#include "linted/assets.h"
#include "linted/error.h"
#include "linted/gpu.h"
#include "linted/log.h"
#include "linted/mem.h"
#include "linted/sched.h"
#include "linted/util.h"

#include <math.h>
#include <pthread.h>
#include <sched.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GLES3/gl3.h>

/*
 * @file
 *
 * @todo Factor out the render loop into another thread.
 *
 * @todo Once `EGL_KHR_get_all_proc_addresses` is supported by my Mesa
 *       use it.
 */

struct command_queue {
	pthread_spinlock_t lock;

	struct linted_gpu_update update;
	linted_gpu_x11_window window;
	unsigned width;
	unsigned height;
	uint64_t skipped_updates_counter;

	bool time_to_quit : 1U;
	bool has_new_window : 1U;
	bool remove_window : 1U;
	bool update_pending : 1U;
	bool resize_pending : 1U;
};

struct privates {
	struct linted_gpu_update update;
	bool update_pending;

	struct timespec last_time;

	EGLSurface surface;
	EGLContext context;
	EGLDisplay display;
	EGLConfig config;

	linted_gpu_x11_window window;

	unsigned width;
	unsigned height;

	GLuint program;

	GLuint vertex_buffer;
	GLuint normal_buffer;
	GLuint index_buffer;

	GLint model_view_projection_matrix;
	GLint eye_vertex;

	bool has_egl_surface : 1U;
	bool has_egl_context : 1U;
	bool has_window : 1U;

	bool resize_pending : 1U;

	bool has_current_context : 1U;
	bool has_setup_gl : 1U;
};

struct linted_gpu_context {
	struct command_queue command_queue;
	struct privates privates;

	pthread_t thread;
	EGLDisplay display;
	EGLConfig config;
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
    EGL_SAMPLE_BUFFERS, 1,                       /**/
    EGL_DEPTH_SIZE, 16,                          /**/
    EGL_COLOR_BUFFER_TYPE, EGL_RGB_BUFFER,       /**/
    EGL_RED_SIZE, 8,                             /**/
    EGL_GREEN_SIZE, 8,                           /**/
    EGL_BLUE_SIZE, 8,                            /**/
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT | EGL_SWAP_BEHAVIOR_PRESERVED_BIT,
    EGL_NONE};

static EGLint const context_attr[] = {EGL_CONTEXT_CLIENT_VERSION,
                                      3, /**/
                                      EGL_NONE};

static void *gpu_routine(void *);

static linted_error destroy_gl(struct privates *privates);
static linted_error remove_current_context(struct privates *privates);
static linted_error destroy_egl_context(struct privates *privates);
static linted_error destroy_egl_surface(struct privates *privates);

static linted_error create_egl_context(struct privates *privates);
static linted_error create_egl_surface(struct privates *privates);
static linted_error make_current(struct privates *privates);
static linted_error setup_gl(struct privates *privates);

static void real_draw(struct privates *privates);

static void flush_gl_errors(void);

static struct matrix
model_view_projection(GLfloat x_rotation, GLfloat z_rotation,
                      GLfloat x_position, GLfloat y_position,
                      GLfloat z_position, unsigned width,
                      unsigned height);

static struct matrix matrix_multiply(struct matrix a, struct matrix b);

static linted_error get_gl_error(void);

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
		EGLint err_egl = eglGetError();
		switch (err_egl) {
		/* In this case no display was found. */
		case EGL_SUCCESS:
			err = LINTED_ERROR_INVALID_PARAMETER;
			break;

		case EGL_BAD_ALLOC:
			err = LINTED_ERROR_OUT_OF_MEMORY;
			break;

		default:
			LINTED_ASSERT(false);
		}
		goto release_thread;
	}

	if (EGL_FALSE == eglInitialize(display, 0, 0)) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		/* Shouldn't happen */
		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);

		switch (err_egl) {
		case EGL_NOT_INITIALIZED:
		case EGL_BAD_ALLOC:
			err = LINTED_ERROR_OUT_OF_MEMORY;
			goto destroy_display;
		}

		LINTED_ASSERT(false);
	}

	EGLConfig config;
	EGLint matching_config_count;
	{
		EGLConfig xx;
		EGLint yy;
		if (EGL_FALSE ==
		    eglChooseConfig(display, attr_list, &xx, 1U, &yy))
			goto choose_config_failed;
		config = xx;
		matching_config_count = yy;
		goto choose_config_succeeded;
	}

choose_config_failed : {
	EGLint err_egl = eglGetError();
	LINTED_ASSUME(err_egl != EGL_SUCCESS);

	LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
	LINTED_ASSERT(err_egl != EGL_BAD_ATTRIBUTE);
	LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);
	LINTED_ASSERT(err_egl != EGL_BAD_PARAMETER);

	switch (err_egl) {
	case EGL_BAD_ALLOC:
		err = LINTED_ERROR_OUT_OF_MEMORY;
		goto destroy_display;
	}

	LINTED_ASSERT(false);
}

choose_config_succeeded:
	if (matching_config_count < 1) {
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto destroy_display;
	}

	{
		struct privates xx = {0};
		xx.display = display;
		xx.config = config;

		xx.surface = EGL_NO_SURFACE;

		xx.width = 1U;
		xx.height = 1U;
		xx.resize_pending = true;

		gpu_context->privates = xx;
	}

	{
		struct command_queue xx = {0};
		gpu_context->command_queue = xx;
		pthread_spin_init(&gpu_context->command_queue.lock,
		                  false);
	}

	err = pthread_create(&gpu_context->thread, 0, gpu_routine,
	                     &gpu_context->command_queue);
	if (err != 0)
		goto destroy_display;

	gpu_context->display = display;
	gpu_context->config = config;

	*gpu_contextp = gpu_context;

	return 0;

destroy_display:
	if (EGL_FALSE == eglTerminate(display)) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(false);
	}

release_thread:
	if (EGL_FALSE == eglReleaseThread()) {
		/* There are no given conditions in the standard this
		 * is possible for and to my knowledge no
		 * implementation gives special conditions for this to
		 * happen.
		 */
		LINTED_ASSERT(false);
	}

	linted_mem_free(gpu_context);

	return err;
}

linted_error
linted_gpu_context_destroy(struct linted_gpu_context *gpu_context)
{
	linted_error err = 0;

	struct command_queue *command_queue =
	    &gpu_context->command_queue;

	{
		pthread_spin_lock(&command_queue->lock);
		command_queue->time_to_quit = true;
		pthread_spin_unlock(&command_queue->lock);
	}

	pthread_join(gpu_context->thread, 0);

	return err;
}

linted_error
linted_gpu_set_x11_window(struct linted_gpu_context *gpu_context,
                          linted_gpu_x11_window new_window)
{
	if (new_window > UINT32_MAX)
		return LINTED_ERROR_INVALID_PARAMETER;

	struct command_queue *command_queue =
	    &gpu_context->command_queue;

	{
		pthread_spin_lock(&command_queue->lock);
		command_queue->remove_window = false;
		command_queue->has_new_window = true;
		command_queue->window = new_window;
		pthread_spin_unlock(&command_queue->lock);
	}

	return 0;
}

linted_error
linted_gpu_remove_window(struct linted_gpu_context *gpu_context)
{
	struct command_queue *command_queue =
	    &gpu_context->command_queue;

	{
		pthread_spin_lock(&command_queue->lock);
		command_queue->remove_window = true;
		pthread_spin_unlock(&command_queue->lock);
	}

	return 0;
}

void linted_gpu_update_state(struct linted_gpu_context *gpu_context,
                             struct linted_gpu_update const *glupdate)
{
	struct command_queue *command_queue =
	    &gpu_context->command_queue;

	{
		pthread_spin_lock(&command_queue->lock);
		command_queue->update = *glupdate;
		command_queue->update_pending = true;
		++command_queue->skipped_updates_counter;
		pthread_spin_unlock(&command_queue->lock);
	}
}

void linted_gpu_resize(struct linted_gpu_context *gpu_context,
                       unsigned width, unsigned height)
{
	struct command_queue *command_queue =
	    &gpu_context->command_queue;

	{
		pthread_spin_lock(&command_queue->lock);
		command_queue->width = width;
		command_queue->height = height;
		command_queue->resize_pending = true;
		pthread_spin_unlock(&command_queue->lock);
	}
}

static struct timespec timespec_subtract(struct timespec x,
                                         struct timespec y)
{
	struct timespec result = {0};

	long const second = 1000000000;

	if (x.tv_nsec < y.tv_nsec) {
		long nsec = (y.tv_nsec - x.tv_nsec) / second + 1;
		y.tv_nsec -= second * nsec;
		y.tv_sec += nsec;
	}

	if (x.tv_nsec - y.tv_nsec > second) {
		long nsec = (x.tv_nsec - y.tv_nsec) / second;
		y.tv_nsec += second * nsec;
		y.tv_sec -= nsec;
	}

	result.tv_sec = x.tv_sec - y.tv_sec;
	result.tv_nsec = x.tv_nsec - y.tv_nsec;

	return result;
}

static void *gpu_routine(void *arg)
{
	linted_error err = 0;

	struct linted_gpu_context *gpu_context = arg;
	struct command_queue *command_queue =
	    &gpu_context->command_queue;
	struct privates *privates = &gpu_context->privates;

	struct timespec last_time = {0};

	for (;;) {
		linted_gpu_x11_window new_window;
		struct linted_gpu_update update;
		unsigned width;
		unsigned height;
		uint64_t skipped_updates_counter = 0U;

		bool time_to_quit;
		bool has_new_window;
		bool remove_window;
		bool update_pending;
		bool resize_pending;
		{
			pthread_spin_lock(&command_queue->lock);

			time_to_quit = command_queue->time_to_quit;
			has_new_window = command_queue->has_new_window;
			remove_window = command_queue->remove_window;
			update_pending = command_queue->update_pending;
			resize_pending = command_queue->resize_pending;

			if (has_new_window)
				new_window = command_queue->window;

			if (update_pending) {
				update = command_queue->update;
				skipped_updates_counter =
				    command_queue
				        ->skipped_updates_counter;
			}

			if (resize_pending) {
				width = command_queue->width;
				height = command_queue->height;
			}

			command_queue->skipped_updates_counter = 0U;

			command_queue->time_to_quit = false;
			command_queue->has_new_window = false;
			command_queue->remove_window = false;
			command_queue->update_pending = false;
			command_queue->resize_pending = false;

			pthread_spin_unlock(&command_queue->lock);
		}

		if (time_to_quit)
			break;

		if (remove_window) {
			destroy_egl_context(privates);
			privates->has_window = false;
		}

		if (has_new_window) {
			destroy_egl_context(privates);
			privates->window = new_window;
			privates->has_window = true;
		}

		if (update_pending) {
			privates->update = update;
			privates->update_pending = true;
		}

		if (resize_pending) {
			privates->width = width;
			privates->height = height;
			privates->resize_pending = true;
		}

		err = setup_gl(privates);
		if (err != 0) {
			sched_yield();
			continue;
		}

		real_draw(privates);

		{
			GLenum attachments[] = {GL_DEPTH, GL_STENCIL};
			glInvalidateFramebuffer(
			    GL_FRAMEBUFFER,
			    LINTED_ARRAY_SIZE(attachments),
			    attachments);
		}

		if (EGL_FALSE == eglSwapBuffers(privates->display,
		                                privates->surface)) {
			EGLint err_egl = eglGetError();
			LINTED_ASSUME(err_egl != EGL_SUCCESS);

			/* Shouldn't Happen */
			LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
			LINTED_ASSERT(err_egl != EGL_BAD_SURFACE);
			LINTED_ASSERT(err_egl != EGL_BAD_CONTEXT);
			LINTED_ASSERT(err_egl != EGL_BAD_MATCH);
			LINTED_ASSERT(err_egl != EGL_BAD_ACCESS);
			LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);
			LINTED_ASSERT(err_egl !=
			              EGL_BAD_CURRENT_SURFACE);

			/* Maybe the current surface or context can
			 * become invalidated somehow? */
			switch (err_egl) {
			case EGL_BAD_NATIVE_PIXMAP:
			case EGL_BAD_NATIVE_WINDOW:
			case EGL_CONTEXT_LOST:
				destroy_egl_context(privates);
				continue;

			case EGL_BAD_ALLOC:
				abort();
			}

			LINTED_ASSERT(false);
		}

		{
			GLenum attachments[] = {GL_COLOR, GL_DEPTH,
			                        GL_STENCIL};
			glInvalidateFramebuffer(
			    GL_FRAMEBUFFER,
			    LINTED_ARRAY_SIZE(attachments),
			    attachments);
		}

		if (1) {
			if (skipped_updates_counter > 2U)
				linted_log(LINTED_LOG_INFO,
				           "skipped updates: %lu",
				           skipped_updates_counter);
		}

		if (0) {
			struct timespec now;
			linted_sched_time(&now);

			struct timespec diff =
			    timespec_subtract(now, last_time);

			long const second = 1000000000;

			double nanoseconds =
			    diff.tv_sec * second + diff.tv_nsec;

			if (nanoseconds <= 0.0)
				nanoseconds = 1.0;

			linted_log(LINTED_LOG_INFO,
			           "FPS: %lf, SPF: %lf",
			           second / (double)nanoseconds,
			           nanoseconds / (double)second);

			last_time = now;
		}
	}

	destroy_egl_context(privates);

	return 0;
}

static linted_error destroy_gl(struct privates *privates)
{
	if (!privates->has_setup_gl)
		return 0;
	privates->has_setup_gl = false;

	GLuint vertex_buffer = privates->vertex_buffer;
	GLuint normal_buffer = privates->normal_buffer;
	GLuint index_buffer = privates->index_buffer;

	GLuint program = privates->program;

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

	{
		GLuint xx[] = {vertex_buffer, normal_buffer,
		               index_buffer};
		glDeleteBuffers(LINTED_ARRAY_SIZE(xx), xx);
	}

	glUseProgram(0);
	glDeleteProgram(program);

	return 0;
}

static linted_error remove_current_context(struct privates *privates)
{
	if (!privates->has_current_context)
		return 0;
	privates->has_current_context = false;

	linted_error err = 0;

	EGLDisplay display = privates->display;

	if (EGL_FALSE == eglMakeCurrent(display, EGL_NO_SURFACE,
	                                EGL_NO_SURFACE,
	                                EGL_NO_CONTEXT)) {

		EGLint err_egl = eglGetError();

		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		/* Shouldn't Happen */

		LINTED_ASSERT(err_egl != EGL_BAD_ACCESS);
		LINTED_ASSERT(err_egl != EGL_BAD_CONTEXT);
		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_BAD_MATCH);
		LINTED_ASSERT(err_egl != EGL_BAD_PARAMETER);
		LINTED_ASSERT(err_egl != EGL_BAD_SURFACE);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);

		/* Don't Apply */
		LINTED_ASSERT(err_egl != EGL_BAD_CURRENT_SURFACE);
		LINTED_ASSERT(err_egl != EGL_BAD_CONFIG);

		switch (err_egl) {
		default:
			LINTED_ASSERT(false);

		/* Maybe the current surface or context can
		 * become invalidated somehow? */
		case EGL_CONTEXT_LOST:
		case EGL_BAD_NATIVE_PIXMAP:
		case EGL_BAD_NATIVE_WINDOW:
			if (0 == err)
				err = LINTED_ERROR_INVALID_PARAMETER;
			break;

		case EGL_BAD_ALLOC:
			if (0 == err)
				err = LINTED_ERROR_OUT_OF_MEMORY;
			break;
		}
	}

	return err;
}

static linted_error destroy_egl_surface(struct privates *privates)
{
	destroy_gl(privates);

	remove_current_context(privates);

	if (!privates->has_egl_surface)
		return 0;
	privates->has_egl_surface = false;

	EGLDisplay display = privates->display;
	EGLSurface surface = privates->surface;

	if (EGL_FALSE == eglDestroySurface(display, surface)) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_BAD_SURFACE);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);
		LINTED_ASSERT(false);
	}

	return 0;
}

static linted_error destroy_egl_context(struct privates *privates)
{
	destroy_gl(privates);

	remove_current_context(privates);

	destroy_egl_surface(privates);

	if (!privates->has_egl_context)
		return 0;
	privates->has_egl_context = false;

	EGLDisplay display = privates->display;
	EGLContext context = privates->context;

	if (EGL_FALSE == eglDestroyContext(display, context)) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_BAD_CONTEXT);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);
		LINTED_ASSERT(false);
	}

	return 0;
}

static linted_error create_egl_context(struct privates *privates)
{
	if (privates->has_egl_context)
		return 0;

	EGLDisplay display = privates->display;
	EGLConfig config = privates->config;

	EGLContext context = eglCreateContext(
	    display, config, EGL_NO_CONTEXT, context_attr);
	if (EGL_NO_CONTEXT == context) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		/* Shouldn't Happen */
		LINTED_ASSERT(err_egl != EGL_BAD_ACCESS);
		LINTED_ASSERT(err_egl != EGL_BAD_ATTRIBUTE);
		LINTED_ASSERT(err_egl != EGL_BAD_CONFIG);
		LINTED_ASSERT(err_egl != EGL_BAD_CONTEXT);
		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_BAD_MATCH);
		LINTED_ASSERT(err_egl != EGL_BAD_PARAMETER);
		LINTED_ASSERT(err_egl != EGL_BAD_SURFACE);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);

		switch (err_egl) {
		case EGL_BAD_ALLOC:
			return LINTED_ERROR_OUT_OF_MEMORY;
		}

		LINTED_ASSERT(false);
	}
	privates->context = context;
	privates->has_egl_context = true;

	return 0;
}

static linted_error create_egl_surface(struct privates *privates)
{
	if (privates->has_egl_surface)
		return 0;

	if (!privates->has_window)
		return LINTED_ERROR_INVALID_PARAMETER;

	EGLDisplay display = privates->display;
	EGLConfig config = privates->config;
	linted_gpu_x11_window window = privates->window;

	EGLSurface surface =
	    eglCreateWindowSurface(display, config, window, 0);
	if (EGL_NO_SURFACE == surface) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);
		LINTED_ASSERT(err_egl != EGL_BAD_ATTRIBUTE);
		LINTED_ASSERT(err_egl != EGL_BAD_CONFIG);
		LINTED_ASSERT(err_egl != EGL_BAD_MATCH);

		switch (err_egl) {
		case EGL_BAD_NATIVE_WINDOW:
			return LINTED_ERROR_INVALID_PARAMETER;

		case EGL_BAD_ALLOC:
			return LINTED_ERROR_OUT_OF_MEMORY;
		}

		LINTED_ASSERT(false);
	}

	if (EGL_FALSE == eglSurfaceAttrib(display, surface,
	                                  EGL_SWAP_BEHAVIOR,
	                                  EGL_BUFFER_DESTROYED)) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);
		LINTED_ASSERT(err_egl != EGL_BAD_ATTRIBUTE);
		LINTED_ASSERT(err_egl != EGL_BAD_MATCH);

		LINTED_ASSERT(false);
	}

	privates->surface = surface;
	privates->has_egl_surface = true;

	return 0;
}

static linted_error make_current(struct privates *privates)
{
	if (privates->has_current_context)
		return 0;

	linted_error err = 0;

	err = create_egl_context(privates);
	if (err != 0)
		return err;

	err = create_egl_surface(privates);
	if (err != 0)
		return err;

	EGLDisplay display = privates->display;
	EGLSurface surface = privates->surface;
	EGLContext context = privates->context;

	if (EGL_FALSE ==
	    eglMakeCurrent(display, surface, surface, context)) {

		EGLint err_egl = eglGetError();

		LINTED_ASSUME(err_egl != EGL_SUCCESS);

		/* Shouldn't Happen */

		LINTED_ASSERT(err_egl != EGL_BAD_ACCESS);
		LINTED_ASSERT(err_egl != EGL_BAD_CONTEXT);
		LINTED_ASSERT(err_egl != EGL_BAD_DISPLAY);
		LINTED_ASSERT(err_egl != EGL_BAD_MATCH);
		LINTED_ASSERT(err_egl != EGL_BAD_PARAMETER);
		LINTED_ASSERT(err_egl != EGL_BAD_SURFACE);
		LINTED_ASSERT(err_egl != EGL_NOT_INITIALIZED);

		/* Don't Apply */
		LINTED_ASSERT(err_egl != EGL_BAD_CURRENT_SURFACE);
		LINTED_ASSERT(err_egl != EGL_BAD_CONFIG);

		switch (err_egl) {
		default:
			LINTED_ASSERT(false);

		/* Maybe the current surface or context can
		 * become invalidated somehow? */
		case EGL_CONTEXT_LOST:
		case EGL_BAD_NATIVE_PIXMAP:
		case EGL_BAD_NATIVE_WINDOW:
			if (0 == err)
				err = LINTED_ERROR_INVALID_PARAMETER;
			break;

		case EGL_BAD_ALLOC:
			if (0 == err)
				err = LINTED_ERROR_OUT_OF_MEMORY;
			break;
		}
	}
	if (err != 0)
		return err;

	if (EGL_FALSE == eglSwapInterval(display, 1)) {
		EGLint err_egl = eglGetError();
		LINTED_ASSUME(err_egl != EGL_SUCCESS);
		switch (err_egl) {
		case EGL_NOT_INITIALIZED:
		case EGL_BAD_ALLOC:
			err = LINTED_ERROR_OUT_OF_MEMORY;
			return err;
		default:
			LINTED_ASSERT(false);
		}
	}

	privates->has_current_context = true;
	return 0;
}

static linted_error setup_gl(struct privates *privates)
{
	if (privates->has_setup_gl)
		return 0;

	linted_error err = 0;

	err = make_current(privates);
	if (err != 0)
		return err;

	glHint(GL_GENERATE_MIPMAP_HINT, GL_NICEST);
	glHint(GL_FRAGMENT_SHADER_DERIVATIVE_HINT, GL_NICEST);

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
		err = LINTED_ERROR_INVALID_PARAMETER;

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
		err = LINTED_ERROR_INVALID_PARAMETER;

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
		err = LINTED_ERROR_INVALID_PARAMETER;

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

	GLint eye_vertex = glGetUniformLocation(program, "eye_vertex");

	GLint mvp_matrix = glGetUniformLocation(
	    program, "model_view_projection_matrix");

	GLint maybe_vertex = glGetAttribLocation(program, "vertex");
	if (maybe_vertex < 0) {
		err = LINTED_ERROR_INVALID_PARAMETER;
		goto cleanup_buffers;
	}
	GLuint vertex = maybe_vertex;

	GLint maybe_normal = glGetAttribLocation(program, "normal");
	if (maybe_normal < 0) {
		err = LINTED_ERROR_INVALID_PARAMETER;
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

	glClearColor(0.94, 0.9, 1.0, 0.0);

	privates->program = program;
	privates->vertex_buffer = vertex_buffer;
	privates->normal_buffer = normal_buffer;
	privates->index_buffer = index_buffer;
	privates->model_view_projection_matrix = mvp_matrix;
	privates->eye_vertex = eye_vertex;

	privates->update_pending = true;
	privates->resize_pending = true;

	privates->has_setup_gl = true;

	return 0;

cleanup_buffers : {
	GLuint xx[] = {vertex_buffer, normal_buffer, index_buffer};
	glDeleteBuffers(LINTED_ARRAY_SIZE(xx), xx);
}

cleanup_program:
	glDeleteProgram(program);

	return err;
}

static void real_draw(struct privates *privates)
{
	struct linted_gpu_update const *update = &privates->update;

	unsigned width = privates->width;
	unsigned height = privates->height;

	bool update_pending = privates->update_pending;
	bool resize_pending = privates->resize_pending;

	GLint mvp_matrix = privates->model_view_projection_matrix;
	GLint eye_vertex = privates->eye_vertex;

	if (update_pending || resize_pending) {
		/* X, Y, Z, W coords of the resultant vector are the
		 * sums of the columns (row major order).
		 */

		GLfloat z_rotation = update->z_rotation;
		GLfloat x_rotation = update->x_rotation;

		GLfloat x_position = update->x_position;
		GLfloat y_position = update->y_position;
		GLfloat z_position = update->z_position;

		if (mvp_matrix >= 0) {
			struct matrix mvp = model_view_projection(
			    x_rotation, z_rotation, x_position,
			    y_position, z_position, width, height);
			glUniformMatrix4fv(mvp_matrix, 1U, false,
			                   (void const *)&mvp);
		}
	}

	if (resize_pending) {
		glViewport(0, 0, width, height);
		privates->resize_pending = false;
	}

	if (update_pending) {
		GLfloat x_position = update->x_position;
		GLfloat y_position = update->y_position;
		GLfloat z_position = update->z_position;

		if (eye_vertex >= 0)
			glUniform3f(eye_vertex, x_position, y_position,
			            z_position);
		privates->update_pending = false;
	}

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT |
	        GL_STENCIL_BUFFER_BIT);
	glDrawElements(GL_TRIANGLES, 3U * linted_assets_indices_size,
	               GL_UNSIGNED_SHORT, 0);
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
		return LINTED_ERROR_OUT_OF_MEMORY;

	return 0;
}

static struct matrix
model_view_projection(GLfloat x_rotation, GLfloat z_rotation,
                      GLfloat x_position, GLfloat y_position,
                      GLfloat z_position, unsigned width,
                      unsigned height)
{
	/* Rotate the camera */
	GLfloat cos_x = cosf(x_rotation);
	GLfloat sin_x = sinf(x_rotation);

	GLfloat cos_z = cosf(z_rotation);
	GLfloat sin_z = sinf(z_rotation);

	double aspect = width / (double)height;
	double fov = acos(-1.0) / 4;

	double d = 1 / tan(fov / 2);
	double far = 1000;
	double near = 1;

	{
		struct matrix model_view;
		{
			struct matrix rotations;
			{
				struct matrix const x_rotation_matrix =
				    {{{1, 0, 0, 0},
				      {0, cos_x, -sin_x, 0},
				      {0, sin_x, cos_x, 0},
				      {0, 0, 0, 1}}};

				struct matrix const z_rotation_matrix =
				    {{{cos_z, sin_z, 0, 0},
				      {-sin_z, cos_z, 0, 0},
				      {0, 0, 1, 0},
				      {0, 0, 0, 1}}};

				rotations =
				    matrix_multiply(z_rotation_matrix,
				                    x_rotation_matrix);
			}

			/* Translate the camera */
			struct matrix const camera = {
			    {{1, 0, 0, 0},
			     {0, 1, 0, 0},
			     {0, 0, 1, 0},
			     {x_position, y_position, z_position, 1}}};

			model_view = matrix_multiply(camera, rotations);
		}

		struct matrix const projection = {
		    {{d / aspect, 0, 0, 0},
		     {0, d, 0, 0},
		     {0, 0, (far + near) / (near - far),
		      2 * far * near / (near - far)},
		     {0, 0, -1, 0}}};

		return matrix_multiply(model_view, projection);
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
