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
#ifndef LINTED_MEM_H
#define LINTED_MEM_H

#include "linted/error.h"
#include "linted/util.h"

#include <errno.h>
#include <stddef.h>

/**
 * @file
 *
 * Allocates memory.
 */

static inline linted_error linted_mem_safe_multiply(size_t nmemb, size_t size, size_t *resultp)
{
	if (size > 0U && ((size_t)-1) / size < nmemb)
		return ENOMEM;

	*resultp = nmemb * size;
	return 0;
}

static inline linted_error linted_mem_alloc(void **memp, size_t size)
{
	extern void *malloc(size_t size);

	void *memory = malloc(size);
	if (size > 0U && 0 == memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

static inline linted_error linted_mem_alloc_array(void **memp, size_t nmemb, size_t size)
{
	extern void *malloc(size_t size);

	linted_error errnum;

	size_t total;
	errnum = linted_mem_safe_multiply(nmemb, size, &total);
	if (errnum != 0)
		return errnum;

	void *memory = malloc(total);
	if (total > 0U && 0 == memory) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

static inline linted_error linted_mem_alloc_zeroed(void **memp, size_t size)
{
	extern void *calloc(size_t nmemb, size_t size);

	void *memory = calloc(1U, size);
	if (size > 0U && 0 == memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

static inline linted_error linted_mem_alloc_array_zeroed(void **memp, size_t nmemb,
                                           size_t size)
{
	extern void *calloc(size_t nmemb, size_t size);

	void *memory = calloc(nmemb, size);
	if (nmemb > 0U && size > 0U && 0 == memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;

	return 0;
}

static inline linted_error linted_mem_realloc(void **memp, void *memory, size_t new_size)
{
	extern void *realloc(void *ptr, size_t size);

	void *new_memory = realloc(memory, new_size);
	if (new_size > 0U && 0 == new_memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = new_memory;
	return 0;
}

static inline linted_error linted_mem_realloc_array(void **memp, void *memory, size_t nmemb,
                                      size_t size)
{
	extern void *realloc(void *ptr, size_t size);

	linted_error errnum;

	size_t total;
	errnum = linted_mem_safe_multiply(nmemb, size, &total);
	if (errnum != 0)
		return errnum;

	void *new_memory = realloc(memory, total);
	if (total > 0U && 0 == new_memory) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = new_memory;
	return 0;
}

static inline void linted_mem_free(void *memory)
{
       extern void free(void *ptr);
       free(memory);
}

#endif /* LINTED_MEM_H */
