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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LINTED_MEM_H
#define LINTED_MEM_H

#include "linted/error.h"

#include <stddef.h>

/**
 * @file
 *
 * Allocates memory.
 */

/* For consistent platform behaviour we return the null pointer on
 * zero sized allocations.
 */

static inline linted_error
linted_mem_safe_multiply(size_t nmemb, size_t size, size_t *resultp)
{
	if (size > 0U && ((size_t)-1) / size < nmemb)
		return LINTED_ERROR_OUT_OF_MEMORY;

	*resultp = nmemb *size;
	return 0;
}

static inline linted_error linted_mem_alloc(void **memp, size_t size)
{
	extern void *malloc(size_t size);

	void *memory;
	if (0U == size) {
		memory = 0;
	} else {
		memory = malloc(size);
		if (0 == memory)
			return LINTED_ERROR_OUT_OF_MEMORY;
	}

	*memp = memory;
	return 0;
}

static inline linted_error
linted_mem_alloc_array(void **memp, size_t nmemb, size_t size)
{
	extern void *malloc(size_t size);

	linted_error err;

	size_t total;
	err = linted_mem_safe_multiply(nmemb, size, &total);
	if (err != 0)
		return err;

	void *memory;
	if (0U == size) {
		memory = 0;
	} else {
		memory = malloc(total);
		if (0 == memory)
			return LINTED_ERROR_OUT_OF_MEMORY;
	}

	*memp = memory;
	return 0;
}

static inline linted_error linted_mem_alloc_zeroed(void **memp,
                                                   size_t size)
{
	extern void *calloc(size_t nmemb, size_t size);

	void *memory;
	if (0U == size) {
		memory = 0;
	} else {
		memory = calloc(1U, size);
		if (0 == memory)
			return LINTED_ERROR_OUT_OF_MEMORY;
	}

	*memp = memory;
	return 0;
}

static inline linted_error
linted_mem_alloc_array_zeroed(void **memp, size_t nmemb, size_t size)
{
	extern void *calloc(size_t nmemb, size_t size);

	void *memory;
	if (0U == nmemb || 0U == size) {
		memory = 0;
	} else {
		memory = calloc(nmemb, size);
		if (0 == memory)
			return LINTED_ERROR_OUT_OF_MEMORY;
	}
	*memp = memory;
	return 0;
}

static inline linted_error linted_mem_realloc(void **memp, void *memory,
                                              size_t new_size)
{
	extern void *realloc(void *ptr, size_t size);
	extern void free(void *ptr);

	void *new_memory;
	if (0U == new_size) {
		free(memory);
		new_memory = 0;
	} else {
		new_memory = realloc(memory, new_size);
		if (0 == new_memory)
			return LINTED_ERROR_OUT_OF_MEMORY;
	}

	*memp = new_memory;
	return 0;
}

static inline linted_error linted_mem_realloc_array(void **memp,
                                                    void *memory,
                                                    size_t nmemb,
                                                    size_t size)
{
	extern void *realloc(void *ptr, size_t size);
	extern void free(void *ptr);

	linted_error err;

	size_t total;
	err = linted_mem_safe_multiply(nmemb, size, &total);
	if (err != 0)
		return err;

	void *new_memory;
	if (0U == total) {
		free(memory);
		new_memory = 0;
	} else {
		new_memory = realloc(memory, total);
		if (0 == new_memory)
			return LINTED_ERROR_OUT_OF_MEMORY;
	}

	*memp = new_memory;
	return 0;
}

static inline void linted_mem_free(void *memory)
{
	extern void free(void *ptr);

	/* This is primarily for making debugging easier and not for
	 * any sort of optimization */
	if (memory != 0)
		free(memory);
}

#endif /* LINTED_MEM_H */
