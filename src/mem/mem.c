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
#include "config.h"

#include "linted/mem.h"
#include "linted/util.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

static linted_error safe_multiply(size_t nmemb, size_t size, size_t *resultp);

linted_error linted_mem_alloc(void **memp, size_t size)
{
	if (0U == size)
		size = 1U;

	void *memory = malloc(size);
	if (NULL == memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

linted_error linted_mem_alloc_array(void **memp, size_t nmemb, size_t size)
{
	linted_error errnum;

	size_t total;
	errnum = safe_multiply(nmemb, size, &total);
	if (errnum != 0)
		return errnum;

	void *memory = malloc(total);
	if (NULL == memory) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

linted_error linted_mem_alloc_zeroed(void **memp, size_t size)
{
	if (0U == size)
		size = 1U;

	void *memory = calloc(1U, size);
	if (NULL == memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

linted_error linted_mem_alloc_array_zeroed(void **memp, size_t nmemb,
                                           size_t size)
{
	if (0U == nmemb || 0U == size) {
		void *memory = malloc(1U);
		if (NULL == memory) {
			linted_error errnum = errno;
			LINTED_ASSUME(errnum != 0);
			return errnum;
		}
		*memp = memory;
		return 0;
	}

	void *memory = calloc(nmemb, size);
	if (NULL == memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = memory;
	return 0;
}

linted_error linted_mem_realloc(void **memp, void *memory, size_t new_size)
{
	if (0U == new_size)
		new_size = 1U;

	void *new_memory = realloc(memory, new_size);
	if (NULL == new_memory) {
		linted_error errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = new_memory;
	return 0;
}

linted_error linted_mem_realloc_array(void **memp, void *memory, size_t nmemb,
                                      size_t size)
{
	linted_error errnum;

	size_t total;
	errnum = safe_multiply(nmemb, size, &total);
	if (errnum != 0)
		return errnum;

	void *new_memory = realloc(memory, total);
	if (NULL == new_memory) {
		errnum = errno;
		LINTED_ASSUME(errnum != 0);
		return errnum;
	}

	*memp = new_memory;
	return 0;
}

void linted_mem_free(void *memory)
{
	free(memory);
}

static linted_error safe_multiply(size_t nmemb, size_t size, size_t *resultp)
{
	size_t result;

	size_t mul_no_overflow = 1UL << (sizeof(size_t) * 4U);
	if (0U == size) {
		result = 1U;
		/*@ assert result >= size; */
		/*@ assert result >= nmemb * size; */
	} else if (0U == nmemb) {
		result = 1U;
		/*@ assert result >= nmemb; */
		/*@ assert result >= nmemb * size; */
	} else {
		size_t xx;
		size_t yy;

		if (nmemb >= mul_no_overflow)
			goto nmemb_greater;

		if (size >= mul_no_overflow)
			goto size_greater;

		goto none_greater;

		switch (0) {
		nmemb_greater:
			if (size >= mul_no_overflow)
				return ENOMEM;

			xx = size;
			yy = nmemb;
			goto check_mul;

		size_greater:
			xx = nmemb;
			yy = size;
			goto check_mul;

		check_mul:
			if (SIZE_MAX / xx >= yy)
				return ENOMEM;

			result = xx * yy;
			break;

		none_greater:
			result = nmemb * size;
			break;
		}
	}

	*resultp = result;
	return 0;
}
