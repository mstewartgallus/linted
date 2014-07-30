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

#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>

linted_error linted_mem_alloc(void **memp, size_t size)
{
    if (0U == size) {
        size = 1U;
    }

    void *memory = malloc(size);
    if (NULL == memory) {
        linted_error errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    *memp = memory;
    return 0;
}

linted_error linted_mem_alloc_array(void **memp, size_t nmemb, size_t size)
{
    if (size != 0U && SIZE_MAX / size < nmemb) {
        return ENOMEM;
    }

    size_t total = size * nmemb;
    if (0U == total) {
        total = 1U;
    }

    void *memory = malloc(total);
    if (NULL == memory) {
        linted_error errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    *memp = memory;
    return 0;
}

linted_error linted_mem_alloc_zeroed(void **memp, size_t size)
{
    if (0U == size) {
        size = 1U;
    }

    void *memory = calloc(1U, size);
    if (NULL == memory) {
        linted_error errnum = errno;
        assert(errnum != 0);
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
            assert(errnum != 0);
            return errnum;
        }
        *memp = memory;
        return 0;
    }

    void *memory = calloc(nmemb, size);
    if (NULL == memory) {
        linted_error errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    *memp = memory;
    return 0;
}

linted_error linted_mem_realloc(void **memp, void *memory, size_t new_size)
{
    if (0U == new_size) {
        new_size = 1U;
    }

    void *new_memory = realloc(memory, new_size);
    if (NULL == new_memory) {
        linted_error errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    *memp = new_memory;
    return 0;
}

linted_error linted_mem_realloc_array(void **memp, void *memory, size_t nmemb,
                                      size_t size)
{
    if (size != 0U && SIZE_MAX / size < nmemb) {
        return ENOMEM;
    }

    size_t total = size * nmemb;
    if (0U == total) {
        total = 1U;
    }

    void *new_memory = realloc(memory, total);
    if (NULL == new_memory) {
        linted_error errnum = errno;
        assert(errnum != 0);
        return errnum;
    }

    *memp = new_memory;
    return 0;
}

void linted_mem_free(void *memory)
{
    free(memory);
}
