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
    if (0u == size) {
        size = 1u;
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
    if (size != 0u && SIZE_MAX / size < nmemb) {
        return ENOMEM;
    }

    size_t total = size * nmemb;
    if (0u == total) {
        total = 1u;
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
    if (0u == size) {
        size = 1u;
    }

    void *memory = calloc(1u, size);
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
    if (0u == nmemb || 0u == size) {
        void * memory = malloc(1u);
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
    if (0u == new_size) {
        new_size = 1u;
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
    if (size != 0u && SIZE_MAX / size < nmemb) {
        return ENOMEM;
    }

    size_t total = size * nmemb;
    if (0u == total) {
        total = 1u;
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
