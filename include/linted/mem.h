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

#include <stddef.h>

/**
 * @file
 *
 * Allocates memory.
 */

/*@
  requires \valid(memp);
 */
linted_error linted_mem_alloc(void ** restrict memp, size_t size);

/*@
  requires \valid(memp);
 */
linted_error linted_mem_alloc_array(void ** restrict memp, size_t nmemb,
                                    size_t size);

linted_error linted_mem_alloc_zeroed(void ** restrict memp,
                                     size_t size);
linted_error linted_mem_alloc_array_zeroed(void ** restrict memp, size_t nmemb,
                                           size_t size);

linted_error linted_mem_realloc(void ** restrict memp, void * restrict memory,
                                size_t new_size);
linted_error linted_mem_realloc_array(void ** restrict memp,
                                      void * restrict memory,
                                      size_t nmemb, size_t size);

void linted_mem_free(void * restrict memory);

#endif /* LINTED_MEM_H */
