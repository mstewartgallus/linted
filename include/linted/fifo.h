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
#ifndef LINTED_FIFO_H
#define LINTED_FIFO_H

#include "linted/error.h"
#include "linted/ko.h"

#include <sys/types.h>

/**
 * @file
 *
 * Abstracts over the concept of a filesystem pipe.
 */

typedef linted_ko linted_fifo;

#define LINTED_FIFO_RDONLY 1UL
#define LINTED_FIFO_WRONLY (1UL << 1U)
#define LINTED_FIFO_RDWR (1UL << 2U)

linted_error linted_fifo_create(linted_fifo *fifop, linted_ko dirko,
                                char const *pathname, unsigned long flags,
                                mode_t mode);

#endif /* LINTED_FIFO_H */
