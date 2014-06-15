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
#ifndef LINTED_FILE_H
#define LINTED_FILE_H

#include "linted/error.h"
#include "linted/ko.h"

#include <sys/types.h>

/**
 * @file
 *
 * Abstracts over the concept of a regular filesystem file.
 */

typedef linted_ko linted_file;

#define LINTED_FILE_RDONLY 1
#define LINTED_FILE_WRONLY (1 << 2)
#define LINTED_FILE_RDWR (1 << 3)

#define LINTED_FILE_SYNC (1 << 4)

#define LINTED_FILE_EXCL (1 << 5)

linted_error linted_file_create(linted_file *filep, linted_ko dirko,
                                char const *pathname, int flags, mode_t mode);

#endif /* LINTED_FILE_H */
