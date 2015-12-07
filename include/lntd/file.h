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
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
#ifndef LNTD_FILE_H
#define LNTD_FILE_H

#include "lntd/error.h"
#include "lntd/ko.h"

#include <sys/types.h>

/**
 * @file
 *
 * Abstracts over the concept of a regular filesystem file.
 */

typedef lntd_ko lntd_file;

#define LNTD_FILE_RDONLY 1UL
#define LNTD_FILE_WRONLY (1UL << 1U)
#define LNTD_FILE_RDWR (1UL << 2U)

#define LNTD_FILE_SYNC (1UL << 3U)

#define LNTD_FILE_EXCL (1UL << 4U)

lntd_error lntd_file_create(lntd_file *filep, lntd_ko dirko,
                            char const *pathname, unsigned long flags,
                            mode_t mode);

#endif /* LNTD_FILE_H */
