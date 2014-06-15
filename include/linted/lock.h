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
#ifndef LINTED_LOCK_H
#define LINTED_LOCK_H

#include "linted/error.h"
#include "linted/ko.h"

#include <sys/types.h>

/**
 * @file
 *
 * Abstracts over the concept of a lock file.
 */

#if defined _WIN32 || defined _WIN64
typedef void *linted_lock;
#elif defined __linux__
typedef pid_t linted_lock;
#else
#error no implemented lock for this platform
#endif

#define LINTED_LOCK_RDONLY 1
#define LINTED_LOCK_WRONLY (1 << 2)
#define LINTED_LOCK_RDWR (1 << 3)

#define LINTED_LOCK_EXCL (1 << 4)

linted_error linted_lock_file_create(linted_ko *kop, linted_ko dirko,
                                     char const *pathname, int flags,
                                     mode_t mode);

linted_error linted_lock_acquire(linted_lock *lockp, linted_ko lock_file);
linted_error linted_lock_release(linted_lock lock);

#endif /* LINTED_LOCK_H */
